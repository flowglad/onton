#!/usr/bin/env python3
"""Public resource-v1 process fixture and independent physical/Git smoke oracle.

No inventory, historical-state read, or stream reconnect operation is exposed.
Private test orchestration observes child PIDs, backend events and Git directly.
This is local executable coverage, not the complete FC-16/provider timing matrix.
"""
import base64
import copy
import json
import os
from pathlib import Path
import selectors
import signal
import shutil
import socket
import socketserver
import subprocess
import sys
import tempfile
import threading
import time


def git(path, *args):
    return subprocess.check_output(["git", "-C", str(path), *args], stderr=subprocess.PIPE, text=True).strip()


ACTIVE = None


def wait_for(label, predicate, seconds=90):
    deadline = time.monotonic() + seconds
    while time.monotonic() < deadline:
        result = predicate()
        if result:
            return result
        if ACTIVE is not None and ACTIVE.poll() is not None:
            raise AssertionError('coordinator exited early: ' + str(ACTIVE.returncode))
        time.sleep(0.05)
    raise AssertionError("deadline: " + label)


def send(sock, value):
    sock.sendall((json.dumps(value, separators=(",", ":")) + "\n").encode())


def line(sock):
    data = bytearray()
    while not data.endswith(b"\n"):
        available = sock.recv(65536, socket.MSG_PEEK)
        if not available:
            raise EOFError("fixture response lost")
        end = available.find(b"\n")
        part = sock.recv(end + 1 if end >= 0 else len(available))
        data.extend(part)
        if len(data) > 24 * 1024 * 1024:
            raise ValueError("fixture bound")
    return json.loads(data)


def helper():
    root = Path(os.environ["ONTON_PROCESS_FIXTURE"])
    stream = len(sys.argv) > 2 and sys.argv[2] == "connect"
    with socket.socket(socket.AF_UNIX) as sock:
        sock.connect(str(root / "rpc"))
        if stream:
            run, owner, epoch, revision, request, resource = sys.argv[3:]
            send(sock, dict(op="connect", run_id=run, coordinator_id=owner,
                            owner_epoch=epoch, revision=revision, request_id=request, resource_id=resource))
            if line(sock).get("status") != "ok":
                sys.exit(1)
            select = selectors.DefaultSelector()
            select.register(sock, selectors.EVENT_READ, 1)
            select.register(sys.stdin.buffer, selectors.EVENT_READ, 0)
            while True:
                for key, _ in select.select():
                    data = os.read(key.fileobj.fileno(), 65536)
                    if not data:
                        return
                    if key.data:
                        sys.stdout.buffer.write(data)
                        sys.stdout.buffer.flush()
                    else:
                        sock.sendall(data)
        else:
            request = json.loads(sys.stdin.readline())
            if len(sys.argv) == 1:
                request = {"forge": request}
            send(sock, request)
            print(json.dumps(line(sock)), flush=True)


class World:
    def __init__(self, root, remote, template):
        self.root, self.remote, self.template = root, remote, template
        self.lock = threading.RLock()
        self.owner, self.run, self.digest = None, None, None
        self.epoch, self.revision, self.version = 1, 1, 0
        self.payload = None
        self.receipts, self.resources, self.prs = {}, {}, {}
        self.closed = False
        self.drop_write = False
        self.refuse_receipt = False
        self.refuse_stop = False
        self.requests = []

    def reply(self, status="ok", reason="none", **fields):
        return dict(status=status, reason=reason, run_id=self.run, owner_epoch=self.epoch,
                    revision=self.revision, state_version=self.version, **fields)

    def stop(self, resource):
        process, directory, _ = self.resources[resource]
        backend = directory / "backend.pid"
        if backend.exists():
            try:
                os.kill(int(backend.read_text()), signal.SIGTERM)
            except ProcessLookupError:
                pass
        if process.poll() is None:
            try:
                os.killpg(process.pid, signal.SIGTERM)
            except PermissionError:
                process.terminate()
            try:
                process.wait(timeout=8)
            except subprocess.TimeoutExpired:
                try:
                    os.killpg(process.pid, signal.SIGKILL)
                except PermissionError:
                    process.kill()
                process.wait(timeout=5)
        assert process.poll() is not None, "retirement requires actual wait status"

    def resource(self, request):
        with self.lock:
            self.requests.append(copy.deepcopy(request))
            with open(self.root / 'requests', 'a') as log:
                log.write(json.dumps(dict(time=time.monotonic(), op=request['op'], version=self.version))+'\n')
            op = request["op"]
            if op == "open":
                if self.owner and (self.owner != request["coordinator_id"] or self.digest != request["config_digest"]):
                    return self.reply("rejected", "stale_guard")
                self.owner, self.run, self.digest = request["coordinator_id"], request["run_id"], request["config_digest"]
                return self.reply()
            if op == "takeover":
                if int(request["owner_epoch"]) != self.epoch or int(request["revision"]) != self.revision:
                    return self.reply("rejected", "stale_guard")
                self.owner = request["coordinator_id"]
                self.epoch += 1
                self.revision += 1
                return self.reply()
            if request["coordinator_id"] != self.owner or int(request["owner_epoch"]) != self.epoch:
                return self.reply("rejected", "stale_guard")
            if op == "write-status":
                if self.refuse_receipt:
                    return self.reply("pending", "storage_uncertain")
                receipt = self.receipts.get(request["write_id"])
                if not receipt:
                    return self.reply("rejected", "not_found")
                return self.reply(receipt_state_version=receipt[0])
            if int(request["revision"]) != self.revision:
                return self.reply("rejected", "stale_guard")
            if op == "state-read":
                if self.payload is None:
                    return self.reply("rejected", "not_found")
                return self.reply(payload=self.payload)
            if op == "state-write":
                if self.closed:
                    return self.reply("rejected", "closed")
                if int(request["state_version"]) != self.version:
                    return self.reply("rejected", "stale_guard")
                self.version += 1
                self.revision += 1
                self.payload = request["payload"]
                self.receipts[request["write_id"]] = (self.version, self.payload)
                if self.drop_write:
                    self.drop_write = False
                    return None
                return self.reply()
            if op in ("launch", "replace"):
                name = request["resource_id"]
                if name in self.resources:
                    return self.reply(resource_id=name)
                if op == "replace":
                    predecessor = self.resources[request["predecessor_id"]][0]
                    if predecessor.poll() is None:
                        return self.reply("pending", "retirement_unproven")
                directory = self.root / name
                directory.mkdir()
                manifest = request["manifest"]
                assert manifest["preparation"] == "coding"
                for item in manifest["inputs"]:
                    path = directory / item["path"]
                    assert ".." not in Path(item["path"]).parts and not Path(item["path"]).is_absolute()
                    path.parent.mkdir(parents=True, exist_ok=True)
                    path.write_bytes(base64.b64decode(item["data"], validate=True))
                subprocess.run(["git", "clone", "--no-local", "-q", str(self.remote), str(directory / "repo")], check=True)
                git(directory / "repo", "config", "user.name", "Process Fixture")
                git(directory / "repo", "config", "user.email", "fixture@example.invalid")
                (directory / 'forge.json').write_text(json.dumps({'branch': 'process-smoke/patch-' + request['patch_id'], 'state': 'not_merged'}))
                (directory / 'home').mkdir(mode=0o700)
                env = dict(os.environ, HOME=str(directory / 'home'), ONTON_DATA_DIR=str(directory / "data"), ONTON_FIXTURE_RESOURCE=str(directory),
                           ONTON_WORKER_FORGE_FIXTURE=str(directory / 'forge.json'))
                errors = open(directory / "stderr", "wb")
                process = subprocess.Popen([manifest["executable"], *manifest["args"]], cwd=directory,
                                           stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=errors,
                                           env=env, start_new_session=True)
                self.resources[name] = (process, directory, False)
                self.revision += 1
                return self.reply(resource_id=name)
            if op == "stop":
                if self.refuse_stop:
                    return self.reply("pending", "retirement_unproven")
                self.stop(request["resource_id"])
                self.revision += 1
                return self.reply(resource_id=request["resource_id"])
            if op == "close":
                if any(p.poll() is None for p, _, _ in self.resources.values()):
                    return self.reply("pending", "retirement_unproven")
                self.closed = True
                self.revision += 1
                return self.reply()
            if op == "observe":
                return self.reply(resource_id=request["resource_id"])
            raise AssertionError("non-public resource operation " + op)

    def forge(self, request):
        with self.lock:
            op = request["operation"]
            value = None
            if op == "check-access":
                pass
            elif op == "list-prs":
                value = [[n, p["base"], False] for n, p in self.prs.items() if p["head"] == request["branch"]]
            elif op == "create":
                assert not any(p["head"] == request["head"] for p in self.prs.values()), "duplicate owned PR"
                value = len(self.prs) + 1
                self.prs[value] = dict(request)
            elif op == "pr-state":
                pr = self.prs[request["pr"]]
                value = copy.deepcopy(self.template)
                value.update(pr_number=request["pr"], is_draft=pr["draft"], head_branch=pr["head"], base_branch=pr["base"],
                             head_oid=git(self.remote, "rev-parse", "refs/heads/" + pr["head"]),
                             base_oid=git(self.remote, "rev-parse", "refs/heads/" + pr["base"]))
            elif op == "set-body":
                self.prs[request["pr"]]["body"] = request["body"]
            elif op == "set-draft":
                self.prs[request["pr"]]["draft"] = request["draft"]
            elif op == "set-base":
                self.prs[request["pr"]]["base"] = request["base"]
            elif op == "queue-removal":
                value = []
            else:
                return {"ok": False, "error": "fixture operation unavailable"}
            return {"ok": True, "value": value}

    def connection(self, sock, request):
        with self.lock:
            name = request["resource_id"]
            process, directory, connected = self.resources[name]
            if connected or request["coordinator_id"] != self.owner or int(request["owner_epoch"]) != self.epoch:
                send(sock, self.reply("rejected", "stale_guard"))
                return
            self.resources[name] = process, directory, True
            send(sock, self.reply())
        select = selectors.DefaultSelector()
        select.register(sock, selectors.EVENT_READ, 0)
        select.register(process.stdout, selectors.EVENT_READ, 1)
        try:
            while True:
                for key, _ in select.select(timeout=1):
                    data = os.read(key.fileobj.fileno(), 65536)
                    if not data:
                        return
                    with open(directory / ('worker-stream' if key.data else 'host-stream'), 'ab') as transcript:
                        transcript.write(data)
                    if key.data:
                        sock.sendall(data)
                    else:
                        process.stdin.write(data)
                        process.stdin.flush()
        except (BrokenPipeError, ConnectionResetError, OSError):
            pass
        finally:
            try:
                process.stdin.close()
            except OSError:
                pass


class Handler(socketserver.BaseRequestHandler):
    def handle(self):
        try:
            request = line(self.request)
            world = self.server.world
            if request.get("op") == "connect":
                world.connection(self.request, request)
            else:
                response = world.forge(request["forge"]) if "forge" in request else world.resource(request)
                if response is not None:
                    send(self.request, response)
        except (EOFError, BrokenPipeError, ConnectionResetError):
            pass


BACKEND = '''#!/usr/bin/env python3
import atexit,json,os,pathlib,signal,subprocess,time
root=pathlib.Path(os.environ['ONTON_PROCESS_FIXTURE'])
resource=pathlib.Path(os.environ['ONTON_FIXTURE_RESOURCE'])
(resource/'backend.pid').write_text(str(os.getpid()))
atexit.register(lambda: (resource/'backend.pid').unlink(missing_ok=True))
signal.signal(signal.SIGTERM, lambda *_: exit(0))
branch=subprocess.check_output(['git','branch','--show-current'],text=True).strip()
patch=branch.rsplit('-',1)[-1]
def event(kind):
    with open(root/'backend-events','a') as f: f.write(json.dumps(dict(kind=kind,pid=os.getpid(),resource=resource.name,patch=patch))+'\\n')
event('start')
while not (root/'release').exists(): time.sleep(.05)
p=pathlib.Path('contribution-'+patch)
p.write_text((p.read_text() if p.exists() else '')+'fixture contribution\\n')
subprocess.run(['git','add',str(p)],check=True,stdout=subprocess.DEVNULL)
subprocess.run(['git','commit','-qm','fixture contribution'],check=True,stdout=subprocess.DEVNULL)
notes=pathlib.Path(os.environ['ONTON_DATA_DIR'])/'process-smoke'/'artifacts'/patch
notes.mkdir(parents=True,exist_ok=True)
(notes/'pr-body.md').write_text('Verified process fixture contribution '+patch+'\\n')
event('finish')
for row in [dict(type='thread.started',thread_id='fixture'),dict(type='turn.started'),dict(type='turn.completed')]: print(json.dumps(row),flush=True)
'''


def control(path, identity, kind, patch=None, payload=""):
    request = dict(version=3, id=identity, run_id="process-run", config_id="fixture-config", kind=kind, payload=payload, patch_id=patch)
    if patch:
        request["patch_id"] = patch
    with socket.socket(socket.AF_UNIX) as sock:
        sock.settimeout(60)
        sock.connect(str(path))
        send(sock, request)
        # Consume the public bounded status response.
        response = bytearray()
        while not response.endswith(b"\n"):
            response.extend(sock.recv(1))
        return json.loads(response)["status"]


def scenario(coordinator, worker, template):
    global ACTIVE
    root = Path(tempfile.mkdtemp(prefix="onton-d-", dir="/tmp"))
    print("distributed process fixture:", root, flush=True)
    seed = root / "seed"
    seed.mkdir()
    subprocess.run(["git", "init", "-q", "-b", "main", str(seed)], check=True)
    git(seed, "config", "user.name", "Process Fixture")
    git(seed, "config", "user.email", "fixture@example.invalid")
    (seed / "base").write_text("base\n")
    git(seed, "add", "base")
    git(seed, "commit", "-qm", "base")
    remote = root / "origin.git"
    subprocess.run(["git", "clone", "--bare", "-q", str(seed), str(remote)], check=True)
    observer = root / "observer"
    subprocess.run(["git", "clone", "--no-local", "-q", str(remote), str(observer)], check=True)
    plan = root / "gameplan.yaml"
    plan.write_text("projectName: process-smoke\nowner: fixture\nrepo: fixture\nproblemStatement: process smoke\nsolutionSummary: separate workers\npatches:\n" +
                    "".join(f"  - number: {n}\n    title: patch {n}\n    description: contribute {n}\n    dependsOn: []\n" for n in range(1, 6)))
    config = root / "config.json"
    config.write_text(json.dumps(dict(version=3, capacity=2, repo_root=str(observer), main_branch="main",
                                     worker_executable=worker, backend="codex", execution_mode="mainline")))
    helper_path = root / "helper"
    helper_path.write_text("#!/bin/sh\nexec python3 " + str(Path(__file__).resolve()) + ' "$@"\n')
    helper_path.chmod(0o700)
    fake_bin = root / "bin"
    fake_bin.mkdir()
    (fake_bin / "codex").write_text(BACKEND)
    (fake_bin / "codex").chmod(0o700)
    world = World(root, remote, template)
    class Server(socketserver.ThreadingUnixStreamServer):
        daemon_threads = True
    server = Server(str(root / "rpc"), Handler)
    server.world = world
    threading.Thread(target=server.serve_forever, daemon=True).start()
    env = dict(os.environ, ONTON_PROCESS_FIXTURE=str(root), HOME=str(root),
               PATH=str(fake_bin) + os.pathsep + os.environ.get("PATH", ""), GIT_CONFIG_NOSYSTEM="1", ONTON_COORDINATOR_TRACE_POLICY="1")
    original = dict(os.environ)
    os.environ.update(env)  # spawned fixture workers inherit the same isolated environment
    process = None
    def events():
        path = root / "backend-events"
        return [json.loads(row) for row in path.read_text().splitlines()] if path.exists() else []
    def start(owner, fresh=True):
        argv = [coordinator, "--gameplan", str(plan), "--config", str(config), "--run-id", "process-run",
                "--config-id", "fixture-config", "--coordinator-id", owner, "--resource-helper", str(helper_path),
                "--forge-helper", str(helper_path), "--control-socket", str(root / "control")]
        argv += ["--fresh"] if fresh else ["--takeover-epoch", str(world.epoch), "--takeover-revision", str(world.revision)]
        global ACTIVE
        ACTIVE = subprocess.Popen(argv, env=env, stdout=open(root / (owner + ".out"), "wb"), stderr=open(root / (owner + ".err"), "wb"))
        return ACTIVE
    try:
        # A lost successful first write must be reconciled through its known receipt.
        world.drop_write = True
        process = start("first")
        wait_for("two backend processes", lambda: len([e for e in events() if e["kind"] == "start"]) == 2)
        time.sleep(0.5)
        assert len(world.resources) == 2, "capacity must bound possible execution, not only completed workers"
        first_resources = set(world.resources)
        assert control(root / "control", "fresh-guidance", "guidance", "1", "retain this input through takeover") == "accepted"
        assert control(root / "control", "fresh-guidance", "guidance", "1", "retain this input through takeover") == "already_applied"
        assert control(root / "control", "fresh-guidance", "guidance", "1", "changed") == "rejected"
        # Kill the actual coordinator. The successor must not reconnect old streams.
        process.kill()
        process.wait(timeout=10)
        ACTIVE = None
        wait_for("EOF cleanup", lambda: all(p.poll() is not None for p, _, _ in world.resources.values()), seconds=20)
        for _, directory, _ in world.resources.values():
            shutil.rmtree(directory / 'repo')
            shutil.rmtree(directory / 'home')
        # UNIX socket crash residue is removed only by the owning test; the production
        # socket API deliberately refuses to unlink a possibly live foreign endpoint.
        (root / "control").unlink(missing_ok=True)
        process = start("successor", fresh=False)
        wait_for("replacement backend processes", lambda: len([e for e in events() if e["kind"] == "start"]) >= 4)
        assert all(world.resources[r][0].poll() is not None for r in first_resources)
        (root / "release").write_text("release")
        wait_for("five unique owned PRs", lambda: len(world.prs) == 5, seconds=360)
        wait_for("all workers idle", lambda: all(not e["draft"] for e in world.prs.values()), seconds=90)
        assert len({p["head"] for p in world.prs.values()}) == 5
        for n in range(1, 6):
            content = git(remote, "show", f"process-smoke/patch-{n}:contribution-{n}")
            assert content.startswith("fixture contribution"), "origin must contain the real backend result"
        # The independent model tracks physical backend starts/finishes. Killed
        # predecessor PIDs are positively excluded before replacement admission.
        live, peak = set(), 0
        for e in events():
            if e["resource"] in first_resources:
                continue
            if e["kind"] == "start":
                live.add(e["pid"])
                peak = max(peak, len(live))
            else:
                live.discard(e["pid"])
        assert peak == 2, ("backend execution overlap", peak, events())
        assert not live
        count = len(world.resources)
        time.sleep(1)
        assert len(world.resources) == count, "idle resources must not be reaped or recreated"
        assert control(root / "control", "sleep-generation", "sleep") == "accepted"
        wait_for("sleep retirement", lambda: all(p.poll() is not None for p, _, _ in world.resources.values()))
        assert not world.closed, "sleep must not seal opaque storage"
        assert control(root / "control", "wake-generation", "wake") == "accepted"
        wait_for("acknowledged-idle maintenance replacements", lambda: len(world.resources) >= count + 5, seconds=300)
        assert len([e for e in events() if e["kind"] == "start" and e["resource"] not in first_resources]) < 10, "idle recovery must not replay all finished work"
        assert control(root / "control", "cancel-generation", "cancel") == "accepted"
        wait_for("terminal helper close", lambda: world.closed)
        assert process.wait(timeout=20) == 0
        assert all(p.poll() is not None for p, _, _ in world.resources.values())
        print("PASS exact coordinator/worker processes: capacity-two/five-patch, commit-response loss, coordinator takeover, retained idle, guidance dedupe, Git publications, owned PR uniqueness, sleep/wake and cancellation", flush=True)
    except Exception:
        if world.payload:
            (root / "last-envelope.json").write_bytes(base64.b64decode(world.payload))
        print("PRs", world.prs.keys(), "drafts", [p["draft"] for p in world.prs.values()], file=sys.stderr)
        if process and process.poll() is None:
            process.terminate()
        print((root / "requests").read_text()[-6000:], file=sys.stderr)
        for path in root.glob("*.err"):
            print(path, path.read_text()[-12000:], file=sys.stderr)
        for _, directory, _ in world.resources.values():
            print(directory / "stderr", (directory / "stderr").read_text()[-6000:], file=sys.stderr)
        raise
    finally:
        if process and process.poll() is None:
            process.kill()
            process.wait(timeout=10)
        for resource in list(world.resources):
            world.stop(resource)
        server.shutdown()
        server.server_close()
        os.environ.clear()
        os.environ.update(original)


if __name__ == "__main__":
    if len(sys.argv) >= 2 and sys.argv[1] == "--test":
        with open(sys.argv[4]) as template:
            scenario(str(Path(sys.argv[2]).resolve()), str(Path(sys.argv[3]).resolve()), json.load(template))
    else:
        helper()
