/** Filesystem boundary for tool-owned gameplan writes. */
import { lstat, mkdir, realpath, open } from "node:fs/promises";
import { constants } from "node:fs";
import { execFile } from "node:child_process";
import { fileURLToPath } from "node:url";
import { pythonCommand } from "./validator.js";
import { basename, join, resolve } from "node:path";
import { isGameplanWrite } from "./guard.js";

declare const admittedWrite: unique symbol;
export interface GameplanWrite {
  readonly [admittedWrite]: true;
  readonly path: string;
  readonly root: string;
  readonly device: string;
  readonly inode: string;
}

/** A symlinked configured directory selects its canonical root; file symlinks are refused. */
export async function resolveGameplanWrite(cwd: string, directory: string, target: string): Promise<GameplanWrite> {
  if (!isGameplanWrite(cwd, directory, target)) throw new Error(`Refusing gameplan write: ${target}`);
  const root = resolve(cwd, directory);
  await mkdir(root, { recursive: true });
  const canonical = await realpath(root);
  const path = join(canonical, basename(resolve(cwd, target)));
  try {
    const stat = await lstat(path);
    if (!stat.isFile() || stat.isSymbolicLink()) throw new Error(`Refusing non-regular gameplan file: ${target}`);
  } catch (error) {
    if ((error as NodeJS.ErrnoException).code !== "ENOENT") throw error;
  }
  const directoryHandle = await open(canonical, constants.O_RDONLY | constants.O_DIRECTORY | constants.O_NOFOLLOW);
  try {
    const stat = await directoryHandle.stat({ bigint: true });
    return { path, root: canonical, device: String(stat.dev), inode: String(stat.ino) } as GameplanWrite;
  } finally { await directoryHandle.close(); }
}

/** Python exposes descriptor-relative open/rename on both supported POSIX platforms;
 * Node's filesystem API does not. The helper verifies the admitted directory identity,
 * prepares a complete replacement there, then publishes it with a single atomic rename.
 */
export async function commitGameplan(admission: GameplanWrite, text: string, expected: string | Buffer | undefined): Promise<void> {
  const [command = "python3", ...prefix] = pythonCommand();
  const helper = fileURLToPath(new URL("./commit_gameplan.py", import.meta.url));
  await new Promise<void>((done, reject) => {
    const child = execFile(command, [...prefix, helper], { timeout: 120_000, maxBuffer: 64 * 1024 }, (error, _stdout, stderr) => {
      if (error) reject(new Error(stderr.trim() || error.message));
      else done();
    });
    child.stdin?.on("error", () => {}); // Process failure is reported by the completion callback.
    child.stdin?.end(JSON.stringify({ ...admission, name: basename(admission.path), text, expected: expected === undefined ? null : Buffer.from(expected).toString("base64") }));
  });
}

// Status snapshots are metadata reads, not an unbounded document-loading API.
const MAX_SNAPSHOT_BYTES = 16 * 1024 * 1024;

/** Read a bounded regular gameplan file without creating directories or following a leaf link. */
export async function readGameplanSnapshot(cwd: string, directory: string, target: string): Promise<Buffer> {
  if (!isGameplanWrite(cwd, directory, target)) throw new Error(`Refusing gameplan snapshot: ${target}`);
  const root = await realpath(resolve(cwd, directory));
  const path = join(root, basename(resolve(cwd, target)));
  if (!(await lstat(path)).isFile()) throw new Error("Gameplan snapshot requires a regular file.");
  // NONBLOCK prevents a FIFO swapped in after lstat from blocking open; fstat checks the
  // descriptor itself before reading. NOFOLLOW rejects a swapped-in symlink.
  const file = await open(path, constants.O_RDONLY | constants.O_NOFOLLOW | constants.O_NONBLOCK);
  try {
    const stat = await file.stat();
    if (!stat.isFile() || stat.size > MAX_SNAPSHOT_BYTES) throw new Error("Invalid or oversized gameplan snapshot.");
    const chunks: Buffer[] = [];
    let size = 0;
    while (true) {
      const buffer = Buffer.alloc(Math.min(64 * 1024, MAX_SNAPSHOT_BYTES + 1 - size));
      const { bytesRead } = await file.read(buffer);
      if (bytesRead === 0) return Buffer.concat(chunks, size);
      size += bytesRead;
      if (size > MAX_SNAPSHOT_BYTES) throw new Error("Gameplan snapshot exceeds 16 MiB.");
      chunks.push(buffer.subarray(0, bytesRead));
    }
  } finally { await file.close(); }
}
