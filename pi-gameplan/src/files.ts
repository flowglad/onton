/** Filesystem boundary for tool-owned gameplan writes. */
import { lstat, mkdir, realpath, open } from "node:fs/promises";
import { constants } from "node:fs";
import { basename, join, resolve } from "node:path";
import { isGameplanWrite } from "./guard.js";

/** A symlinked configured directory selects its canonical root; file symlinks are refused. */
export async function resolveGameplanWrite(cwd: string, directory: string, target: string): Promise<string> {
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
  return path;
}

/** Commit only through the no-follow descriptor; the formatter never receives this path. */
export async function commitGameplan(path: string, text: string, expected: string | undefined): Promise<void> {
  const flags = constants.O_RDWR | constants.O_NOFOLLOW | (expected === undefined ? constants.O_CREAT | constants.O_EXCL : 0);
  const file = await open(path, flags, 0o666);
  try {
    if (expected !== undefined && await file.readFile("utf8") !== expected) {
      throw new Error("Gameplan changed before commit; retry the operation.");
    }
    await file.truncate(0);
    const buffer = Buffer.from(text, "utf8");
    let offset = 0;
    while (offset < buffer.length) {
      const { bytesWritten } = await file.write(buffer, offset, buffer.length - offset, offset);
      if (bytesWritten === 0) throw new Error("Gameplan commit made no progress.");
      offset += bytesWritten;
    }
  } finally {
    await file.close();
  }
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
