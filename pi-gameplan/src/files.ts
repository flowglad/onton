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
