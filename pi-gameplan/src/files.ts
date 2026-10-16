/** Filesystem boundary for tool-owned gameplan writes. */
import { lstat, mkdir, realpath } from "node:fs/promises";
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
