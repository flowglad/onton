import { mkdir, open, realpath, rename, rm } from "node:fs/promises";
import { dirname, isAbsolute, relative, resolve, sep } from "node:path";
import { randomUUID } from "node:crypto";

function inside(root: string, path: string): boolean {
  const rel = relative(root, path);
  return rel === "" || (!isAbsolute(rel) && rel !== ".." && !rel.startsWith(`..${sep}`));
}

/** Existing artifacts may be read only inside the real supervisor artifact root. */
export async function artifactPath(root: string, name: string): Promise<string> {
  const base = await realpath(root);
  const path = await realpath(resolve(base, name));
  if (isAbsolute(name) || !inside(base, path)) throw new Error("Artifact must be relative to Onton's artifacts root");
  return path;
}

/** Replace complete responses atomically; never expose a partial supervisor signal. */
export async function writeResponse(root: string, name: string, content: string): Promise<string> {
  if (!content.trim()) throw new Error("A response must contain text");
  const base = await realpath(root);
  const target = resolve(base, name);
  if (!inside(base, target)) throw new Error("Response escapes patch artifacts");
  await mkdir(dirname(target), { recursive: true });
  const parent = await realpath(dirname(target));
  if (!inside(base, parent)) throw new Error("Response directory escapes patch artifacts");
  const path = resolve(parent, target.slice(dirname(target).length + 1));
  const temporary = `${path}.${randomUUID()}.tmp`;
  try {
    const file = await open(temporary, "wx", 0o600);
    try { await file.writeFile(content, "utf8"); } finally { await file.close(); }
    await rename(temporary, path);
    return path;
  } finally {
    await rm(temporary, { force: true });
  }
}

export async function readChunk(path: string, offset: number, limit: number) {
  if (!Number.isSafeInteger(offset) || offset < 0 || !Number.isSafeInteger(limit) || limit < 1 || limit > 65536) {
    throw new Error("Use a nonnegative byte offset and a limit of 1–65536 bytes");
  }
  const file = await open(path, "r");
  try {
    // Look ahead for the rest of a code point. Normally stop before a split;
    // if the first character exceeds the limit, include it to ensure progress.
    const buffer = Buffer.alloc(limit + 3);
    const { bytesRead } = await file.read(buffer, 0, buffer.length, offset);
    const continuation = (index: number) => (buffer[index]! & 0xc0) === 0x80;
    if (bytesRead > 0 && continuation(0)) throw new Error("Byte offset splits a UTF-8 character");
    let end = Math.min(limit, bytesRead);
    while (end > 0 && end < bytesRead && continuation(end)) end--;
    if (end === 0 && bytesRead > 0) {
      end = 1;
      while (end < bytesRead && continuation(end)) end++;
    }
    const text = new TextDecoder("utf-8", { fatal: true, ignoreBOM: true }).decode(buffer.subarray(0, end));
    return { text, next_offset: offset + end, eof: offset + end >= (await file.stat()).size };
  } finally { await file.close(); }
}
