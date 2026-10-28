/** Bounded snapshot reads for final review and approval invalidation. */
import { constants } from "node:fs";
import { lstat, open } from "node:fs/promises";

const MAX_DRAFT_BYTES = 16 * 1024 * 1024;

export async function readDraft(path: string): Promise<Buffer> {
  if (!(await lstat(path)).isFile()) throw new Error("The workstream draft must be a regular file.");
  // A swapped-in pipe cannot block open; a swapped-in symlink cannot be followed.
  const file = await open(path, constants.O_RDONLY | constants.O_NOFOLLOW | constants.O_NONBLOCK);
  try {
    const stat = await file.stat();
    if (!stat.isFile()) throw new Error("The workstream draft must be a regular file.");
    if (stat.size > MAX_DRAFT_BYTES) throw new Error("The workstream draft exceeds 16 MiB.");
    const chunks: Buffer[] = [];
    let size = 0;
    while (true) {
      const buffer = Buffer.alloc(Math.min(64 * 1024, MAX_DRAFT_BYTES + 1 - size));
      const { bytesRead } = await file.read(buffer);
      if (bytesRead === 0) return Buffer.concat(chunks, size);
      size += bytesRead;
      if (size > MAX_DRAFT_BYTES) throw new Error("The workstream draft exceeds 16 MiB.");
      chunks.push(buffer.subarray(0, bytesRead));
    }
  } finally { await file.close(); }
}
