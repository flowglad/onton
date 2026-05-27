# typed: false
# frozen_string_literal: true

class Onton < Formula
  desc "OCaml orchestrator for parallel Claude Code agents executing gameplan patches"
  homepage "https://github.com/flowglad/onton"
  version "0.35.0"
  license "MIT"

  # x86_64/Intel build temporarily dropped — no x86_64 macOS CI runner is
  # available (GitHub retired macos-13). Tracked in #316. Without an on_intel
  # block, `brew install` fails cleanly on Intel rather than installing an
  # unrunnable ARM64 binary.
  on_arm do
    url "https://github.com/flowglad/onton/releases/download/v0.35.0/onton-arm64-apple-darwin.tar.gz"
    sha256 "65fa00694c893f62dc38e8418899625fcade3ef28c7b78ef24e8f7a873932d5b"
  end

  depends_on "gmp"

  def install
    bin.install "onton"
    # Rewrite CI's hardcoded libgmp path to this machine's Homebrew prefix
    old_path = Utils.popen_read("otool", "-L", bin/"onton")
      .lines.find { |l| l.include?("libgmp") }&.strip&.split&.first
    gmp_lib = (Formula["gmp"].opt_lib/"libgmp.10.dylib").to_s
    if old_path && old_path != gmp_lib
      system "install_name_tool", "-change", old_path, gmp_lib, bin/"onton"
    end
  end

  test do
    assert_match "onton", shell_output("#{bin}/onton --version 2>&1", 0)
  end
end
