# typed: false
# frozen_string_literal: true

class Onton < Formula
  desc "OCaml orchestrator for parallel Claude Code agents executing gameplan patches"
  homepage "https://github.com/flowglad/onton"
  version "0.0.0-intel-test"
  license "MIT"

  # x86_64/Intel build temporarily dropped — no x86_64 macOS CI runner is
  # available (GitHub retired macos-13). Tracked in #316. Without an on_intel
  # block, `brew install` fails cleanly on Intel rather than installing an
  # unrunnable ARM64 binary.
  on_arm do
    url "https://github.com/flowglad/onton/releases/download/v0.0.0-intel-test/onton-arm64-apple-darwin.tar.gz"
    sha256 "1db62f838ae8abdb2b10642ce5afbe537387c2ee0763089febc13c06c18b8d9f"
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
