# typed: false
# frozen_string_literal: true

class Onton < Formula
  desc "OCaml orchestrator for parallel Claude Code agents executing gameplan patches"
  homepage "https://github.com/flowglad/onton"
  version "0.43.0"
  license "MIT"

  on_arm do
    url "https://github.com/flowglad/onton/releases/download/v0.43.0/onton-arm64-apple-darwin.tar.gz"
    sha256 "050becb7d58f4afffc89d489d4387c30273bef64b8d7fa3e1834d06fe5cfa969"
  end

  on_intel do
    url "https://github.com/flowglad/onton/releases/download/v0.43.0/onton-x86_64-apple-darwin.tar.gz"
    sha256 "23d80e565e825931e103595942ffbd430f55845d2d6b9e2cd2f6e2350c5f97a3"
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
