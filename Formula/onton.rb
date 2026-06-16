# typed: false
# frozen_string_literal: true

class Onton < Formula
  desc "OCaml orchestrator for parallel Claude Code agents executing gameplan patches"
  homepage "https://github.com/flowglad/onton"
  version "0.43.1"
  license "MIT"

  on_arm do
    url "https://github.com/flowglad/onton/releases/download/v0.43.1/onton-arm64-apple-darwin.tar.gz"
    sha256 "04b649f0511b1f9aa2832caf0ed28cd0048208ffa495c02f1efc563a5b531328"
  end

  on_intel do
    url "https://github.com/flowglad/onton/releases/download/v0.43.1/onton-x86_64-apple-darwin.tar.gz"
    sha256 "0887eb96caebb7594aa191cd5bb0f9c8c9c3dd68ba4b4e10ae2e9bbe4ad0724b"
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
