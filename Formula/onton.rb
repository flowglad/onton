# typed: false
# frozen_string_literal: true

class Onton < Formula
  desc "OCaml orchestrator for parallel Claude Code agents executing gameplan patches"
  homepage "https://github.com/flowglad/onton"
  version "0.48.1"
  license "MIT"

  on_arm do
    url "https://github.com/flowglad/onton/releases/download/v0.48.1/onton-arm64-apple-darwin.tar.gz"
    sha256 "975d16c0f0367fa9d31c82c643e7c168caf67685645489a2cffb11857b4598f8"
  end

  on_intel do
    url "https://github.com/flowglad/onton/releases/download/v0.48.1/onton-x86_64-apple-darwin.tar.gz"
    sha256 "e00c1a8d5ac2d01ce85a678f8d88914d6cf1283fb94c257f5b7a08229a107faf"
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
