# typed: false
# frozen_string_literal: true

class Onton < Formula
  desc "OCaml orchestrator for parallel Claude Code agents executing gameplan patches"
  homepage "https://github.com/flowglad/onton"
  version "0.51.2"
  license "MIT"

  on_arm do
    url "https://github.com/flowglad/onton/releases/download/v0.51.2/onton-arm64-apple-darwin.tar.gz"
    sha256 "e09affd17bc0e566c0b016b71c93613128283c16104cdb3aa83df543e8ca4fd9"
  end

  on_intel do
    url "https://github.com/flowglad/onton/releases/download/v0.51.2/onton-x86_64-apple-darwin.tar.gz"
    sha256 "c62ffbb14f41a53f95e4b191548a93df625af546af9413b6345af3a3b5044508"
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
