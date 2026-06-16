# typed: false
# frozen_string_literal: true

class Onton < Formula
  desc "OCaml orchestrator for parallel Claude Code agents executing gameplan patches"
  homepage "https://github.com/flowglad/onton"
  version "0.43.3"
  license "MIT"

  on_arm do
    url "https://github.com/flowglad/onton/releases/download/v0.43.3/onton-arm64-apple-darwin.tar.gz"
    sha256 "c2490c0441203ff6d85f5e2c6b2868c79e732e9f5ed5975622ac196b12014da3"
  end

  on_intel do
    url "https://github.com/flowglad/onton/releases/download/v0.43.3/onton-x86_64-apple-darwin.tar.gz"
    sha256 "b04567e7f28e8004f5d52b6a158764b33888e8477d92d8b1ee51d51d182cde18"
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
