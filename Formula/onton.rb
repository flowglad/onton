# typed: false
# frozen_string_literal: true

class Onton < Formula
  desc "OCaml orchestrator for parallel Claude Code agents executing gameplan patches"
  homepage "https://github.com/flowglad/onton"
  version "0.49.1"
  license "MIT"

  on_arm do
    url "https://github.com/flowglad/onton/releases/download/v0.49.1/onton-arm64-apple-darwin.tar.gz"
    sha256 "228ff54cdfcf5c10ef3ef1d29eaebff590fe76a5f7d03bb99ef0ca3a1342c94c"
  end

  on_intel do
    url "https://github.com/flowglad/onton/releases/download/v0.49.1/onton-x86_64-apple-darwin.tar.gz"
    sha256 "c9be7b81d8dce277ce87b7653040ee5337fd2f65247b8be2988152d45f821d76"
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
