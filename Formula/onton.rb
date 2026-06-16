# typed: false
# frozen_string_literal: true

class Onton < Formula
  desc "OCaml orchestrator for parallel Claude Code agents executing gameplan patches"
  homepage "https://github.com/flowglad/onton"
  version "0.43.2"
  license "MIT"

  on_arm do
    url "https://github.com/flowglad/onton/releases/download/v0.43.2/onton-arm64-apple-darwin.tar.gz"
    sha256 "a759721258b015afafff90e70ccc96f17b9cbc665372e73b09e4bc02ee9a5da3"
  end

  on_intel do
    url "https://github.com/flowglad/onton/releases/download/v0.43.2/onton-x86_64-apple-darwin.tar.gz"
    sha256 "07131c14512649615761c2466b05a20164be7468f8f8b90204d9cc5c195bd960"
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
