# typed: false
# frozen_string_literal: true

class Onton < Formula
  desc "OCaml orchestrator for parallel Claude Code agents executing gameplan patches"
  homepage "https://github.com/flowglad/onton"
  version "0.50.0"
  license "MIT"

  on_arm do
    url "https://github.com/flowglad/onton/releases/download/v0.50.0/onton-arm64-apple-darwin.tar.gz"
    sha256 "6d3dec01a54973a49069f2ea7a7ae3001c4e2012dfad25bc61a04aabd939bdd2"
  end

  on_intel do
    url "https://github.com/flowglad/onton/releases/download/v0.50.0/onton-x86_64-apple-darwin.tar.gz"
    sha256 "85d4eb651f025d69740b0fb6bc62d6ebb704a6da5121ccb2b3f41dfad0054639"
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
