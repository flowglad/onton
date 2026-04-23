# typed: false
# frozen_string_literal: true

class Onton < Formula
  desc "OCaml orchestrator for parallel Claude Code agents executing gameplan patches"
  homepage "https://github.com/flowglad/onton"
  version "0.15.2"
  license "MIT"

  url "https://github.com/flowglad/onton/releases/download/v0.15.2/onton-arm64-apple-darwin.tar.gz"
  sha256 "0f712aa9e100b10ff3d2da17ba7ae6c96c6c806028dae3ba6741143339f60695"

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
