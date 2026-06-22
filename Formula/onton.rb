# typed: false
# frozen_string_literal: true

class Onton < Formula
  desc "OCaml orchestrator for parallel Claude Code agents executing gameplan patches"
  homepage "https://github.com/flowglad/onton"
  version "0.44.0"
  license "MIT"

  on_arm do
    url "https://github.com/flowglad/onton/releases/download/v0.44.0/onton-arm64-apple-darwin.tar.gz"
    sha256 "afa9b6515f57083c53a5141c97d92979e2301ebdb1c4b796a280356d6fadf074"
  end

  on_intel do
    url "https://github.com/flowglad/onton/releases/download/v0.44.0/onton-x86_64-apple-darwin.tar.gz"
    sha256 "d56e3bc28f089a35ce6d17705575718a8d46a5c89417985908395a1bd244e8da"
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
