# typed: false
# frozen_string_literal: true

class Onton < Formula
  desc "OCaml orchestrator for parallel Claude Code agents executing gameplan patches"
  homepage "https://github.com/flowglad/onton"
  version "0.46.0"
  license "MIT"

  on_arm do
    url "https://github.com/flowglad/onton/releases/download/v0.46.0/onton-arm64-apple-darwin.tar.gz"
    sha256 "2fe77b4810798df751a33a1804737f5b5b08a8557c0e6dcaa81edb92f0df4779"
  end

  on_intel do
    url "https://github.com/flowglad/onton/releases/download/v0.46.0/onton-x86_64-apple-darwin.tar.gz"
    sha256 "de356927f0c8a583c867010557f70b11fa1efe51f4e5f057342de092ff2d3e02"
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
