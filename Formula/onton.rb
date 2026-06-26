# typed: false
# frozen_string_literal: true

class Onton < Formula
  desc "OCaml orchestrator for parallel Claude Code agents executing gameplan patches"
  homepage "https://github.com/flowglad/onton"
  version "0.47.0"
  license "MIT"

  on_arm do
    url "https://github.com/flowglad/onton/releases/download/v0.47.0/onton-arm64-apple-darwin.tar.gz"
    sha256 "2a4df3d584aa6a2acd9410d83c24cbc8aa5c4c0c1c05ee9e9ba123d0747cf5de"
  end

  on_intel do
    url "https://github.com/flowglad/onton/releases/download/v0.47.0/onton-x86_64-apple-darwin.tar.gz"
    sha256 "9e6575be5153036e60203e1cd93a6f18edbec08d50f8ac38d2d5c77d183ba725"
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
