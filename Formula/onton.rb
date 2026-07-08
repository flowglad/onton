# typed: false
# frozen_string_literal: true

class Onton < Formula
  desc "OCaml orchestrator for parallel Claude Code agents executing gameplan patches"
  homepage "https://github.com/flowglad/onton"
  version "0.50.2"
  license "MIT"

  on_arm do
    url "https://github.com/flowglad/onton/releases/download/v0.50.2/onton-arm64-apple-darwin.tar.gz"
    sha256 "9c1f3e432b29cca2f68d5efdd021c6d1088e2ed65428e9e9bbdd1799d73e372d"
  end

  on_intel do
    url "https://github.com/flowglad/onton/releases/download/v0.50.2/onton-x86_64-apple-darwin.tar.gz"
    sha256 "2a09f88106dc0d73e88e022a982cd210197083766e08087ecb7914be6bba566d"
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
