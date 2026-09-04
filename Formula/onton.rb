# typed: false
# frozen_string_literal: true

class Onton < Formula
  desc "OCaml orchestrator for parallel Claude Code agents executing gameplan patches"
  homepage "https://github.com/flowglad/onton"
  version "0.53.1"
  license "MIT"

  on_arm do
    url "https://github.com/flowglad/onton/releases/download/v0.53.1/onton-arm64-apple-darwin.tar.gz"
    sha256 "9cc88b6efea05c9555742b5a446e335803c01fb6a0da5c3f4d11a99b04051985"
  end

  on_intel do
    url "https://github.com/flowglad/onton/releases/download/v0.53.1/onton-x86_64-apple-darwin.tar.gz"
    sha256 "35588dfa94ce0875e8e3b83d9895c8fd31908bfc0a6cf86bf952834f8ede7a9e"
  end

  depends_on "gmp"

  def install
    bin.install "onton", "onton-setsid-exec"
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
    assert_predicate bin/"onton-setsid-exec", :executable?
  end
end
