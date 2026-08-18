# typed: false
# frozen_string_literal: true

class Onton < Formula
  desc "OCaml orchestrator for parallel Claude Code agents executing gameplan patches"
  homepage "https://github.com/flowglad/onton"
  version "0.52.1"
  license "MIT"

  on_arm do
    url "https://github.com/flowglad/onton/releases/download/v0.52.1/onton-arm64-apple-darwin.tar.gz"
    sha256 "a150e478d2ae26555a3ee35e48194d7f04f42bd698dade9283a218cd8da8b67b"
  end

  on_intel do
    url "https://github.com/flowglad/onton/releases/download/v0.52.1/onton-x86_64-apple-darwin.tar.gz"
    sha256 "d1af8cc4065af0a6a3681cd659ec48a39b34dab4e86b4cdd2f0a2147bae2c5ca"
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
