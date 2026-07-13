# typed: false
# frozen_string_literal: true

class Onton < Formula
  desc "OCaml orchestrator for parallel Claude Code agents executing gameplan patches"
  homepage "https://github.com/flowglad/onton"
  version "0.51.0"
  license "MIT"

  on_arm do
    url "https://github.com/flowglad/onton/releases/download/v0.51.0/onton-arm64-apple-darwin.tar.gz"
    sha256 "adc9b3c36e6b5f2cf4337d1b2c5f97551eb5c4d07a01cd640d930e005fbe7f7f"
  end

  on_intel do
    url "https://github.com/flowglad/onton/releases/download/v0.51.0/onton-x86_64-apple-darwin.tar.gz"
    sha256 "747eda91cf96b425b3810164d347edf43d75f475a8431a60ac59fca2ebfc403d"
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
