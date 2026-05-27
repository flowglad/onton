# typed: false
# frozen_string_literal: true

class Onton < Formula
  desc "OCaml orchestrator for parallel Claude Code agents executing gameplan patches"
  homepage "https://github.com/flowglad/onton"
  version "0.38.0"
  license "MIT"

  on_arm do
    url "https://github.com/flowglad/onton/releases/download/v0.38.0/onton-arm64-apple-darwin.tar.gz"
    sha256 "fe77cc36f15116f525a413e3d7375d7094ef9b19bc4f9eed31c8b14bccd63623"
  end

  on_intel do
    url "https://github.com/flowglad/onton/releases/download/v0.38.0/onton-x86_64-apple-darwin.tar.gz"
    sha256 "b36b5c736b6ec394abf899fbd4929466696b712b2c28d270028ad0f3bcd90fe0"
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
