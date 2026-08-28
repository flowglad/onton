# typed: false
# frozen_string_literal: true

class Onton < Formula
  desc "OCaml orchestrator for parallel Claude Code agents executing gameplan patches"
  homepage "https://github.com/flowglad/onton"
  version "0.53.0"
  license "MIT"

  on_arm do
    url "https://github.com/flowglad/onton/releases/download/v0.53.0/onton-arm64-apple-darwin.tar.gz"
    sha256 "3766a28e8bc5e344bedd1a821cb8d12d3eb0d0460da674d44d6ae9ee9cda7a83"
  end

  on_intel do
    url "https://github.com/flowglad/onton/releases/download/v0.53.0/onton-x86_64-apple-darwin.tar.gz"
    sha256 "0b799c3511081db598ac7f09faecb6899d2c214f8cce723ec23378490e857fff"
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
