# typed: false
# frozen_string_literal: true

class Onton < Formula
  desc "OCaml orchestrator for parallel Claude Code agents executing gameplan patches"
  homepage "https://github.com/flowglad/onton"
  version "0.45.0"
  license "MIT"

  on_arm do
    url "https://github.com/flowglad/onton/releases/download/v0.45.0/onton-arm64-apple-darwin.tar.gz"
    sha256 "49c5d7fd1297ddceb39170e1170b5439ea3f17cd71f49e6acc3d7bc7542696a5"
  end

  on_intel do
    url "https://github.com/flowglad/onton/releases/download/v0.45.0/onton-x86_64-apple-darwin.tar.gz"
    sha256 "76e6bf2c8c19804277682dc77e6ab59070d80bd92dfea5109986c25a249eeafa"
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
