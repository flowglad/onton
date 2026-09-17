# typed: false
# frozen_string_literal: true

class Onton < Formula
  desc "OCaml orchestrator for parallel Claude Code agents executing gameplan patches"
  homepage "https://github.com/flowglad/onton"
  version "0.54.2"
  license "MIT"

  on_arm do
    url "https://github.com/flowglad/onton/releases/download/v0.54.2/onton-arm64-apple-darwin.tar.gz"
    sha256 "530806fee7aa0ad1b96cd98edfd7a48547649825c701c54922929df557466dd2"
  end

  on_intel do
    url "https://github.com/flowglad/onton/releases/download/v0.54.2/onton-x86_64-apple-darwin.tar.gz"
    sha256 "f78b8d912bccbb7290eebc32ea55ca43c7e4eb3f23a90af91905eea90d60f4ea"
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
