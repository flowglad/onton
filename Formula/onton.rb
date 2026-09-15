# typed: false
# frozen_string_literal: true

class Onton < Formula
  desc "OCaml orchestrator for parallel Claude Code agents executing gameplan patches"
  homepage "https://github.com/flowglad/onton"
  version "0.54.0"
  license "MIT"

  on_arm do
    url "https://github.com/flowglad/onton/releases/download/v0.54.0/onton-arm64-apple-darwin.tar.gz"
    sha256 "3febe972cb670af22fc1064dd9590682fdefb283049ee3419861d0fcf14db026"
  end

  on_intel do
    url "https://github.com/flowglad/onton/releases/download/v0.54.0/onton-x86_64-apple-darwin.tar.gz"
    sha256 "7acbeb6e0923e66daf646aad148dc93ba35fcd4dd8ba9696bc7d76c73632c5c8"
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
