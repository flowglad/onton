# typed: false
# frozen_string_literal: true

class Onton < Formula
  desc "OCaml orchestrator for parallel Claude Code agents executing gameplan patches"
  homepage "https://github.com/flowglad/onton"
  version "0.40.0"
  license "MIT"

  on_arm do
    url "https://github.com/flowglad/onton/releases/download/v0.40.0/onton-arm64-apple-darwin.tar.gz"
    sha256 "38f93fd63d37a225bde1e73c76a95df35ebb63d228db474a33689a3fa2130774"
  end

  on_intel do
    url "https://github.com/flowglad/onton/releases/download/v0.40.0/onton-x86_64-apple-darwin.tar.gz"
    sha256 "71951a30fbbc762fffa88e5a75797b06342caf2815ec21cc63440c3e81bcb715"
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
