# typed: false
# frozen_string_literal: true

class Onton < Formula
  desc "OCaml orchestrator for parallel Claude Code agents executing gameplan patches"
  homepage "https://github.com/flowglad/onton"
  version "0.49.0"
  license "MIT"

  on_arm do
    url "https://github.com/flowglad/onton/releases/download/v0.49.0/onton-arm64-apple-darwin.tar.gz"
    sha256 "41d4776feb697ec59e4e1d0a7bd517ba8b6f9eeb9a065c3d153bac3f89fa5259"
  end

  on_intel do
    url "https://github.com/flowglad/onton/releases/download/v0.49.0/onton-x86_64-apple-darwin.tar.gz"
    sha256 "9e2148bada62e989ca22b6a7112c3bd3fb14754fa5ce15dc8e5a3e250a128f3b"
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
