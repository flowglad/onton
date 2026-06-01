# typed: false
# frozen_string_literal: true

class Onton < Formula
  desc "OCaml orchestrator for parallel Claude Code agents executing gameplan patches"
  homepage "https://github.com/flowglad/onton"
  version "0.39.0"
  license "MIT"

  on_arm do
    url "https://github.com/flowglad/onton/releases/download/v0.39.0/onton-arm64-apple-darwin.tar.gz"
    sha256 "6076c0e09da7ba5eedf107ef2f2145c1b82af0960e2ac67c651bf60d367f396f"
  end

  on_intel do
    url "https://github.com/flowglad/onton/releases/download/v0.39.0/onton-x86_64-apple-darwin.tar.gz"
    sha256 "a5d16fdc218bdedb966acbca373f77bb0992a6deaad66dbe4192ba12133adf1f"
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
