# typed: false
# frozen_string_literal: true

class Onton < Formula
  desc "OCaml orchestrator for parallel Claude Code agents executing gameplan patches"
  homepage "https://github.com/flowglad/onton"
  version "0.51.1"
  license "MIT"

  on_arm do
    url "https://github.com/flowglad/onton/releases/download/v0.51.1/onton-arm64-apple-darwin.tar.gz"
    sha256 "8e6e7f45bce9a71b01f8bde48d6b072117bfa53ca1227285b35f0cf4e7a6d65d"
  end

  on_intel do
    url "https://github.com/flowglad/onton/releases/download/v0.51.1/onton-x86_64-apple-darwin.tar.gz"
    sha256 "f7b3b54bca6dd239b5877385078ae708ae444c0dd6cf213203e5e8154deed8ac"
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
