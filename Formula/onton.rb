# typed: false
# frozen_string_literal: true

class Onton < Formula
  desc "OCaml orchestrator for parallel Claude Code agents executing gameplan patches"
  homepage "https://github.com/flowglad/onton"
  version "0.41.0"
  license "MIT"

  on_arm do
    url "https://github.com/flowglad/onton/releases/download/v0.41.0/onton-arm64-apple-darwin.tar.gz"
    sha256 "206ef2aeba1b24fcd1b815a308dbb7ee4f5130b4feb17c1ca7119b93a45e425e"
  end

  on_intel do
    url "https://github.com/flowglad/onton/releases/download/v0.41.0/onton-x86_64-apple-darwin.tar.gz"
    sha256 "800b9f3d7f8d591fe55c81924c89a07ebd88be5fe446e63aa119c03cca859d2a"
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
