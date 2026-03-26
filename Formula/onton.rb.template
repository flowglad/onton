# typed: false
# frozen_string_literal: true

class Onton < Formula
  desc "OCaml orchestrator for parallel Claude Code agents executing gameplan patches"
  homepage "https://github.com/flowglad/onton"
  version "VERSION"
  license "MIT"

  on_macos do
    if Hardware::CPU.arm?
      url "https://github.com/flowglad/onton/releases/download/vVERSION/onton-arm64-apple-darwin.tar.gz"
      sha256 "SHA256_ARM64"
    else
      url "https://github.com/flowglad/onton/releases/download/vVERSION/onton-x86_64-apple-darwin.tar.gz"
      sha256 "SHA256_X86_64"
    end
  end

  def install
    bin.install "onton"
  end

  test do
    assert_match "onton", shell_output("#{bin}/onton --version 2>&1", 0)
  end
end
