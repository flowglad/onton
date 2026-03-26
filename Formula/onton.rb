# typed: false
# frozen_string_literal: true

class Onton < Formula
  desc "OCaml orchestrator for parallel Claude Code agents executing gameplan patches"
  homepage "https://github.com/flowglad/onton"
  version "0.2.4"
  license "MIT"

  url "https://github.com/flowglad/onton/releases/download/v0.2.4/onton-arm64-apple-darwin.tar.gz"
  sha256 "617ee8cc42ac64bebcbe6a40e3a47cd2a73546880dc1ea0f9b17315b59997421"

  depends_on "gmp"

  def install
    bin.install "onton"
  end

  test do
    assert_match "onton", shell_output("#{bin}/onton --version 2>&1", 0)
  end
end
