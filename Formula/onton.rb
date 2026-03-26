# typed: false
# frozen_string_literal: true

class Onton < Formula
  desc "OCaml orchestrator for parallel Claude Code agents executing gameplan patches"
  homepage "https://github.com/flowglad/onton"
  version "0.2.3"
  license "MIT"

  url "https://github.com/flowglad/onton/releases/download/v0.2.3/onton-arm64-apple-darwin.tar.gz"
  sha256 "564121f9ecbe06dc07ac1bad1ef8322a07beac21a4fb10444d33ffa68bc5e493"

  def install
    bin.install "onton"
  end

  test do
    assert_match "onton", shell_output("#{bin}/onton --version 2>&1", 0)
  end
end
