# typed: false
# frozen_string_literal: true

class Onton < Formula
  desc "OCaml orchestrator for parallel Claude Code agents executing gameplan patches"
  homepage "https://github.com/flowglad/onton"
  version "0.2.5"
  license "MIT"

  url "https://github.com/flowglad/onton/releases/download/v0.2.5/onton-arm64-apple-darwin.tar.gz"
  sha256 "ba28878cada79b76c493a9bd280b36adba7e005c03fe37a34a4e119e3d1e56ec"

  def install
    bin.install "onton"
  end

  test do
    assert_match "onton", shell_output("#{bin}/onton --version 2>&1", 0)
  end
end
