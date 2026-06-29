# typed: false
# frozen_string_literal: true

class Onton < Formula
  desc "OCaml orchestrator for parallel Claude Code agents executing gameplan patches"
  homepage "https://github.com/flowglad/onton"
  version "0.48.0"
  license "MIT"

  on_arm do
    url "https://github.com/flowglad/onton/releases/download/v0.48.0/onton-arm64-apple-darwin.tar.gz"
    sha256 "920c9127fe89381c7088e619620a0641d77cdd603bac23af8480f04ec9952795"
  end

  on_intel do
    url "https://github.com/flowglad/onton/releases/download/v0.48.0/onton-x86_64-apple-darwin.tar.gz"
    sha256 "82e66fecad80f9a87e7f4b34e792626332aff6126126294c6b9929213b357790"
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
