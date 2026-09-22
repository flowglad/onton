# typed: false
# frozen_string_literal: true

class Onton < Formula
  desc "OCaml orchestrator for parallel Claude Code agents executing gameplan patches"
  homepage "https://github.com/flowglad/onton"
  version "0.56.0"
  license "MIT"

  on_arm do
    url "https://github.com/flowglad/onton/releases/download/v0.56.0/onton-arm64-apple-darwin.tar.gz"
    sha256 "ec8b60f2fff56749ff589d768d4b998078ead82b64f22217a05a58b3ec4186ab"
  end

  on_intel do
    url "https://github.com/flowglad/onton/releases/download/v0.56.0/onton-x86_64-apple-darwin.tar.gz"
    sha256 "1c8717d3171f76afca5acd31e3a5d9320ba1b9c3bfbcd8a45649625a70ab707c"
  end

  depends_on "gmp"

  def install
    bin.install "onton", "onton-setsid-exec"
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
    assert_predicate bin/"onton-setsid-exec", :executable?
  end
end
