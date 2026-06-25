# typed: false
# frozen_string_literal: true

class Onton < Formula
  desc "OCaml orchestrator for parallel Claude Code agents executing gameplan patches"
  homepage "https://github.com/flowglad/onton"
  version "0.45.1"
  license "MIT"

  on_arm do
    url "https://github.com/flowglad/onton/releases/download/v0.45.1/onton-arm64-apple-darwin.tar.gz"
    sha256 "9955140ee05850ff14fd6ef4034c8d26aed66c37b038b540bebc14f9bc118f01"
  end

  on_intel do
    url "https://github.com/flowglad/onton/releases/download/v0.45.1/onton-x86_64-apple-darwin.tar.gz"
    sha256 "594cd228139ecac40ff7ba287498c47ab26313a2688929dc539bdd6d48f41247"
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
