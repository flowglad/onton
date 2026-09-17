# typed: false
# frozen_string_literal: true

class Onton < Formula
  desc "OCaml orchestrator for parallel Claude Code agents executing gameplan patches"
  homepage "https://github.com/flowglad/onton"
  version "0.54.1"
  license "MIT"

  on_arm do
    url "https://github.com/flowglad/onton/releases/download/v0.54.1/onton-arm64-apple-darwin.tar.gz"
    sha256 "e8a8fd3d16fdc4cc3cb8eb1a73f58795b3c5438aa1885d66bda2f99a4388acff"
  end

  on_intel do
    url "https://github.com/flowglad/onton/releases/download/v0.54.1/onton-x86_64-apple-darwin.tar.gz"
    sha256 "1462d716a4451a3d9d7733992eb8ffb0f74870b7471457682db419e3173e4724"
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
