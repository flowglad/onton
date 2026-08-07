# typed: false
# frozen_string_literal: true

class Onton < Formula
  desc "OCaml orchestrator for parallel Claude Code agents executing gameplan patches"
  homepage "https://github.com/flowglad/onton"
  version "0.52.0"
  license "MIT"

  on_arm do
    url "https://github.com/flowglad/onton/releases/download/v0.52.0/onton-arm64-apple-darwin.tar.gz"
    sha256 "7d66bdab8bfb1ba4ec6fffc3703c9844218c2314e232f419a39af464c5dc33ba"
  end

  on_intel do
    url "https://github.com/flowglad/onton/releases/download/v0.52.0/onton-x86_64-apple-darwin.tar.gz"
    sha256 "01dbe023516674c9668a81fe28fea5c687f8f998bae6af030aedff882f2db60c"
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
