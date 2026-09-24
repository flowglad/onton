# typed: false
# frozen_string_literal: true

class Onton < Formula
  desc "OCaml orchestrator for parallel Claude Code agents executing gameplan patches"
  homepage "https://github.com/flowglad/onton"
  version "0.58.0"
  license "MIT"

  on_arm do
    url "https://github.com/flowglad/onton/releases/download/v0.58.0/onton-arm64-apple-darwin.tar.gz"
    sha256 "bcdca9006448269b44bf982a2ce0d4e3870be42434d6156bbe367c5a92be743f"
  end

  on_intel do
    url "https://github.com/flowglad/onton/releases/download/v0.58.0/onton-x86_64-apple-darwin.tar.gz"
    sha256 "bbe2bf6473c4e055fc895d7c98a64b4818ca97735c13c22ceced0c9d3caf593c"
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
