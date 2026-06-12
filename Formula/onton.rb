# typed: false
# frozen_string_literal: true

class Onton < Formula
  desc "OCaml orchestrator for parallel Claude Code agents executing gameplan patches"
  homepage "https://github.com/flowglad/onton"
  version "0.42.0"
  license "MIT"

  on_arm do
    url "https://github.com/flowglad/onton/releases/download/v0.42.0/onton-arm64-apple-darwin.tar.gz"
    sha256 "bebd56de541d8561eabe735b7afb851a11d30977f959bc9115e60445788876a1"
  end

  on_intel do
    url "https://github.com/flowglad/onton/releases/download/v0.42.0/onton-x86_64-apple-darwin.tar.gz"
    sha256 "ab63d7a90460a32d06c8e7db3d8a08c9c826449ac37f286a95367dd8e743d9fe"
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
