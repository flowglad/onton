# typed: false
# frozen_string_literal: true

class Onton < Formula
  desc "OCaml orchestrator for parallel Claude Code agents executing gameplan patches"
  homepage "https://github.com/flowglad/onton"
  version "0.69.1"
  license "MIT"

  on_arm do
    url "https://github.com/flowglad/onton/releases/download/v0.69.1/onton-arm64-apple-darwin.tar.gz"
    sha256 "b575e6ab8ac5c8adedc8b8b8c345bcfff9a793acdbed3354808e76a4df011f5e"
  end

  depends_on "gmp"

  def install
    bin.install "onton", "onton-setsid-exec", "onton-worker"
    # Rewrite CI's hardcoded libgmp path to this machine's Homebrew prefix
    gmp_lib = (Formula["gmp"].opt_lib/"libgmp.10.dylib").to_s
    %w[onton onton-worker].each do |name|
      old_path = Utils.popen_read("otool", "-L", bin/name)
        .lines.find { |l| l.include?("libgmp") }&.strip&.split&.first
      if old_path && old_path != gmp_lib
        system "install_name_tool", "-change", old_path, gmp_lib, bin/name
      end
    end
  end

  test do
    assert_match "onton", shell_output("#{bin}/onton --version 2>&1", 0)
    assert_predicate bin/"onton-setsid-exec", :executable?
    assert_match "Usage: onton-worker", shell_output("#{bin}/onton-worker 2>&1", 2)
  end
end
