{pkgs}: let
  logo = pkgs.writeShellScriptBin "logo" ''
    set -euo pipefail
    echo -e "\n$(tput setaf 2)"
    echo "AoC" | ${pkgs.figlet}/bin/figlet
    echo -e "$(tput sgr0)\n"
  '';
  build = pkgs.writeShellScriptBin "build" ''
    set -euo pipefail
    nix-build nix/release.nix
  '';
  aoc = pkgs.writeShellScriptBin "aoc" ''
    set -euo pipefail
    result/bin/aoc +RTS -N -T -RTS -- "$@"
  '';
in [logo build aoc]
