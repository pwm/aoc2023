{
  system ? builtins.currentSystem,
  compiler ? null,
}: let
  pkgs = import ./nix {inherit system compiler;};
in
  pkgs.mkShell {
    buildInputs = [
      pkgs.aoc.shell
    ];
    shellHook = ''
      export NIX_SHELL_PATH=${pkgs.aoc.shell}
      export LD_LIBRARY_PATH=${pkgs.aoc.shell}/lib:$LD_LIBRARY_PATH
      logo
    '';
  }
