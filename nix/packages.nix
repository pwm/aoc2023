{
  pkgs,
  compiler,
}: let
  lib = pkgs.lib;

  util = import ./util.nix {
    inherit pkgs;
    inherit (pkgs) lib gitignoreFilter;
  };

  conf = lib.importTOML ../nixkell.toml;

  ghcVersion =
    if compiler != null
    then compiler
    else conf.ghc.version;

  ghcVer = "ghc" + util.removeChar "." ghcVersion;

  hlsDisablePlugins =
    pkgs.lib.foldr
    (plugin: hls: pkgs.haskell.lib.disableCabalFlag (hls.override {${"hls-" + plugin + "-plugin"} = null;}) plugin);

  ourHaskell = pkgs.haskell.packages.${ghcVer}.override {
    overrides = hfinal: hprev: let
      depsFromDir = pkgs.haskell.lib.packagesFromDirectory {
        directory = ./packages;
      };
      manual = _hfinal: hprev: {
        profiteur = (pkgs.haskell.lib.doJailbreak (pkgs.haskell.lib.dontCheck hprev.profiteur)).overrideAttrs (prev: {
          meta = prev.meta // {broken = false;};
        });

        haskell-language-server =
          hlsDisablePlugins hprev.haskell-language-server conf.hls.disable_plugins;

        aoc = let
          filteredSrc = util.filterSrc ../. {
            ignoreFiles = conf.ignore.files;
            ignorePaths = conf.ignore.paths;
          };
        in
          (hprev.callCabal2nix "aoc" filteredSrc {}).overrideAttrs (prev: {
            preCheck = ''
              export AOC_INPUT_PATH=$out/input

              # Note: On the GH actions Linux runner $out did not exist at this point
              # hence creating it. On the MacOS runner and locally it does exist.
              mkdir -vp $AOC_INPUT_PATH

              if [[ -n "$(ls --almost-all --ignore=.gitkeep input)" ]]; then
                cp -vr input/* $AOC_INPUT_PATH
              fi
            '';
          });
      };

      profilingOverrides = hfinal: hprev: {
        compiler = pkgs.haskell.compiler.${ghcVer}.override {
          enableProfiling = true;
          enableLibraryProfiling = true;
        };
        mkDerivation = args:
          hprev.mkDerivation (args // {enableLibraryProfiling = true;});
      };
    in
      lib.composeExtensions depsFromDir manual hfinal hprev
      // (
        if conf.ghc.profiling
        then profilingOverrides hfinal hprev
        else {}
      );
  };

  ghc = ourHaskell.ghc.withPackages (
    _ps: pkgs.haskell.lib.getHaskellBuildInputs ourHaskell.aoc
  );

  haskell_tools = map (p: ourHaskell.${lib.removePrefix "haskellPackages." p}) conf.env.haskell_tools;

  tools = map util.getDrv conf.env.tools;

  scripts = import ./scripts.nix {inherit pkgs;};
in {
  bin = pkgs.haskell.lib.dontCheck(util.leanPkg ourHaskell.aoc);

  shell = pkgs.buildEnv {
    name = "aoc-env";
    paths = [ghc] ++ haskell_tools ++ tools ++ scripts;
  };
}
