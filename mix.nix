{
  packages = [ "base" "containers" "directory" "hashable" "haskell-src-exts" ];
  base = ./.;
  ghcOpts = ["-O" "-Wall"];
  executables = {
    mix = {
      file = "mix/Main.hs";
    };
  };
  sources = import ./sources.nix;
}
