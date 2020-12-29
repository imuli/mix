{
  packages = [ "base" "containers" "directory" "hashable" "haskell-src-exts" ];
  base = ./.;
  ghcOpts = ["-O" "-Wall"];
  executables = {
    mix = {
      file = "mix/Main.hs";
    };
  };
  sources = {
    "src/Mix/Util.hs" = { transDeps = []; };
    "src/Mix/Modules.hs" = { transDeps = ["src/Mix/Util.hs"]; };
    "mix/Main.hs" = { transDeps = ["src/Mix/Modules.hs" "src/Mix/Util.hs"]; };
  };
}
