{ nixpkgs ? import <nixpkgs> {}
, coreutils ? nixpkgs.coreutils
, findutils ? nixpkgs.findutils
, hpkgs ? nixpkgs.haskellPackages
, lib ? nixpkgs.lib
, stdenv ? nixpkgs.stdenv
, writeScript ? nixpkgs.writeScript
, mixnix ? import ./mix.nix
}:
let sources = mixnix.sources;
    fileName = file: builtins.replaceStrings ["/" "."] ["_" "_"] file;
    packages = p: builtins.map (pack: p.${pack} or null) mixnix.packages;
    ghc = hpkgs.ghcWithPackages packages;
    ghcOpts = mixnix.ghcOpts;
    objs = lib.mapAttrs buildModule sources;

    buildModule = file:
      { name ? fileName file
      , transDeps
      }: (
        let srcFile = (toString mixnix.base) + "/" + file;
            src = lib.cleanSourceWith {
              filter = (path: type: toString path == srcFile || (type == "directory" && lib.strings.hasPrefix (toString path) srcFile));
              src = mixnix.base;
            };
            depsArg = "-i" + lib.makeSearchPathOutput "" "" (map (f: objs.${f}) transDeps);
            ghcArgs = lib.strings.escapeShellArgs (lib.concatLists [ghcOpts [depsArg file]]);
        in builtins.derivation {
          inherit name;
          system = stdenv.system;
          PATH = lib.makeBinPath [coreutils ghc];
          builder = writeScript (name + "-builder") ''
            #!${stdenv.shell}
            set -e
            cp -r ${src}/. ./
            mkdir -p $out tmp
            ghc -c -tmpdir tmp -outputdir $out ${ghcArgs} 2>&1
          '';
        });

    buildExec = name: { file
                      , postBuild ? ""
                      }: (
      let source = sources.${file};
          transDeps = [file] ++ source.transDeps;
          depPaths = lib.strings.escapeShellArgs (map (f: objs.${f}) transDeps);
          packageList = map (p: "-package ${p}") mixnix.packages;
          ghcArgs = lib.strings.escapeShellArgs (ghcOpts ++ packageList);
      in builtins.derivation {
        inherit name;
        system = stdenv.system;
        PATH = lib.makeBinPath [coreutils findutils ghc];
        builder = writeScript (name + "-builder") ''
          #!${stdenv.shell}
          set -e
          mkdir -p $out/bin tmp
          find ${depPaths} -name '*.o' -exec ghc -tmpdir tmp -o $out/bin/${name} ${ghcArgs} '{}' '+' 2>&1
          ${postBuild}
        '';
      }
    );
 in lib.mapAttrs buildExec mixnix.executables
