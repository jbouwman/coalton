{
  description = "A flake for coalton";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-25.05";
    flake-parts.url = "github:hercules-ci/flake-parts";
  };
  outputs = inputs@{ nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [ flake-parts.flakeModules.easyOverlay ];
      systems = nixpkgs.lib.platforms.all;
      perSystem = { config, pkgs, system, ... }:
        let
          ############ Settings ############
          ## Project name
          pname = "coalton";
          ## Source directory
          src = ./.;
          ## Dependencies
          lispLibs = lisp: with lisp.pkgs; [
            alexandria
            computable-reals
            concrete-syntax-tree
            eclector
            eclector-concrete-syntax-tree
            fiasco
            float-features
            html-entities
            trivial-garbage
            trivial-gray-streams
            yason
          ];
          ## Non-Lisp dependencies
          nativeLibs = with pkgs; [ mpfr ];
          ## Supported Lisp implementations
          lispImpls = [
            "sbcl"
            "ccl"
          ];
          ##################################
          systems = [
            "coalton" # Main ASDF system for Coalton
            "coalton-compiler" # Compiler ASDF system for Coalton
            "coalton-asdf" # ASDF extension for .coal file
            "coalton/testing" # A Helper ASDF system to test software
          ];
          version = let
            txt = builtins.readFile "${src}/VERSION.txt";
            ver = builtins.replaceStrings [''"''] [""] txt;
          in ver;
          isAvailable = impl: let
            basePkgs = import nixpkgs { inherit system; overlays = []; };
            lisp = basePkgs.${impl};
          in (builtins.tryEval lisp).success
             && (builtins.elem system lisp.meta.platforms)
             && (!lisp.meta.broken);
          availableLispImpls = builtins.filter isAvailable lispImpls;
          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath nativeLibs;
          ## SBCL: the jbouwman/sbcl fork pinned by the kreisler build (the
          ## "epsilon" SBCL, 2.6.4). Used here for two reasons: GOAL-025
          ## targets integrating Coalton into kreisler, so the compiler
          ## should build under the same SBCL; and mutual-recursion-through-
          ## typeclass tail-call elimination -- exercised by the test suite
          ## (tests/runtime-tests.ct) -- was fixed upstream in SBCL 2.5.7.38,
          ## whereas nixpkgs nixos-25.05 ships 2.5.4. The fork is built from
          ## source and wrapped with wrapLisp so the nixpkgs lisp package set
          ## (withPackages / buildASDFSystem) is available on top of it. Keep
          ## the rev and hash in sync with kreisler's nixos/packages/
          ## sbcl-epsilon.nix (epsilon/vendor/sbcl/SBCL_REV).
          sbclEpsilonRaw = pkgs.stdenv.mkDerivation {
            pname = "sbcl-epsilon";
            version = "2.6.4.28179.73f27e460.epsilon";
            src = pkgs.fetchFromGitHub {
              owner = "jbouwman";
              repo = "sbcl";
              rev = "73f27e46007b94d1ff76964eb2d85e6d46fa0f2d";
              hash = "sha256-e+Wlo96Vz0+eGEIePvq/QFZ8vyLI7/H55DIodaRmidc=";
            };
            nativeBuildInputs = [ pkgs.sbcl ];
            buildInputs = [ pkgs.zlib pkgs.zstd ];
            postPatch = ''
              echo '"2.6.4.28179.73f27e460.epsilon"' > version.lisp-expr
              patchShebangs .
            '';
            dontConfigure = true;
            buildPhase = ''
              runHook preBuild
              sh make.sh --fancy --with-sb-fiber --prefix=$out
              runHook postBuild
            '';
            doCheck = false;
            installPhase = ''
              runHook preInstall
              INSTALL_ROOT=$out sh install.sh
              runHook postInstall
            '';
            ## The fork installs its binary as bin/sbcl; wrapLisp would
            ## otherwise derive the program name from pname (sbcl-epsilon).
            meta.mainProgram = "sbcl";
          };
          epsilonSbcl = pkgs.wrapLisp {
            pkg = sbclEpsilonRaw;
            faslExt = "fasl";
            flags = [ "--dynamic-space-size" "3000" ];
          };
          ## Use the epsilon SBCL in place of nixpkgs sbcl; other impls (ccl)
          ## are taken from nixpkgs unchanged.
          lispFor = impl: if impl == "sbcl" then epsilonSbcl else pkgs.${impl};
          bundledPackage = { lisp }: rec {
            sourceErrorLib = lisp.buildASDFSystem {
              inherit version;
              pname = "source-error";
              src = "${src}/source-error";
              systems = [ "source-error" ];
              lispLibs = with lisp.pkgs; [ alexandria ];
            };
            mainLib = lisp.buildASDFSystem {
              inherit pname version src systems nativeLibs;
              lispLibs = (lispLibs lisp) ++ [ sourceErrorLib ];
            };
            lisp' = lisp.withPackages (ps: [ mainLib ]) // {
              inherit (lisp) meta;
            };
          };
          recipe = {
            sbcl = bundledPackage {
              lisp = epsilonSbcl;
            };
            ccl = bundledPackage {
              lisp = pkgs.ccl;
            };
          };
          packages = impl: [
            {
              name = "${impl}-${pname}";
              value = recipe.${impl}.mainLib;
            }
          ];
          devPackages = impl:
            (lispFor impl).withPackages (ps: lispLibs (lispFor impl));
          overlays = impl: [
            {
              name = impl;
              value = (lispFor impl).withOverrides
                (self: super: { ${pname} = config.packages."${impl}-${pname}"; });
            }
          ];
        in {
          overlayAttrs =
            builtins.listToAttrs (builtins.concatMap overlays availableLispImpls);
          devShells.default = pkgs.mkShell {
            inherit LD_LIBRARY_PATH;
            shellHook = ''
              export CL_SOURCE_REGISTRY=$PWD:$PWD/source-error
            '';
            packages = builtins.map devPackages availableLispImpls;
          };
          packages = builtins.listToAttrs
            (builtins.concatMap packages availableLispImpls);
        };
    };
}
