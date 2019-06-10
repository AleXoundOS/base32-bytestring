{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, bytestring, cpu, criterion, hspec
      , QuickCheck, stdenv, cabal-install
      }:
      mkDerivation {
        pname = "base32-bytestring";
        version = "0.2.1.0";
        sha256 = "0z0q3fw3jzprgxmq9b2iz98kf4hwl3nydrzlaiwk81aplisfdgkl";
        libraryHaskellDepends = [ base bytestring cpu ];
        testHaskellDepends = [ base bytestring hspec QuickCheck cabal-install];
        benchmarkHaskellDepends = [ base bytestring criterion ];
        homepage = "https://github.com/pxqr/base32-bytestring";
        description = "Fast base32 and base32hex codec for ByteStrings";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
