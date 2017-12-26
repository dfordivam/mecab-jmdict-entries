{ nixpkgs ? import <nixpkgs> {}
, reflex-platform ? import ../code/reflex-platform {}
, haskellPackages1 ? reflex-platform.ghc }:

let

  inherit (nixpkgs) pkgs;

  haskellPackages = haskellPackages1.override {
     overrides = self: super:
      {
        mecab = pkgs.haskell.lib.doJailbreak super.mecab;
      };
    };

  f = { mkDerivation, base, binary, bytestring, conduit, containers
      , data-default, hs-nlp-jp-utils, jmdict-ast, jmdict-xml-parser
      , lens, mecab, pretty-simple, protolude, resourcet, stdenv, text
      , xml-conduit
      }:
      mkDerivation {
        pname = "mecab-jmdict";
        version = "0.1.0.0";
        src = ./.;
        libraryHaskellDepends = [
          base binary bytestring conduit containers data-default
          hs-nlp-jp-utils jmdict-ast jmdict-xml-parser lens mecab
          pretty-simple protolude resourcet text xml-conduit
        ];
        description = "Mecab entries from JMDict";
        license = stdenv.lib.licenses.mit;
      };

  hs-nlp-jp-utils
    = haskellPackages.callCabal2nix "hs-nlp-jp-utils"
      (pkgs.fetchFromGitHub
      { owner = "dfordivam";
        repo = "hs-nlp-jp-utils";
        rev = "92c3a1ee168f121935ad8090f495c777df6a464c";
        sha256 = "1k754dkzns90bznrl6yp0vnjvd707bai3v6gi7vik2dynvc2g7z0";
      }) {};
  jmdict-ast
    = haskellPackages.callCabal2nix "jmdict-ast"
     (pkgs.fetchFromGitHub
    { owner = "dfordivam";
      repo = "jmdict-ast";
      rev = "f681d1fa37c29e17d3c58e87c54dced96adbedd5";
      sha256 = "028rx943pxhjkwza6v4js31rvmznwp74xk8dcmrx3nz7v1dxgxvw";
    }) {};
  jmdict-xml-parser
    = haskellPackages.callCabal2nix "jmdict-xml-parser"
    (pkgs.fetchFromGitHub
    { owner = "dfordivam";
      repo = "jmdict-parser";
      rev = "fc1d700fd97dc3be053c87cfa5625111c788835f";
      sha256 = "19m8ykvwg8nl5a327lwg7axd7dp9988f0y3lli0a322w2g7q5d25";
    }) {inherit jmdict-ast;};

  drv = haskellPackages.callPackage f {
    inherit hs-nlp-jp-utils jmdict-xml-parser jmdict-ast;
    };

in

  if pkgs.lib.inNixShell then drv.env else drv
