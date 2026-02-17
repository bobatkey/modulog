{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-25.11";
  };
  outputs = { self, flake-utils, nixpkgs }:
        flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        ocamlPackages = pkgs.ocaml-ng.ocamlPackages_5_3;

        ppx_monoid = { lib, fetchurl, ocamlPackages }:
          ocamlPackages.buildDunePackage (finalAttrs: {
            pname = "ppx_monoid";
            version = "0.3.3";
            minimalOcamlVersion = "4.04.1";
            src = fetchurl {
              url = "https://github.com/bobatkey/ppx-monoid/archive/refs/tags/0.3.3.tar.gz";
              hash = "sha256-dTtfKywyvYsFT/DwX3YwvXCfOGYI2qfr2xVEVsoBqQg=";
            };
            propagatedBuildInputs = with ocamlPackages; [
              ppxlib
            ];
          });
      in
        { devShells.default = pkgs.mkShell {
            nativeBuildInputs = with ocamlPackages; [
              ocaml
              findlib
              pkgs.dune
              ocaml-lsp
              utop
            ];
            buildInputs = with ocamlPackages; [
              cmdliner
              fmt
              (pkgs.callPackage ppx_monoid { inherit ocamlPackages; })
              ocamlgraph
              menhir
              menhirLib
            ];
          };
        });
}
