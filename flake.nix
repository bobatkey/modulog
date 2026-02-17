{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-25.11";
  };
  outputs = { self, flake-utils, nixpkgs }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        localPackages = {
          ppx_monoid = { lib, fetchurl, ocamlPackages }:
            ocamlPackages.buildDunePackage (finalAttrs: {
              pname = "ppx_monoid";
              version = "0.3.3";
              minimalOcamlVersion = "4.04.1";
              src = fetchurl {
                url = "https://github.com/bobatkey/ppx-monoid/archive/refs/tags/${finalAttrs.version}.tar.gz";
                hash = "sha256-dTtfKywyvYsFT/DwX3YwvXCfOGYI2qfr2xVEVsoBqQg=";
              };
              propagatedBuildInputs = with ocamlPackages; [
                ppxlib
              ];
            });

          modules = { lib, ocamlPackages }:
            ocamlPackages.buildDunePackage (finalAttrs: {
              pname = "modules";
              version = "0.0.1";
              minimalOcamlVersion = "5.0.0";
              src = ./.;
            });

          display_names = { lib, ocamlPackages }:
            ocamlPackages.buildDunePackage (finalAttrs: {
              pname = "display_names";
              version = "0.0.1";
              minimalOcamlVersion = "5.0.0";
              src = ./.;
            });

          idealised_algol = { lib, ocamlPackages }:
            ocamlPackages.buildDunePackage (finalAttrs: {
              pname = "idealised_algol";
              version = "0.0.1";
              minimalOcamlVersion = "5.0.0";
              src = ./.;
              propagatedBuildInputs = with ocamlPackages; [
                display_names
                fmt
                ppx_monoid
              ];
            });

          datalog = { lib, ocamlPackages }:
            ocamlPackages.buildDunePackage (finalAttrs: {
              pname = "datalog";
              version = "0.0.1";
              minimalOcamlVersion = "5.0.0";
              src = ./.;
              propagatedBuildInputs = with ocamlPackages; [
                ocamlgraph
                fmt
                display_names
              ];
            });

          relation_machine = { lib, ocamlPackages }:
            ocamlPackages.buildDunePackage (finalAttrs: {
              pname = "relation_machine";
              version = "0.0.1";
              minimalOcamlVersion = "5.0.0";
              src = ./.;
              propagatedBuildInputs = with ocamlPackages; [
                ocamlgraph
                fmt
                ppx_monoid
                idealised_algol
                datalog
              ];
            });

          modulog = { lib, ocamlPackages }:
            ocamlPackages.buildDunePackage (finalAttrs: {
              pname = "modulog";
              version = "0.0.1";
              minimalOcamlVersion = "5.0.0";
              src = ./.;
              nativeBuildInputs = with ocamlPackages; [
                menhir
              ];
              propagatedBuildInputs = with ocamlPackages; [
                fmt
                datalog
                menhirLib
                modules
              ];
            });

          modulog-bin = { lib, ocamlPackages }:
            ocamlPackages.buildDunePackage (finalAttrs: {
              pname = "modulog-bin";
              version = "0.0.1";
              minimalOcamlVersion = "5.0.0";
              src = ./.;
              propagatedBuildInputs = with ocamlPackages; [
                cmdliner
                modulog
                relation_machine
              ];
            });
        };

        ocamlPackages =
          pkgs.ocaml-ng.ocamlPackages_5_3 //
          builtins.mapAttrs
            (name: value: pkgs.callPackage value { inherit ocamlPackages; })
            localPackages;
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
              ppx_monoid
              ocamlgraph
              menhir
              menhirLib
            ];
          };

          packages = rec {
            inherit (ocamlPackages)
              modules
              display_names
              idealised_algol
              relation_machine
              datalog
              modulog
              modulog-bin;
            default = modulog-bin;
          };
        });
}
