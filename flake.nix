{
  description = "weft: development tools";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs =
    { self, nixpkgs }:
    let
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];
      forAllSystems = f: nixpkgs.lib.genAttrs systems (system: f nixpkgs.legacyPackages.${system});
    in
    {
      devShells = forAllSystems (
        pkgs:
        let
          ocamlPackages = pkgs.ocaml-ng.ocamlPackages_5_5;
          # The test library and its ppx (expect tests, coverage, mutation).
          # nixpkgs has 0.1.0, and we use 0.2.0.
          windtrapSrc = pkgs.fetchFromGitHub {
            owner = "invariant-hq";
            repo = "windtrap";
            rev = "v0.2.0";
            hash = "sha256-ZNgC26Oko47M8FyzibvnRNozt4vVGf6MvBlGztzTg+c=";
          };
          windtrap = ocamlPackages.buildDunePackage {
            pname = "windtrap";
            version = "0.2.0";
            src = windtrapSrc;
          };
          ppx_windtrap = ocamlPackages.buildDunePackage {
            pname = "ppx_windtrap";
            version = "0.2.0";
            src = windtrapSrc;
            propagatedBuildInputs = [
              windtrap
              ocamlPackages.ppxlib
            ];
          };
        in
        {
          default = pkgs.mkShell {
            packages = [
              # OCaml 5.5, dune and the test library.
              windtrap
              ppx_windtrap
              ocamlPackages.ocaml
              ocamlPackages.dune_3
              ocamlPackages.findlib
              pkgs.ocamlformat_0_29_0
              # The TLA+ tools (tlc, tla2sany, pcal) for the specs in spec/tla.
              pkgs.tlaplus
              pkgs.jdk21_headless

              # Dev tools
              ocamlPackages.ocaml-lsp
              ocamlPackages.utop
              ocamlPackages.ocamlformat
              ocamlPackages.odoc
            ];
          };
        }
      );
    };
}
