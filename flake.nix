{
  description = "weft: concurrency for OCaml 5, and its development tools";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  # Another flake can use weft in three ways:
  #
  # - In its own OCaml scope, as for a package of nixpkgs. This uses the
  #   compiler of that scope:
  #
  #     ocamlPackages = pkgs.ocaml-ng.ocamlPackages_5_5.overrideScope
  #       (final: prev: { weft = final.callPackage weft.lib.package { }; });
  #
  # - With the overlay, which adds weft to ocaml-ng.ocamlPackages_5_5.
  #
  # - As weft.packages.${system}.weft, with
  #   inputs.weft.inputs.nixpkgs.follows = "nixpkgs".
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

      # The libraries weft, weft.posix, weft.linux (on Linux), weft.kqueue (on
      # macOS and FreeBSD) and weft.sim. The tests need windtrap, so the
      # package does not run them. The source has only the files of the build.
      package =
        { lib, buildDunePackage }:
        buildDunePackage {
          pname = "weft";
          version = "0.1.0-dev";
          minimalOCamlVersion = "5.5";
          src = lib.fileset.toSource {
            root = ./.;
            fileset = lib.fileset.unions [
              ./dune-project
              ./weft.opam
              ./src
              ./LICENSE
              ./README.md
            ];
          };
          doCheck = false;
          meta = {
            description = "Concurrency for OCaml 5 with CML events and a deterministic simulator";
            homepage = "https://github.com/bikallem/weft";
            license = lib.licenses.isc;
          };
        };
    in
    {
      lib = { inherit package; };

      overlays.default = final: prev: {
        ocaml-ng = prev.ocaml-ng // {
          ocamlPackages_5_5 = prev.ocaml-ng.ocamlPackages_5_5.overrideScope (
            ofinal: _: { weft = ofinal.callPackage package { }; }
          );
        };
      };

      packages = forAllSystems (pkgs: rec {
        weft = pkgs.ocaml-ng.ocamlPackages_5_5.callPackage package { };
        default = weft;
      });

      devShells = forAllSystems (
        pkgs:
        let
          ocamlPackages = pkgs.ocaml-ng.ocamlPackages_5_5;
          # The test library and its ppx (expect tests, coverage, mutation).
          # We use the fork bikallem/windtrap at a commit of its main branch.
          # The fork has changes after the release 0.2.0 of invariant-hq, and
          # it has no release. CI on FreeBSD pins the same commit (ci.yml).
          windtrapVersion = "0.2.0-unstable-2026-10-14";
          windtrapSrc = pkgs.fetchFromGitHub {
            owner = "bikallem";
            repo = "windtrap";
            rev = "b80c58524f8ce268dcc6dbfb598b249b7174f351";
            hash = "sha256-VPMbLWQS2MuwcIXqUvGHhh4klndNOyPTNijXLpDOvt0=";
          };
          windtrap = ocamlPackages.buildDunePackage {
            pname = "windtrap";
            version = windtrapVersion;
            src = windtrapSrc;
          };
          ppx_windtrap = ocamlPackages.buildDunePackage {
            pname = "ppx_windtrap";
            version = windtrapVersion;
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
              ocamlPackages.dscheck
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
