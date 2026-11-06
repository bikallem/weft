{
  description = "weft: concurrency for OCaml 5, and its development tools";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  # The test library and its ppx (expect tests, coverage, mutation). We use
  # the fork bikallem/windtrap. The fork has changes after the release 0.2.0
  # of invariant-hq, and it has no release. flake.lock pins a commit of its
  # main branch, and CI on FreeBSD pins the same commit (ci.yml).
  inputs.windtrap = {
    url = "github:bikallem/windtrap";
    inputs.nixpkgs.follows = "nixpkgs";
  };

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
  #
  # The package weft-tls (library weft-tls) has the same three ways:
  # weft.lib.tlsPackage in a scope that also has weft, the overlay, and
  # weft.packages.${system}.weft-tls.
  outputs =
    {
      self,
      nixpkgs,
      windtrap,
    }:
    let
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];
      forAllSystems = f: nixpkgs.lib.genAttrs systems (system: f nixpkgs.legacyPackages.${system});

      # The libraries weft, weft.posix, weft.linux (on Linux), weft.kqueue (on
      # macOS and FreeBSD), weft.sim and weft.flow. The tests need windtrap, so
      # the package does not run them. The source has only the files of the
      # build.
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
              ./LICENSE.MPL-2.0
              ./README.md
            ];
          };
          doCheck = false;
          meta = {
            description = "Concurrency for OCaml 5 with CML events and a deterministic simulator";
            homepage = "https://github.com/bikallem/weft";
            # The files of weft.flow that came from caravan are MPL-2.0.
            license = [
              lib.licenses.isc
              lib.licenses.mpl20
            ];
          };
        };

      # The library weft-tls: the TLS flow of caravan (design section 2.19).
      # The scope of the caller gives weft, tls and mirage-crypto-rng. The
      # source has only the files of the build, and dune builds only the
      # package weft-tls.
      tlsPackage =
        {
          lib,
          buildDunePackage,
          weft,
          tls,
          mirage-crypto-rng,
        }:
        buildDunePackage {
          pname = "weft-tls";
          version = "0.1.0-dev";
          minimalOCamlVersion = "5.5";
          src = lib.fileset.toSource {
            root = ./.;
            fileset = lib.fileset.unions [
              ./dune-project
              ./weft-tls.opam
              ./src/tls
              ./LICENSE
              ./LICENSE.MPL-2.0
              ./README.md
            ];
          };
          propagatedBuildInputs = [
            weft
            tls
            mirage-crypto-rng
          ];
          doCheck = false;
          meta = {
            description = "TLS flows for weft, with ocaml-tls";
            homepage = "https://github.com/bikallem/weft";
            # The library is a changed copy of a file of caravan.
            license = lib.licenses.mpl20;
          };
        };
    in
    {
      lib = { inherit package tlsPackage; };

      overlays.default = final: prev: {
        ocaml-ng = prev.ocaml-ng // {
          ocamlPackages_5_5 = prev.ocaml-ng.ocamlPackages_5_5.overrideScope (
            ofinal: _: {
              weft = ofinal.callPackage package { };
              weft-tls = ofinal.callPackage tlsPackage { };
            }
          );
        };
      };

      packages = forAllSystems (
        pkgs:
        let
          ocamlPackages = pkgs.ocaml-ng.ocamlPackages_5_5;
        in
        rec {
          weft = ocamlPackages.callPackage package { };
          weft-tls = ocamlPackages.callPackage tlsPackage { inherit weft; };
          default = weft;
        }
      );

      devShells = forAllSystems (
        pkgs:
        let
          # nixpkgs has the older windtrap 0.2.0. Thus we override the two
          # packages with the fork, built with the OCaml of this scope.
          ocamlPackages = pkgs.ocaml-ng.ocamlPackages_5_5.overrideScope (
            final: _: {
              windtrap = final.callPackage windtrap.lib.windtrap { };
              ppx_windtrap = final.callPackage windtrap.lib.ppx_windtrap { };
            }
          );
        in
        {
          default = pkgs.mkShell {
            packages = [
              # OCaml 5.5, dune and the test library.
              ocamlPackages.windtrap
              ocamlPackages.ppx_windtrap
              ocamlPackages.dscheck
              # weft-tls and its tests. mirage-crypto-ec makes the keys of the
              # test certificates.
              ocamlPackages.tls
              ocamlPackages.x509
              ocamlPackages.domain-name
              ocamlPackages.ipaddr
              ocamlPackages.mirage-crypto-rng
              ocamlPackages.mirage-crypto-ec
              ocamlPackages.ptime
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
