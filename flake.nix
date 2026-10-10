{
  description = "affect-new: development tools";

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
      devShells = forAllSystems (pkgs: {
        # The TLA+ tools (tlc, tla2sany, pcal) for the specs in spec/tla.
        default = pkgs.mkShell {
          packages = [
            pkgs.tlaplus
            pkgs.jdk21_headless
          ];
        };
      });
    };
}
