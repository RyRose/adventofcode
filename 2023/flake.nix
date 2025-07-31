{
  description = "Scala project flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs {
          inherit system;
        };
      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = [
            pkgs.scala-cli # Scala CLI (recommended for new projects)
            pkgs.jdk17 # Java 17 (can change to jdk21, etc.)
            pkgs.metals # Optional: Scala LSP for IDEs
            pkgs.sbt # Optional: for legacy builds
          ];
        };
      }
    );
}
