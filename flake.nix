{
  description = "Erlang development environment for the `json-rpc` project.";

  inputs = {
    # Pinned to a channel branch rather than `master` so the toolchain is
    # reproducible: `master` moves on every nixpkgs commit and would give a
    # different Erlang/rebar3 to every developer and CI run that updates.
    nixpkgs = { url = "github:NixOS/nixpkgs/nixos-unstable"; };
    flake-utils = { url = "github:numtide/flake-utils"; };
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
      in
      {
        devShells.default = pkgs.mkShell {
          packages = with pkgs; [
            beam28Packages.erlang
            beam28Packages.rebar3
            erlang-language-platform
            erlfmt
          ];
        };

        formatter = pkgs.nixpkgs-fmt;
      }
    );
}
