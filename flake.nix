{
  description = "Erlang development environment for the `json-rpc` project.";

  inputs = {
    nixpkgs = { url = "github:NixOS/nixpkgs/master"; };
    flake-utils = { url = "github:numtide/flake-utils"; };
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
      in
      {
        devShell = pkgs.mkShell {
          shellHook = ''
            echo "Entering development shell... type `exit` to quit."
          '';
          buildInputs = with pkgs; [
            erlfmt
            erlang
            rebar3
          ];
        };
      }
    );
}
