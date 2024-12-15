{
  inputs = {
    opam-nix.url = "github:tweag/opam-nix";
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.follows = "opam-nix/nixpkgs";
  };
  outputs = { self, flake-utils, opam-nix, nixpkgs }@inputs:
    let package = "cpspg";
    in flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        on = opam-nix.lib.${system};
        scope =
          on.buildOpamProject { } package ./. { ocaml-system = "*"; };
        overlay = final: prev:
          {
            # Your overrides go here

          };
      in rec {
        legacyPackages = scope.overrideScope overlay;

        packages = rec {
          default = self.legacyPackages.${system}.${package};
          cpspg = default;
        };

        devShells.default = with pkgs; pkgs.mkShell {
          buildInputs = [ 
              # Source file formatting
              nixpkgs-fmt
              ocamlformat
              # For `dune build --watch ...`
              fswatch
              # For `dune build @doc`
              ocamlPackages.odoc
              # OCaml editor support
              ocamlPackages.ocaml-lsp
              # Nicely formatted types on hover
              ocamlPackages.ocamlformat-rpc-lib
              # Fancy REPL thing
              ocamlPackages.utop
              legacyPackages.cpspg
          ];
        };
      });
}
