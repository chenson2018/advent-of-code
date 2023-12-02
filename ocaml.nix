with import <nixpkgs> { };

mkShell {
  name = "aoc-ocaml";

  buildInputs = [
		ocaml
		dune_3
		ocamlPackages.core
		ocamlPackages.utop
  ];
}
