with import <nixpkgs> { };

mkShell {
  buildInputs = [
    # OCaml
		ocaml
		dune_3
		ocamlPackages.core
		ocamlPackages.utop
		ocamlPackages.ppx_deriving

    # Haskell
    ghc 
    ormolu
  ];
}
