let
  nixpkgs = <nixpkgs>;
  pkgs = import nixpkgs {};
in
pkgs.mkShell {
  packages = with pkgs; [
    haskellPackages.stack
    haskell-language-server
    strictdoc
  ];
}
