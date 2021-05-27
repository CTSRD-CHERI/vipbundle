{ pkgs ? import <nixpkgs> {} } :
with pkgs;
mkShell {
  name = "vipbundle-dev-env";
  buildInputs = [
    (ghc.withPackages (hPkgs: with hPkgs; [ regex-tdfa ]))
  ];
}
