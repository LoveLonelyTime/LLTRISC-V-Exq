let
  pkgs = import <nixpkgs> {};
  firrtl = pkgs.callPackage ./firrtl.nix {};
in
pkgs.mkShellNoCC {
  packages = [
    pkgs.mill pkgs.dtc pkgs.verilator firrtl
  ];
}
