let
  nixpkgs = (import <nixpkgs> {}).fetchgit {
    url    = "git@github.com:nixos/nixpkgs.git";
    rev    = "718106e958cbd872ecf3e08a451b80f68f950dae";
    sha256 = "72ef1a4b66312676d0b7e1684d3d68a5e82fdff1725d8816a9dac7eff4ee81e8";
  };
in
{ pkgs ? import nixpkgs { config.allowUnfree = true; }
, haskellPackages ? pkgs.haskellPackages
, src ? ./.
, name ? "markov-processes"
}:
let
  inherit (haskellPackages) buildLocalCabalWithArgs callPackage;
in buildLocalCabalWithArgs {
  inherit src name;
  args = {
    assertions   = callPackage ./nix/assertions {};
    memoize      = callPackage ./nix/memoize {};
    functorInfix = callPackage ./nix/functor-infix {};
  };
}
