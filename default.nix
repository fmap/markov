{ pkgs ? import <nixpkgs> { config.allowUnfree = true; }
, src ? ./.
}:
{
  build = pkgs.haskellPackages.buildLocalCabal src "hmm";
}
