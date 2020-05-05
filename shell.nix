# Nix expression for starting jupyter notebook
let
  pkgs   = import <nixpkgs> {inherit config overlays;};
  config = {
    allowBroken = true;
  };
  overlays = [ overlayHs ];
  # Haskell overlay
  overlayHs = self: super: {
    haskell = super.haskell // {
      packageOverrides = haskOverrides;
    };
  };
  lib           = pkgs.haskell.lib;
  haskOverrides = hsNew: hsOld: rec {
    Chart       = hsNew.callCabal2nix "Chart" ./chart/Chart.cabal             {};
    Chart-cairo = hsNew.callCabal2nix "Chart" ./chart-cairo/Chart-cairo.cabal {};
  };
  # Packages
  locals = p: with p;
    [ Chart
      Chart-cairo
    ];
in
  pkgs.haskellPackages.shellFor {
    packages = locals;
  }
  

