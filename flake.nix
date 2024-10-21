{
  description = "owt";

  nixConfig = {
    extra-substituters = [
      "https://ghc.cachix.org"
      "https://nix-community.cachix.org"
    ];
    extra-trusted-public-keys = [
      "ghc.cachix.org-1:a751hwq9ydeP3Nr6h84iA9zSjxg9Z3uznqi4YBGjsiw="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    ];
  };

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        packageName = "owt";
        pkgs = import nixpkgs { inherit system; config = { allowBroken = true; }; };
        hsPkgs = pkgs.haskell.packages.ghc981.override {
          overrides = final: prev: {
            base64 = prev.base64_1_0;
          };
        };
      in rec {
        packages.${packageName} = hsPkgs.developPackage {
          name = packageName;
          root = ./.;
        };
        packages.default = packages.${packageName};
      }
    );
}
