{
  description = "owt";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; config = { allowBroken = true; }; };
        packageName = "owt";
        hsPkgs = with pkgs; haskell.packages.ghc981.override {
          overrides = final: prev: {
            req-conduit = haskell.lib.dontCheck prev.req-conduit;
          };
        };
      in with pkgs; rec {
        packages.${packageName} =
          hsPkgs.callCabal2nixWithOptions "${packageName}" ./. "--no-check" {  };
        packages.default = packages.${packageName};
        devShells.default = mkShell {};
      }
    );
}
