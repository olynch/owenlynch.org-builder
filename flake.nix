{
  description = "Hakyll Website";

  inputs = {
    nixpkgs.url = "nixpkgs/20.09";

    hakyll-src = {
      url = "github:jaspervdj/hakyll/v4.13.4.1";
      flake = false;
    };
    hakyll-sass-src = {
      url = "github:meoblast001/hakyll-sass/release-0.2.4";
      flake = false;
    };

    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, hakyll-src, hakyll-sass-src }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        haskellPackages = pkgs.haskellPackages.extend (hsSelf: hsSuper: {
          hakyll = hsSuper.callCabal2nix "hakyll" "${hakyll-src}" { };
          hakyll-sass = hsSuper.callCabal2nix "hakyll-sass" "${hakyll-sass-src}" { };
        });
        packageName = "owenlynch-org";
      in {
        packages.${packageName} = haskellPackages.callCabal2nix packageName self { };

        defaultPackage = self.packages.${system}.${packageName};

        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [
            haskellPackages.haskell-language-server
            ghcid
            cabal-install
          ];

          inputsFrom = builtins.attrValues self.packages.${system};
        };

        hydraJobs.build = self.packages.${packageName};
      }
    );
}
