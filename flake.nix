{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        my-python-packages = ps:
          with ps; [
            pip
            python-lsp-server
            pylsp-mypy
            pyls-flake8
            pyls-isort
            rope
            pylsp-rope
            python-lsp-black
            ipython
            ipywidgets
            jupyter
            jupyterlab
            lxml
            matplotlib
            networkx
            numpy
            openpyxl
            pandas
            pandas-datareader
            patsy
            scikit-learn
            scikitimage
            scipy
            seaborn
            spyder
            statsmodels
          ];
        pmt = pkgs.rPackages.buildRPackage {
          name = "pmt";
          src = pkgs.fetchFromGitHub {
            owner = "jbedo";
            repo = "pmt";
            rev = "f5af7ada6f419382415335f5ae283f8a4643f79c";
            sha256 = "ODIRNRKsqBOqrOINQpOBOET5izKmhnp2F8DCVl4BOQI=";
          };
        };
        rEnvPackages = with pkgs.rPackages; [
          languageserver
          lintr
          tidyverse
          markdown
          rmarkdown
          knitr
          xtable
          nor1mix
          locfit
          hdrcde
          devtools
          ncdf4 # for climod
        ];
        R-dev = pkgs.rWrapper.override { packages = rEnvPackages; };
      in {
        ## TODO: add a pre-commit hook to git describe > ...
        ## https://github.com/cachix/pre-commit-hooks.nix
        ## also, the build output should run the R code, make figures,
        ## then tectonic -X build
        devShell = with pkgs;
          mkShellNoCC {
            name = "R";
            buildInputs = [ my-python-packages R-dev ];

            # If for some reason you need to install
            # packages manually
            shellHook = ''
              mkdir -p "$(pwd)/_libs"
              export R_LIBS_USER="$(pwd)/_libs"
            '';
          };

        # apps."x86_64-linux"
        apps.tex_and_rstudio = {
          type = "app";
          program = let
            rstudioTexWrapper = pkgs.runCommand "rstudioTexWrapper" {
              buildInputs = [ pkgs.makeWrapper ];
            }
            # wants full texlive for framed.sty
              (''
                mkdir -p $out/bin
                makeWrapper ${
                  pkgs.rstudioWrapper.override {
                    packages = with (pkgs.rPackages); [
                      markdown
                      rmarkdown
                      knitr
                      tidyverse
                      lintr
                      xtable
                      shiny
                      locfit
                      ks
                      hdrcde
                      ncdf4
                      devtools
                    ];
                  }
                }/bin/rstudio $out/bin/rstudio --prefix PATH : ${
                  pkgs.lib.makeBinPath [
                    pkgs.texlive.combined.scheme-full
                    pkgs.pandoc
                    pkgs.zlib
                  ]
                }
              '');
          in "${rstudioTexWrapper}/bin/rstudio";
        };
        packages = {
          document = pkgs.stdenvNoCC.mkDerivation rec {
            name = "auction4567";
            src = self;
            buildInputs = [
              # Add packages below
              pkgs.tectonic
              R-dev
            ];
            phases = [ "unpackPhase" "buildPhase" "installPhase" ];
            buildPhase = ''
              export PATH="${pkgs.lib.makeBinPath buildInputs}";
              export HOME="$TMP" ;
              Rscript ./src/code/data.R
              tectonic -X build
            '';
            installPhase = ''
              mkdir -p $out
              cp build/${name}/${name}.pdf $out/
            '';
          };
        };
        defaultPackage = self.packages.${system}.document;
      });
}
