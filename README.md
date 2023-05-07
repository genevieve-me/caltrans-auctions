# Python libraries

The Python code for this paper can all be found in `./src/code/data.R`.
It relies on `pandas` for general data manipulation, `seaborn` for
visualization, `numpy` for auxiliary functions like `cumsum`, and `statsmodels`
for regression and other modelling.

## R libraries

Similarly, the R code for this paper is at `./src/code/data.R`.
It relies on the libraries `hdrcde`, for conditional kernel density estimation
with the `cde` function,
`climod`, for integrating that density with `pdf2cdf`, and
`sfsmisc` for integration of generic densities with `integrate.xy`.
R libraries have better facilities than Python for KDE as of 2023.

# LaTeX

The LaTeX code for this paper should be built with LuaLaTeX or XeLaTeX because
it relies on the `unicode-math` library for modern mathematical symbols.
It is compiled with [Tectonic](https://tectonic-typesetting.github.io/en-US/),
with file structure described in `Tectonic.toml`.

# Building

This repository can be built with
```
Rscript ./src/code/*.R
tectonic -X build
```
or with `nix build`, which will automatically pull in all dependencies
and offers improved reproducibility.

## Developing

The devshell for this repository is most easily used with nix-direnv, which can
be set up with [home-manager](https://github.com/nix-community/home-manager):

```
...
programs.direnv = {
  enable = true;
  nix-direnv.enable = true;
};
...
```

On non-NixOS systems, home-manager can be installed
[as described in its manual](https://nix-community.github.io/home-manager/index.html#sec-flakes-standalone);
i.e., run
```
nix flake new ~/.config/nixpkgs -t github:nix-community/home-manager
nix build --no-link path:~/.config/nixpkgs#homeConfigurations.jdoe.activationPackage
"$(nix path-info path:~/.config/nixpkgs#homeConfigurations.jdoe.activationPackage)"/activate
home-manager switch --flake 'path:~/.config/nixpkgs#jdoe'
```
