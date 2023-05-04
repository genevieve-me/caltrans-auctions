## Building

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
