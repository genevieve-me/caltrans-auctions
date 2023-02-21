This is based on direnv, which can be set up with
[home-manager](https://github.com/nix-community/home-manager) via:

```
programs.direnv = {
  enable = true;
  nix-direnv.enable = true;
};
```

On non-NixOS systems, home-manager can be installed
[as described in its manual](https://nix-community.github.io/home-manager/index.html#sec-flakes-standalone)
- i.e., run
```
nix flake new ~/.config/nixpkgs -t github:nix-community/home-manager
nix build --no-link path:~/.config/nixpkgs#homeConfigurations.jdoe.activationPackage
"$(nix path-info path:~/.config/nixpkgs#homeConfigurations.jdoe.activationPackage)"/activate
home-manager switch --flake 'path:~/.config/nixpkgs#jdoe'
```
