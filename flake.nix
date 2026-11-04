{
  description = "Nix functions that render structured data into source text and store it";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = {nixpkgs, ...}: {
    lib = import ./lib.nix {inherit (nixpkgs) lib;};
  };
}
