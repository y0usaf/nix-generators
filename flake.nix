{
  description = "Nix functions that render structured data into source text and store it";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  inputs.iets = {
    url = "github:numtide/iets";
    inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = {
    self,
    nixpkgs,
    iets,
  }: {
    lib = import ./lib.nix {inherit (nixpkgs) lib;};

    checks.x86_64-linux.source-file = let
      pkgs = nixpkgs.legacyPackages.x86_64-linux;
    in
      pkgs.runCommand "nix-generators-source-file" {
        nativeBuildInputs = [pkgs.nix iets.packages.x86_64-linux.default pkgs.diffutils];
      } ''
        export HOME=$TMPDIR NIX_CONFIG="experimental-features = nix-command"
        mkdir -p nixpkgs work
        cp -r ${nixpkgs}/lib ${nixpkgs}/.version nixpkgs/
        cp ${./lib.nix} work/lib.nix
        cp ${./checks/source-file.nix} work/source-file.nix
        expression="import $PWD/work/source-file.nix rec { lib = import $PWD/nixpkgs/lib; generators = import $PWD/work/lib.nix { inherit lib; }; }"
        nix eval --store $TMPDIR/store --impure --json --expr "$expression" > work/nix.json
        iets eval --no-cache --impure --json -E "$expression" > work/iets.json
        cat work/nix.json work/iets.json
        diff work/nix.json work/iets.json
        touch $out
      '';
  };
}
