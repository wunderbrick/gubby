# use nix-prefetch-git https://github.com/nixos/nixpkgs-channels.git refs/heads/nixos-20.03 > pinned-nixpkgs.json
{
  bootstrap ? import <nixpkgs> {}
, json
}:
let 
  nixpkgs = builtins.fromJSON (builtins.readFile json);
  src = bootstrap.fetchFromGitHub {
    owner = "NixOS";
    repo  = "nixpkgs-channels";
    inherit (nixpkgs) rev sha256;
  };
in 
  import src {}