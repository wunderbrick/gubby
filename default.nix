{ system ? builtins.currentSystem }:

let
  pkgs = import ./nix/pkgs-from-json.nix { json = ./nixos-20-03.json; };
in
(import ./reflex-platform { inherit system; config.android_sdk.accept_license = true; }).project ({ pkgs, ... }: {
  useWarp = true;

  packages = {
    gubby = ./gubby;
  };

  android.frontend = {
    executableName = "gubby";
    applicationId = "org.example.gubby";
    displayName = "Gubby";
  };

  shells = {
    ghc = ["gubby"];
    ghcjs = ["gubby"];
  };

  # Addition tools outside of Haskell packages you want to include in the shell, e.g., MongoDB
  shellToolOverrides = self: super: {
    #inherit (pkgs) ghc;
  };

  # Haskell packages to override
  overrides = self: super: {
    #ghcide = pkgs.haskell.lib.dontCheck super.ghcide; 
    # Editors aren't picking up ghcide in nix-shell, not sure why. 
    # I have 20.03 pinned in my configuration.nix to stay synced with current Reflex-Platform tooling and work around this.
  };
})
