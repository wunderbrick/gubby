{ system ? builtins.currentSystem }:
(import ./reflex-platform { inherit system; config.android_sdk.accept_license = true; }).project ({ pkgs, ... }: {
  useWarp = true;

  packages = {
    frontend = ./frontend;
  };

  android.frontend = {
    executableName = "frontend";
    applicationId = "org.example.gubby";
    displayName = "Gubby";
  };

  shells = {
    ghc = ["frontend"];
    ghcjs = ["frontend"];
  };
})
