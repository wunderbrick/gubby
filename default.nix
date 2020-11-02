{ system ? builtins.currentSystem }:
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
})
