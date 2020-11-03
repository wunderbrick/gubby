{ bootstrap ? import <nixpkgs> {}, system ? builtins.currentSystem }:
let
  reflex-platform = bootstrap.fetchFromGitHub {
    owner = "reflex-frp";
    repo  = "reflex-platform";
    rev = "8d421e9e06b0477cbc065346aaf596c9db6cc387";
    sha256 = "06fy5b0mk5k2ps1h78yihf4j76cb855r86y9p4jv5d91nfyl2dck";
  };
in 
  import reflex-platform {inherit system; config.android_sdk.accept_license = true;}