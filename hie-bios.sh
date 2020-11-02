#!/usr/bin/env bash
source "$HOME/.bash_profile"; # NOTE: Some editors need help finding 'ob' and this assumes that `ob` is made available on your `$PATH` in `.bash_profile`
echo '-ignore-dot-ghci -no-user-package-db -package-env -F -optF __preprocessor-apply-packages -optF ./gubby/src -i./gubby/src' > "$HIE_BIOS_OUTPUT"
