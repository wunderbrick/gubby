# Gubby: A Creature You Take Care Of

![Image of Gubby](https://raw.githubusercontent.com/wunderbrick/gubby/master/gubby.png)

My first Haskell project and my first [Reflex-FRP](https://reflex-frp.org/) project! May contain bugs.

Beware! Gubby harbors a dark secret.

## Live Demo

[Gubby](https://wunderbrick.github.io/gubby/)

## Development

### Getting Started

* Clone with `--recurse-submodules`.

* Enable the Reflex binary cache to prevent endless compiling. If you're on NixOS, see [Reflex binary cache](https://github.com/reflex-frp/reflex-platform/blob/develop/notes/NixOS.md). If you're not using NixOS then just let the `try-reflex` script configure the cache for you. Regardless of your platform you need to run `try-reflex` to do some additional setup.

* `cd reflex-platform` and run `./try-reflex`. Go ahead and exit the shell you're dropped into when this is finished and `cd ../`.

This project skeleton was based off of this [document](https://github.com/reflex-frp/reflex-platform/blob/develop/docs/project-development.rst) which might be helpful to you.

### Develop with Live Reload (Almost)

`nix-shell -A shells.ghc --run 'ghcid -W -c "cabal --project-file=cabal.project new-repl gubby" -T Main.main'`

Go to http://localhost:3003/ in your browser. You still have to refresh the page on changes but you get automatic recompilation.

### Compile With GHCJS for Web Deployment

`nix-shell -A shells.ghcjs --run "cabal --project-file=cabal-ghcjs.project --builddir=gubby/dist-ghcjs new-build gubby"`

Just use the resulting files with your favorite server like you would any other HTML/JS/CSS.

### Build an Android APK

`nix-build -o YOUR-DIR-HERE -A android.frontend`
