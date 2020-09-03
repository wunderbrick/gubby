# Gubby: A Creature You Take Care Of

My first Haskell project and my first [Reflex-FRP](https://reflex-frp.org/) project!

## Live Demo

[Gubby](https://wunderbrick.github.io/)

## Development

### Develop with Live Reload (Almost)

`nix-shell -A shells.ghc --run 'ghcid -W -c "cabal --project-file=frontend/cabal.project new-repl all" -T Main.main'`

Go to http://localhost:3003/ in your browser. You still have to refresh the page on changes but you get automatic recompilation.

### Compile With GHCJS for Web Deployment

`nix-shell -A shells.ghcjs --run "cabal --project-file=frontend/cabal.project --builddir=frontend/dist-ghcjs new-build all"`

Just use the resulting files with your favorite server like you would any other HTML/JS/CSS.

### Build an Android APK

`nix-build -o YOUR-DIR-HERE -A android.frontend`