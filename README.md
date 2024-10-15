# Infinite scrolling via HTMX.

This project is a Haskell-based web server that demonstrates infinite scrolling with constant space using HTMX.

## Features

- Infinite scrolling of integers with constant HTML space
- HTMX integration for dynamic content loading
- Simple and efficient server setup with Scotty

## Requirements

- [GHC (Glasgow Haskell Compiler)](https://www.haskell.org/ghc/)
- [Cabal](https://www.haskell.org/cabal/)
- [NIX](https://nixos.org/download.html) (optional, to build the docker image)
- [Just](https://github.com/casey/just) (for running tasks)
-
## Build via nix

```sh
just build
```

## Run via cabal

Run the server with on port 3001 with a pagesize of 100 rows:

```sh
cabal run htmx-scroll -- --port 3001 --page 100
```

## Notes on stability

1. If we zoom out enough, the pagesize will be too small to fill the viewport and the requests will loop indefinitely.
