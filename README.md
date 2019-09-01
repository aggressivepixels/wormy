# Wormy

**Wormy** is a Snake clone built on [Elm](https://elm-lang.com).

Play it online at [itch.io](https://aggressivepixels.itch.io/wormy).

## Building

Assuming you have Elm 0.19 installed, just clone the repository, `cd` into it and then run `elm make src/Main.elm --output=wormy.html`. This will create a `wormy.html` file that you can run in your browser.

There's also a shell script that generates an optimized and minified build called [`make-and-minify.sh`](make-and-minify.sh). Run `./make-and-minify.sh src/Main.elm` to generate `elm.js` (the optimized build) and `elm.min.js` (the minified build). [`index.html`](index.html) depends on `elm.min.js`, so once you build it you can use [`index.html`](index.html) to run the game.

## Disclaimer

This was a small project to learn functional programming. It's **NOT** good code. It's quite brittle, in fact, and underuses Elm's type system quite a bit. If you're new to functional programming, like me, and you're looking for inspiration or guidance, you should look elsewhere.

## License

The code is in the public domain. Check the [license file](LICENSE) for details.
