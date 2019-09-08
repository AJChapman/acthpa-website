# ACTHPA Website Generator

This is a static site generator for the [ACTHPA website](http://www.acthpa.org).
It is still a work in progress, and has not yet replaced the old site.

## Contributing

If you notice a mistake on the website, please [open an issue](https://github.com/AJChapman/acthpa-website/issues/new).

If you are a member of the ACTHPA, and would like to contribute to the website, then please feel free to open up a pull request.
If you don't know what a pull request is, or can't figure out how the site works, please contact [Alex Chapman](mailto:alex@farfromthere.net).

## How it Works

The pages of the site are generated from their [MarkDown](https://pandoc.org/MANUAL.html#pandocs-markdown) source files (the files ending in .md).
These are combined with the html templates in the `templates` folder, using the [Hakyll library](https://jaspervdj.be/hakyll/) via the Haskell source code in the `src` directory.
Finally, the site resources in the `css`, `js`, and `images` folders are used to bring the site together.

## Building

To build, you will need either [Haskell stack](https://github.com/AJChapman/acthpa-website/issues/new), or [the Nix package manager](https://nixos.org/nix/).

To build with nix, [follow these instructions](README-nix.md).

To build with stack, run `stack build`.

## Testing

Once you have built the executable, you can run it to test the generated site with:

    cabal new-exec -- site watch

Or if you used stack to build:

    stack exec -- site watch

This will start a web server on port 8000 of your machine, so you can view the test site in your browser at http://localhost:8000.
The site will automatically update as you make changes to the site content (templates and markdown files), but you will need to rebuild to see the effects of changing any of the Haskell code.

## Generating the Site

To generate the site (in the _site subfolder), run:

    cabal new-exec -- site build

Or with stack:

    stack exec -- site build

## Deploying the Site

To deploy the site, assuming you have ssh access to the web server, run:

    cabal new-exec -- site deploy

Or with stack:

    stack exec -- site deploy
