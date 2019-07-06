# My Personal Web Page

This repository contains sources and content of my personal website which you can [find on the internet](https://turbomack.github.io/).
Feel free to use both content and source code as you wish as log as you respect licenses.

## Installation

This project is using [nix](https://nixos.org/nix/) as a build tool.

For interactive shell environment run:

```
$ nix-shell --pure
```

from within this directory. This should start bash with `site` binary available to use.

Following commands are available

```
$ site build  # compiles HTML
$ site watch  # runs developement server with watcher
$ site clean  # cleans the cache
$ site help   # show full list of commands
```

## Publishing

There is interactive script that automates publishing to new version.

from within nix-shell:

```
$ publish
```

then you have to checkout `master` branch and commit changes and push to origin.

## Licenses

- [Source code](LICENSE) - BSD-3-Clause
- [Content](posts/LICENSE) - Creative Commons Attribution-ShareAlike 4.0 International License
