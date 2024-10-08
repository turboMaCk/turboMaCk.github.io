# My Personal Web Page

This repository contains sources and content of my personal website which you can [find on the internet](https://turbomack.github.io/).
Feel free to use both content and source code as you wish as log as you respect licenses.

## Usage

This project is using [nix](https://nixos.org/nix/) as a build tool.
Make sure you have [flake support](https://nixos.wiki/wiki/flakes) enabled in your installation.

For interactive shell environment run:

```
$ nix develop
```

from within this directory. This should start bash with `site` binary available to use.

Following commands are available

```
$ site build  # compiles HTML
$ site watch  # runs developement server with watcher
$ site clean  # cleans the cache
$ site help   # show full list of commands
```

You can also build site binary:

```
nix build
```

and run it directly

```
./result/bin/site help
```


## Publishing

There is interactive script that automates publishing to new version.
Because I'm deploying this as a GitHub page which is main personal page
published HTML must be in master branch. This is why my deployment flow involves some git hacks.


```
$ nix develop
$ site build
$ git branch -D master
$ git checkout -b master
$ publish
$ git add -A
$ git commit -m "publish"
$ git push origin master
```

## Developement of Haskell bin

Nix shell can be used to work on the Haskell source code for generator as well.
In this case `default.nix` should be used rather than `shell.nix` though.
**This also requires cabal to be globally available on the machine (no `--pure` is passed to shell)

```
$ nix-shell default.nix -A env
$ cd src
```

## Licenses

- [Source code](LICENSE) - BSD-3-Clause
- [Content](posts/LICENSE) - Creative Commons Attribution-ShareAlike 4.0 International License
