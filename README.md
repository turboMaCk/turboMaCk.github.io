# My Personal Web Page

This repository contains sources and content of my personal website which you can [find on the internet](https://turbomack.github.io/).
Feel free to use both content and source code as you wish as log as you respect licenses.

## Installation

First you need to clone [hakyll-sass](https://github.com/meoblast001/hakyll-sass) cloned locally.
Then within the same folder you can clone this repository and install rest of the deps.

```
$ stack build
$ stack exec stte build
```

For live dev server use

```
$ stack exec site watch
```

## Publishing

There is interactive script that automates publishing to new version.

```
$ stack exec publish
```

then you have to checkout `master` branch and commit changes and push to origin.

## Licenses

- [Source code](LICENSE) - BSD-3-Clause
- [Content](posts/LICENSE) - Creative Commons Attribution-ShareAlike 4.0 International License
