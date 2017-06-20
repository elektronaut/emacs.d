# My Emacs config

This is my ever-evolving Emacs configuration, updated as I explore and
learn. Web development is my bread and butter, but I tinker with other
things as well.

Some settings are based on [Emacs Prelude][prelude]. The look and feel
is heavily inspired by [Doom][doom].

## Setup

All you need to do is:

``` shell
git clone https://github.com/elektronaut/emacs.d.git ~/.emacs.d
```

Emacs will handle the rest on startup.

## Installing Emacs

I use [Mitsuharu Yamamoto's Mac port][emacs-mac], which offers a
slightly nicer integration with OS X. To install with Homebrew:

``` shell
brew tap railwaycat/emacsmacport
brew install emacs-mac --with-modern-icon --with-natural-title-bar
```

## Organization

* [core](core): Anything pertaining to the core Emacs experience
* [modules](modules): Language/major mode specific configuration

[use-package]: https://github.com/jwiegley/use-package
[prelude]: https://github.com/bbatsov/prelude
[doom]: https://github.com/hlissner/.emacs.d
[emacs-mac]: https://bitbucket.org/mituharu/emacs-mac/overview
