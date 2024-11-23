# Nyx - My Emacs config

This is a clean, maintainable setup that has evolved through daily
use. While it's primarily for my own needs, feel free to explore and
borrow ideas for your own configuration.

## Features

- Uses `use-package` for declarative package configuration
- Custom modeline, look and feel inspired by [Doom Emacs][doom]
- Workspace organization with [persp-mode][persp-mode] and [project.el][project.el]
- Version control with [Magit][magit]
- LSP integration with [lsp-mode][lsp-mode]
- Modern completion UI with [Vertico][vertico], [Orderless][orderless], 
  [Consult][consult] and [Marginalia][marginalia]
- [Org-mode][org-mode] and [Org-roam][org-roam] for task management and notes
- Email client using [mu4e][mu4e]

## Installation

First, back up (or get rid of) your existing Emacs configuration:

``` shell
mv ~/.emacs.d ~/.emacs.d.backup
```

Clone the repo:

``` shell
git clone https://github.com/elektronaut/emacs.d.git ~/.emacs.d
```

That's it. Emacs should handle the rest on startup.

I use [Emacs Plus][emacs-plus] on Mac OS using Homebrew.

[emacs-plus]: https://github.com/d12frosted/homebrew-emacs-plus
[persp-mode]: https://github.com/Bad-ptr/persp-mode.el
[project.el]: https://www.gnu.org/software/emacs/manual/html_node/emacs/Projects.html
[magit]: https://magit.vc/
[lsp-mode]: https://github.com/emacs-lsp/lsp-mode
[vertico]: https://github.com/minad/vertico
[consult]: https://github.com/minad/consult
[marginalia]: https://github.com/minad/marginalia
[orderless]: https://github.com/oantolin/orderless
[org-mode]: https://orgmode.org/
[org-roam]: https://www.orgroam.com/
[mu4e]: https://github.com/djcb/mu
[doom]: https://github.com/doomemacs/doomemacs

