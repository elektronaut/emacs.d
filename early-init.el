;;; early-init.el --- early bird  -*- lexical-binding: t; no-byte-compile: t -*-

;; Workaround for mangled PATH in emacs-plus on MacOS Sequoia.
;; See https://github.com/d12frosted/homebrew-emacs-plus/issues/733
(setenv "PATH" "/Users/inge/.local/share/mise/installs/ruby/3.4.3/bin:/Users/inge/.local/share/mise/installs/node/22.14.0/bin:/opt/homebrew/bin:/opt/homebrew/sbin:/Users/inge/.bin:/
Users/inge/.iterm2:/Users/inge/.asdf/bin:/Users/inge/.asdf/shims:/Users/inge/Library/Qt5.5.1/5.5/clang_64/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/var/run/com.apple.security.crypte
xd/codex.system/bootstrap/usr/local/bin:/var/run/com.apple.security.cryptexd/codex.system/bootstrap/usr/bin:/var/run/com.apple.security.cryptexd/codex.system/bootstrap/usr/appleinternal/bin
:/opt/X11/bin:/Users/inge/Library/pnpm:/Applications/Ghostty.app/Contents/MacOS")
(setq exec-path (split-string (getenv "PATH") path-separator))

;; Defer GC for now
(setopt gc-cons-threshold most-positive-fixnum)

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(when (eq system-type 'darwin)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark)))

;; Set location of compilation cache
(when (and (fboundp 'startup-redirect-eln-cache)
           (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (startup-redirect-eln-cache
   (convert-standard-filename
    (expand-file-name  "var/eln-cache/" user-emacs-directory))))

;; Disable packages
(setq package-enable-at-startup nil)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setopt frame-inhibit-implied-resize t)
