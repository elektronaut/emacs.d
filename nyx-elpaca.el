;;; nyx-elpaca.el --- Elpaca -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defvar elpaca-installer-version 0.12)
(defvar elpaca-lock-file (expand-file-name "elpaca-lock.eld" user-emacs-directory))
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-sources-directory (expand-file-name "sources/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca-activate)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-sources-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Uncomment for systems which cannot create symlinks:
;; (elpaca-no-symlink-mode)

;; Install a package via the elpaca macro
;; See the "recipes" section of the manual for more details.

;; (elpaca example-package)

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode))

;; Emacs.app bundles older `compat' and `transient' which elpaca treats as
;; satisfying the dependency (they're listed in `elpaca-ignored-dependencies'),
;; so packages that need newer versions get compiled and loaded against the
;; stale built-ins.  Two failure modes:
;;   - compat: packages using `static-when', `static-if' or `incf' (transient,
;;     magit, consult) fail with `void-function'/`invalid-function'.
;;   - transient: because it's ignored, elpaca doesn't walk *its* dependencies
;;     when ordering builds, so dependents like `rg' get byte-compiled before
;;     transient 0.9.2's own dep `cond-let' is on the load path.  That build
;;     errors out and the package is silently left unbuilt -- e.g. `rg-project'
;;     then fails at runtime with "Cannot open load file: ... rg".
;; Drop both from the ignore list so elpaca installs and manages current
;; versions (and their transitive deps) as normal, correctly-ordered packages.
(dolist (pkg '(compat transient))
  (setq elpaca-ignored-dependencies (delq pkg elpaca-ignored-dependencies)))

;;When installing a package used in the init file itself,
;;e.g. a package which adds a use-package key word,
;;use the :wait recipe keyword to block until that package is installed/configured.
;;For example:
;;(use-package general :ensure (:wait t) :demand t)

;; Expands to: (elpaca evil (use-package evil :demand t))
;; (use-package evil :ensure t :demand t)

;;Turns off elpaca-use-package-mode current declaration
;;Note this will cause evaluate the declaration immediately. It is not deferred.
;;Useful for configuring built-in emacs features.
;;(use-package emacs :ensure nil :config (setq ring-bell-function #'ignore))

;;; Updating packages past the lock file
;;
;; A present `elpaca-lock-file' pins every package to a commit through the
;; `elpaca-menu-lock-file' menu (it's first in `elpaca-menu-functions'), which
;; leaves each repo in detached HEAD.  Elpaca has no built-in "update through
;; the lock" flow, so `nyx-elpaca-unlock' disables pinning for the running
;; session: it clears the lock menu, re-resolves every queued recipe so the
;; `:ref' pin drops, and re-attaches each repo to its default branch *at the
;; current commit* (the working tree does not move).  Afterwards review and
;; merge updates with `elpaca-manager' / `elpaca-log', then capture new pins
;; with `elpaca-write-lock-file'.  Restart Emacs to restore pinning.

(defun nyx-elpaca--reattach-branch (e)
  "Re-attach E's repo to its tracking branch if it is in detached HEAD.
Uses the recipe's `:branch' when set (e.g. GNU ELPA `externals/<pkg>'),
otherwise the remote's default branch.  The branch is (re)pointed at the
current commit so the working tree does not move, and its upstream is set
only when the matching remote-tracking branch exists.  Return the branch
name, or nil if nothing was done."
  (let ((default-directory (elpaca<-source-dir e)))
    (when (and (stringp default-directory)
               (file-directory-p (expand-file-name ".git" default-directory))
               (not (zerop (call-process "git" nil nil nil "symbolic-ref" "-q" "HEAD"))))
      (let* ((recipe (elpaca<-recipe e))
             (remote (car-safe (car-safe (plist-get recipe :remotes))))
             (remote (if (stringp remote) remote "origin"))
             (branch (or (plist-get recipe :branch)
                         (ignore-errors (elpaca-git--remote-default-branch remote))
                         "main")))
        (when (zerop (call-process "git" nil nil nil "checkout" "-B" branch))
          ;; Only track an upstream when the remote-tracking branch exists;
          ;; otherwise leave the branch untracked rather than point it at a
          ;; non-existent ref (which breaks `elpaca' fetch/merge).
          (when (zerop (call-process "git" nil nil nil "show-ref" "--verify" "--quiet"
                                     (format "refs/remotes/%s/%s" remote branch)))
            (call-process "git" nil nil nil "branch"
                          (format "--set-upstream-to=%s/%s" remote branch) branch))
          branch)))))

(defun nyx-elpaca-unlock ()
  "Disable lock-file pinning for this session so packages can be updated.
Clears `elpaca-lock-file' and its menu cache, re-resolves every queued
package so it is no longer pinned to a commit, and re-attaches each repo
to its default branch.  Review and merge updates with `elpaca-manager'
or the elpaca UI, then run `elpaca-write-lock-file' to capture new pins.
Restart Emacs to restore pinning from the lock file."
  (interactive)
  (require 'elpaca-git nil t)
  (setq elpaca-lock-file nil
        elpaca-menu-lock-file--cache nil)
  (let ((resolved 0) (reattached 0) (failed 0))
    (cl-loop for (id . e) in (elpaca--queued) do
             (condition-case err
                 (progn
                   (setf (elpaca<-recipe e) (elpaca--normalize-recipe (elpaca<-order e)))
                   (cl-incf resolved)
                   (when (nyx-elpaca--reattach-branch e) (cl-incf reattached)))
               (error (cl-incf failed)
                      (message "nyx-elpaca-unlock: %s: %s" id
                               (error-message-string err)))))
    (message (concat "nyx-elpaca-unlock: unpinned %d package(s), re-attached %d repo(s)%s. "
                     "Use M-x elpaca-manager to fetch/merge, then M-x elpaca-write-lock-file.")
             resolved reattached
             (if (> failed 0) (format ", %d failed" failed) ""))))

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:

(provide 'nyx-elpaca)
;;; nyx-elpaca.el ends here
