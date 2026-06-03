((ace-window :source "elpaca-menu-lock-file" :recipe
             (:package "ace-window" :repo "abo-abo/ace-window"
                       :fetcher github :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                        "*.texinfo" "doc/dir" "doc/*.info"
                        "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                        "docs/dir" "docs/*.info" "docs/*.texi"
                        "docs/*.texinfo"
                        (:exclude ".dir-locals.el" "test.el"
                                  "tests.el" "*-test.el" "*-tests.el"
                                  "LICENSE" "README*" "*-pkg.el"))
                       :source "elpaca-menu-lock-file" :protocol https
                       :inherit t :depth treeless :ref
                       "77115afc1b0b9f633084cf7479c767988106c196" :id
                       ace-window :type git))
 (aio :source "elpaca-menu-lock-file" :recipe
      (:package "aio" :fetcher github :repo "skeeto/emacs-aio" :files
                ("aio.el" "README.md" "UNLICENSE") :source
                "elpaca-menu-lock-file" :protocol https :inherit t
                :depth treeless :ref
                "58157e51e7eb7a4b954894ee4182564c507a2f01" :id aio
                :type git))
 (alchemist :source "elpaca-menu-lock-file" :recipe
            (:package "alchemist" :fetcher github :repo
                      "tonini/alchemist.el" :files
                      (:defaults "*.exs" "alchemist-server") :source
                      "elpaca-menu-lock-file" :protocol https :inherit
                      t :depth treeless :ref
                      "6f99367511ae209f8fe2c990779764bbb4ccb6ed" :id
                      alchemist :type git))
 (anaconda-mode :source "elpaca-menu-lock-file" :recipe
                (:package "anaconda-mode" :fetcher github :repo
                          "pythonic-emacs/anaconda-mode" :files
                          (:defaults "anaconda-mode.py") :source
                          "elpaca-menu-lock-file" :protocol https
                          :inherit t :depth treeless :ref
                          "d2adcbdd0bdd2b4d644717aebcf63f21dad47ae3"
                          :id anaconda-mode :type git))
 (apache-mode :source "elpaca-menu-lock-file" :recipe
              (:package "apache-mode" :fetcher github :repo
                        "emacs-php/apache-mode" :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                         "*.texinfo" "doc/dir" "doc/*.info"
                         "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                         "docs/dir" "docs/*.info" "docs/*.texi"
                         "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el"
                                   "tests.el" "*-test.el" "*-tests.el"
                                   "LICENSE" "README*" "*-pkg.el"))
                        :source "elpaca-menu-lock-file" :protocol
                        https :inherit t :depth treeless :ref
                        "a3db91fc6c4d46ca0625e79df5d6689a607b1927" :id
                        apache-mode :type git))
 (apheleia :source "elpaca-menu-lock-file" :recipe
           (:package "apheleia" :fetcher github :repo
                     "radian-software/apheleia" :files
                     (:defaults ("scripts" "scripts/formatters"))
                     :source "elpaca-menu-lock-file" :protocol https
                     :inherit t :depth treeless :ref
                     "143c1dffed15f1cab3eb06e148fe11224e39471c" :id
                     apheleia :type git))
 (async :source "elpaca-menu-lock-file" :recipe
        (:package "async" :repo "jwiegley/emacs-async" :fetcher github
                  :files
                  ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                   "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                   "doc/*.texinfo" "lisp/*.el" "docs/dir"
                   "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                   (:exclude ".dir-locals.el" "test.el" "tests.el"
                             "*-test.el" "*-tests.el" "LICENSE"
                             "README*" "*-pkg.el"))
                  :source "elpaca-menu-lock-file" :protocol https
                  :inherit t :depth treeless :ref
                  "31cb2fea8f4bc7a593acd76187a89075d8075500" :id async
                  :type git))
 (auctex :source "elpaca-menu-lock-file" :recipe
         (:package "auctex" :repo
                   ("https://git.savannah.gnu.org/git/auctex.git"
                    . "auctex")
                   :branch "main" :files ("*" (:exclude ".git"))
                   :source "elpaca-menu-lock-file" :protocol https
                   :inherit t :depth treeless :ref
                   "79e0b72b347f801526e98872c4b940d866d78e1d" :id
                   auctex :type git))
 (auto-compile :source "elpaca-menu-lock-file" :recipe
               (:package "auto-compile" :repo
                         "emacscollective/auto-compile" :fetcher
                         github :files
                         ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                          "*.texinfo" "doc/dir" "doc/*.info"
                          "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                          "docs/dir" "docs/*.info" "docs/*.texi"
                          "docs/*.texinfo"
                          (:exclude ".dir-locals.el" "test.el"
                                    "tests.el" "*-test.el"
                                    "*-tests.el" "LICENSE" "README*"
                                    "*-pkg.el"))
                         :source "elpaca-menu-lock-file" :protocol
                         https :inherit t :depth treeless :wait t :ref
                         "eaed414fa7dae04c7701dbc591de52fb3e81aa91"
                         :id auto-compile :type git))
 (avy :source "elpaca-menu-lock-file" :recipe
      (:package "avy" :repo "abo-abo/avy" :fetcher github :files
                ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                 "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                 "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                 "docs/*.texinfo"
                 (:exclude ".dir-locals.el" "test.el" "tests.el"
                           "*-test.el" "*-tests.el" "LICENSE"
                           "README*" "*-pkg.el"))
                :source "elpaca-menu-lock-file" :protocol https
                :inherit t :depth treeless :ref
                "933d1f36cca0f71e4acb5fac707e9ae26c536264" :id avy
                :type git))
 (browse-at-remote :source "elpaca-menu-lock-file" :recipe
                   (:package "browse-at-remote" :repo
                             "rmuslimov/browse-at-remote" :fetcher
                             github :files
                             ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                              "*.texinfo" "doc/dir" "doc/*.info"
                              "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                              "docs/dir" "docs/*.info" "docs/*.texi"
                              "docs/*.texinfo"
                              (:exclude ".dir-locals.el" "test.el"
                                        "tests.el" "*-test.el"
                                        "*-tests.el" "LICENSE"
                                        "README*" "*-pkg.el"))
                             :source "elpaca-menu-lock-file" :protocol
                             https :inherit t :depth treeless :ref
                             "38e5ffd77493c17c821fd88f938dbf42705a5158"
                             :id browse-at-remote :type git))
 (browse-kill-ring :source "elpaca-menu-lock-file" :recipe
                   (:package "browse-kill-ring" :repo
                             "browse-kill-ring/browse-kill-ring"
                             :fetcher github :files
                             ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                              "*.texinfo" "doc/dir" "doc/*.info"
                              "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                              "docs/dir" "docs/*.info" "docs/*.texi"
                              "docs/*.texinfo"
                              (:exclude ".dir-locals.el" "test.el"
                                        "tests.el" "*-test.el"
                                        "*-tests.el" "LICENSE"
                                        "README*" "*-pkg.el"))
                             :source "elpaca-menu-lock-file" :protocol
                             https :inherit t :depth treeless :ref
                             "26ea5759c996e782abadc61b85c5f9324d1d1dae"
                             :id browse-kill-ring :type git))
 (bufler :source "elpaca-menu-lock-file" :recipe
         (:package "bufler" :fetcher github :repo
                   "alphapapa/bufler.el" :files
                   (:defaults (:exclude "helm-bufler.el")) :source
                   "elpaca-menu-lock-file" :protocol https :inherit t
                   :depth treeless :ref
                   "b96822d2132fda6bd1dd86f017d7e76e3b990c82" :id
                   bufler :type git))
 (burly :source "elpaca-menu-lock-file" :recipe
        (:package "burly" :fetcher github :repo "alphapapa/burly.el"
                  :files
                  ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                   "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                   "doc/*.texinfo" "lisp/*.el" "docs/dir"
                   "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                   (:exclude ".dir-locals.el" "test.el" "tests.el"
                             "*-test.el" "*-tests.el" "LICENSE"
                             "README*" "*-pkg.el"))
                  :source "elpaca-menu-lock-file" :protocol https
                  :inherit t :depth treeless :ref
                  "d5b7133b5b629dd6bca29bb16660a9e472e82e25" :id burly
                  :type git))
 (caml :source "elpaca-menu-lock-file" :recipe
       (:package "caml" :repo "ocaml/caml-mode" :fetcher github :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                  "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                  "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                  "docs/*.texinfo"
                  (:exclude ".dir-locals.el" "test.el" "tests.el"
                            "*-test.el" "*-tests.el" "LICENSE"
                            "README*" "*-pkg.el"))
                 :source "elpaca-menu-lock-file" :protocol https
                 :inherit t :depth treeless :ref
                 "744333dc4c4bd8b93e037efa8f7362b0903b96a2" :id caml
                 :type git))
 (cask-mode :source "elpaca-menu-lock-file" :recipe
            (:package "cask-mode" :repo "Wilfred/cask-mode" :fetcher
                      github :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                       "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                       "doc/*.texinfo" "lisp/*.el" "docs/dir"
                       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE"
                                 "README*" "*-pkg.el"))
                      :source "elpaca-menu-lock-file" :protocol https
                      :inherit t :depth treeless :ref
                      "be8b69e55916cf2e78886927f58c7c49b969c0b8" :id
                      cask-mode :type git))
 (cider :source "elpaca-menu-lock-file" :recipe
        (:package "cider" :fetcher github :repo "clojure-emacs/cider"
                  :files
                  ("lisp/*.el" "bin/*.sh" "*.el" "clojure.sh"
                   "lein.sh" (:exclude ".dir-locals.el"))
                  :old-names (nrepl) :source "elpaca-menu-lock-file"
                  :protocol https :inherit t :depth treeless :ref
                  "70cf7bd4e15ae5b6de09d0cb8b96a2beaf5cf92b" :id cider
                  :type git))
 (clojure-mode :source "elpaca-menu-lock-file" :recipe
               (:package "clojure-mode" :repo
                         "clojure-emacs/clojure-mode" :fetcher github
                         :files ("clojure-mode.el") :source
                         "elpaca-menu-lock-file" :protocol https
                         :inherit t :depth treeless :ref
                         "467922210962061777e00d6cc46f5db2d00fb3ef"
                         :id clojure-mode :type git))
 (coffee-mode :source "elpaca-menu-lock-file" :recipe
              (:package "coffee-mode" :repo "defunkt/coffee-mode"
                        :fetcher github :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                         "*.texinfo" "doc/dir" "doc/*.info"
                         "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                         "docs/dir" "docs/*.info" "docs/*.texi"
                         "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el"
                                   "tests.el" "*-test.el" "*-tests.el"
                                   "LICENSE" "README*" "*-pkg.el"))
                        :source "elpaca-menu-lock-file" :protocol
                        https :inherit t :depth treeless :ref
                        "1c6adeae14f3795d3b1e44321189ed7c2c93c967" :id
                        coffee-mode :type git))
 (company :source "elpaca-menu-lock-file" :recipe
          (:package "company" :fetcher github :repo
                    "company-mode/company-mode" :files
                    (:defaults "icons"
                               ("images/small"
                                "doc/images/small/*.png"))
                    :source "elpaca-menu-lock-file" :protocol https
                    :inherit t :depth treeless :ref
                    "fad9f207e00a851c0d96dd532c1b175326ac3e3d" :id
                    company :type git))
 (company-anaconda :source "elpaca-menu-lock-file" :recipe
                   (:package "company-anaconda" :fetcher github :repo
                             "pythonic-emacs/company-anaconda" :files
                             ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                              "*.texinfo" "doc/dir" "doc/*.info"
                              "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                              "docs/dir" "docs/*.info" "docs/*.texi"
                              "docs/*.texinfo"
                              (:exclude ".dir-locals.el" "test.el"
                                        "tests.el" "*-test.el"
                                        "*-tests.el" "LICENSE"
                                        "README*" "*-pkg.el"))
                             :source "elpaca-menu-lock-file" :protocol
                             https :inherit t :depth treeless :ref
                             "83aac410420ff5dfd62d0ccede187ae27b2e12cc"
                             :id company-anaconda :type git))
 (company-emoji :source "elpaca-menu-lock-file" :recipe
                (:package "company-emoji" :repo "egirl/company-emoji"
                          :fetcher codeberg :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                           "*.texinfo" "doc/dir" "doc/*.info"
                           "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                           "docs/dir" "docs/*.info" "docs/*.texi"
                           "docs/*.texinfo"
                           (:exclude ".dir-locals.el" "test.el"
                                     "tests.el" "*-test.el"
                                     "*-tests.el" "LICENSE" "README*"
                                     "*-pkg.el"))
                          :source "elpaca-menu-lock-file" :protocol
                          https :inherit t :depth treeless :ref
                          "dcc0e42757b872c16426122a7ddbcf66f9aec62d"
                          :id company-emoji :type git))
 (compat :source "elpaca-menu-lock-file" :recipe
         (:package "compat" :repo
                   ("https://github.com/emacs-compat/compat"
                    . "compat")
                   :tar "31.0.0.1" :host gnu :files
                   ("*" (:exclude ".git")) :source "GNU ELPA" :id
                   compat :type git :protocol https :inherit t :depth
                   treeless :ref
                   "ad1806f7f677c4e295ed1e603ae6a6a5d9f9d399"))
 (cond-let
   :source "elpaca-menu-lock-file" :recipe
   (:package "cond-let" :fetcher github :repo "tarsius/cond-let"
             :files
             ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
              "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
              "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
              "docs/*.texinfo"
              (:exclude ".dir-locals.el" "test.el" "tests.el"
                        "*-test.el" "*-tests.el" "LICENSE" "README*"
                        "*-pkg.el"))
             :source "elpaca-menu-lock-file" :protocol https :inherit
             t :depth treeless :ref
             "8bf87d45e169ebc091103b2aae325aece3aa804d" :id cond-let
             :type git))
 (consult :source "elpaca-menu-lock-file" :recipe
          (:package "consult" :repo "minad/consult" :fetcher github
                    :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                     "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                     "doc/*.texinfo" "lisp/*.el" "docs/dir"
                     "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el"
                               "*-test.el" "*-tests.el" "LICENSE"
                               "README*" "*-pkg.el"))
                    :source "elpaca-menu-lock-file" :protocol https
                    :inherit t :depth treeless :ref
                    "d1d39d52151a10f7ca29aa291886e99534cc94db" :id
                    consult :type git))
 (consult-company :source "elpaca-menu-lock-file" :recipe
                  (:package "consult-company" :fetcher github :repo
                            "mohkale/consult-company" :files
                            ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                             "*.texinfo" "doc/dir" "doc/*.info"
                             "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                             "docs/dir" "docs/*.info" "docs/*.texi"
                             "docs/*.texinfo"
                             (:exclude ".dir-locals.el" "test.el"
                                       "tests.el" "*-test.el"
                                       "*-tests.el" "LICENSE"
                                       "README*" "*-pkg.el"))
                            :source "elpaca-menu-lock-file" :protocol
                            https :inherit t :depth treeless :ref
                            "6e309fa9115c9ecd29aa27bff4e3b733979e5dbc"
                            :id consult-company :type git))
 (consult-dash :source "elpaca-menu-lock-file" :recipe
               (:package "consult-dash" :fetcher codeberg :repo
                         "ravi/consult-dash" :files
                         ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                          "*.texinfo" "doc/dir" "doc/*.info"
                          "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                          "docs/dir" "docs/*.info" "docs/*.texi"
                          "docs/*.texinfo"
                          (:exclude ".dir-locals.el" "test.el"
                                    "tests.el" "*-test.el"
                                    "*-tests.el" "LICENSE" "README*"
                                    "*-pkg.el"))
                         :source "elpaca-menu-lock-file" :protocol
                         https :inherit t :depth treeless :ref
                         "edb57bf8cdbef422b88667fadc83e1bb046957a6"
                         :id consult-dash :type git))
 (consult-dir :source "elpaca-menu-lock-file" :recipe
              (:package "consult-dir" :fetcher github :repo
                        "karthink/consult-dir" :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                         "*.texinfo" "doc/dir" "doc/*.info"
                         "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                         "docs/dir" "docs/*.info" "docs/*.texi"
                         "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el"
                                   "tests.el" "*-test.el" "*-tests.el"
                                   "LICENSE" "README*" "*-pkg.el"))
                        :source "elpaca-menu-lock-file" :protocol
                        https :inherit t :depth treeless :ref
                        "1497b46d6f48da2d884296a1297e5ace1e050eb5" :id
                        consult-dir :type git))
 (consult-flycheck :source "elpaca-menu-lock-file" :recipe
                   (:package "consult-flycheck" :fetcher github :repo
                             "minad/consult-flycheck" :files
                             ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                              "*.texinfo" "doc/dir" "doc/*.info"
                              "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                              "docs/dir" "docs/*.info" "docs/*.texi"
                              "docs/*.texinfo"
                              (:exclude ".dir-locals.el" "test.el"
                                        "tests.el" "*-test.el"
                                        "*-tests.el" "LICENSE"
                                        "README*" "*-pkg.el"))
                             :source "elpaca-menu-lock-file" :protocol
                             https :inherit t :depth treeless :ref
                             "9fe96c4b75c8566170ad41a04c3849d2e2606104"
                             :id consult-flycheck :type git))
 (consult-ls-git :source "elpaca-menu-lock-file" :recipe
                 (:package "consult-ls-git" :repo "rcj/consult-ls-git"
                           :fetcher github :files
                           ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                            "*.texinfo" "doc/dir" "doc/*.info"
                            "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                            "docs/dir" "docs/*.info" "docs/*.texi"
                            "docs/*.texinfo"
                            (:exclude ".dir-locals.el" "test.el"
                                      "tests.el" "*-test.el"
                                      "*-tests.el" "LICENSE" "README*"
                                      "*-pkg.el"))
                           :source "elpaca-menu-lock-file" :protocol
                           https :inherit t :depth treeless :ref
                           "85882e4b7af9ad40160d985e42b36b0fd6400ead"
                           :id consult-ls-git :type git))
 (consult-lsp :source "elpaca-menu-lock-file" :recipe
              (:package "consult-lsp" :fetcher github :repo
                        "gagbo/consult-lsp" :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                         "*.texinfo" "doc/dir" "doc/*.info"
                         "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                         "docs/dir" "docs/*.info" "docs/*.texi"
                         "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el"
                                   "tests.el" "*-test.el" "*-tests.el"
                                   "LICENSE" "README*" "*-pkg.el"))
                        :source "elpaca-menu-lock-file" :protocol
                        https :inherit t :depth treeless :ref
                        "d11102c9db33c4ca7817296a2edafc3e26a61117" :id
                        consult-lsp :type git))
 (consult-org-roam :source "elpaca-menu-lock-file" :recipe
                   (:package "consult-org-roam" :repo
                             "jgru/consult-org-roam" :fetcher github
                             :files
                             ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                              "*.texinfo" "doc/dir" "doc/*.info"
                              "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                              "docs/dir" "docs/*.info" "docs/*.texi"
                              "docs/*.texinfo"
                              (:exclude ".dir-locals.el" "test.el"
                                        "tests.el" "*-test.el"
                                        "*-tests.el" "LICENSE"
                                        "README*" "*-pkg.el"))
                             :source "elpaca-menu-lock-file" :protocol
                             https :inherit t :depth treeless :ref
                             "7d825fffd6f990b800485376f62dd9b277991709"
                             :id consult-org-roam :type git))
 (consult-yasnippet :source "elpaca-menu-lock-file" :recipe
                    (:package "consult-yasnippet" :fetcher github
                              :repo "mohkale/consult-yasnippet" :files
                              ("*.el" "*.el.in" "dir" "*.info"
                               "*.texi" "*.texinfo" "doc/dir"
                               "doc/*.info" "doc/*.texi"
                               "doc/*.texinfo" "lisp/*.el" "docs/dir"
                               "docs/*.info" "docs/*.texi"
                               "docs/*.texinfo"
                               (:exclude ".dir-locals.el" "test.el"
                                         "tests.el" "*-test.el"
                                         "*-tests.el" "LICENSE"
                                         "README*" "*-pkg.el"))
                              :source "elpaca-menu-lock-file"
                              :protocol https :inherit t :depth
                              treeless :ref
                              "a3482dfbdcbe487ba5ff934a1bb6047066ff2194"
                              :id consult-yasnippet :type git))
 (crux :source "elpaca-menu-lock-file" :recipe
       (:package "crux" :fetcher github :repo "bbatsov/crux" :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                  "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                  "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                  "docs/*.texinfo"
                  (:exclude ".dir-locals.el" "test.el" "tests.el"
                            "*-test.el" "*-tests.el" "LICENSE"
                            "README*" "*-pkg.el"))
                 :source "elpaca-menu-lock-file" :protocol https
                 :inherit t :depth treeless :ref
                 "3b72275fce66162770b53cf72eb72515c3e68492" :id crux
                 :type git))
 (csv-mode :source "elpaca-menu-lock-file" :recipe
           (:package "csv-mode" :repo
                     ("https://github.com/emacsmirror/gnu_elpa"
                      . "csv-mode")
                     :branch "externals/csv-mode" :files
                     ("*" (:exclude ".git")) :source
                     "elpaca-menu-lock-file" :protocol https :inherit
                     t :depth treeless :ref
                     "ba5dc934b9dbdc2b57ab1917a669cdfd7d1838d3" :id
                     csv-mode :type git))
 (cython-mode :source "elpaca-menu-lock-file" :recipe
              (:package "cython-mode" :fetcher github :repo
                        "cython/emacs-cython-mode" :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                         "*.texinfo" "doc/dir" "doc/*.info"
                         "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                         "docs/dir" "docs/*.info" "docs/*.texi"
                         "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el"
                                   "tests.el" "*-test.el" "*-tests.el"
                                   "LICENSE" "README*" "*-pkg.el"))
                        :source "elpaca-menu-lock-file" :protocol
                        https :inherit t :depth treeless :ref
                        "3e4790559d3168fe992cf2aa62f01423038cedb5" :id
                        cython-mode :type git))
 (d-mode :source "elpaca-menu-lock-file" :recipe
         (:package "d-mode" :fetcher github :repo
                   "Emacs-D-Mode-Maintainers/Emacs-D-Mode" :files
                   ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                    "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                    "doc/*.texinfo" "lisp/*.el" "docs/dir"
                    "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                    (:exclude ".dir-locals.el" "test.el" "tests.el"
                              "*-test.el" "*-tests.el" "LICENSE"
                              "README*" "*-pkg.el"))
                   :source "elpaca-menu-lock-file" :protocol https
                   :inherit t :depth treeless :ref
                   "a4f9026d9e7fadfcfb138189c18c639b57467ce5" :id
                   d-mode :type git))
 (dart-mode :source "elpaca-menu-lock-file" :recipe
            (:package "dart-mode" :fetcher github :repo
                      "emacsorphanage/dart-mode" :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                       "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                       "doc/*.texinfo" "lisp/*.el" "docs/dir"
                       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE"
                                 "README*" "*-pkg.el"))
                      :source "elpaca-menu-lock-file" :protocol https
                      :inherit t :depth treeless :ref
                      "773e9ebc74a258af2db395b01febfb652a42f3ab" :id
                      dart-mode :type git))
 (dash :source "elpaca-menu-lock-file" :recipe
       (:package "dash" :fetcher github :repo "magnars/dash.el" :files
                 ("dash.el" "dash.texi") :source
                 "elpaca-menu-lock-file" :protocol https :inherit t
                 :depth treeless :wait t :ref
                 "fb443e7a6e660ba849cafcd01021d9aac3ac6764" :id dash
                 :type git))
 (dash-docs :source "elpaca-menu-lock-file" :recipe
            (:package "dash-docs" :repo "dash-docs-el/dash-docs"
                      :fetcher github :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                       "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                       "doc/*.texinfo" "lisp/*.el" "docs/dir"
                       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE"
                                 "README*" "*-pkg.el"))
                      :source "elpaca-menu-lock-file" :protocol https
                      :inherit t :depth treeless :ref
                      "29848b6b347ac520f7646c200ed2ec36cea3feda" :id
                      dash-docs :type git))
 (diff-hl :source "elpaca-menu-lock-file" :recipe
          (:package "diff-hl" :fetcher github :repo "dgutov/diff-hl"
                    :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                     "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                     "doc/*.texinfo" "lisp/*.el" "docs/dir"
                     "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el"
                               "*-test.el" "*-tests.el" "LICENSE"
                               "README*" "*-pkg.el"))
                    :source "elpaca-menu-lock-file" :protocol https
                    :inherit t :depth treeless :ref
                    "3eefe68941933c8549049502007411ed2bf70387" :id
                    diff-hl :type git))
 (discover-my-major :source "elpaca-menu-lock-file" :recipe
                    (:package "discover-my-major" :fetcher git :url
                              "https://framagit.org/steckerhalter/discover-my-major.git"
                              :files
                              ("*.el" "*.el.in" "dir" "*.info"
                               "*.texi" "*.texinfo" "doc/dir"
                               "doc/*.info" "doc/*.texi"
                               "doc/*.texinfo" "lisp/*.el" "docs/dir"
                               "docs/*.info" "docs/*.texi"
                               "docs/*.texinfo"
                               (:exclude ".dir-locals.el" "test.el"
                                         "tests.el" "*-test.el"
                                         "*-tests.el" "LICENSE"
                                         "README*" "*-pkg.el"))
                              :source "elpaca-menu-lock-file"
                              :protocol https :inherit t :depth
                              treeless :ref
                              "c592e5e67454f0d1b68669ac0c270073164b16b3"
                              :id discover-my-major :type git))
 (docker :source "elpaca-menu-lock-file" :recipe
         (:package "docker" :fetcher github :repo "Silex/docker.el"
                   :files
                   ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                    "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                    "doc/*.texinfo" "lisp/*.el" "docs/dir"
                    "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                    (:exclude ".dir-locals.el" "test.el" "tests.el"
                              "*-test.el" "*-tests.el" "LICENSE"
                              "README*" "*-pkg.el"))
                   :source "elpaca-menu-lock-file" :protocol https
                   :inherit t :depth treeless :ref
                   "916686b86e83a3bd2281fbc5e6f98962aa747429" :id
                   docker :type git))
 (docker-compose-mode :source "elpaca-menu-lock-file" :recipe
                      (:package "docker-compose-mode" :repo
                                "meqif/docker-compose-mode" :fetcher
                                github :files
                                (:defaults
                                 (:exclude
                                  "docker-compose-mode-helpers.el"))
                                :source "elpaca-menu-lock-file"
                                :protocol https :inherit t :depth
                                treeless :ref
                                "abaa4f3aeb5c62d7d16e186dd7d77f4e846e126a"
                                :id docker-compose-mode :type git))
 (dockerfile-mode :source "elpaca-menu-lock-file" :recipe
                  (:package "dockerfile-mode" :fetcher github :repo
                            "spotify/dockerfile-mode" :files
                            ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                             "*.texinfo" "doc/dir" "doc/*.info"
                             "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                             "docs/dir" "docs/*.info" "docs/*.texi"
                             "docs/*.texinfo"
                             (:exclude ".dir-locals.el" "test.el"
                                       "tests.el" "*-test.el"
                                       "*-tests.el" "LICENSE"
                                       "README*" "*-pkg.el"))
                            :source "elpaca-menu-lock-file" :protocol
                            https :inherit t :depth treeless :ref
                            "97733ce074b1252c1270fd5e8a53d178b66668ed"
                            :id dockerfile-mode :type git))
 (doom-themes :source "elpaca-menu-lock-file" :recipe
              (:package "doom-themes" :fetcher github :repo
                        "doomemacs/themes" :files
                        (:defaults "themes/*.el" "themes/*/*.el"
                                   "extensions/*.el")
                        :source "elpaca-menu-lock-file" :protocol
                        https :inherit t :depth treeless :wait t :ref
                        "53645a905dfb3055db52f5d418d5ef612027e062" :id
                        doom-themes :type git))
 (dumb-jump :source "elpaca-menu-lock-file" :recipe
            (:package "dumb-jump" :repo "jacktasia/dumb-jump" :fetcher
                      github :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                       "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                       "doc/*.texinfo" "lisp/*.el" "docs/dir"
                       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE"
                                 "README*" "*-pkg.el"))
                      :source "elpaca-menu-lock-file" :protocol https
                      :inherit t :depth treeless :ref
                      "8c4df89c946c00d44955c0f5dda535b9bcaf8642" :id
                      dumb-jump :type git))
 (eat :source "elpaca-menu-lock-file" :recipe
      (:package "eat" :repo
                ("https://codeberg.org/akib/emacs-eat" . "eat") :files
                ("*" (:exclude ".git")) :source
                "elpaca-menu-lock-file" :protocol https :inherit t
                :depth treeless :ref
                "c8d54d649872bfe7b2b9f49ae5c2addbf12d3b99" :id eat
                :type git))
 (editorconfig :source "elpaca-menu-lock-file" :recipe
               (:package "editorconfig" :fetcher github :repo
                         "editorconfig/editorconfig-emacs" :old-names
                         (editorconfig-core editorconfig-fnmatch)
                         :files
                         ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                          "*.texinfo" "doc/dir" "doc/*.info"
                          "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                          "docs/dir" "docs/*.info" "docs/*.texi"
                          "docs/*.texinfo"
                          (:exclude ".dir-locals.el" "test.el"
                                    "tests.el" "*-test.el"
                                    "*-tests.el" "LICENSE" "README*"
                                    "*-pkg.el"))
                         :source "elpaca-menu-lock-file" :protocol
                         https :inherit t :depth treeless :ref
                         "b18fcf7fdea1ce84b7fdc60360ad8016b5c00d79"
                         :id editorconfig :type git))
 (elisp-refs :source "elpaca-menu-lock-file" :recipe
             (:package "elisp-refs" :repo "Wilfred/elisp-refs"
                       :fetcher github :files
                       (:defaults (:exclude "elisp-refs-bench.el"))
                       :source "elpaca-menu-lock-file" :protocol https
                       :inherit t :depth treeless :ref
                       "541a064c3ce27867872cf708354a65d83baf2a6d" :id
                       elisp-refs :type git))
 (elisp-slime-nav :source "elpaca-menu-lock-file" :recipe
                  (:package "elisp-slime-nav" :repo
                            "purcell/elisp-slime-nav" :fetcher github
                            :files
                            ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                             "*.texinfo" "doc/dir" "doc/*.info"
                             "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                             "docs/dir" "docs/*.info" "docs/*.texi"
                             "docs/*.texinfo"
                             (:exclude ".dir-locals.el" "test.el"
                                       "tests.el" "*-test.el"
                                       "*-tests.el" "LICENSE"
                                       "README*" "*-pkg.el"))
                            :source "elpaca-menu-lock-file" :protocol
                            https :inherit t :depth treeless :ref
                            "0b97339c552cac92788cab7c0724ca7ba160cd0e"
                            :id elisp-slime-nav :type git))
 (elixir-mode :source "elpaca-menu-lock-file" :recipe
              (:package "elixir-mode" :fetcher github :repo
                        "elixir-editors/emacs-elixir" :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                         "*.texinfo" "doc/dir" "doc/*.info"
                         "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                         "docs/dir" "docs/*.info" "docs/*.texi"
                         "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el"
                                   "tests.el" "*-test.el" "*-tests.el"
                                   "LICENSE" "README*" "*-pkg.el"))
                        :source "elpaca-menu-lock-file" :protocol
                        https :inherit t :depth treeless :ref
                        "00d6580a040a750e019218f9392cf9a4c2dac23a" :id
                        elixir-mode :type git))
 (elm-mode :source "elpaca-menu-lock-file" :recipe
           (:package "elm-mode" :fetcher github :repo
                     "jcollard/elm-mode" :files (:defaults "elm.tags")
                     :source "elpaca-menu-lock-file" :protocol https
                     :inherit t :depth treeless :ref
                     "90b72cd2c9bc4506f531bcdcd73fa2530d9f4f7c" :id
                     elm-mode :type git))
 (elpaca :source
   "elpaca-menu-lock-file" :recipe
   (:source nil :package "elpaca" :id elpaca :repo
            "https://github.com/progfolio/elpaca.git" :ref
            "27c2889f66368bde12b4e243582e343ed9cb75e3" :depth 1
            :inherit ignore :files
            (:defaults "elpaca-test.el" (:exclude "extensions"))
            :build (:not elpaca-activate) :type git :protocol https))
 (elpaca-use-package :source "elpaca-menu-lock-file" :recipe
                     (:package "elpaca-use-package" :wait t :repo
                               "https://github.com/progfolio/elpaca.git"
                               :files
                               ("extensions/elpaca-use-package.el")
                               :main
                               "extensions/elpaca-use-package.el"
                               :build
                               (:not elpaca-source elpaca-build-docs)
                               :source "Elpaca extensions" :id
                               elpaca-use-package :type git :protocol
                               https :inherit t :depth treeless :ref
                               "27c2889f66368bde12b4e243582e343ed9cb75e3"))
 (emacsql :source "elpaca-menu-lock-file" :recipe
          (:package "emacsql" :fetcher github :repo "magit/emacsql"
                    :files (:defaults "README.md" "sqlite") :source
                    "elpaca-menu-lock-file" :protocol https :inherit t
                    :depth treeless :ref
                    "6ef7473248fce048c7daa00c3b2ae75af987c89c" :id
                    emacsql :type git))
 (embark :source "elpaca-menu-lock-file" :recipe
         (:package "embark" :repo "oantolin/embark" :fetcher github
                   :files ("embark.el" "embark-org.el" "embark.texi")
                   :source "elpaca-menu-lock-file" :protocol https
                   :inherit t :depth treeless :ref
                   "7b3b2fa239c34c2e304eab4367a4f5924c047e2b" :id
                   embark :type git))
 (embark-consult :source "elpaca-menu-lock-file" :recipe
                 (:package "embark-consult" :repo "oantolin/embark"
                           :fetcher github :files
                           ("embark-consult.el") :source
                           "elpaca-menu-lock-file" :protocol https
                           :inherit t :depth treeless :ref
                           "7b3b2fa239c34c2e304eab4367a4f5924c047e2b"
                           :id embark-consult :type git))
 (epl :source "elpaca-menu-lock-file" :recipe
      (:package "epl" :repo "cask/epl" :fetcher github :files
                ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                 "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                 "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                 "docs/*.texinfo"
                 (:exclude ".dir-locals.el" "test.el" "tests.el"
                           "*-test.el" "*-tests.el" "LICENSE"
                           "README*" "*-pkg.el"))
                :source "elpaca-menu-lock-file" :protocol https
                :inherit t :depth treeless :ref
                "78ab7a85c08222cd15582a298a364774e3282ce6" :id epl
                :type git))
 (erlang :source "elpaca-menu-lock-file" :recipe
         (:package "erlang" :fetcher github :repo "erlang/otp"
                   :version-regexp "OTP-%v" :files
                   ("lib/tools/emacs/*.el"
                    (:exclude "lib/tools/emacs/erlang_appwiz.el"))
                   :source "elpaca-menu-lock-file" :protocol https
                   :inherit t :depth treeless :ref
                   "a361d74875f919d801248f05dbc9293b768235df" :id
                   erlang :type git))
 (exec-path-from-shell :source "elpaca-menu-lock-file" :recipe
                       (:package "exec-path-from-shell" :fetcher
                                 github :repo
                                 "purcell/exec-path-from-shell" :files
                                 ("*.el" "*.el.in" "dir" "*.info"
                                  "*.texi" "*.texinfo" "doc/dir"
                                  "doc/*.info" "doc/*.texi"
                                  "doc/*.texinfo" "lisp/*.el"
                                  "docs/dir" "docs/*.info"
                                  "docs/*.texi" "docs/*.texinfo"
                                  (:exclude ".dir-locals.el" "test.el"
                                            "tests.el" "*-test.el"
                                            "*-tests.el" "LICENSE"
                                            "README*" "*-pkg.el"))
                                 :source "elpaca-menu-lock-file"
                                 :protocol https :inherit t :depth
                                 treeless :ref
                                 "7552abf032a383ff761e7d90e6b5cbb4658a728a"
                                 :id exec-path-from-shell :type git))
 (expand-region :source "elpaca-menu-lock-file" :recipe
                (:package "expand-region" :repo
                          "magnars/expand-region.el" :fetcher github
                          :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                           "*.texinfo" "doc/dir" "doc/*.info"
                           "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                           "docs/dir" "docs/*.info" "docs/*.texi"
                           "docs/*.texinfo"
                           (:exclude ".dir-locals.el" "test.el"
                                     "tests.el" "*-test.el"
                                     "*-tests.el" "LICENSE" "README*"
                                     "*-pkg.el"))
                          :source "elpaca-menu-lock-file" :protocol
                          https :inherit t :depth treeless :ref
                          "351279272330cae6cecea941b0033a8dd8bcc4e8"
                          :id expand-region :type git))
 (f :source "elpaca-menu-lock-file" :recipe
    (:package "f" :fetcher github :repo "rejeep/f.el" :files
              ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
               "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
               "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
               "docs/*.texinfo"
               (:exclude ".dir-locals.el" "test.el" "tests.el"
                         "*-test.el" "*-tests.el" "LICENSE" "README*"
                         "*-pkg.el"))
              :source "elpaca-menu-lock-file" :protocol https :inherit
              t :depth treeless :ref
              "931b6d0667fe03e7bf1c6c282d6d8d7006143c52" :id f :type
              git))
 (feature-mode :source "elpaca-menu-lock-file" :recipe
               (:package "feature-mode" :fetcher github :repo
                         "freesteph/cucumber.el" :files
                         (:defaults "gherkin-languages.json"
                                    "snippets" "support")
                         :source "elpaca-menu-lock-file" :protocol
                         https :inherit t :depth treeless :ref
                         "8d43c37ddf986af769870da27c31c1911f35b205"
                         :id feature-mode :type git))
 (flycheck :source "elpaca-menu-lock-file" :recipe
           (:package "flycheck" :repo "flycheck/flycheck" :fetcher
                     github :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                      "doc/*.texinfo" "lisp/*.el" "docs/dir"
                      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE"
                                "README*" "*-pkg.el"))
                     :source "elpaca-menu-lock-file" :protocol https
                     :inherit t :depth treeless :ref
                     "5c24d1b732e86433f34ebf5ec7ca3c985edbc744" :id
                     flycheck :type git))
 (flycheck-clojure :source "elpaca-menu-lock-file" :recipe
                   (:package "flycheck-clojure" :fetcher github :repo
                             "clojure-emacs/squiggly-clojure" :files
                             ("elisp/flycheck-clojure/*.el") :source
                             "elpaca-menu-lock-file" :protocol https
                             :inherit t :depth treeless :ref
                             "592c4f89efb5112784cbf94c9ea6fdd045771b62"
                             :id flycheck-clojure :type git))
 (geiser :source "elpaca-menu-lock-file" :recipe
         (:package "geiser" :fetcher gitlab :repo
                   "emacs-geiser/geiser" :files
                   ("elisp/*.el" "doc/dir" "doc/geiser.texi") :source
                   "elpaca-menu-lock-file" :protocol https :inherit t
                   :depth treeless :ref
                   "8842104d1521a00c182ce78e9d50d394e9ba86f5" :id
                   geiser :type git))
 (gh :source "elpaca-menu-lock-file" :recipe
     (:package "gh" :repo "sigma/gh.el" :fetcher github :files
               ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                "docs/*.texinfo"
                (:exclude ".dir-locals.el" "test.el" "tests.el"
                          "*-test.el" "*-tests.el" "LICENSE" "README*"
                          "*-pkg.el"))
               :source "elpaca-menu-lock-file" :protocol https
               :inherit t :depth treeless :ref
               "b5a8d8209340d49ad82dab22d23dae0434499fdf" :id gh :type
               git))
 (gist :source "elpaca-menu-lock-file" :recipe
       (:package "gist" :repo "defunkt/gist.el" :fetcher github :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                  "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                  "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                  "docs/*.texinfo"
                  (:exclude ".dir-locals.el" "test.el" "tests.el"
                            "*-test.el" "*-tests.el" "LICENSE"
                            "README*" "*-pkg.el"))
                 :source "elpaca-menu-lock-file" :protocol https
                 :inherit t :depth treeless :ref
                 "314fe6ab80fae35b95f0734eceb82f72813b6f41" :id gist
                 :type git))
 (git-modes :source "elpaca-menu-lock-file" :recipe
            (:package "git-modes" :fetcher github :repo
                      "magit/git-modes" :old-names
                      (gitattributes-mode gitconfig-mode
                                          gitignore-mode)
                      :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                       "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                       "doc/*.texinfo" "lisp/*.el" "docs/dir"
                       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE"
                                 "README*" "*-pkg.el"))
                      :source "elpaca-menu-lock-file" :protocol https
                      :inherit t :depth treeless :ref
                      "c3faeeea1982786f78d8c38397dec0f078eaec84" :id
                      git-modes :type git))
 (git-timemachine :source "elpaca-menu-lock-file" :recipe
                  (:package "git-timemachine" :fetcher codeberg :repo
                            "pidu/git-timemachine" :files
                            ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                             "*.texinfo" "doc/dir" "doc/*.info"
                             "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                             "docs/dir" "docs/*.info" "docs/*.texi"
                             "docs/*.texinfo"
                             (:exclude ".dir-locals.el" "test.el"
                                       "tests.el" "*-test.el"
                                       "*-tests.el" "LICENSE"
                                       "README*" "*-pkg.el"))
                            :source "elpaca-menu-lock-file" :protocol
                            https :inherit t :depth treeless :ref
                            "d1346a76122595aeeb7ebb292765841c6cfd417b"
                            :id git-timemachine :type git))
 (go-mode :source "elpaca-menu-lock-file" :recipe
          (:package "go-mode" :repo "dominikh/go-mode.el" :fetcher
                    github :files ("go-mode.el") :source
                    "elpaca-menu-lock-file" :protocol https :inherit t
                    :depth treeless :ref
                    "0ed3c5227e7f622589f1411b4939c3ee34711ebd" :id
                    go-mode :type git))
 (gptel :source "elpaca-menu-lock-file" :recipe
        (:package "gptel" :repo "karthink/gptel" :fetcher github
                  :files
                  ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                   "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                   "doc/*.texinfo" "lisp/*.el" "docs/dir"
                   "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                   (:exclude ".dir-locals.el" "test.el" "tests.el"
                             "*-test.el" "*-tests.el" "LICENSE"
                             "README*" "*-pkg.el"))
                  :source "elpaca-menu-lock-file" :protocol https
                  :inherit t :depth treeless :ref
                  "11ddace991f63e98212be400570d85d716d2db83" :id gptel
                  :type git))
 (groovy-mode :source "elpaca-menu-lock-file" :recipe
              (:package "groovy-mode" :fetcher github :repo
                        "Groovy-Emacs-Modes/groovy-emacs-modes" :files
                        ("*groovy*.el") :source
                        "elpaca-menu-lock-file" :protocol https
                        :inherit t :depth treeless :ref
                        "7b8520b2e2d3ab1d62b35c426e17ac25ed0120bb" :id
                        groovy-mode :type git))
 (haml-mode :source "elpaca-menu-lock-file" :recipe
            (:package "haml-mode" :repo "nex3/haml-mode" :fetcher
                      github :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                       "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                       "doc/*.texinfo" "lisp/*.el" "docs/dir"
                       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE"
                                 "README*" "*-pkg.el"))
                      :source "elpaca-menu-lock-file" :protocol https
                      :inherit t :depth treeless :ref
                      "3bb4a96535eb5c81dbe6a43bfa8d67a778d449c0" :id
                      haml-mode :type git))
 (haskell-mode :source "elpaca-menu-lock-file" :recipe
               (:package "haskell-mode" :repo "haskell/haskell-mode"
                         :fetcher github :files
                         (:defaults "NEWS" "logo.svg") :source
                         "elpaca-menu-lock-file" :protocol https
                         :inherit t :depth treeless :ref
                         "2dd755a5fa11577a9388af88f385d2a8e18f7a8d"
                         :id haskell-mode :type git))
 (helpful :source "elpaca-menu-lock-file" :recipe
          (:package "helpful" :repo "Wilfred/helpful" :fetcher github
                    :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                     "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                     "doc/*.texinfo" "lisp/*.el" "docs/dir"
                     "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el"
                               "*-test.el" "*-tests.el" "LICENSE"
                               "README*" "*-pkg.el"))
                    :source "elpaca-menu-lock-file" :protocol https
                    :inherit t :depth treeless :ref
                    "03756fa6ad4dcca5e0920622b1ee3f70abfc4e39" :id
                    helpful :type git))
 (ht :source "elpaca-menu-lock-file" :recipe
     (:package "ht" :fetcher github :repo "Wilfred/ht.el" :files
               ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                "docs/*.texinfo"
                (:exclude ".dir-locals.el" "test.el" "tests.el"
                          "*-test.el" "*-tests.el" "LICENSE" "README*"
                          "*-pkg.el"))
               :source "elpaca-menu-lock-file" :protocol https
               :inherit t :depth treeless :ref
               "1c49aad1c820c86f7ee35bf9fff8429502f60fef" :id ht :type
               git))
 (htmlize :source "elpaca-menu-lock-file" :recipe
          (:package "htmlize" :fetcher github :repo
                    "emacsorphanage/htmlize" :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                     "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                     "doc/*.texinfo" "lisp/*.el" "docs/dir"
                     "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el"
                               "*-test.el" "*-tests.el" "LICENSE"
                               "README*" "*-pkg.el"))
                    :source "elpaca-menu-lock-file" :protocol https
                    :inherit t :depth treeless :ref
                    "fa644880699adea3770504f913e6dddbec90c076" :id
                    htmlize :type git))
 (hydra :source "elpaca-menu-lock-file" :recipe
        (:package "hydra" :repo "abo-abo/hydra" :fetcher github :files
                  (:defaults (:exclude "lv.el")) :source
                  "elpaca-menu-lock-file" :protocol https :inherit t
                  :depth treeless :ref
                  "59a2a45a35027948476d1d7751b0f0215b1e61aa" :id hydra
                  :type git))
 (inf-ruby :source "elpaca-menu-lock-file" :recipe
           (:package "inf-ruby" :repo "nonsequitur/inf-ruby" :fetcher
                     github :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                      "doc/*.texinfo" "lisp/*.el" "docs/dir"
                      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE"
                                "README*" "*-pkg.el"))
                     :source "elpaca-menu-lock-file" :protocol https
                     :inherit t :depth treeless :ref
                     "274398a24288a7db430a656b580ffbf889ca02aa" :id
                     inf-ruby :type git))
 (inheritenv :source "elpaca-menu-lock-file" :recipe
             (:package "inheritenv" :fetcher github :repo
                       "purcell/inheritenv" :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                        "*.texinfo" "doc/dir" "doc/*.info"
                        "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                        "docs/dir" "docs/*.info" "docs/*.texi"
                        "docs/*.texinfo"
                        (:exclude ".dir-locals.el" "test.el"
                                  "tests.el" "*-test.el" "*-tests.el"
                                  "LICENSE" "README*" "*-pkg.el"))
                       :source "elpaca-menu-lock-file" :protocol https
                       :inherit t :depth treeless :ref
                       "b9e67cc20c069539698a9ac54d0e6cc11e616c6f" :id
                       inheritenv :type git))
 (js2-mode :source "elpaca-menu-lock-file" :recipe
           (:package "js2-mode" :repo "mooz/js2-mode" :fetcher github
                     :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                      "doc/*.texinfo" "lisp/*.el" "docs/dir"
                      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE"
                                "README*" "*-pkg.el"))
                     :source "elpaca-menu-lock-file" :protocol https
                     :inherit t :depth treeless :ref
                     "e0c302872de4d26a9c1614fac8d6b94112b96307" :id
                     js2-mode :type git))
 (json-mode :source "elpaca-menu-lock-file" :recipe
            (:package "json-mode" :fetcher github :repo
                      "json-emacs/json-mode" :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                       "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                       "doc/*.texinfo" "lisp/*.el" "docs/dir"
                       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE"
                                 "README*" "*-pkg.el"))
                      :source "elpaca-menu-lock-file" :protocol https
                      :inherit t :depth treeless :ref
                      "466d5b563721bbeffac3f610aefaac15a39d90a9" :id
                      json-mode :type git))
 (json-snatcher :source "elpaca-menu-lock-file" :recipe
                (:package "json-snatcher" :fetcher github :repo
                          "Sterlingg/json-snatcher" :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                           "*.texinfo" "doc/dir" "doc/*.info"
                           "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                           "docs/dir" "docs/*.info" "docs/*.texi"
                           "docs/*.texinfo"
                           (:exclude ".dir-locals.el" "test.el"
                                     "tests.el" "*-test.el"
                                     "*-tests.el" "LICENSE" "README*"
                                     "*-pkg.el"))
                          :source "elpaca-menu-lock-file" :protocol
                          https :inherit t :depth treeless :ref
                          "b28d1c0670636da6db508d03872d96ffddbc10f2"
                          :id json-snatcher :type git))
 (kdl-mode :source "elpaca-menu-lock-file" :recipe
           (:package "kdl-mode" :fetcher github :repo
                     "taquangtrung/emacs-kdl-mode" :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                      "doc/*.texinfo" "lisp/*.el" "docs/dir"
                      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE"
                                "README*" "*-pkg.el"))
                     :source "elpaca-menu-lock-file" :protocol https
                     :inherit t :depth treeless :ref
                     "2d849e298199f490e4894c01764a8a83decd704a" :id
                     kdl-mode :type git))
 (kivy-mode :source "elpaca-menu-lock-file" :recipe
            (:package "kivy-mode" :repo "kivy/kivy" :fetcher github
                      :files ("kivy/tools/highlight/kivy-mode.el")
                      :source "elpaca-menu-lock-file" :protocol https
                      :inherit t :depth treeless :ref
                      "22426a44a31e5e0fca07471e6cf51f5bf21275f8" :id
                      kivy-mode :type git))
 (kotlin-mode :source "elpaca-menu-lock-file" :recipe
              (:package "kotlin-mode" :repo
                        "Emacs-Kotlin-Mode-Maintainers/kotlin-mode"
                        :fetcher github :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                         "*.texinfo" "doc/dir" "doc/*.info"
                         "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                         "docs/dir" "docs/*.info" "docs/*.texi"
                         "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el"
                                   "tests.el" "*-test.el" "*-tests.el"
                                   "LICENSE" "README*" "*-pkg.el"))
                        :source "elpaca-menu-lock-file" :protocol
                        https :inherit t :depth treeless :ref
                        "fddd747e5b4736e8b27a147960f369b86179ddff" :id
                        kotlin-mode :type git))
 (kubernetes :source "elpaca-menu-lock-file" :recipe
             (:package "kubernetes" :repo
                       "kubernetes-el/kubernetes-el" :fetcher github
                       :files
                       (:defaults (:exclude "kubernetes-evil.el"))
                       :source "elpaca-menu-lock-file" :protocol https
                       :inherit t :depth treeless :ref
                       "036583995bfceb0231738f65dd09c029ad812b02" :id
                       kubernetes :type git))
 (less-css-mode :source "elpaca-menu-lock-file" :recipe
                (:package "less-css-mode" :repo
                          "purcell/less-css-mode" :fetcher github
                          :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                           "*.texinfo" "doc/dir" "doc/*.info"
                           "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                           "docs/dir" "docs/*.info" "docs/*.texi"
                           "docs/*.texinfo"
                           (:exclude ".dir-locals.el" "test.el"
                                     "tests.el" "*-test.el"
                                     "*-tests.el" "LICENSE" "README*"
                                     "*-pkg.el"))
                          :source "elpaca-menu-lock-file" :protocol
                          https :inherit t :depth treeless :ref
                          "c78b88ff9af245daf05d45f5adf4befc11c1d4ef"
                          :id less-css-mode :type git))
 (llama :source "elpaca-menu-lock-file" :recipe
        (:package "llama" :fetcher github :repo "tarsius/llama" :files
                  ("llama.el" ".dir-locals.el") :source
                  "elpaca-menu-lock-file" :protocol https :inherit t
                  :depth treeless :ref
                  "2a89ba755b0459914a44b1ffa793e57f759a5b85" :id llama
                  :type git))
 (logito :source "elpaca-menu-lock-file" :recipe
         (:package "logito" :repo "sigma/logito" :fetcher github
                   :files
                   ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                    "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                    "doc/*.texinfo" "lisp/*.el" "docs/dir"
                    "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                    (:exclude ".dir-locals.el" "test.el" "tests.el"
                              "*-test.el" "*-tests.el" "LICENSE"
                              "README*" "*-pkg.el"))
                   :source "elpaca-menu-lock-file" :protocol https
                   :inherit t :depth treeless :ref
                   "d5934ce10ba3a70d3fcfb94d742ce3b9136ce124" :id
                   logito :type git))
 (lorem-ipsum :source "elpaca-menu-lock-file" :recipe
              (:package "lorem-ipsum" :fetcher github :repo
                        "jschaf/emacs-lorem-ipsum" :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                         "*.texinfo" "doc/dir" "doc/*.info"
                         "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                         "docs/dir" "docs/*.info" "docs/*.texi"
                         "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el"
                                   "tests.el" "*-test.el" "*-tests.el"
                                   "LICENSE" "README*" "*-pkg.el"))
                        :source "elpaca-menu-lock-file" :protocol
                        https :inherit t :depth treeless :ref
                        "4e87a899868e908a7a9e1812831d76c8d072f885" :id
                        lorem-ipsum :type git))
 (lsp-mode :source "elpaca-menu-lock-file" :recipe
           (:package "lsp-mode" :repo "emacs-lsp/lsp-mode" :fetcher
                     github :files (:defaults "clients/*.*") :source
                     "elpaca-menu-lock-file" :protocol https :inherit
                     t :depth treeless :ref
                     "06ec7b6525249ed0e1f066827de3e6dbddf33c3a" :id
                     lsp-mode :type git))
 (lua-mode :source "elpaca-menu-lock-file" :recipe
           (:package "lua-mode" :repo "immerrr/lua-mode" :fetcher
                     github :files
                     (:defaults (:exclude "init-tryout.el")) :source
                     "elpaca-menu-lock-file" :protocol https :inherit
                     t :depth treeless :ref
                     "2f6b8d7a6317e42c953c5119b0119ddb337e0a5f" :id
                     lua-mode :type git))
 (lv :source "elpaca-menu-lock-file" :recipe
     (:package "lv" :repo "abo-abo/hydra" :fetcher github :files
               ("lv.el") :source "elpaca-menu-lock-file" :protocol
               https :inherit t :depth treeless :ref
               "59a2a45a35027948476d1d7751b0f0215b1e61aa" :id lv :type
               git))
 (macrostep :source "elpaca-menu-lock-file" :recipe
            (:package "macrostep" :fetcher github :repo
                      "emacsorphanage/macrostep" :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                       "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                       "doc/*.texinfo" "lisp/*.el" "docs/dir"
                       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE"
                                 "README*" "*-pkg.el"))
                      :source "elpaca-menu-lock-file" :protocol https
                      :inherit t :depth treeless :ref
                      "d0928626b4711dcf9f8f90439d23701118724199" :id
                      macrostep :type git))
 (magit :source "elpaca-menu-lock-file" :recipe
        (:package "magit" :fetcher github :repo "magit/magit" :files
                  ("lisp/magit*.el" "lisp/git-*.el" "docs/magit.texi"
                   "docs/AUTHORS.md" "LICENSE" ".dir-locals.el"
                   ("git-hooks" "git-hooks/*")
                   (:exclude "lisp/magit-section.el"))
                  :source "elpaca-menu-lock-file" :protocol https
                  :inherit t :depth treeless :ref
                  "492ef97c3f06232121139534aca3e6dc7f1d3a89" :id magit
                  :type git))
 (magit-popup :source "elpaca-menu-lock-file" :recipe
              (:package "magit-popup" :fetcher github :repo
                        "magit/magit-popup" :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                         "*.texinfo" "doc/dir" "doc/*.info"
                         "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                         "docs/dir" "docs/*.info" "docs/*.texi"
                         "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el"
                                   "tests.el" "*-test.el" "*-tests.el"
                                   "LICENSE" "README*" "*-pkg.el"))
                        :source "elpaca-menu-lock-file" :protocol
                        https :inherit t :depth treeless :ref
                        "d8585fa39f88956963d877b921322530257ba9f5" :id
                        magit-popup :type git))
 (magit-section :source "elpaca-menu-lock-file" :recipe
                (:package "magit-section" :fetcher github :repo
                          "magit/magit" :files
                          ("lisp/magit-section.el"
                           "docs/magit-section.texi"
                           "magit-section-pkg.el")
                          :source "elpaca-menu-lock-file" :protocol
                          https :inherit t :depth treeless :ref
                          "492ef97c3f06232121139534aca3e6dc7f1d3a89"
                          :id magit-section :type git))
 (makey :source "elpaca-menu-lock-file" :recipe
        (:package "makey" :fetcher github :repo "mickeynp/makey"
                  :files
                  ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                   "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                   "doc/*.texinfo" "lisp/*.el" "docs/dir"
                   "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                   (:exclude ".dir-locals.el" "test.el" "tests.el"
                             "*-test.el" "*-tests.el" "LICENSE"
                             "README*" "*-pkg.el"))
                  :source "elpaca-menu-lock-file" :protocol https
                  :inherit t :depth treeless :ref
                  "c0b6bd5956744dd64052e54574e35d39f7c9d75b" :id makey
                  :type git))
 (marginalia :source "elpaca-menu-lock-file" :recipe
             (:package "marginalia" :repo "minad/marginalia" :fetcher
                       github :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                        "*.texinfo" "doc/dir" "doc/*.info"
                        "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                        "docs/dir" "docs/*.info" "docs/*.texi"
                        "docs/*.texinfo"
                        (:exclude ".dir-locals.el" "test.el"
                                  "tests.el" "*-test.el" "*-tests.el"
                                  "LICENSE" "README*" "*-pkg.el"))
                       :source "elpaca-menu-lock-file" :protocol https
                       :inherit t :depth treeless :ref
                       "0d08fbea0f1182627891240780081ba528c1348b" :id
                       marginalia :type git))
 (markdown-mode :source "elpaca-menu-lock-file" :recipe
                (:package "markdown-mode" :fetcher github :repo
                          "jrblevin/markdown-mode" :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                           "*.texinfo" "doc/dir" "doc/*.info"
                           "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                           "docs/dir" "docs/*.info" "docs/*.texi"
                           "docs/*.texinfo"
                           (:exclude ".dir-locals.el" "test.el"
                                     "tests.el" "*-test.el"
                                     "*-tests.el" "LICENSE" "README*"
                                     "*-pkg.el"))
                          :source "elpaca-menu-lock-file" :protocol
                          https :inherit t :depth treeless :ref
                          "92802fae9ebbc8c2e4c281c06dcdbd74b8bca80e"
                          :id markdown-mode :type git))
 (marshal :source "elpaca-menu-lock-file" :recipe
          (:package "marshal" :fetcher github :repo "sigma/marshal.el"
                    :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                     "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                     "doc/*.texinfo" "lisp/*.el" "docs/dir"
                     "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el"
                               "*-test.el" "*-tests.el" "LICENSE"
                               "README*" "*-pkg.el"))
                    :source "elpaca-menu-lock-file" :protocol https
                    :inherit t :depth treeless :ref
                    "bc00044d9073482f589aad959e34d563598f682a" :id
                    marshal :type git))
 (mise :source "elpaca-menu-lock-file" :recipe
       (:package "mise" :fetcher github :repo "eki3z/mise.el" :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                  "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                  "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                  "docs/*.texinfo"
                  (:exclude ".dir-locals.el" "test.el" "tests.el"
                            "*-test.el" "*-tests.el" "LICENSE"
                            "README*" "*-pkg.el"))
                 :source "elpaca-menu-lock-file" :protocol https
                 :inherit t :depth treeless :ref
                 "849c44b36594c80c57a555c5671dd1a5a83d3184" :id mise
                 :type git))
 (move-text :source "elpaca-menu-lock-file" :recipe
            (:package "move-text" :fetcher github :repo
                      "emacsfodder/move-text" :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                       "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                       "doc/*.texinfo" "lisp/*.el" "docs/dir"
                       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE"
                                 "README*" "*-pkg.el"))
                      :source "elpaca-menu-lock-file" :protocol https
                      :inherit t :depth treeless :ref
                      "2a8ebefeb0b363681e9562847eca3fd66e090d70" :id
                      move-text :type git))
 (multi-term :source "elpaca-menu-lock-file" :recipe
             (:package "multi-term" :fetcher github :repo
                       "manateelazycat/multi-term" :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                        "*.texinfo" "doc/dir" "doc/*.info"
                        "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                        "docs/dir" "docs/*.info" "docs/*.texi"
                        "docs/*.texinfo"
                        (:exclude ".dir-locals.el" "test.el"
                                  "tests.el" "*-test.el" "*-tests.el"
                                  "LICENSE" "README*" "*-pkg.el"))
                       :source "elpaca-menu-lock-file" :protocol https
                       :inherit t :depth treeless :ref
                       "017c77c550115936860e2ea71b88e585371475d5" :id
                       multi-term :type git))
 (multiple-cursors :source "elpaca-menu-lock-file" :recipe
                   (:package "multiple-cursors" :fetcher github :repo
                             "magnars/multiple-cursors.el" :files
                             ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                              "*.texinfo" "doc/dir" "doc/*.info"
                              "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                              "docs/dir" "docs/*.info" "docs/*.texi"
                              "docs/*.texinfo"
                              (:exclude ".dir-locals.el" "test.el"
                                        "tests.el" "*-test.el"
                                        "*-tests.el" "LICENSE"
                                        "README*" "*-pkg.el"))
                             :source "elpaca-menu-lock-file" :protocol
                             https :inherit t :depth treeless :ref
                             "ddd677091afc7d65ce56d11866e18aeded110ada"
                             :id multiple-cursors :type git))
 (nginx-mode :source "elpaca-menu-lock-file" :recipe
             (:package "nginx-mode" :fetcher github :repo
                       "ajc/nginx-mode" :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                        "*.texinfo" "doc/dir" "doc/*.info"
                        "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                        "docs/dir" "docs/*.info" "docs/*.texi"
                        "docs/*.texinfo"
                        (:exclude ".dir-locals.el" "test.el"
                                  "tests.el" "*-test.el" "*-tests.el"
                                  "LICENSE" "README*" "*-pkg.el"))
                       :source "elpaca-menu-lock-file" :protocol https
                       :inherit t :depth treeless :ref
                       "c4ac5de975d65c84893a130a470af32a48b0b66c" :id
                       nginx-mode :type git))
 (no-littering :source "elpaca-menu-lock-file" :recipe
               (:package "no-littering" :fetcher github :repo
                         "emacscollective/no-littering" :files
                         ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                          "*.texinfo" "doc/dir" "doc/*.info"
                          "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                          "docs/dir" "docs/*.info" "docs/*.texi"
                          "docs/*.texinfo"
                          (:exclude ".dir-locals.el" "test.el"
                                    "tests.el" "*-test.el"
                                    "*-tests.el" "LICENSE" "README*"
                                    "*-pkg.el"))
                         :source "elpaca-menu-lock-file" :protocol
                         https :inherit t :depth treeless :wait t :ref
                         "ec8b6b76e6a8e53fa5e471949021dee68f3bc2e6"
                         :id no-littering :type git))
 (operate-on-number :source "elpaca-menu-lock-file" :recipe
                    (:package "operate-on-number" :fetcher github
                              :repo "knu/operate-on-number.el" :files
                              ("*.el" "*.el.in" "dir" "*.info"
                               "*.texi" "*.texinfo" "doc/dir"
                               "doc/*.info" "doc/*.texi"
                               "doc/*.texinfo" "lisp/*.el" "docs/dir"
                               "docs/*.info" "docs/*.texi"
                               "docs/*.texinfo"
                               (:exclude ".dir-locals.el" "test.el"
                                         "tests.el" "*-test.el"
                                         "*-tests.el" "LICENSE"
                                         "README*" "*-pkg.el"))
                              :source "elpaca-menu-lock-file"
                              :protocol https :inherit t :depth
                              treeless :ref
                              "0ddebae1885c1b54eae1d79e66204d6d83c5065b"
                              :id operate-on-number :type git))
 (orderless :source "elpaca-menu-lock-file" :recipe
            (:package "orderless" :repo "oantolin/orderless" :fetcher
                      github :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                       "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                       "doc/*.texinfo" "lisp/*.el" "docs/dir"
                       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE"
                                 "README*" "*-pkg.el"))
                      :source "elpaca-menu-lock-file" :protocol https
                      :inherit t :depth treeless :ref
                      "3a2a32181f7a5bd7b633e40d89de771a5dd88cc7" :id
                      orderless :type git))
 (org-autolist :source "elpaca-menu-lock-file" :recipe
               (:package "org-autolist" :fetcher github :repo
                         "calvinwyoung/org-autolist" :files
                         ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                          "*.texinfo" "doc/dir" "doc/*.info"
                          "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                          "docs/dir" "docs/*.info" "docs/*.texi"
                          "docs/*.texinfo"
                          (:exclude ".dir-locals.el" "test.el"
                                    "tests.el" "*-test.el"
                                    "*-tests.el" "LICENSE" "README*"
                                    "*-pkg.el"))
                         :source "elpaca-menu-lock-file" :protocol
                         https :inherit t :depth treeless :ref
                         "0f5dc4e00cb050b94289504925b36c7650552a1a"
                         :id org-autolist :type git))
 (org-mime :source "elpaca-menu-lock-file" :recipe
           (:package "org-mime" :repo "org-mime/org-mime" :fetcher
                     github :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                      "doc/*.texinfo" "lisp/*.el" "docs/dir"
                      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE"
                                "README*" "*-pkg.el"))
                     :source "elpaca-menu-lock-file" :protocol https
                     :inherit t :depth treeless :ref
                     "ffaad784a8597ee52842a578c01bd347d3e0281d" :id
                     org-mime :type git))
 (org-ql :source "elpaca-menu-lock-file" :recipe
         (:package "org-ql" :fetcher github :repo "alphapapa/org-ql"
                   :files (:defaults (:exclude "helm-org-ql.el"))
                   :source "elpaca-menu-lock-file" :protocol https
                   :inherit t :depth treeless :ref
                   "4b8330a683c43bb4a2c64ccce8cd5a90c8b174ca" :id
                   org-ql :wait t :type git))
 (org-roam :source "elpaca-menu-lock-file" :recipe
           (:package "org-roam" :fetcher github :repo
                     "org-roam/org-roam" :files
                     (:defaults "extensions/*") :source
                     "elpaca-menu-lock-file" :protocol https :inherit
                     t :depth treeless :ref
                     "18dfe8128216cfe8c55074a46f6e617da971de57" :id
                     org-roam :type git))
 (org-super-agenda :source "elpaca-menu-lock-file" :recipe
                   (:package "org-super-agenda" :fetcher github :repo
                             "alphapapa/org-super-agenda" :files
                             ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                              "*.texinfo" "doc/dir" "doc/*.info"
                              "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                              "docs/dir" "docs/*.info" "docs/*.texi"
                              "docs/*.texinfo"
                              (:exclude ".dir-locals.el" "test.el"
                                        "tests.el" "*-test.el"
                                        "*-tests.el" "LICENSE"
                                        "README*" "*-pkg.el"))
                             :source "elpaca-menu-lock-file" :protocol
                             https :inherit t :depth treeless :ref
                             "fb20ad9c8a9705aa05d40751682beae2d094e0fe"
                             :id org-super-agenda :type git))
 (ov :source "elpaca-menu-lock-file" :recipe
     (:package "ov" :fetcher github :repo "emacsorphanage/ov" :files
               ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                "docs/*.texinfo"
                (:exclude ".dir-locals.el" "test.el" "tests.el"
                          "*-test.el" "*-tests.el" "LICENSE" "README*"
                          "*-pkg.el"))
               :source "elpaca-menu-lock-file" :protocol https
               :inherit t :depth treeless :ref
               "e2971ad986b6ac441e9849031d34c56c980cf40b" :id ov :type
               git))
 (ox-clip :source "elpaca-menu-lock-file" :recipe
          (:package "ox-clip" :fetcher github :repo "jkitchin/ox-clip"
                    :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                     "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                     "doc/*.texinfo" "lisp/*.el" "docs/dir"
                     "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el"
                               "*-test.el" "*-tests.el" "LICENSE"
                               "README*" "*-pkg.el"))
                    :source "elpaca-menu-lock-file" :protocol https
                    :inherit t :depth treeless :ref
                    "a549cc8e1747beb6b7e567ffac27e31ba45cb8e8" :id
                    ox-clip :type git))
 (parseclj :source "elpaca-menu-lock-file" :recipe
           (:package "parseclj" :repo "clojure-emacs/parseclj"
                     :fetcher github :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                      "doc/*.texinfo" "lisp/*.el" "docs/dir"
                      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE"
                                "README*" "*-pkg.el"))
                     :source "elpaca-menu-lock-file" :protocol https
                     :inherit t :depth treeless :ref
                     "6af22372e0fe14df882dd300b22b12ba2d7e00b0" :id
                     parseclj :type git))
 (parseedn :source "elpaca-menu-lock-file" :recipe
           (:package "parseedn" :repo "clojure-emacs/parseedn"
                     :fetcher github :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                      "doc/*.texinfo" "lisp/*.el" "docs/dir"
                      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE"
                                "README*" "*-pkg.el"))
                     :source "elpaca-menu-lock-file" :protocol https
                     :inherit t :depth treeless :ref
                     "3407e4530a367b6c2b857dae261cdbb67a440aaa" :id
                     parseedn :type git))
 (pcache :source "elpaca-menu-lock-file" :recipe
         (:package "pcache" :repo "sigma/pcache" :fetcher github
                   :files
                   ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                    "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                    "doc/*.texinfo" "lisp/*.el" "docs/dir"
                    "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                    (:exclude ".dir-locals.el" "test.el" "tests.el"
                              "*-test.el" "*-tests.el" "LICENSE"
                              "README*" "*-pkg.el"))
                   :source "elpaca-menu-lock-file" :protocol https
                   :inherit t :depth treeless :ref
                   "e287b5d116679f79789ee9ee22ee213dc6cef68c" :id
                   pcache :type git))
 (persp-mode :source "elpaca-menu-lock-file" :recipe
             (:package "persp-mode" :repo "Bad-ptr/persp-mode.el"
                       :fetcher github :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                        "*.texinfo" "doc/dir" "doc/*.info"
                        "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                        "docs/dir" "docs/*.info" "docs/*.texi"
                        "docs/*.texinfo"
                        (:exclude ".dir-locals.el" "test.el"
                                  "tests.el" "*-test.el" "*-tests.el"
                                  "LICENSE" "README*" "*-pkg.el"))
                       :source "elpaca-menu-lock-file" :protocol https
                       :inherit t :depth treeless :wait t :ref
                       "fab4bf76927445d2e431f06e74572acba81f47d5" :id
                       persp-mode :type git))
 (php-mode :source "elpaca-menu-lock-file" :recipe
           (:package "php-mode" :repo "emacs-php/php-mode" :fetcher
                     github :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                      "doc/*.texinfo" "lisp/*.el" "docs/dir"
                      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE"
                                "README*" "*-pkg.el"))
                     :source "elpaca-menu-lock-file" :protocol https
                     :inherit t :depth treeless :ref
                     "d9858333e42f42c1486a84bc5277e9d8e37e40cc" :id
                     php-mode :type git))
 (pkg-info :source "elpaca-menu-lock-file" :recipe
           (:package "pkg-info" :repo "emacsorphanage/pkg-info"
                     :fetcher github :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                      "doc/*.texinfo" "lisp/*.el" "docs/dir"
                      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE"
                                "README*" "*-pkg.el"))
                     :source "elpaca-menu-lock-file" :protocol https
                     :inherit t :depth treeless :ref
                     "76ba7415480687d05a4353b27fea2ae02b8d9d61" :id
                     pkg-info :type git))
 (popup :source "elpaca-menu-lock-file" :recipe
        (:package "popup" :fetcher github :repo
                  "auto-complete/popup-el" :files
                  ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                   "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                   "doc/*.texinfo" "lisp/*.el" "docs/dir"
                   "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                   (:exclude ".dir-locals.el" "test.el" "tests.el"
                             "*-test.el" "*-tests.el" "LICENSE"
                             "README*" "*-pkg.el"))
                  :source "elpaca-menu-lock-file" :protocol https
                  :inherit t :depth treeless :ref
                  "45a0b759076ce4139aba36dde0a2904136282e73" :id popup
                  :type git))
 (pretty-hydra :source "elpaca-menu-lock-file" :recipe
               (:package "pretty-hydra" :repo
                         "jerrypnz/major-mode-hydra.el" :fetcher
                         github :files ("pretty-hydra.el") :source
                         "elpaca-menu-lock-file" :protocol https
                         :inherit t :depth treeless :ref
                         "2494d71e24b61c1f5ef2dc17885e2f65bf98b3b2"
                         :id pretty-hydra :type git))
 (protobuf-mode :source "elpaca-menu-lock-file" :recipe
                (:package "protobuf-mode" :fetcher github :repo
                          "protocolbuffers/protobuf" :files
                          ("editors/protobuf-mode.el") :source
                          "elpaca-menu-lock-file" :protocol https
                          :inherit t :depth treeless :ref
                          "2e44dfbce3c2a3fa40ba5782f64271adff3e0a1f"
                          :id protobuf-mode :type git))
 (puppet-mode :source "elpaca-menu-lock-file" :recipe
              (:package "puppet-mode" :fetcher github :repo
                        "voxpupuli/puppet-mode" :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                         "*.texinfo" "doc/dir" "doc/*.info"
                         "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                         "docs/dir" "docs/*.info" "docs/*.texi"
                         "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el"
                                   "tests.el" "*-test.el" "*-tests.el"
                                   "LICENSE" "README*" "*-pkg.el"))
                        :source "elpaca-menu-lock-file" :protocol
                        https :inherit t :depth treeless :ref
                        "f02aa30f5c0ce7fd0e1ffb245e5c832538bf6a91" :id
                        puppet-mode :type git))
 (python-mode :source "elpaca-menu-lock-file" :recipe
              (:package "python-mode" :fetcher gitlab :repo
                        "python-mode-devs/python-mode" :files
                        ("python-mode.el"
                         ("completion" "completion/pycomplete.*"))
                        :source "elpaca-menu-lock-file" :protocol
                        https :inherit t :depth treeless :ref
                        "2a6fdb1c823bf8125f6354727db63c8894092821" :id
                        python-mode :type git))
 (pythonic :source "elpaca-menu-lock-file" :recipe
           (:package "pythonic" :fetcher github :repo
                     "pythonic-emacs/pythonic" :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                      "doc/*.texinfo" "lisp/*.el" "docs/dir"
                      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE"
                                "README*" "*-pkg.el"))
                     :source "elpaca-menu-lock-file" :protocol https
                     :inherit t :depth treeless :ref
                     "bf364a29e2f21828941ee3d11a27127bc260740f" :id
                     pythonic :type git))
 (queue :source "elpaca-menu-lock-file" :recipe
        (:package "queue" :repo
                  ("https://github.com/emacsmirror/gnu_elpa" . "queue")
                  :branch "externals/queue" :files
                  ("*" (:exclude ".git")) :source
                  "elpaca-menu-lock-file" :protocol https :inherit t
                  :depth treeless :ref
                  "f986fb68e75bdae951efb9e11a3012ab6bd408ee" :id queue
                  :type git))
 (rainbow-delimiters :source "elpaca-menu-lock-file" :recipe
                     (:package "rainbow-delimiters" :fetcher github
                               :repo "Fanael/rainbow-delimiters"
                               :files
                               ("*.el" "*.el.in" "dir" "*.info"
                                "*.texi" "*.texinfo" "doc/dir"
                                "doc/*.info" "doc/*.texi"
                                "doc/*.texinfo" "lisp/*.el" "docs/dir"
                                "docs/*.info" "docs/*.texi"
                                "docs/*.texinfo"
                                (:exclude ".dir-locals.el" "test.el"
                                          "tests.el" "*-test.el"
                                          "*-tests.el" "LICENSE"
                                          "README*" "*-pkg.el"))
                               :source "elpaca-menu-lock-file"
                               :protocol https :inherit t :depth
                               treeless :ref
                               "f40ece58df8b2f0fb6c8576b527755a552a5e763"
                               :id rainbow-delimiters :type git))
 (rainbow-mode :source "elpaca-menu-lock-file" :recipe
               (:package "rainbow-mode" :repo
                         ("https://github.com/emacsmirror/gnu_elpa"
                          . "rainbow-mode")
                         :branch "externals/rainbow-mode" :files
                         ("*" (:exclude ".git")) :source
                         "elpaca-menu-lock-file" :protocol https
                         :inherit t :depth treeless :ref
                         "f7db3b5919f70420a91eb199f8663468de3033f3"
                         :id rainbow-mode :type git))
 (rake :source "elpaca-menu-lock-file" :recipe
       (:package "rake" :repo "asok/rake" :fetcher github :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                  "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                  "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                  "docs/*.texinfo"
                  (:exclude ".dir-locals.el" "test.el" "tests.el"
                            "*-test.el" "*-tests.el" "LICENSE"
                            "README*" "*-pkg.el"))
                 :source "elpaca-menu-lock-file" :protocol https
                 :inherit t :depth treeless :ref
                 "452ea0caca33376487103c64177c295ed2960cca" :id rake
                 :type git))
 (reformatter :source "elpaca-menu-lock-file" :recipe
              (:package "reformatter" :repo
                        "purcell/emacs-reformatter" :fetcher github
                        :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                         "*.texinfo" "doc/dir" "doc/*.info"
                         "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                         "docs/dir" "docs/*.info" "docs/*.texi"
                         "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el"
                                   "tests.el" "*-test.el" "*-tests.el"
                                   "LICENSE" "README*" "*-pkg.el"))
                        :source "elpaca-menu-lock-file" :protocol
                        https :inherit t :depth treeless :ref
                        "c0ddac04b7b937ed56d6bf97e4bfcc4eccfa501a" :id
                        reformatter :type git))
 (request :source "elpaca-menu-lock-file"
   :recipe
   (:package "request" :repo "tkf/emacs-request" :fetcher github
             :files ("request.el") :source "elpaca-menu-lock-file"
             :protocol https :inherit t :depth treeless :ref
             "c22e3c23a6dd90f64be536e176ea0ed6113a5ba6" :id request
             :type git))
 (rg :source "elpaca-menu-lock-file" :recipe
     (:package "rg" :fetcher github :repo "dajva/rg.el" :files
               ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                "docs/*.texinfo"
                (:exclude ".dir-locals.el" "test.el" "tests.el"
                          "*-test.el" "*-tests.el" "LICENSE" "README*"
                          "*-pkg.el"))
               :source "elpaca-menu-lock-file" :protocol https
               :inherit t :depth treeless :ref
               "a614e7d7709c7bf5c5accff4003d351c3f28ee98" :id rg :type
               git))
 (robe :source "elpaca-menu-lock-file" :recipe
       (:package "robe" :repo "dgutov/robe" :fetcher github :files
                 ("*robe.el" "lib") :source "elpaca-menu-lock-file"
                 :protocol https :inherit t :depth treeless :ref
                 "9d26c9ebaaab999fb53ac4375ae3eac0b6bdfb76" :id robe
                 :type git))
 (rspec-mode :source "elpaca-menu-lock-file" :recipe
             (:package "rspec-mode" :repo "pezra/rspec-mode" :fetcher
                       github :files ("rspec-mode.el" "snippets")
                       :source "elpaca-menu-lock-file" :protocol https
                       :inherit t :depth treeless :ref
                       "29df3d081c6a1cbdf840cd13d45ea1c100c5bbaa" :id
                       rspec-mode :type git))
 (rubocop :source "elpaca-menu-lock-file" :recipe
          (:package "rubocop" :repo "rubocop/rubocop-emacs" :fetcher
                    github :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                     "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                     "doc/*.texinfo" "lisp/*.el" "docs/dir"
                     "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el"
                               "*-test.el" "*-tests.el" "LICENSE"
                               "README*" "*-pkg.el"))
                    :source "elpaca-menu-lock-file" :protocol https
                    :inherit t :depth treeless :ref
                    "f5fd18aa810c3d3269188cbbd731ddc09006f8f5" :id
                    rubocop :type git))
 (ruby-tools :source "elpaca-menu-lock-file" :recipe
             (:package "ruby-tools" :repo "rejeep/ruby-tools.el"
                       :fetcher github :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                        "*.texinfo" "doc/dir" "doc/*.info"
                        "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                        "docs/dir" "docs/*.info" "docs/*.texi"
                        "docs/*.texinfo"
                        (:exclude ".dir-locals.el" "test.el"
                                  "tests.el" "*-test.el" "*-tests.el"
                                  "LICENSE" "README*" "*-pkg.el"))
                       :source "elpaca-menu-lock-file" :protocol https
                       :inherit t :depth treeless :ref
                       "6b97066b58a4f82eb2ecea6434a0a7e981aa4c18" :id
                       ruby-tools :type git))
 (rust-mode :source "elpaca-menu-lock-file" :recipe
            (:package "rust-mode" :repo "rust-lang/rust-mode" :fetcher
                      github :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                       "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                       "doc/*.texinfo" "lisp/*.el" "docs/dir"
                       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE"
                                 "README*" "*-pkg.el"))
                      :source "elpaca-menu-lock-file" :protocol https
                      :inherit t :depth treeless :ref
                      "f68ddca5c22b94a2de7c9ce20d629cd78d60b269" :id
                      rust-mode :type git))
 (s :source "elpaca-menu-lock-file" :recipe
    (:package "s" :fetcher github :repo "magnars/s.el" :files
              ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
               "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
               "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
               "docs/*.texinfo"
               (:exclude ".dir-locals.el" "test.el" "tests.el"
                         "*-test.el" "*-tests.el" "LICENSE" "README*"
                         "*-pkg.el"))
              :source "elpaca-menu-lock-file" :protocol https :inherit
              t :depth treeless :ref
              "dda84d38fffdaf0c9b12837b504b402af910d01d" :id s :type
              git))
 (sass-mode :source "elpaca-menu-lock-file" :recipe
            (:package "sass-mode" :repo "nex3/sass-mode" :fetcher
                      github :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                       "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                       "doc/*.texinfo" "lisp/*.el" "docs/dir"
                       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE"
                                 "README*" "*-pkg.el"))
                      :source "elpaca-menu-lock-file" :protocol https
                      :inherit t :depth treeless :ref
                      "247a0d4b509f10b28e4687cd8763492bca03599b" :id
                      sass-mode :type git))
 (scala-mode :source "elpaca-menu-lock-file" :recipe
             (:package "scala-mode" :fetcher github :repo
                       "hvesalai/emacs-scala-mode" :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                        "*.texinfo" "doc/dir" "doc/*.info"
                        "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                        "docs/dir" "docs/*.info" "docs/*.texi"
                        "docs/*.texinfo"
                        (:exclude ".dir-locals.el" "test.el"
                                  "tests.el" "*-test.el" "*-tests.el"
                                  "LICENSE" "README*" "*-pkg.el"))
                       :source "elpaca-menu-lock-file" :protocol https
                       :inherit t :depth treeless :ref
                       "50bcafa181baec7054e27f4bca55d5f9277c6350" :id
                       scala-mode :type git))
 (sesman :source "elpaca-menu-lock-file" :recipe
         (:package "sesman" :repo "vspinu/sesman" :fetcher github
                   :files
                   ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                    "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                    "doc/*.texinfo" "lisp/*.el" "docs/dir"
                    "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                    (:exclude ".dir-locals.el" "test.el" "tests.el"
                              "*-test.el" "*-tests.el" "LICENSE"
                              "README*" "*-pkg.el"))
                   :source "elpaca-menu-lock-file" :protocol https
                   :inherit t :depth treeless :ref
                   "7bca68dbbab0af26a6a23be1ff5fa97f9a18e022" :id
                   sesman :type git))
 (slim-mode :source "elpaca-menu-lock-file" :recipe
            (:package "slim-mode" :repo "slim-template/emacs-slim"
                      :fetcher github :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                       "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                       "doc/*.texinfo" "lisp/*.el" "docs/dir"
                       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE"
                                 "README*" "*-pkg.el"))
                      :source "elpaca-menu-lock-file" :protocol https
                      :inherit t :depth treeless :ref
                      "0b1b3803290f749cb85084adc75013254b513d41" :id
                      slim-mode :type git))
 (slime :source "elpaca-menu-lock-file" :recipe
        (:package "slime" :fetcher github :repo "slime/slime" :files
                  ("*.el" ("lib" "lib/hyperspec.el") "swank" "*.lisp"
                   "*.asd" "doc/slime.texi" "doc/slime.info" "doc/dir"
                   "ChangeLog" ("contrib" "contrib/*")
                   (:exclude "contrib/test" "contrib/Makefile"))
                  :source "elpaca-menu-lock-file" :protocol https
                  :inherit t :depth treeless :ref
                  "064716011575908ca11623592664196b5a38a560" :id slime
                  :type git))
 (smartparens :source "elpaca-menu-lock-file" :recipe
              (:package "smartparens" :fetcher github :repo
                        "Fuco1/smartparens" :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                         "*.texinfo" "doc/dir" "doc/*.info"
                         "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                         "docs/dir" "docs/*.info" "docs/*.texi"
                         "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el"
                                   "tests.el" "*-test.el" "*-tests.el"
                                   "LICENSE" "README*" "*-pkg.el"))
                        :source "elpaca-menu-lock-file" :protocol
                        https :inherit t :depth treeless :ref
                        "82d2cf084a19b0c2c3812e0550721f8a61996056" :id
                        smartparens :type git))
 (snap-indent :source "elpaca-menu-lock-file" :recipe
              (:package "snap-indent" :fetcher github :repo
                        "jeffvalk/snap-indent" :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                         "*.texinfo" "doc/dir" "doc/*.info"
                         "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                         "docs/dir" "docs/*.info" "docs/*.texi"
                         "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el"
                                   "tests.el" "*-test.el" "*-tests.el"
                                   "LICENSE" "README*" "*-pkg.el"))
                        :source "elpaca-menu-lock-file" :protocol
                        https :inherit t :depth treeless :ref
                        "0bac6b7eca59793cd0d810d85658b2a5dd05d5d9" :id
                        snap-indent :type git))
 (spinner :source "elpaca-menu-lock-file" :recipe
          (:package "spinner" :repo
                    ("https://github.com/Malabarba/spinner.el"
                     . "spinner")
                    :files ("*" (:exclude ".git")) :source
                    "elpaca-menu-lock-file" :protocol https :inherit t
                    :depth treeless :ref
                    "d4647ae87fb0cd24bc9081a3d287c860ff061c21" :id
                    spinner :type git))
 (string-inflection :source "elpaca-menu-lock-file" :recipe
                    (:package "string-inflection" :fetcher github
                              :repo "akicho8/string-inflection" :files
                              ("*.el" "*.el.in" "dir" "*.info"
                               "*.texi" "*.texinfo" "doc/dir"
                               "doc/*.info" "doc/*.texi"
                               "doc/*.texinfo" "lisp/*.el" "docs/dir"
                               "docs/*.info" "docs/*.texi"
                               "docs/*.texinfo"
                               (:exclude ".dir-locals.el" "test.el"
                                         "tests.el" "*-test.el"
                                         "*-tests.el" "LICENSE"
                                         "README*" "*-pkg.el"))
                              :source "elpaca-menu-lock-file"
                              :protocol https :inherit t :depth
                              treeless :ref
                              "4a2f87d7b47f5efe702a78f8a40a98df36eeba13"
                              :id string-inflection :type git))
 (stylus-mode :source "elpaca-menu-lock-file" :recipe
              (:package "stylus-mode" :fetcher github :repo
                        "brianc/jade-mode" :files ("stylus-mode.el")
                        :source "elpaca-menu-lock-file" :protocol
                        https :inherit t :depth treeless :ref
                        "1ad7c51f3c6a6ae64550d9510c5e4e8470014375" :id
                        stylus-mode :type git))
 (swift-mode :source "elpaca-menu-lock-file" :recipe
             (:package "swift-mode" :repo "swift-emacs/swift-mode"
                       :fetcher github :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                        "*.texinfo" "doc/dir" "doc/*.info"
                        "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                        "docs/dir" "docs/*.info" "docs/*.texi"
                        "docs/*.texinfo"
                        (:exclude ".dir-locals.el" "test.el"
                                  "tests.el" "*-test.el" "*-tests.el"
                                  "LICENSE" "README*" "*-pkg.el"))
                       :source "elpaca-menu-lock-file" :protocol https
                       :inherit t :depth treeless :ref
                       "cfae3b85ad09bd293df941261afbc21e41bbb5f8" :id
                       swift-mode :type git))
 (tablist :source "elpaca-menu-lock-file" :recipe
          (:package "tablist" :fetcher github :repo
                    "emacsorphanage/tablist" :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                     "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                     "doc/*.texinfo" "lisp/*.el" "docs/dir"
                     "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el"
                               "*-test.el" "*-tests.el" "LICENSE"
                               "README*" "*-pkg.el"))
                    :source "elpaca-menu-lock-file" :protocol https
                    :inherit t :depth treeless :ref
                    "fcd37147121fabdf003a70279cf86fbe08cfac6f" :id
                    tablist :type git))
 (textile-mode :source "elpaca-menu-lock-file" :recipe
               (:package "textile-mode" :repo "juba/textile-mode"
                         :fetcher github :files
                         ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                          "*.texinfo" "doc/dir" "doc/*.info"
                          "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                          "docs/dir" "docs/*.info" "docs/*.texi"
                          "docs/*.texinfo"
                          (:exclude ".dir-locals.el" "test.el"
                                    "tests.el" "*-test.el"
                                    "*-tests.el" "LICENSE" "README*"
                                    "*-pkg.el"))
                         :source "elpaca-menu-lock-file" :protocol
                         https :inherit t :depth treeless :ref
                         "2ad90cb6ed2560ee147417a3ec65222cc2ad33f1"
                         :id textile-mode :type git))
 (thrift :source "elpaca-menu-lock-file" :recipe
         (:package "thrift" :fetcher github :repo "facebook/fbthrift"
                   :files ("thrift/contrib/thrift.el") :source
                   "elpaca-menu-lock-file" :protocol https :inherit t
                   :depth treeless :ref
                   "4022aedee5acd37660c37faf125ce5b4ec7f44ae" :id
                   thrift :type git))
 (transient :source "elpaca-menu-lock-file" :recipe
            (:package "transient" :fetcher github :repo
                      "magit/transient" :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                       "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                       "doc/*.texinfo" "lisp/*.el" "docs/dir"
                       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE"
                                 "README*" "*-pkg.el"))
                      :source "elpaca-menu-lock-file" :protocol https
                      :inherit t :depth treeless :wait t :ref
                      "66be2fa4c80577646c549bfc592deddbb97c1b7a" :id
                      transient :type git))
 (treesit-auto :source "elpaca-menu-lock-file" :recipe
               (:package "treesit-auto" :fetcher github :repo
                         "renzmann/treesit-auto" :files
                         ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                          "*.texinfo" "doc/dir" "doc/*.info"
                          "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                          "docs/dir" "docs/*.info" "docs/*.texi"
                          "docs/*.texinfo"
                          (:exclude ".dir-locals.el" "test.el"
                                    "tests.el" "*-test.el"
                                    "*-tests.el" "LICENSE" "README*"
                                    "*-pkg.el"))
                         :source "elpaca-menu-lock-file" :protocol
                         https :inherit t :depth treeless :ref
                         "016bd286a1ba4628f833a626f8b9d497882ecdf3"
                         :id treesit-auto :type git))
 (ts :source "elpaca-menu-lock-file" :recipe
     (:package "ts" :fetcher github :repo "alphapapa/ts.el" :files
               ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                "docs/*.texinfo"
                (:exclude ".dir-locals.el" "test.el" "tests.el"
                          "*-test.el" "*-tests.el" "LICENSE" "README*"
                          "*-pkg.el"))
               :source "elpaca-menu-lock-file" :protocol https
               :inherit t :depth treeless :ref
               "552936017cfdec89f7fc20c254ae6b37c3f22c5b" :id ts :type
               git))
 (tuareg :source "elpaca-menu-lock-file" :recipe
         (:package "tuareg" :fetcher github :repo "ocaml/tuareg"
                   :files
                   ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                    "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                    "doc/*.texinfo" "lisp/*.el" "docs/dir"
                    "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                    (:exclude ".dir-locals.el" "test.el" "tests.el"
                              "*-test.el" "*-tests.el" "LICENSE"
                              "README*" "*-pkg.el"))
                   :source "elpaca-menu-lock-file" :protocol https
                   :inherit t :depth treeless :ref
                   "f0cb55f2177f6fc978d98d018910fe5b1890fe0c" :id
                   tuareg :type git))
 (typescript-mode :source "elpaca-menu-lock-file" :recipe
                  (:package "typescript-mode" :fetcher github :repo
                            "emacs-typescript/typescript.el" :files
                            ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                             "*.texinfo" "doc/dir" "doc/*.info"
                             "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                             "docs/dir" "docs/*.info" "docs/*.texi"
                             "docs/*.texinfo"
                             (:exclude ".dir-locals.el" "test.el"
                                       "tests.el" "*-test.el"
                                       "*-tests.el" "LICENSE"
                                       "README*" "*-pkg.el"))
                            :source "elpaca-menu-lock-file" :protocol
                            https :inherit t :depth treeless :ref
                            "2535780bdb318d86761b9bd21b0347ca6a89628f"
                            :id typescript-mode :type git))
 (ultra-scroll :source "elpaca-menu-lock-file" :recipe
               (:package "ultra-scroll" :fetcher github :repo
                         "jdtsmith/ultra-scroll" :files
                         ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                          "*.texinfo" "doc/dir" "doc/*.info"
                          "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                          "docs/dir" "docs/*.info" "docs/*.texi"
                          "docs/*.texinfo"
                          (:exclude ".dir-locals.el" "test.el"
                                    "tests.el" "*-test.el"
                                    "*-tests.el" "LICENSE" "README*"
                                    "*-pkg.el"))
                         :source "elpaca-menu-lock-file" :protocol
                         https :inherit t :depth treeless :ref
                         "08758c6772c5fbce54fb74fb5cce080b6425c6ce"
                         :id ultra-scroll :type git))
 (undo-tree :source "elpaca-menu-lock-file" :recipe
            (:package "undo-tree" :repo
                      ("https://gitlab.com/tsc25/undo-tree"
                       . "undo-tree")
                      :files ("*" (:exclude ".git")) :source
                      "elpaca-menu-lock-file" :protocol https :inherit
                      t :depth treeless :ref
                      "2bf5e230f1d11df7bbd9d8c722749e34482bc458" :id
                      undo-tree :type git))
 (vertico :source "elpaca-menu-lock-file" :recipe
          (:package "vertico" :repo "minad/vertico" :files
                    (:defaults "extensions/vertico-*.el") :fetcher
                    github :source "elpaca-menu-lock-file" :protocol
                    https :inherit t :depth treeless :ref
                    "f4e54a2cc26a69e5916b9341a16455939519dd74" :id
                    vertico :type git))
 (visual-fill-column :source "elpaca-menu-lock-file" :recipe
                     (:package "visual-fill-column" :fetcher codeberg
                               :repo "joostkremers/visual-fill-column"
                               :files
                               ("*.el" "*.el.in" "dir" "*.info"
                                "*.texi" "*.texinfo" "doc/dir"
                                "doc/*.info" "doc/*.texi"
                                "doc/*.texinfo" "lisp/*.el" "docs/dir"
                                "docs/*.info" "docs/*.texi"
                                "docs/*.texinfo"
                                (:exclude ".dir-locals.el" "test.el"
                                          "tests.el" "*-test.el"
                                          "*-tests.el" "LICENSE"
                                          "README*" "*-pkg.el"))
                               :source "elpaca-menu-lock-file"
                               :protocol https :inherit t :depth
                               treeless :ref
                               "e1be9a1545157d24454d950c0ac79553c540edb7"
                               :id visual-fill-column :type git))
 (vterm :source "elpaca-menu-lock-file" :recipe
        (:package "vterm" :fetcher github :repo
                  "akermu/emacs-libvterm" :files
                  ("CMakeLists.txt" "elisp.c" "elisp.h"
                   "emacs-module.h" "etc" "utf8.c" "utf8.h" "vterm.el"
                   "vterm-module.c" "vterm-module.h")
                  :source "elpaca-menu-lock-file" :protocol https
                  :inherit t :depth treeless :ref
                  "a01a2894a1c1e81a39527835a9169e35b7ec5dec" :id vterm
                  :type git))
 (web-mode :source "elpaca-menu-lock-file" :recipe
           (:package "web-mode" :repo "fxbois/web-mode" :fetcher
                     github :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                      "doc/*.texinfo" "lisp/*.el" "docs/dir"
                      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE"
                                "README*" "*-pkg.el"))
                     :source "elpaca-menu-lock-file" :protocol https
                     :inherit t :depth treeless :ref
                     "1e7694aee87722f9e51b6e39c35d175d83a1fb2c" :id
                     web-mode :type git))
 (wgrep :source "elpaca-menu-lock-file" :recipe
        (:package "wgrep" :fetcher github :repo
                  "mhayashi1120/Emacs-wgrep" :files ("wgrep.el")
                  :source "elpaca-menu-lock-file" :protocol https
                  :inherit t :depth treeless :ref
                  "49f09ab9b706d2312cab1199e1eeb1bcd3f27f6f" :id wgrep
                  :type git))
 (which-key :source "elpaca-menu-lock-file" :recipe
            (:package "which-key" :repo "justbur/emacs-which-key"
                      :fetcher github :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                       "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                       "doc/*.texinfo" "lisp/*.el" "docs/dir"
                       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE"
                                 "README*" "*-pkg.el"))
                      :source "elpaca-menu-lock-file" :protocol https
                      :inherit t :depth treeless :ref
                      "38d4308d1143b61e4004b6e7a940686784e51500" :id
                      which-key :type git))
 (with-editor :source "elpaca-menu-lock-file" :recipe
              (:package "with-editor" :fetcher github :repo
                        "magit/with-editor" :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                         "*.texinfo" "doc/dir" "doc/*.info"
                         "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                         "docs/dir" "docs/*.info" "docs/*.texi"
                         "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el"
                                   "tests.el" "*-test.el" "*-tests.el"
                                   "LICENSE" "README*" "*-pkg.el"))
                        :source "elpaca-menu-lock-file" :protocol
                        https :inherit t :depth treeless :ref
                        "902b4d572af2c2f36060da01e3c33d194cdec32b" :id
                        with-editor :type git))
 (writeroom-mode :source "elpaca-menu-lock-file" :recipe
                 (:package "writeroom-mode" :fetcher github :repo
                           "joostkremers/writeroom-mode" :files
                           ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                            "*.texinfo" "doc/dir" "doc/*.info"
                            "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                            "docs/dir" "docs/*.info" "docs/*.texi"
                            "docs/*.texinfo"
                            (:exclude ".dir-locals.el" "test.el"
                                      "tests.el" "*-test.el"
                                      "*-tests.el" "LICENSE" "README*"
                                      "*-pkg.el"))
                           :source "elpaca-menu-lock-file" :protocol
                           https :inherit t :depth treeless :ref
                           "cca2b4b3cfcfea1919e1870519d79ed1a69aa5e2"
                           :id writeroom-mode :type git))
 (yaml-mode :source "elpaca-menu-lock-file" :recipe
            (:package "yaml-mode" :repo "yoshiki/yaml-mode" :fetcher
                      github :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                       "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                       "doc/*.texinfo" "lisp/*.el" "docs/dir"
                       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE"
                                 "README*" "*-pkg.el"))
                      :source "elpaca-menu-lock-file" :protocol https
                      :inherit t :depth treeless :ref
                      "d91f878729312a6beed77e6637c60497c5786efa" :id
                      yaml-mode :type git))
 (yari :source "elpaca-menu-lock-file" :recipe
       (:package "yari" :repo "hron/yari.el" :fetcher github :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                  "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                  "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                  "docs/*.texinfo"
                  (:exclude ".dir-locals.el" "test.el" "tests.el"
                            "*-test.el" "*-tests.el" "LICENSE"
                            "README*" "*-pkg.el"))
                 :source "elpaca-menu-lock-file" :protocol https
                 :inherit t :depth treeless :ref
                 "de61285ceb21f56c29f4be12e2e65b2aa2bccf56" :id yari
                 :type git))
 (yasnippet :source "elpaca-menu-lock-file" :recipe
            (:package "yasnippet" :repo "joaotavora/yasnippet"
                      :fetcher github :files
                      ("yasnippet.el" "snippets") :source
                      "elpaca-menu-lock-file" :protocol https :inherit
                      t :depth treeless :ref
                      "c1e6ff23e9af16b856c88dfaab9d3ad7b746ad37" :id
                      yasnippet :type git)))
