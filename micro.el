;; Load path settings
(setq emacs-dir "~/src/mine/emacs.d")
(add-to-list 'load-path emacs-dir)
(require 'here-env)

;; Basic environment ----------------------------------------------------------

(require 'functions)
(require 'base)
(if (eq system-type 'darwin) (osx-support) )
(require 'visual)

(require 'my-package-conf)
(require 'my-python)

;; Host specific stuff - should always have the last word ---------------------

(setq yas-snippet-dirs
      '("~/src/mine/emacs.d/snippet"  ;; personal snippets
        ))

(require 'warnings)
(add-to-list 'warning-suppress-types '(yasnippet backquote-change))

;; linum ----------------------------------------------------------------------

;; Line numbers to the left
(require 'linum)
(global-linum-mode t)
(setq linum-format "%d ")

(custom-set-faces
 '(linum ((t
           (:inherit (shadow default)
                     :foreground "dark orange"
                     :background "black"
                     ))) ))
