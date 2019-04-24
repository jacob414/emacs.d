(require 'here-env)

;; Basic environment ----------------------------------------------------------

(require 'functions)
(require 'base)
(if (eq system-type 'darwin) (osx-support) )

(add-to-list 'load-path "~/src/ext/emacs/spacemacs-theme")
(require 'spacemacs-dark-theme)

;; (require 'zenburn-theme)

;; Host specific stuff - should always have the last word ---------------------

(setq yas-snippet-dirs
      '("~/src/mine/emacs.d/snippet"  ;; personal snippets
        ))
(require 'yasnippet)
(yas/global-mode)
(define-key yas-minor-mode-map (kbd "SPC") yas-maybe-expand)

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
