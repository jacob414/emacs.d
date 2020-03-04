;; Common libraries -----------------------------------------------------------

(require 'let-alist)
(require 's)
(require 'f)

;; ELPA -----------------------------------------------------------------------

(when modern
  (require 'package)
  (setq package-user-dir (concat "~/src/mine/elpa.d"))
  (setq proto "https")
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  (add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (add-to-list 'package-archives (cons "marmalade" "http://melpa.org/packages/") t)
  (package-initialize)
  (setq package-check-signature nil)
  )

;; linum ----------------------------------------------------------------------

;; Line numbers to the left
(require 'linum)
(global-linum-mode t)

;; Jump buffers with M-<arrows>
(require 'windmove)
(windmove-default-keybindings 'meta)

;; Highlight current line
(require 'highlight-current-line)
(highlight-current-line-on 1)

(provide 'basic-extras)
