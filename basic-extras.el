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

;; linum (migrated to display-line-numbers elsewhere) ------------------------
;; Intentionally removed; line numbers are configured in `visual.el` using
;; `display-line-numbers-mode`.

;; Highlight current line
(global-hl-line-mode +1)

;; typopunct ----------------------------------------------------------------------

(add-to-list 'load-path (concat emacs-dir "/site-lisp/typopunct"))
(require 'typopunct)

(provide 'basic-extras)
