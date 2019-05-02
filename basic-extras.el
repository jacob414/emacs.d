;; ELPA -----------------------------------------------------------------------

(when modern
  (require 'package)
  (setq package-user-dir (concat "~/src/mine/elpa.d"))
  (setq source '(("melpa" . "http://melpa.milkbox.net/packages/")
                ("marmalade" . "http://marmalade-repo.org/packages/")
                ("elpa" . "http://tromey.com/elpa/")))
  (add-to-list 'package-archives source t)
  (package-initialize) )

(unless (package-installed-p 'exec-path-from-shell)
  (package-install 'exec-path-from-shell))

(exec-path-from-shell-initialize)

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
