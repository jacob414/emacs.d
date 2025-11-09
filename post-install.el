;;; Package setup: GNU + MELPA only
(require 'package)

;; Where packages get installed
(setq package-user-dir (expand-file-name "~/src/mine/elpa.d/"))
(make-directory package-user-dir t)

;; Archives (prefer GNU first, keep MELPA second)
(setq package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))
(setq package-archive-priorities '(("gnu" . 10) ("melpa" . 5)))

;; Initialize package.el (Emacs 27+ usually does this earlier; guard just in case)
(unless (bound-and-true-p package--initialized)
  (package-initialize))

;; First-time setup: populate archive contents if empty
(unless package-archive-contents
  (package-refresh-contents))

(message "Refreshing package contents…")
(ignore-errors (package-refresh-contents))

(let ((pkgs '(magit use-package epl auto-complete dash elixir-mode haskell-mode
              exec-path-from-shell highlight-current-line drag-stuff ws-trim
              paredit multiple-cursors web-mode yapfify swift-mode apache-mode
              markdown-mode yaml-mode nix-mode nodejs-repl dockerfile-mode
              prog-fill wc-mode langtool flymake-cursor expand-region
              writegood-mode csv-mode elpy importmagic flycheck-mypy lsp-mode
              writeroom-mode rg persistent-overlays buttercup el-mock
              undercover package-lint denote)))
  (dolist (p pkgs)
    (message "Installing %s…" p)
    (ignore-errors (package-install p))))

