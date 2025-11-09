;; Terminal-first init that mirrors the full GUI stack
;; Use this with: emacs -nw -q -l ~/src/mine/emacs.d/cli-ui.el

;; Load path settings
(setq emacs-dir "~/src/mine/emacs.d")
(add-to-list 'load-path emacs-dir)
(add-to-list 'load-path (concat emacs-dir "/site-lisp"))
(add-to-list 'load-path (concat emacs-dir "/hosts"))

;; Basic environment and paths
(require 'here-env)                   ;; recentf, site-lisp projects, host paths
(setq create-lockfiles nil)
(menu-bar-mode -1)

;; Packages and dependencies (match GUI boot)
;; Keep package files alongside GUI config
(require 'package)
(setq package-user-dir (expand-file-name "~/src/mine/elpa.d/"))
(setq package-quickstart-file (expand-file-name "package-quickstart.el" package-user-dir))
(setq package-enable-at-startup t
      package-quickstart t)
(require 'deps)
(my/check-deps-on-startup-maybe-install)

;; Core modules
(require 'base)         ;; UI basics, yasnippet setup helper
(require 'functions)    ;; utility functions (incl. osx-support)
(require 'visual)       ;; theme functions (my-zenburn, etc.)

;; Apply the same theme as GUI immediately (will also be re-applied by late-config hook)
(when (fboundp 'my-zenburn) (my-zenburn))

;; Extended configuration (file associations, language env, host configs, etc.)
(require 'late-config)

;; Global keybindings
(require 'custom-keybindings)

;; Load Custom settings like GUI init does
(let ((custom-file (locate-user-emacs-file "custom.el")))
  (when (and custom-file (file-readable-p custom-file))
    (load custom-file 'noerror 'nomessage)))
