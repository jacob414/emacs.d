;; Are we 'modern'?
(setq modern (>= emacs-major-version 23))

;; Load path settings
(setq emacs-dir "~/src/mine/emacs.d")

(add-to-list 'load-path emacs-dir)
(add-to-list 'load-path (concat emacs-dir "/site-lisp"))
(add-to-list 'load-path (concat emacs-dir "/site-lisp/zenburn"))
(add-to-list 'load-path (concat emacs-dir "/site-lisp/expand-region"))
(add-to-list 'load-path (concat emacs-dir "/hosts"))

(dolist
    (project (directory-files (concat emacs-dir "/site-lisp") t "\\w+"))
  (when (file-directory-p project)
    (add-to-list 'load-path project)))

;; Basic environment ----------------------------------------------------------

(require 'base)

;; ELPA -----------------------------------------------------------------------

(when modern
  (require 'package)
  (setq package-user-dir (concat "~/src/mine/elpa.d"))
  (dolist (source '(("melpa" . "http://melpa.milkbox.net/packages/")
                    ("marmalade" . "http://marmalade-repo.org/packages/")
                    ("elpa" . "http://tromey.com/elpa/")))
  (add-to-list 'package-archives source t))
  (package-initialize) )

;; linum ----------------------------------------------------------------------

;; Line numbers to the left
(require 'linum)
(global-linum-mode t)

;; Jump buffers with M-<arrows>
(require 'windmove)
(windmove-default-keybindings 'meta)

;; Desktop mode & bookmarks
(require 'bookmark+)
(require 'desktop)
(desktop-save-mode 1)

;; Highlight current line
(require 'highlight-current-line)
(highlight-current-line-on 1)

;; Drag stuff
(require 'drag-stuff)
(drag-stuff-mode t)

(global-set-key (kbd "C-c y") 'yas/reload-all)

;; White-space trim
(require 'ws-trim)
(setq-default ws-trim-level 1)
(global-ws-trim-mode t)

;; Load Par Edit --------------------------------------------------------------

(require 'paredit)

;; Scheme settings ------------------------------------------------------------

(require 'xscheme)

;; Load custom functions ------------------------------------------------------
(require 'functions)
(if (eq system-type 'darwin) (osx-support) )

;; web-mode -------------------------------------------------------------------

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode)) ;; Override?
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))

(setq web-mode-content-types-alist
      '(("jsx"  . "/Users/jacob/src/lab/miveo/roc-0/.*\\.js[x]?\\'")
        ("jsx"  . "/Users/jacob/src/lab/cra-pre/.*\\.js[x]?\\'")
        ("jsx"  . "/Users/jacob/src/oss/cra-0/.*\\.js[x]?\\'")
        ("jsx"  . "/Users/jacob/src/miveo/.*\\.js[x]?\\'")
        ))

(defun my-web-mode-hook ()
  "web-mode settings"
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (local-set-key (kbd "C-x f") 'web-mode-fold-or-unfold) )
(add-hook 'web-mode-hook  'my-web-mode-hook)

(setq auto-mode-alist
      (append '(("\\.json$" . web-mode)) auto-mode-alist))

;; Javascript settings --------------------------------------------------------

(add-hook 'js-mode-hook
          '(lambda ()
             (interactive)
             (highlight-lines-matching-regexp "debugger" 'hi-red-b)
             (highlight-lines-matching-regexp "console\.log" 'hi-red-b)
             (highlight-lines-matching-regexp "alert\(" 'hi-red-b)
             )
          )

;; Python settings ------------------------------------------------------------

(add-hook 'python-mode-hook
          '(lambda ()
             (interactive)
             (highlight-lines-matching-regexp ".set_trace" 'hi-red-b)
             (set 'python-indent 4)
             )
          )

(setq auto-mode-alist
      (append '(("\\.wsgi$" . python-mode)
                ("\\.pyx$" . python-mode)) auto-mode-alist))

;; CoffeScript settings -------------------------------------------------------

(autoload 'coffee-mode "coffee-mode" "CoffeScript editing mode." t)

(defun cs-compile-and-run ()
  (interactive)
  (save-buffer)
  (coffe-compile-buffer) )

(setq auto-mode-alist
      (append '(("\\.coffee$" . coffee-mode)) auto-mode-alist))

(add-hook 'coffee-mode-hook
          '(lambda()
             (interactive)
             (local-set-key (kbd "s-r") 'cs-compile-and-run)
             (set (make-local-variable 'tab-width) 2) )
          )

;; Objective C settings -------------------------------------------------------

(add-hook 'objc-mode-hook (lambda () (subword-mode 1)))

;; Apache settings ------------------------------------------------------------

(autoload 'apache-mode "apache-mode" "Apache config edit mode." t)

(setq auto-mode-alist
      (append '(("\\.htaccess$" . apache-mode)) auto-mode-alist))

;; Lisp settings --------------------------------------------------------------

(setq auto-mode-alist
      (append '(("\\.nu$" . lisp-mode)) auto-mode-alist))

;; Markdown settings ----------------------------------------------------------

(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)

(setq auto-mode-alist
      (append '(("\\.md$" . markdown-mode)) auto-mode-alist))

;; Ruby settings --------------------------------------------------------------

(setq auto-mode-alist
      (append '(("\\.po$" . ruby-mode)       ;; until I get po-mode sorted

                ("Podfile" . ruby-mode)      ;; Cocoapods
                ("\\.podspec$" . ruby-mode)  ;; Cocoapods

                ("Gemfile" . ruby-mode)      ;; Bundler (for Heroku et al)
                ("\\.ru$" . ruby-mode)        ;; Rack apps

                ("Rakefile" . ruby-mode)) auto-mode-alist))

(add-hook 'ruby-mode-hook
          '(lambda ()
             (local-set-key "\r" 'newline-and-indent)
             (local-set-key (kbd "C-j") 'my-greedy-joinlines) )
          )

;; PHP settings ---------------------------------------------------------------

(setq auto-mode-alist
      ;; for now, save installation work
      (append '(("\\.php$" . java-mode)) auto-mode-alist))

;; YAM-mode ------------------------------------------------------------------

(require 'yaml-mode)

(setq auto-mode-alist
      (append '(("\\.yml$" . yaml-mode)
                ("\\.yaml$" . yaml-mode)) auto-mode-alist))

;; SASS -----------------------------------------------------------------------

(setq auto-mode-alist
      (append '(("\\.sass$" . sass-mode)) auto-mode-alist))

(require 'scss-mode)

(setq auto-mode-alist
      (append '(("\\.scss$" . scss-mode)) auto-mode-alist))

(add-hook 'scss-mode-hook
          '(lambda ()
             (setq scss-compile-at-save nil) ) )

;; Nix expressions ------------------------------------------------------------

(autoload 'nix-mode "nix-mode"
  "Major mode for editing NGINX configuration files" t)

(setq auto-mode-alist
      (append '(("\\.nix$" . nix-mode)) auto-mode-alist))

;; Nginx configuration --------------------------------------------------------

(autoload 'nginx-mode "nginx-mode"
  "Major mode for editing NGINX configuration files" t)

(setq auto-mode-alist
      (append '(("\\nginx*.conf$" . conf-mode)) auto-mode-alist))

;; Various configuration ------------------------------------------------------

(setq auto-mode-alist
      ;; for now, save installation work
      (append '(("\\.gitignore$" . conf-mode)) auto-mode-alist))

;; Text-mode ------------------------------------------------------------------

(require 'wc)

(defun text-env ()
  (interactive)
  (local-set-key (kbd "C-c m") 'oe-commit-message)
  (local-set-key (kbd "<f7>") 'ispell)
  (local-set-key (kbd "C-c s s") 'my-ispell-use-sv)
  (local-set-key (kbd "C-c s e") 'my-ispell-use-en)
  (local-set-key (kbd "C-c c") 'ispell-complete-word)
  (local-set-key (kbd "C-c w") 'count-words)
)

(defun text-mode-env ()
  (interactive)
  (turn-on-auto-fill)
  (text-env)
)

(setq auto-mode-alist
      (append '(("\\.tmp$" . text-mode)) auto-mode-alist))

(setq auto-mode-alist
      (append '(("\\.eml$" . mail-mode)) auto-mode-alist))

(add-hook 'text-mode-hook 'text-mode-env)
(add-hook 'mail-mode-hook 'text-env)

;; nodejs-repl ----------------------------------------------------------------

(require 'nodejs-repl)

;; Tuareg (OCaml) configuration -----------------------------------------------

(load "~/src/ext/ocaml/tuareg/tuareg-site-file")

;; org-mode settings  ---------------------------------------------------------

;; http://orgmode.org/manual/Conflicts.html
(add-hook 'org-mode-hook
                    (lambda ()
                      (org-set-local 'yas/trigger-key [tab])
                      (local-set-key (kbd "M-<left>") 'windmove-left)
                      (local-set-key (kbd "M-<right>") 'windmove-right)
                      (local-set-key (kbd "M-<down>") 'windmove-down)
                      (local-set-key (kbd "M-<up>") 'windmove-up)
                      (local-set-key (kbd "C-<tab>") 'dabbrev-expand)
                      (local-set-key (kbd "C-.") 'scroll-down-one-line)
                      (local-set-key (kbd "C-,") 'scroll-up-one-line)
                      (define-key yas/keymap [tab] 'yas/next-field-or-maybe-expand)))

;; expand-region --------------------------------------------------------------

(require 'expand-region)

;; Custom ---------------------------------------------------------------------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(case-fold-search t)
 '(css-indent-offset 2)
 '(js-indent-level 2)
 '(make-backup-files nil)
 '(nginx-indent-level 2)
 '(rst-level-face-base-light 38)
 '(safe-local-variable-values (quote ((encoding . utf-8))))
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(yas/field-highlight-face ((t (:background "gray35" :underline t))))
 '(yas/trigger-key "SPC")
 '(nodejs-repl-command "/usr/local/bin/node"))

;; Host specific stuff - should always have the last word ---------------------

(cond
 ((equal (string-match "superfly" system-name) 0)
  (require 'superfly))

 ((equal system-name "medea")
  (require 'medea))

 ((string/starts-with system-name "goldskip")
  (require 'goldskip))

 ((equal system-name "stevie.local")
  (require 'stevie))

 (t
  (require 'generic)) )

;; Load my own keybindings (last to win) --------------------------------------

(require 'custom-keybindings)
