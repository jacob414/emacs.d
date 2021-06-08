;; Basic environment ----------------------------------------------------------

(setq emacs-dir "~/src/mine/emacs.d")
(add-to-list 'load-path emacs-dir)
(add-to-list 'load-path (concat emacs-dir "/site-lisp"))

(require 'here-env)
(require 'visual)
(require 'basic-extras)

(require 'base)
(require 'functions)
(if (eq system-type 'darwin) (osx-support) )
(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x ?") 'magit-diff-buffer-file)

(global-set-key (kbd "C-c y") 'yas/reload-all)

;; rg

(require 'rg)

;; White-space trim
(require 'ws-trim)
(setq-default ws-trim-level 1)
(global-ws-trim-mode t)

;; Load Par Edit --------------------------------------------------------------

(require 'paredit)

;; Persistent overlays --------------------------------------------------------

(add-to-list 'load-path (concat emacs-dir "/site-lisp/persistent-overlay"))
(load-library "persistent-overlays")

;; Use multiple-cursors -------------------------------------------------------

(require 'multiple-cursors)
(global-set-key (kbd "M-<SPC>") 'mc/edit-lines)
(global-set-key (kbd "M-p") 'mc/mark-previous-like-this)
(global-set-key (kbd "M-n") 'mc/mark-next-lines)
(global-set-key (kbd "M-n") 'mc/mark-next-like-this)

;; Scheme settings ------------------------------------------------------------

(require 'xscheme)

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
  (local-set-key (kbd "C-x C-m") 'web-mode-fold-or-unfold)
  (hs-minor-mode) )

(add-hook 'web-mode-hook  'my-web-mode-hook)

(setq auto-mode-alist
      (append '(("\\.json$" . web-mode)) auto-mode-alist))

;; elm-mode settings  ---------------------------------------------------------


(require 'elm-mode)

;; elixir-mode settings  ------------------------------------------------------

(unless (package-installed-p 'elixir-mode)
  (package-install 'elixir-mode))
;; swift-mode settings  -------------------------------------------------------

(unless (package-installed-p 'swift-mode)
  (package-install 'swift-mode))
(add-hook 'swift-mode-hook (lambda () (subword-mode 1)))

;; Python settings ------------------------------------------------------------

(require 'my-python)

;; Objective C settings -------------------------------------------------------

(add-hook 'objc-mode-hook (lambda () (subword-mode 1)))

;; Apache settings ------------------------------------------------------------

(autoload 'apache-mode "apache-mode" "Apache config edit mode." t)

(setq auto-mode-alist
      (append '(("\\.htaccess$" . apache-mode)) auto-mode-alist))

;; Shell settings -------------------------------------------------------------

(setq auto-mode-alist
      (append '(("bash-fc-*" . sh-mode)) auto-mode-alist))


;; Markdown settings ----------------------------------------------------------

(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)

(setq auto-mode-alist
      (append '(("\\.md$" . markdown-mode)) auto-mode-alist))

;; .srt -settings (subtitle files) --------------------------------------------

(setq auto-mode-alist
      (append '(("\\.srt$" . srt-mode))
              auto-mode-alist))

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
      (append '(("\\.php$" . web-mode)) auto-mode-alist))
;; YAM-mode ------------------------------------------------------------------

(require 'yaml-mode)
(setq auto-mode-alist
      (append '(("\\.yml$" . yaml-mode)
                ("\\.yaml$" . yaml-mode)) auto-mode-alist))

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

(require 'wc-mode)

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

(unless (package-installed-p 'elixir-mode)
  (package-install 'langtool))

(require 'langtool)

;; CSV file configuration -----------------------------------------------------

(require 'csv-mode)

;; Tuareg (OCaml) configuration -----------------------------------------------

(load "~/src/ext/ocaml/tuareg/tuareg-site-file")
;; org-mode settings  ---------------------------------------------------------

(require 'my-org)

;; expand-region --------------------------------------------------------------

(require 'expand-region)

;; dockerfile-mode4 -----------------------------------------------------------

(require 'dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

;; prog-fill ------------------------------------------------------------------

(require 'prog-fill)

;; Custom ---------------------------------------------------------------------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(case-fold-search t)
 '(company-idle-delay 0.05)
 '(css-indent-offset 2)
 '(desktop-path '("~/src/tmp/emacs-desktop"))
 '(elm-format-command "/usr/local/bin/elm-format" t)
 '(elm-format-on-save 't)
 '(elm-interactive-command "/usr/local/bin/elm-repl")
 '(elpy-modules
   '(elpy-module-company elpy-module-eldoc elpy-module-flymake elpy-module-pyvenv elpy-module-highlight-indentation elpy-module-yasnippet elpy-module-sane-defaults))
 '(elpy-rpc-python-command "/Users/jacob/opt/plus/py/bin/python")
 '(elpy-rpc-virtualenv-path "/Users/jacob/opt/plus/py/bin/python")
 '(elpy-syntax-check-command "~/opt/plus/py/bin/mypy")
 '(elpy-test-discover-runner-command '("python-shell-interpreter" "-m" "pytest"))
 '(elpy-test-pytest-runner-command '("/Users/jacob/opt/plus/py/bin/pytest"))
 '(elpy-test-runner 'elpy-test-pytest-runner)
 '(js-indent-level 2)
 '(make-backup-files nil)
 '(nginx-indent-level 2)
 '(nodejs-repl-command "/usr/local/bin/node")
 '(package-selected-packages
   '(transient rg bnf-mode writeroom-mode lsp-mode flycheck-mypy flymake-mypy use-package elpy dismal csv csv-mode ox-md langtool writegood-mode expand-region flymake-cursor pymacs drag-stuff highlight-current-line bookmark+ applescript-mode ein-loaddefs "ein" ein swift-mode elixir-mode))
 '(python-check-command '("/Users/jacob/opt/plus/py/bin/mypy"))
 '(python-shell-interpreter "/Users/jacob/opt/plus/py/bin/python")
 '(rst-level-face-base-light 38)
 '(safe-local-variable-values '((encoding . utf-8)))
 '(solarized-contrast 'normal)
 '(swift-mode:basic-offset 2)
 '(uniquify-buffer-name-style 'forward nil (uniquify))
 '(yas/field-highlight-face ((t (:background "gray35" :underline t))))
 '(yas/trigger-key "SPC"))

;; desktop --------------------------------------------------------------------

(require 'desktop)
(desktop-save-mode 1)

;; Host specific stuff - should always have the last word ---------------------

(cond
 ((equal (string-match "superfly" system-name) 0)
  (require 'superfly))

 ((equal system-name "medea")
  (require 'medea))

 ((string/starts-with system-name "goldskip")
  (require 'goldskip))

 ((string/starts-with system-name "cecilia")
  (require 'cecilia))

 ((string/starts-with system-name "sugarline")
  (require 'sugarline))

 ((equal system-name "stevie.local")
  (require 'stevie))

 (t
  (require 'generic)) )

;; Load my own keybindings (last to win) --------------------------------------

(require 'custom-keybindings)
