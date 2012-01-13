;; Load path settings
(setq load-path (cons "~/src/mine/dotfiles/emacs" load-path))
(setq load-path (cons "~/src/mine/dotfiles/emacs/site-lisp" load-path))
(setq load-path (cons "~/src/mine/dotfiles/emacs/hosts" load-path))

;; Basic environment ----------------------------------------------------------

(require 'baseenv)

;; Always indent
(global-set-key (kbd "C-m") 'newline-and-indent)

;; Custom key bindings
(global-set-key (kbd "C-<tab>") 'dabbrev-expand)
(global-set-key (kbd "C-z") 'goto-line)
(global-set-key (kbd "C-(") 'insert-parentheses)
(global-set-key (kbd "C-c i") 'indent-region)
(global-set-key (kbd "<f5>") 'eval-region)
(global-set-key (kbd "C-c p") 'query-replace)
(global-set-key (kbd "C-c a") 'mark-whole-buffer)
(global-set-key (kbd "C-c f") 'rgrep)
(global-set-key (kbd "C-c o") 'occur)
(global-set-key (kbd "C-c k") 'kill-whole-line)
(global-set-key (kbd "C-c m t") 'ws-trim-mode)
(global-set-key (kbd "C-c m l") 'longlines-mode)
(global-set-key (kbd "C-x <SPC>") 'whitespace-mode)
(global-set-key (kbd "C-c T") 'ws-trim-buffer)
(global-set-key (kbd "C-c T") 'ws-trim-buffer)

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

;; YASnippet
(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/src/emacs.d/snippet")

(global-set-key (kbd "C-c y") 'yas/reload-all)

;; White-space trim
(require 'ws-trim)
(setq-default ws-trim-level 1)
(global-ws-trim-mode t)

;; Load custom functions ------------------------------------------------------
(require 'functions)
(if (eq system-type 'darwin) (osx-support) )

;; Javascript settings --------------------------------------------------------

(add-hook 'js-mode-hook
          '(lambda ()
             (interactive)
             (highlight-lines-matching-regexp "debugger" 'hi-red-b)
             (highlight-lines-matching-regexp "console\.log" 'hi-red-b)
             (highlight-lines-matching-regexp "alert\(" 'hi-red-b)
             )
          )

(setq auto-mode-alist
      (append '(("\\.json$" . js-mode)) auto-mode-alist))

;; Python settings ------------------------------------------------------------

(add-hook 'python-mode-hook
          '(lambda ()
             (interactive)
             (highlight-lines-matching-regexp ".set_trace" 'hi-red-b)
             (set 'python-indent 4)
             )
          )

(setq auto-mode-alist
      (append '(("\\.wsgi$" . python-mode)) auto-mode-alist))

;; CoffeScript settings -------------------------------------------------------

(autoload 'coffee-mode "coffee-mode" "CoffeScript editing mode." t)

(setq auto-mode-alist
      (append '(("\\.coffee$" . coffee-mode)) auto-mode-alist))

(add-hook 'coffee-mode-hook
          '(lambda()
             (interactive)
             (set (make-local-variable 'tab-width) 2) )
          )

;; Markdown settings ----------------------------------------------------------

(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)

(setq auto-mode-alist
      (append '(("\\.md$" . markdown-mode)) auto-mode-alist))

;; Ruby settings --------------------------------------------------------------

(setq auto-mode-alist
      (append '(("Rakefile" . ruby-mode)) auto-mode-alist))

(add-hook 'ruby-mode-hook
          '(lambda ()
             (local-set-key "\r" 'newline-and-indent)
             (local-set-key (kbd "C-j") 'my-greedy-joinlines) )
          )

;; YAML-mode ------------------------------------------------------------------

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

;; Text-mode ------------------------------------------------------------------

(require 'wc)

(defun text-env ()
  (interactive)
  (local-set-key (kbd "C-c m") 'oe-commit-message)
  (local-set-key (kbd "<f7>") 'ispell)
  (local-set-key (kbd "C-c s s") 'my-ispell-use-sv)
  (local-set-key (kbd "C-c s e") 'my-ispell-use-en)
  (local-set-key (kbd "C-c c") 'ispell-complete-word)
  (wiki-mode)
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

;; Custom ---------------------------------------------------------------------

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
  '(case-fold-search t)
  '(js-indent-level 2)
  '(make-backup-files nil)
  '(rst-level-face-base-light 38)
  '(safe-local-variable-values (quote ((encoding . utf-8))))
  '(uniquify-buffer-name-style (quote forward) nil (uniquify))
  '(yas/field-highlight-face ((t (:background "gray35" :underline t))))
  '(yas/trigger-key "SPC")
)

;; Host specific stuff - should always have the last word ---------------------

(cond
 ((equal (string-match "superfly" system-name) 0)
  (require 'superfly))

 ((equal system-name "medea")
  (require 'medea))

 ((equal system-name "stevie.local")
  (require 'stevie))

 (t
  (require 'generic)) )
