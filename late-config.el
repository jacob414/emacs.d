(global-set-key (kbd "C-x g") #'my/magit-status)
(global-set-key (kbd "C-x ?") #'my/magit-diff-buffer-file)

;; Temporarily disable Magit extras while troubleshooting
;; (with-eval-after-load 'magit
;;   ...
;; )

;; Startup installs are now managed by deps.el.
;; If you need to force-install optional packages manually, call
;; M-: (my/install-missing-packages) or re-enable a hook to it here.
;; (add-hook 'emacs-startup-hook #'my/install-missing-packages)

;; Apply theme after packages have been installed
(add-hook 'emacs-startup-hook #'my-zenburn)

;; ripgrep
(autoload 'rg "rg" nil t)
(autoload 'rg-project "rg" nil t)

;; Multiple cursors
(autoload 'mc/edit-lines "multiple-cursors" nil t)
(autoload 'mc/mark-previous-like-this "multiple-cursors" nil t)
(autoload 'mc/mark-next-like-this "multiple-cursors" nil t)
(global-set-key (kbd "M-<SPC>") #'mc/edit-lines)

(global-set-key (kbd "M-p") #'backward-paragraph)
(global-set-key (kbd "M-n") #'forward-paragraph)

;; Paredit (manual enable in Lisp modes)
(with-eval-after-load 'paredit
  (define-key paredit-mode-map (kbd "C-j") #'join-line))

;; Web-mode + associations
(autoload 'web-mode "web-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.\(js\|jsx\|ts\|tsx\)\\'" . web-mode))
(defun my/web-mode-setup ()
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2)
  (when (fboundp 'hs-minor-mode) (hs-minor-mode 1)))
(add-hook 'web-mode-hook #'my/web-mode-setup)

;; YAML, Dockerfile, Markdown, Apache, Nix
(autoload 'yaml-mode "yaml-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-mode))

(autoload 'dockerfile-mode "dockerfile-mode" nil t)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

(autoload 'markdown-mode "markdown-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; Override markdown-mode's M-p/M-n to use paragraph navigation
(with-eval-after-load 'markdown-mode
  (define-key markdown-mode-map (kbd "M-p") #'backward-paragraph)
  (define-key markdown-mode-map (kbd "M-n") #'forward-paragraph))

(autoload 'apache-mode "apache-mode" "Apache config edit mode." t)
(add-to-list 'auto-mode-alist '("\\.htaccess\\'" . apache-mode))

(autoload 'nix-mode "nix-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-mode))

;; Rust
(autoload 'rust-mode "rust-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

;; NGINX (fix regex to actually match .conf under nginx)
(add-to-list 'auto-mode-alist '("nginx.*\\.conf\\'" . conf-mode))

;; Text and mail helpers
(when (require 'wc-mode nil 'noerror)
  (defun my/text-env ()
    (local-set-key (kbd "C-c m") 'oe-commit-message)
    (local-set-key (kbd "<f7>") 'ispell)
    (local-set-key (kbd "C-c s s") 'my-ispell-use-sv)
    (local-set-key (kbd "C-c s e") 'my-ispell-use-en)
    (local-set-key (kbd "C-c c") 'ispell-complete-word)
    (local-set-key (kbd "C-c w") 'count-words))
  (defun my/text-mode-env ()
    (turn-on-auto-fill)
    (my/text-env))
  (add-hook 'text-mode-hook #'my/text-mode-env)
  (add-hook 'mail-mode-hook #'my/text-env))

;; Python (keep your existing module; can be modernized later)
(ignore-errors (require 'my-python))

;; Org
(ignore-errors (require 'my-org))

(require 'denote)
;; Remember that the website version of this manual shows the latest
;; developments, which may not be available in the package you are
;; using.  Instead of copying from the web site, refer to the version
;; of the documentation that comes with your package.  Evaluate:
;;
;;     (info "(denote) Sample configuration")
(use-package denote
  :ensure t
  :hook (dired-mode . denote-dired-mode)
  :bind
  (("C-c n n" . denote)
   ("C-c n r" . denote-rename-file)
   ("C-c n l" . denote-link)
   ("C-c n b" . denote-backlinks)
   ("C-c n d" . denote-dired)
   ("C-c n g" . denote-grep))
  :config
  (setq denote-directory (expand-file-name "~/src/mine/skunkworks/denote"))

  ;; Automatically rename Denote buffers when opening them so that
  ;; instead of their long file name they have, for example, a literal
  ;; "[D]" followed by the file's title.  Read the doc string of
  ;; `denote-rename-buffer-format' for how to modify this.
  (denote-rename-buffer-mode 1))

;; Desktop sessions (guarded)
(when (require 'desktop nil 'noerror)
  (desktop-save-mode 1)
  ;; If a previous broken session saved buffers as `fundamental-mode',
  ;; try to re-detect modes from filenames after reading the desktop.
  (defun my/reapply-major-modes-if-fundamental ()
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (and buffer-file-name (eq major-mode 'fundamental-mode))
          (ignore-errors (normal-mode t))))))
  (add-hook 'desktop-after-read-hook #'my/reapply-major-modes-if-fundamental)
  (add-hook 'after-init-hook #'my/reapply-major-modes-if-fundamental))

;; Host-specific last-word overrides, guarded
(defun my/load-host (name feature)
  (let* ((file (expand-file-name (concat name ".el") (concat emacs-dir "/hosts"))))
    (when (file-exists-p file)
      (require feature)
      (message "Loaded host config: %s (system-name: %s)" name system-name))))

(cond
 ((string-prefix-p "superfly" system-name) (my/load-host "superfly" 'superfly))
 ((string= system-name "medea")            (my/load-host "medea" 'medea))
 ((string-prefix-p "goldskip" system-name) (my/load-host "goldskip" 'goldskip))
 ((string-prefix-p "cecilia" system-name)  (my/load-host "cecilia" 'cecilia))
 ((string-prefix-p "sugarline" system-name)(my/load-host "sugarline" 'sugarline))
 ((string-prefix-p "skrotnisse" system-name)(my/load-host "skrotnisse" 'skrotnisse))
 ((string= system-name "stevie.local")     (my/load-host "stevie" 'stevie))
 ;; Load zipfly for both "zipfly" and "zipfly.*" hostnames
 ((or (string-prefix-p "zipfly" system-name)
      (string= system-name "zipfly.lan"))   (my/load-host "zipfly" 'zipfly))
 (t (my/load-host "generic" 'generic)))

;; Final keybindings (your custom keymap module)

;; Faces from Custom live in custom.el now; nothing here.

;; ============================================================
;; Claude Code IDE - Emacs integration for Claude Code CLI
;; ============================================================

;; vterm is required for terminal backend
(use-package vterm
  :ensure t
  :defer t
  :config
  ;; Compile vterm module on first use if needed
  (setq vterm-always-compile-module t))

;; Claude Code IDE
(add-to-list 'load-path (concat emacs-dir "/site-lisp/claude-code-ide"))
(use-package claude-code-ide
  :commands (claude-code-ide claude-code-ide-menu claude-code-ide-toggle)
  :bind (("C-c C-'" . claude-code-ide-menu)    ; Main menu
         ("C-c C-;" . claude-code-ide)          ; Start/toggle Claude
         ("C-c C-:" . claude-code-ide-toggle))  ; Toggle window
  :config
  ;; Window configuration
  (setq claude-code-ide-window-side 'right)
  (setq claude-code-ide-window-width 90)
  (setq claude-code-ide-focus-on-open t)

  ;; Terminal backend
  (setq claude-code-ide-terminal-backend 'vterm)
  (setq claude-code-ide-vterm-anti-flicker t)

  ;; Diff integration - use ediff for reviewing changes
  (setq claude-code-ide-use-ide-diff t)
  (setq claude-code-ide-focus-claude-after-ediff t)

  ;; CLI path (uses PATH by default, but explicit is safer)
  (setq claude-code-ide-cli-path "claude")

  ;; Optional: Enable MCP server for deeper Emacs integration
  ;; (setq claude-code-ide-enable-mcp-server t)
  )

;; End of init.new.el
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cursor ((t (:background "red"))))
 '(highlight-current-line-face ((t (:background "gray35"))))
 '(org-checkbox ((t (:foreground nil :inherit org-todo)))))
(put 'set-goal-column 'disabled nil)


(provide 'late-config)
