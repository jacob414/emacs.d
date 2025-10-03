;; Safety: local variables and modes considered safe
(setq enable-local-variables :safe
      enable-local-eval nil
      enable-dir-local-variables t
      enable-remote-dir-locals nil)

(dolist (pair '((js-indent-level . integerp)
                (web-mode-markup-indent-offset . integerp)
                (web-mode-css-indent-offset . integerp)
                (web-mode-code-indent-offset . integerp)
                (tab-width . integerp)
                (fill-column . integerp)
                (indent-tabs-mode . booleanp)
                (lexical-binding . booleanp)
                (org-confirm-babel-evaluate . booleanp)
                (coding . symbolp)
                (encoding . symbolp)))
  (put (car pair) 'safe-local-variable (cdr pair)))

(defun my/safe-mode-p (mode)
  (memq mode '(org-mode python-mode web-mode markdown-mode text-mode
                emacs-lisp-mode lisp-mode scheme-mode js-mode
                yaml-mode json-mode conf-mode)))
(put 'mode 'safe-local-variable #'my/safe-mode-p)

;; Paths and local modules
(setq emacs-dir (expand-file-name "~/src/mine/emacs.d"))
(add-to-list 'load-path emacs-dir)
(add-to-list 'load-path (concat emacs-dir "/site-lisp"))
(add-to-list 'load-path (concat emacs-dir "/hosts"))

(require 'here-env)     ;; keeps site-lisp/hosts on load-path
(require 'deps)         ;; Checks if expected Elisp modules are
                        ;; present, can install if not.
(my/check-deps-on-startup-maybe-install)
(require 'base)         ;; UI basics, yasnippet setup helper
(require 'functions)    ;; utility functions (incl. osx-support)
(require 'visual)       ;; theme bootstrap

;; macOS specifics (run after init so package features are available)
(when (eq system-type 'darwin)
  (add-hook 'after-init-hook
            (lambda ()
              (when (fboundp 'osx-support)
                (osx-support)))))

;; Whitespace trim (guarded)
(when (require 'ws-trim nil 'noerror)
  (setq-default ws-trim-level 1)
  (when (fboundp 'global-ws-trim-mode)
    (global-ws-trim-mode 1)))

;; Lightweight autoloads + keybindings to avoid eager requires
;; Magit (autoload with installation prompt)
(autoload 'magit-status "magit" nil t)
(autoload 'magit-diff-buffer-file "magit" nil t)

(defun my/magit-status ()
  (interactive)
  (unless (featurep 'magit)
    (my/require-or-install 'magit 'magit))
  (magit-status))

(defun my/magit-diff-buffer-file ()
  (interactive)
  (unless (featurep 'magit)
    (my/require-or-install 'magit 'magit))
  (magit-diff-buffer-file))

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
;; (add-hook 'emacs-startup-hook #'my-zenburn)

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

(autoload 'apache-mode "apache-mode" "Apache config edit mode." t)
(add-to-list 'auto-mode-alist '("\\.htaccess\\'" . apache-mode))

(autoload 'nix-mode "nix-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-mode))

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
(ignore-errors (require 'custom-keybindings))

;; Faces from Custom live in custom.el now; nothing here.

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
