;; Safety: local variables and modes considered safe
(setq enable-local-variables :safe
      enable-local-eval nil
      enable-dir-local-variables t
      enable-remote-dir-locals nil)

(dolist (pair '((python-indent-offset . integerp)
                (js-indent-level . integerp)
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
(require 'base)         ;; UI basics, yasnippet setup helper
(require 'functions)    ;; utility functions (incl. osx-support)
(require 'visual)       ;; theme bootstrap

;; Package archives: ensure GNU + MELPA present for installs
(require 'package)
(unless (assoc "gnu" package-archives)
  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t))
(unless (assoc "melpa" package-archives)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))
(setq package-archive-priorities '(("gnu" . 10) ("melpa" . 5)))

;; On-demand package installation to avoid blocking startup
(defun my/require-or-install (feature pkg)
  "Ensure FEATURE is loaded; offer to install PKG if missing."
  (or (require feature nil 'noerror)
      (when (y-or-n-p (format "%s not installed. Install now? " pkg))
        (unless (bound-and-true-p package--initialized)
          (package-initialize))
        ;; Always refresh before first install attempt to ensure availability
        (ignore-errors (package-refresh-contents))
        (condition-case err
            (progn (package-install pkg)
                   (require feature nil 'noerror))
          (error (message "Install failed: %s" err)
                 nil)))))

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
;; Magit (first-use installation prompt; M-x magit-status works)
(defun magit-status ()
  (interactive)
  (unless (featurep 'magit)
    (unless (my/require-or-install 'magit 'magit)
      (user-error "Magit unavailable")))
  (call-interactively 'magit-status))

(defun magit-diff-buffer-file ()
  (interactive)
  (unless (featurep 'magit)
    (unless (my/require-or-install 'magit 'magit)
      (user-error "Magit unavailable")))
  (call-interactively 'magit-diff-buffer-file))

(global-set-key (kbd "C-x g") #'magit-status)
(global-set-key (kbd "C-x ?") #'magit-diff-buffer-file)

;; Install remaining external packages (runs once; may block to download)
(defvar my/packages-to-install
  '(nginx-mode nix-mode php-mode applescript-mode csharp-mode
    haml-mode sass-mode scss-mode typopunct xml-rpc edit-server
    dropdown-list coffee-mode ws-trim rst po-mode)
  "Packages to migrate from site-lisp to ELPA/MELPA.")

(defun my/install-missing-packages ()
  "Refresh package archives and install any missing packages from list."
  (require 'package)
  (unless (bound-and-true-p package--initialized)
    (package-initialize))
  (let ((missing nil))
    (dolist (p my/packages-to-install)
      (unless (package-installed-p p)
        (push p missing)))
    (when missing
      (message "Installing packages: %s" (nreverse missing))
      (ignore-errors (package-refresh-contents))
      (dolist (p (nreverse missing))
        (ignore-errors (package-install p))))))

;; Run install after startup; comment this out if you prefer manual control
(add-hook 'emacs-startup-hook #'my/install-missing-packages)

;; ripgrep
(autoload 'rg "rg" nil t)
(autoload 'rg-project "rg" nil t)

;; Multiple cursors
(autoload 'mc/edit-lines "multiple-cursors" nil t)
(autoload 'mc/mark-previous-like-this "multiple-cursors" nil t)
(autoload 'mc/mark-next-like-this "multiple-cursors" nil t)
(global-set-key (kbd "M-<SPC>") #'mc/edit-lines)
(global-set-key (kbd "M-p") #'mc/mark-previous-like-this)
(global-set-key (kbd "M-n") #'mc/mark-next-like-this)

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

;; Desktop sessions (guarded)
(when (require 'desktop nil 'noerror)
  (desktop-save-mode 1))

;; Host-specific last-word overrides, guarded
(defun my/load-host (name feature)
  (let* ((file (expand-file-name (concat name ".el") (concat emacs-dir "/hosts"))))
    (when (file-exists-p file)
      (require feature))))

(cond
 ((string-prefix-p "superfly" system-name) (my/load-host "superfly" 'superfly))
 ((string= system-name "medea")            (my/load-host "medea" 'medea))
 ((string-prefix-p "goldskip" system-name) (my/load-host "goldskip" 'goldskip))
 ((string-prefix-p "cecilia" system-name)  (my/load-host "cecilia" 'cecilia))
 ((string-prefix-p "sugarline" system-name)(my/load-host "sugarline" 'sugarline))
 ((string-prefix-p "skrotnisse" system-name)(my/load-host "skrotnisse" 'skrotnisse))
 ((string= system-name "stevie.local")     (my/load-host "stevie" 'stevie))
 ((string= system-name "zipfly.lan")       (my/load-host "zipfly" 'zipfly))
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
