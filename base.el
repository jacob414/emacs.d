;; Basic settings
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

(when (fboundp 'set-fringe-mode) (set-fringe-mode 0))

;; Global wrapping width across modes
(setq-default fill-column 83)

;; Apply now and re-apply after themes change
(add-hook 'after-init-hook #'my/set-line-number-faces)
(when (fboundp 'advice-add)
  (advice-add 'load-theme :after (lambda (&rest _) (my/set-line-number-faces))))

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(setq confirm-nonexistent-file-or-buffer nil)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq font-lock-maximum-decoration t)
(setq ring-bell-function (lambda nil nil))
(setq display-time-24hr-format t)
(display-time)
(setq tramp-default-method "ssh")

;; Disable auto-save
(setq auto-save-default nil)
(setq auto-save-interval 0)
(setq auto-save-timeout 0)

;; Disable backups and lockfiles
(setq make-backup-files nil)   ;; no file~ backups
(setq create-lockfiles nil)    ;; no .#lock files

;; Additional security settings
(setq auth-source-save-behavior nil)  ; Don't save auth info automatically
(setq password-cache-expiry 300)       ; Password cache expires after 5 minutes
(setq epg-gpg-program "gpg2")         ; Use GPG2 if available
(setq epa-file-encrypt-to nil)         ; Don't encrypt to default key
(setq epa-file-select-keys nil)        ; Ask for keys when encrypting

;; Rectangle selections (via CUA)
(when (fboundp 'cua-mode)
  (setq cua-enable-cua-keys nil)
  (cua-mode t) )

;; Parenthesis balance
(show-paren-mode t)
(setq show-paren-style 'expression)
;;(set-face-foreground 'show-paren-mismatch-face "red")

;; y/n keypresses instead of spelled out 'yes'/'no'
(defalias 'yes-or-no-p 'y-or-n-p)

;; Enable auto-fill everywhere by default
;; Avoid `define-globalized-minor-mode` with `auto-fill-mode` (not a
;; modern minor mode variable); set the default function instead.
(setq-default auto-fill-function 'do-auto-fill)
;; Defensively re-enable after major mode changes
(add-hook 'after-change-major-mode-hook
          (lambda ()
            (unless (minibufferp)
              (auto-fill-mode 1))))

;; yasnippet
(defun my-yasnippet ()
  "yasnippet"
  (setq yas-snippet-dirs
        '("~/src/mine/emacs.d/snippet"  ;; personal snippets
          ))
  (require 'yasnippet)
  (yas-global-mode 1)
  (define-key yas-minor-mode-map (kbd "SPC") yas-maybe-expand)

  (require 'warnings)
  (add-to-list 'warning-suppress-types '(yasnippet backquote-change))
  )

(require 'paredit)

(require 'pax)

(provide 'base)
