;; Python settings ------------------------------------------------------------

(unless (package-installed-p 'yapfify)
  (package-refresh-contents)
  (package-install 'yapfify))

(unless (package-installed-p 'elpy)
  (package-refresh-contents)
  (package-install 'elpy))

(defun my-elpy-keys ()
  "Docstring for elpy-mine."
  (interactive)
  (local-unset-key (kbd "M-<left>"))
  (define-key elpy-mode-map (kbd "M-<left>") 'windmove-left)
  (local-unset-key (kbd "M-<right>"))
  (define-key elpy-mode-map (kbd "M-<right>") 'windmove-right)
  (local-unset-key (kbd "M-<down>"))
  (define-key elpy-mode-map (kbd "M-<down>") 'windmove-down)
  (local-unset-key (kbd "M-<up>"))
  (define-key elpy-mode-map (kbd "M-<up>") 'windmove-up)
  )


(add-hook 'python-mode-hook
          '(lambda ()
             (interactive)
             (setq outline-regexp "[^ \t\n]\\|[ \t]*\\(if[ \t]+\\|elif[ \t]+\\|else[ \t]+\\|for[ \t]+\\|while[ \t]+\\|with[ \t]+\\|def[ \t]+\\|class[ \t]+\\)")
             (outline-minor-mode t)
             (persistent-overlays-minor-mode 1)
             (persistent-overlays-load-overlays)
             (setq python-shell-interpreter "~/opt/plus/py/bin/python")

             (add-hook 'before-save-hook 'persistent-overlays-save-overlays nil 'local)

             (require 'yapfify)
             (highlight-lines-matching-regexp ".set_trace" 'hi-red-b)
             (define-key python-mode-map (kbd "C-x C-m") 'outline-toggle-children)
             (define-key python-mode-map (kbd "") 'outline-toggle-children)
             (elpy-enable)
             (my-elpy-keys)
             ) )

(setq auto-mode-alist
      (append '(("\\.wsgi$" . python-mode)
                ("\\.pyx$" . python-mode)) auto-mode-alist))
(setq auto-mode-alist
      (append '(("Pipfile*" . conf-mode)) auto-mode-alist))

;; EIN settings ------------------------------------------------------------

(unless (package-installed-p 'ein)
  (package-install 'ein))
(require 'ein)
(require 'ein-subpackages)

(provide 'my-python)
