;; Python settings ------------------------------------------------------------

(require 'flymake)
(remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)

(unless (package-installed-p 'yapfify)
  (package-refresh-contents)
  (package-install 'yapfify))

(require 'importmagic)

(unless (package-installed-p 'elpy)
  (package-refresh-contents)
  (package-install 'elpy))

(use-package elpy
  :ensure t
  :config
  (unbind-key "M-<left>" elpy-mode-map)
  (unbind-key "M-<right>" elpy-mode-map)
  (unbind-key "M-<up>" elpy-mode-map)
  (unbind-key "M-<down>" elpy-mode-map)
  (bind-keys :map elpy-mode-map
             ("M-<left>" . windmove-left)
             ("M-<right>" . windmove-right)
             ("M-<up>" . windmove-up)
             ("M-<down>" . windmove-down)
             ("s-<left>" . elpy-nav-indent-shift-left)
             ("s-<right>" . elpy-nav-indent-shift-right)))

(setq comint-process-echoes t)

(setq my-venv (expand-file-name "~/opt/plus/py"))
(setq my-vpy (expand-file-name "~/opt/plus/py/bin/python"))
(pyvenv-activate my-venv)
(pyvenv-workon my-venv)

(custom-set-variables
 '(elpy-modules
   (quote
    (elpy-module-company elpy-module-eldoc elpy-module-flymake elpy-module-pyvenv elpy-module-highlight-indentation elpy-module-yasnippet elpy-module-sane-defaults)))
 '(python-shell-interpreter "/Users/jacob/opt/plus/py/bin/python")
 '(elpy-rpc-python-command "/Users/jacob/opt/plus/py/bin/python")
 '(elpy-rpc-virtualenv-path "/Users/jacob/opt/plus/py/bin/python")
 '(elpy-rpc-python-command "/Users/jacob/opt/plus/py/bin/python")
 '(elpy-syntax-check-command "/Users/jacob/opt/plus/py/bin/mypy")
 '(elpy-rpc-virtualenv-path "/Users/jacob/opt/plus/py/bin/python")
 '(elpy-test-discover-runner-command (quote ("python-shell-interpreter" "-m" "pytest")))
 '(elpy-test-pytest-runner-command (quote ("/Users/jacob/opt/plus/py/bin/pytest")))
 '(python-check-command (quote ("/Users/jacob/opt/plus/py/bin/mypy")))
 '(elpy-test-runner (quote elpy-test-pytest-runner))
 '(elpy-rpc-python-command "/Users/jacob/opt/plus/py/bin/python")
 '(elpy-rpc-virtualenv-path "/Users/jacob/opt/plus/py/bin/python")
 '(elpy-syntax-check-command "/Users/jacob/opt/plus/py/bin/mypy")
 '(elpy-test-discover-runner-command (quote ("python-shell-interpreter" "-m" "pytest")))
 '(elpy-test-pytest-runner-command (quote ("/Users/jacob/opt/plus/py/bin/pytest")))
 '(python-check-command (quote ("/Users/jacob/opt/plus/py/bin/mypy")))
 '(python-shell-interpreter "/Users/jacob/opt/plus/py/bin/python")
 '(importmagic-python-interpreter "~/opt/plus/py/bin/python")
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
             (setq company-idle-delay 0.2)
             (importmagic-mode)
             )
          )

(setq auto-mode-alist
      (append '(("\\.wsgi$" . python-mode)
                ("\\.pyx$" . python-mode)) auto-mode-alist))
(setq auto-mode-alist
      (append '(("Pipfile*" . conf-mode)) auto-mode-alist))

;; EIN settings ------------------------------------------------------------

(unless (package-installed-p 'ein)
  (package-install 'ein)
  (package-install 'ein-subpackages))
(require 'ein)
(require 'ein-subpackages)

(provide 'my-python)
