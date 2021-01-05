;; Python settings ------------------------------------------------------------
(setq conda-base (expand-file-name "~/opt/plus/anaconda3/envs/plus"))

(defun plus-conda-path (subpath)
  "Get a subpath in the Conda env."
  (interactive)
  (concat conda-base "/" subpath)
  )

(setq conda-python (plus-conda-path "bin/python"))
(setq conda-mypy (plus-conda-path "bin/mypy"))

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
  (local-set-key (kbd "C-x C-e") 'elpy-shell-send-buffer)
  (bind-keys :map elpy-mode-map
             ("M-<left>" . windmove-left)
             ("M-<right>" . windmove-right)
             ("M-<up>" . windmove-up)
             ("M-<down>" . windmove-down)
             ("s-<left>" . elpy-nav-indent-shift-left)
             ("s-<right>" . elpy-nav-indent-shift-right)))

(setq comint-process-echoes t)

(setq my-venv conda-base)
(setq my-vpy (expand-file-name conda-python))
(pyvenv-activate my-venv)
(pyvenv-workon my-venv)

(custom-set-variables
 '(elpy-modules
   (quote
    (elpy-module-company elpy-module-eldoc elpy-module-flymake elpy-module-pyvenv elpy-module-highlight-indentation elpy-module-yasnippet elpy-module-sane-defaults)))
 '(python-shell-interpreter conda-python)
 '(elpy-rpc-python-command conda-python)
 '(elpy-rpc-virtualenv-path conda-python)
 '(elpy-rpc-python-command  conda-python)
 '(elpy-syntax-check-command conda-mypy)
 '(elpy-rpc-virtualenv-path conda-python)
 '(elpy-test-discover-runner-command (quote ("python-shell-interpreter" "-m" "pytest")))
 '(elpy-test-pytest-runner-command (plus-conda-path "bin/pytest"))
 '(python-check-command conda-mypy)
 '(elpy-test-runner (quote elpy-test-pytest-runner))
 '(elpy-rpc-python-command conda-python)
 '(elpy-rpc-virtualenv-path conda-python)
 '(elpy-syntax-check-command conda-mypy)
 '(elpy-test-discover-runner-command (quote ("python-shell-interpreter" "-m" "pytest")))
 '(elpy-test-pytest-runner-command (plus-conda-path "bin/pytest"))
 '(python-check-command conda-mypy)
 '(python-shell-interpreter conda-python)
)

(defun my-mypy ()
  "Docstring for my-mypy."
  (interactive)
  (run-python)
  (flycheck-mode)
  (flycheck-compile 'python-mypy)
  )

(add-hook 'python-mode-hook
          '(lambda ()
             (interactive)
             (setenv "MYPYPATH"
                     (concat
                      (expand-file-name "~/src/mine/skunkworks/python/stubs")
                      ":"
                      (expand-file-name "~/src/ext/python/typeshed/")
                      )
                     )


             (setq outline-regexp "[^ \t\n]\\|[ \t]*\\(if[ \t]+\\|elif[ \t]+\\|else[ \t]+\\|for[ \t]+\\|while[ \t]+\\|with[ \t]+\\|def[ \t]+\\|class[ \t]+\\)")
             (outline-minor-mode t)
             (persistent-overlays-minor-mode 1)
             (persistent-overlays-load-overlays)
             (setq python-shell-interpreter conda-python)

             (add-hook 'before-save-hook 'persistent-overlays-save-overlays nil 'local)

             (require 'yapfify)
             (highlight-lines-matching-regexp ".set_trace" 'hi-red-b)
             (define-key python-mode-map (kbd "C-x C-m") 'outline-toggle-children)

             (define-key python-mode-map (kbd "C-x C-c") 'my-mypy)
             (define-key python-mode-map (kbd "") 'outline-toggle-children)
             (define-key python-mode-map (kbd "") 'outline-toggle-children)
             (elpy-enable)
             (setq company-idle-delay 0.2)
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
