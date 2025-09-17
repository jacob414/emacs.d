;; Python settings ------------------------------------------------------------

(setq my/venv (expand-file-name "~/opt/plus/def-venv"))
(setq my/python-bin (expand-file-name "~/opt/plus/def-venv/bin/python"))
(setq my/pytest-bin (expand-file-name "~/opt/plus/def-venv/bin/pytest"))
(setq my/mypy-bin (expand-file-name "~/opt/plus/def-venv/bin/mypy"))

(require 'flymake)
(remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)

(unless (package-installed-p 'yapfify)
  (package-refresh-contents)
  (package-install 'yapfify))

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
  (local-set-key (kbd "C-x C-d") 'elpy-pdb-break-at-point)
  (bind-keys :map elpy-mode-map
             ("M-<left>" . windmove-left)
             ("M-<right>" . windmove-right)
             ("M-<up>" . windmove-up)
             ("M-<down>" . windmove-down)
             ("s-<left>" . elpy-nav-indent-shift-left)
             ("s-<right>" . elpy-nav-indent-shift-right)))

(setq elpy-modules '(elpy-module-company elpy-module-eldoc elpy-module-flymake
                                         elpy-module-pyvenv elpy-module-highlight-indentation
                                         elpy-module-yasnippet elpy-module-sane-defaults)
      python-shell-interpreter my/python-bin
      elpy-rpc-python-command my/python-bin
      elpy-rpc-virtualenv-path my/venv
      elpy-syntax-check-command my/mypy-bin
      elpy-test-discover-runner-command '("python" "-m" "pytest")
      elpy-test-pytest-runner-command my/pytest-bin
      python-check-command my/mypy-bin
      elpy-test-runner 'elpy-test-pytest-runner)

(setq comint-process-echoes t)

(pyvenv-activate my/venv)
(pyvenv-workon my/venv)

(defun my-mypy ()
  "Docstring for my-mypy."
  (interactive)
  (run-python)
  (flycheck-mode)
  (flycheck-compile 'python-mypy)
  )

(load-library "persistent-overlays")

(add-hook 'python-mode-hook
          '(lambda ()
             (interactive)
             (setenv "MYPYPATH"
                     (concat
                      (expand-file-name "~/src/mine/skunkworks/python/stubs")
                      ":"
                      (expand-file-name "~/src/ext/python/typeshed/")
                      ":"
                      (expand-file-name "~/src/oss/kingston/")
                      ":"
                      (expand-file-name "~/src/oss/ormsnack/")
                      ":"
                      (expand-file-name "~/src/oss/plus/")
                      )
                     )


             (setq outline-regexp "[^ \t\n]\\|[ \t]*\\(if[ \t]+\\|elif[ \t]+\\|else[ \t]+\\|for[ \t]+\\|while[ \t]+\\|with[ \t]+\\|def[ \t]+\\|class[ \t]+\\)")
             (outline-minor-mode t)
             (persistent-overlays-minor-mode 1)
             (persistent-overlays-load-overlays)
             (setq python-shell-interpreter my/python-bin)

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
;; (require 'ein-subpackages)

;; RST settings ------------------------------------------------------------

(with-eval-after-load 'rst
  (defun my/rst-clear-heading-background ()
    "Remove bright backgrounds from rst headings/adornments in this buffer."
    (dolist (face (face-list))
      (let ((n (symbol-name face)))
        (when (and (string-prefix-p "rst-" n)
                   (string-match-p "\\(level\\|header\\|title\\|adorn\\)" n))
          ;; buffer-local remap (doesn't clobber the theme globally)
          (face-remap-add-relative face '(:background unspecified))))))

  (add-hook 'rst-mode-hook #'my/rst-clear-heading-background))


(provide 'my-python)
