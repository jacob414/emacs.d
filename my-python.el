;; Python settings ------------------------------------------------------------

;; Default virtualenv configuration
(setq my/default-venv (expand-file-name "~/opt/plus/def-venv"))
(setq my/current-venv my/default-venv)
(setq my/last-buffer-dir nil)

;; Function to find project-local .venv directory
(defun my/find-project-venv (start-dir)
  "Find .venv directory by walking up from START-DIR."
  (when start-dir
    (let ((venv-path (expand-file-name ".venv" start-dir)))
      (cond
       ((file-directory-p venv-path) venv-path)
       ((string= start-dir "/") nil)
       (t (my/find-project-venv (file-name-directory (directory-file-name start-dir))))))))

;; Function to update Python paths for current venv
(defun my/update-python-paths (venv-path)
  "Update all Python-related paths for VENV-PATH."
  (setq my/current-venv venv-path
        my/python-bin (expand-file-name "bin/python" venv-path)
        my/pytest-bin (expand-file-name "bin/pytest" venv-path)
        my/mypy-bin (expand-file-name "bin/mypy" venv-path)
        python-shell-interpreter my/python-bin
        elpy-rpc-python-command my/python-bin
        elpy-rpc-virtualenv-path venv-path
        elpy-syntax-check-command my/mypy-bin
        elpy-test-pytest-runner-command my/pytest-bin
        python-check-command my/mypy-bin))

;; Auto-switch virtualenv based on buffer location
(defun my/auto-switch-venv ()
  "Automatically switch virtualenv based on current buffer's directory."
  (when (and buffer-file-name
             (derived-mode-p 'python-mode))
    (let* ((buffer-dir (file-name-directory buffer-file-name))
           (project-venv (my/find-project-venv buffer-dir)))
      (when (not (string= buffer-dir my/last-buffer-dir))
        (setq my/last-buffer-dir buffer-dir)
        (let ((target-venv (or project-venv my/default-venv)))
          (when (not (string= target-venv my/current-venv))
            (my/update-python-paths target-venv)
            (pyvenv-activate target-venv)
            (message "Switched to venv: %s"
                     (if project-venv
                         (file-name-nondirectory (directory-file-name project-venv))
                       "default"))))))))

;; Set initial paths
(my/update-python-paths my/default-venv)

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
      elpy-test-discover-runner-command '("python" "-m" "pytest")
      elpy-test-runner 'elpy-test-pytest-runner)

(setq comint-process-echoes t)

;; Activate default venv on startup
(pyvenv-activate my/default-venv)
(pyvenv-workon my/default-venv)

;; Hook to automatically switch venv when changing buffers
(add-hook 'buffer-list-update-hook #'my/auto-switch-venv)

;; Manual command to check current venv and force switch
(defun my/show-current-venv ()
  "Show current virtualenv and project detection status."
  (interactive)
  (let* ((buffer-dir (when buffer-file-name (file-name-directory buffer-file-name)))
         (project-venv (when buffer-dir (my/find-project-venv buffer-dir)))
         (venv-name (if (string= my/current-venv my/default-venv)
                        "default"
                      (file-name-nondirectory (directory-file-name my/current-venv)))))
    (message "Current venv: %s | Buffer dir: %s | Project venv: %s"
             venv-name
             (or buffer-dir "none")
             (if project-venv
                 (file-name-nondirectory (directory-file-name project-venv))
               "none"))))

(defun my/force-venv-switch ()
  "Force virtualenv switch check for current buffer."
  (interactive)
  (setq my/last-buffer-dir nil)
  (my/auto-switch-venv)
  (my/show-current-venv))

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
