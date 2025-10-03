;; Ensure that dependencies are present. Should detect newy installed
;; Emacses using magit as the canonical minimal dependency. If magit
;; isn't present, ask the user if the large post-install procedure of
;; all expected packages should be done (takes a lot of time).

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
        ;; (ignore-errors (package-refresh-contents))
        (condition-case err
            (message "-------------- INSTALLING %s" pkg)
            (progn (package-install pkg)
                   (require feature nil 'noerror))
          (error (message "Install failed: %s" err)
                 nil)))))

(defun my/check-deps-on-startup-maybe-install ()
  "Detect a fresh environment and offer full install.

Checks whether the `magit` package is installed. If it is, do
nothing. If not, ask the user whether to run the full dependency
installation script and, if confirmed, load `post-install.el`."
  (interactive)
  (require 'package)
  (unless (bound-and-true-p package--initialized)
    (package-initialize))
  (if (package-installed-p 'magit)
      ;; Dependencies look fine; nothing to do.
      t
    (when
        (y-or-n-p
         "Fresh installation suspected. Run full dependency installation script? ")

      (let* ((base (or (and (boundp 'emacs-dir) emacs-dir)
                       (file-name-directory (or load-file-name buffer-file-name))
                       default-directory))
             (post (expand-file-name "post-install.el" base)))
        (if (file-readable-p post)
            (load-file post)
          ;; Fallback to load by feature/name if path lookup fails
          (load "post-install"))))))

;; Optional bulk install of convenience packages migrated from init.el
(defvar my/packages-to-install
  '(nginx-mode nix-mode php-mode applescript-mode csharp-mode
    haml-mode sass-mode scss-mode typopunct xml-rpc edit-server
    dropdown-list coffee-mode ws-trim rst po-mode
    zenburn-theme solarized-theme)
  "Packages to migrate from site-lisp to ELPA/MELPA.")

(defun my/install-missing-packages ()
  "Refresh package archives and install any missing packages from list.

Uses `my/packages-to-install`. Intended for manual use now that
deps.el manages startup installs."
  (interactive)
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

(provide 'deps)
