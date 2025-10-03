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

(provide 'deps)
