(setq elpy-rpc-python-command "~/opt/plus/conda-plus/bin/python")

(setq langtool-bin "/opt/homebrew/bin/languagetool")

(org2web-add-project
 '("my-website"
   :repository-directory "~/src/mine/site"
   :remote (git "git@github.com:jacob414/site.git" "org2web")
   ;; Alternatively, use rclone:
   ;; :remote (rclone "remote-name" "/remote/path/location")
   :site-domain "https://414soft.com"
   :site-main-title "Jacob 414"
   :site-sub-title "Underrubrik?"
   :theme (worg)
   :source-browse-url ("GitHub" "https://github.com/yourusername/yourrepository")
   :personal-avatar "/media/img/avatar.jpg"
   :personal-duoshuo-shortname "your-duoshuo-shortname"
   :web-server-port 7654))

(defun reload-stenkoll ()
  "Reload stenkoll.el and clean up problematic hooks."
  (interactive)
  (message "Reloading stenkoll.el...")

  ;; Remove problematic hook functions that might not exist anymore
  (remove-hook 'org-mode-hook 'stenkoll-maybe-enable-edit-mode)

  ;; Reload the file
  (load-file (expand-file-name "~/src/mine/emacs.d/site-lisp/stenkoll.el"))

  ;; Verify the function is loaded correctly
  (let ((doc (documentation 'stenkoll-edit-issue-by-id)))
    (if (string-match "direct buffer-based editing" doc)
        (message "✓ stenkoll.el reloaded successfully - buffer-based version active")
      (message "⚠ stenkoll.el reloaded but old version may still be active")))

  ;; Show current hook status
  (message "org-mode-hook functions: %s"
           (mapcar (lambda (f)
                     (if (symbolp f)
                         (symbol-name f)
                         "anonymous-function"))
                   org-mode-hook)))

;; Call it immediately
(reload-stenkoll)


(provide 'zipfly)
