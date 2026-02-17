(when (file-directory-p "privates.el") (require 'privates) )

;; Set default publishing options
(setq org-html-validation-link nil ;; Remove validation link
      org-html-doctype "html5"
      org-html-html5-fancy t
      org-html-postamble nil ;; No footer
      org-html-head-include-scripts nil ;; Don't include default scripts
      org-html-head-include-default-style nil ;; Don't include default style
      org-html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"style.css\" />")

(defun org-shell-region (beg end)
  "Wrap the current region with org-mode shell source block markers.
The region between BEG and END will be surrounded by #+BEGIN_SRC shell
and #+END_SRC markers."
  (interactive "r")
  (save-excursion
    ;; Add end marker after the region
    (goto-char end)
    (end-of-line)
    ;; Only insert newline if we're not already at one
    (unless (or (eobp) (looking-at "\n"))
      (insert "\n"))
    (insert "#+END_SRC")
    ;; Add begin marker before the region
    (goto-char beg)
    (beginning-of-line)
    (insert "#+BEGIN_SRC shell\n")))


(defun my-org-prompt ()
  "Docstring for my-org-prompt."
  (interactive)
  (insert "-----")
  (newline)
  (insert (format-time-string "** %Y-%m-%d %H:%M"))
  )

;; Set default publishing options
(setq org-html-validation-link nil ;; Remove validation link
      org-html-doctype "html5"
      org-html-html5-fancy t
      org-html-postamble nil ;; No footer
      org-html-head-include-scripts nil ;; Don't include default scripts
      org-html-head-include-default-style nil ;; Don't include default style
      org-html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"style.css\" />")

;; Define a function to publish your site
(defun my-org-publish-site ()
  "Publish the Org site and copy the style.css file."
  (interactive)
  (let ((org-publish-project-alist
         '(("my-site"
            :base-directory "~/src/mine/site"
            :publishing-directory "~/src/tmp/site"
            :recursive t
            :publishing-function org-html-publish-to-html
            :with-toc nil))))
    ;; Publish the Org files
    (org-publish-all t)

    ;; Copy the style.css file to the publishing directory
    (let* ((source-css-file
            (expand-file-name "style.css" "~/src/mine/site"))
           (destination-css-file
            (expand-file-name "style.css" "~/src/tmp/site")))
      (when (file-exists-p source-css-file)
        (copy-file source-css-file destination-css-file t)))))

(defun org-shell-region (beg end)
  "Wrap the current region with org-mode shell source block markers.
The region between BEG and END will be surrounded by #+BEGIN_SRC shell
and #+END_SRC markers."
  (interactive "r")
  (save-excursion
    ;; Add end marker after the region
    (goto-char end)
    (end-of-line)
    ;; Only insert newline if we're not already at one
    (unless (or (eobp) (looking-at "\n"))
      (insert "\n"))
    (insert "#+END_SRC")
    ;; Add begin marker before the region
    (goto-char beg)
    (beginning-of-line)
    (insert "#+BEGIN_SRC shell\n")))

 (with-eval-after-load 'org
   )

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c #") 'org-shell-region)
  (define-key org-mode-map (kbd "C-c p") 'my-org-prompt)
  (define-key org-mode-map (kbd "s-i") 'org-mark-region-slashes))

;; Deploy static site

;; Define a function to publish your site and upload it using a shell script

(defun my-org-publish-site ()
  "Publish the Org site, copy the style.css file, and run the upload script."
  (interactive)
  ;; Publish the Org files
  (let ((org-publish-project-alist
         '(("my-site"
            :base-directory "~/src/mine/site"
            :publishing-directory "~/src/tmp/site"
            :recursive t
            :publishing-function org-html-publish-to-html
            :with-toc nil))))
    (org-publish-all t)

    ;; Copy the style.css file to the publishing directory
    (let* ((source-css-file
            (expand-file-name "style.css" "~/src/mine/site"))
           (destination-css-file
            (expand-file-name "style.css" "~/src/tmp/site")))
      (when (file-exists-p source-css-file)
        (copy-file source-css-file destination-css-file t))))

  ;; Run the upload script
  (let ((upload-script "~/bin/414pub.sh"))
    (if (file-executable-p upload-script)
        (shell-command (concat "bash " upload-script))
      (message "Upload script %s is not executable or does not exist." upload-script))))

(defvar yank-strip-chars--n 0
  "Internal: number of characters to strip per line for `yank-strip-chars'.")

(defun yank-strip-chars--strip-per-line (s n)
  "Return S with N chars removed from each line.
If N > 0, remove from the *start* of each line.
If N < 0, remove -N chars from the *end* of each line."
  (with-temp-buffer
    (insert s)
    (goto-char (point-min))
    (while (not (eobp))
      (let ((bol (line-beginning-position))
            (eol (line-end-position)))
        (cond
         ((>= n 0)
          (delete-region bol (min (+ bol n) eol)))
         (t
          (let* ((k (- n)) ; k > 0
                 (start (max bol (- eol k))))
            (delete-region start eol)))))
      (forward-line 1))
    (buffer-string)))

(defun yank-strip-chars--transform (string)
  "Transformer used via `yank-transform-functions'."
  (yank-strip-chars--strip-per-line string yank-strip-chars--n))

(defun yank-strip-chars (n)
  "Yank from the kill ring, but first remove N characters from each line.

With a prefix arg, use it as N.
Without a prefix arg, prompt for N.

If N is negative, remove -N characters from the end of each line."
  (interactive
   (list (if current-prefix-arg
             (prefix-numeric-value current-prefix-arg)
           (read-number "Remove how many chars from each line? " 0))))
  (let ((yank-strip-chars--n n)
        (yank-transform-functions
         (cons #'yank-strip-chars--transform yank-transform-functions)))
    (yank)))

(defun pax/yank-log-entry ()
  "Docstring for pax/yank-entry."
  (interactive)
  (newline)
  (yank-strip-chars 2)
  )

(defun pax/yank-to-log ()
  "Docstring for pax/create-log-entry."
  (interactive)
  (my-org-prompt)
  (newline)
  (pax/yank-log-entry)
  )

;; Org-specific keybindings
;; Avoid global C-c p conflicts; bind within org-mode instead.
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c y") #'pax/yank-to-log)
  (define-key org-mode-map (kbd "C-c C-g") #'my-org-publish-site))

;;; ============================================================
;;; Klimatmarsch publishing - loaded from project directory
;;; All marsch code lives in my-org-marsch.el (single source of truth)
;;; ============================================================
(load "~/src/mine/skunkworks/docs/politik/xr/val2026/hemsida/my-org-marsch" nil)

;;; ============================================================
;;; Pax log deterministic headers
;;; ============================================================

(defun pax/heading-1 () (interactive)
       (save-excursion (beginning-of-line) (insert "* ")))
(defun pax/heading-2 () (interactive)
       (save-excursion (beginning-of-line) (insert "** ")))
(defun pax/heading-3 () (interactive)
       (save-excursion (beginning-of-line) (insert "*** ")))
(defun pax/heading-4 () (interactive)
       (save-excursion (beginning-of-line) (insert "**** ")))
(defun pax/heading-5 () (interactive)
       (save-excursion (beginning-of-line) (insert "***** ")))
(defun pax/heading-6 () (interactive)
       (save-excursion (beginning-of-line) (insert "****** ")))

;;; ============================================================
;;; Keybindings (non-marsch - marsch keybindings in my-org-marsch.el)
;;; ============================================================

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c 1") #'pax/heading-1)
  (define-key org-mode-map (kbd "C-c 2") #'pax/heading-2)
  (define-key org-mode-map (kbd "C-c 3") #'pax/heading-3)
  (define-key org-mode-map (kbd "C-c 4") #'pax/heading-4)
  (define-key org-mode-map (kbd "C-c 5") #'pax/heading-5)
  (define-key org-mode-map (kbd "C-c 6") #'pax/heading-6))

;;; ============================================================
;;; Pax answer receiver - receives text from external tools
;;; and inserts it at top of pax-answers.org
;;; ============================================================

(defun pax/receive-answer (text)
  "Insert TEXT at the top of pax-answers.org with a timestamp heading.
Opens or switches to the file, finds the first content position
(after any #+TITLE: or property headers), and inserts a separator,
a ** YYYY-MM-DD HH:MM heading, and the text."
  (interactive "sText to insert: ")
  (let ((answers-file (expand-file-name "~/src/mine/skunkworks/docs/pax/pax-answers.org")))
    ;; Create file with title if it doesn't exist
    (unless (file-exists-p answers-file)
      (with-temp-file answers-file
        (insert "#+TITLE: Pax Answers\n\n")))
    ;; Open or switch to the file
    (find-file answers-file)
    ;; Find insertion point: after file-level headers (lines starting with #+ or :)
    (goto-char (point-min))
    (while (and (not (eobp))
                (looking-at "^\\(#\\+\\|:\\)"))
      (forward-line 1))
    ;; Skip one blank line if present after headers
    (when (looking-at "^[[:space:]]*$")
      (forward-line 1))
    ;; Insert the new entry at this position
    (insert "-----\n"
            (format-time-string "** %Y-%m-%d %H:%M")
            "\n\n"
            text
            "\n\n")
    ;; Save the buffer
    (save-buffer)))

(provide 'pax)

;;; pax.el ends here
