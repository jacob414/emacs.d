;; My org settings - split into own module for readability.

(require 'ox-html)

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c #") 'org-shell-region))

(defun org-mark-region-asterisks ()
  "Wrap the current region with asterisks."
  (interactive)
  (let ((region-start (region-beginning))
        (region-end (region-end)))
    (save-excursion
      (goto-char region-start)
      (insert "*")
      ;; (delete-region region-start region-end)
      (goto-char (+ region-end 1))
      (insert "*"))))

;; Bind the function to s-b in org-mode
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "s-b") 'org-mark-region-asterisks))

(defun org-mark-region-slashes ()
  "Wrap the current region with slashes (italic)."
  (interactive)
  (let ((region-start (region-beginning))
        (region-end (region-end)))
    (save-excursion
      (goto-char region-start)
      (insert "/")
      ;; (delete-region region-start region-end)
      (goto-char (+ region-end 1))
      (insert "/"))))

;; Bind the function to s-b in org-mode
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "s-i") 'org-mark-region-slashes))

;; NOTE: Second duplicate my-org-publish-site removed
;; Was attempting to run ~/bin/414pub.sh which doesn't exist
;; Use my-org-marsch.el system instead (Makefile-based deployment)

(defun my-org-literal ()
  "Makes current selection an org-mode literal."
  (interactive)
  (unless (use-region-p)
    (user-error "No active region: select text to wrap"))
  (org-emphasize ?=)
)

;; #+LaTeX_CLASS: beamer in org files
(unless (boundp 'org-export-latex-classes)
  (setq org-export-latex-classes nil))
(add-to-list 'org-export-latex-classes
  ;; beamer class, for presentations
  '("beamer"
     "\\documentclass[11pt]{beamer}\n
      \\mode<{{{beamermode}}}>\n
      \\usetheme{{{{beamertheme}}}}\n
      \\usecolortheme{{{{beamercolortheme}}}}\n
      \\beamertemplateballitem\n
      \\setbeameroption{show notes}
      \\usepackage[utf8]{inputenc}\n
      \\usepackage[T1]{fontenc}\n
      \\usepackage{hyperref}\n
      \\usepackage{color}
      \\usepackage{listings}
      \\lstset{numbers=none,language=[ISO]C++,tabsize=4,
  frame=single,
  basicstyle=\\small,
  showspaces=false,showstringspaces=false,
  showtabs=false,
  keywordstyle=\\color{blue}\\bfseries,
  commentstyle=\\color{red},
  }\n
      \\usepackage{verbatim}\n
      \\institute{{{{beamerinstitute}}}}\n
       \\subject{{{{beamersubject}}}}\n"

     ("\\section{%s}" . "\\section*{%s}")

     ("\\begin{frame}[fragile]\\frametitle{%s}"
       "\\end{frame}"
       "\\begin{frame}[fragile]\\frametitle{%s}"
       "\\end{frame}")))

  ;; letter class, for formal letters

  (add-to-list 'org-export-latex-classes

  '("letter"
     "\\documentclass[11pt]{letter}\n
      \\usepackage[utf8]{inputenc}\n
      \\usepackage[T1]{fontenc}\n
      \\usepackage{color}"

     ("\\section{%s}" . "\\section*{%s}")
     ("\\subsection{%s}" . "\\subsection*{%s}")
     ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
     ("\\paragraph{%s}" . "\\paragraph*{%s}")
     ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(fset 'my-org-point-add
      [return tab backspace backspace ?- ?  ?\M-8 ?  ?\M-9 ? ])

;; http://orgmode.org/manual/Conflicts.html
(add-hook 'org-mode-hook
          (lambda ()
            ;; Ensure M-q uses Org's filler; global fill-column is set elsewhere
            (when (fboundp 'org-fill-paragraph)
              (setq-local fill-paragraph-function #'org-fill-paragraph))
            (local-set-key (kbd "C-.") 'scroll-down-one-line)
            (local-set-key (kbd "C-,") 'scroll-up-one-line)
            (local-set-key (kbd "C-j") 'my-greedy-joinlines)
            (local-set-key (kbd "C-x C-e") 'langtool-correct-buffer)
            ;; Restrict YAS tab binding to active snippets only.
            ;; Use `yas-keymap` (snippet overlay map), not the global
            ;; `yas-minor-mode-map`, to avoid hijacking TAB in other modes
            ;; like Magit.
            (when (boundp 'yas-keymap)
              (let ((fn (cond ((fboundp 'yas-next-field-or-maybe-expand)
                               #'yas-next-field-or-maybe-expand)
                              ((fboundp 'yas-maybe-expand)
                               #'yas-maybe-expand)
                              (t nil))))
                (when fn
                  (define-key yas-keymap [tab] fn))))
            (setcar (nthcdr 4 org-emphasis-regexp-components) 4)
            ))


(setq org-file-apps
    '(("\\.docx?\\'" . default)
      ("\\.xlsx?\\'" . default)
      ("\\.x?html?\\'" . default)
      ("\\.pdf\\'" . default)
      ("\\.log\\'" . emacs)
      (".*::\\(editme\\)\\'" . (find-file file))
      (auto-mode . emacs)))

(defun my/md-to-org-region (start end)
  "Convert region from markdown to org"
  (interactive "r")
  (shell-command-on-region start end "pandoc -f markdown -t org" t t))

;; Visual enhancements inspired by https://sophiebos.io/posts/beautifying-emacs-org-mode/
(defvar my/org-preferred-variable-pitch-fonts '("Calibre" "Source Sans Pro")
  "List of variable-pitch fonts to try for Org headings.")

(defvar my/org-preferred-fixed-pitch-fonts '("Roboto Mono")
  "List of fixed-pitch fonts to try for Org blocks.")

(defvar my/org-letter-spacing-stretch 'semi-expanded
  "Desired character stretch for Org variable-pitch text.
See `face-width' for the list of accepted values.")

(defun my/org-ensure-emphasis-markers-hidden ()
  "Make sure Org emphasis markers stay invisible in this buffer."
  (when org-hide-emphasis-markers
    (let ((spec buffer-invisibility-spec))
      (unless (or (eq spec t) (member t spec))
        (add-to-invisibility-spec t)))))

(defun my/org--font-installed-p (font)
  "Return non-nil when FONT exists on the current system."
  (and font (member font (font-family-list))))

(defun my/org--first-available-font (candidates)
  "Return first font from CANDIDATES available on this system."
  (seq-find #'my/org--font-installed-p candidates))

(defun my/org-apply-face-customizations ()
  "Adjust Org faces for prettier documents."
  (when (featurep 'org)
    (let ((sans (my/org--first-available-font my/org-preferred-variable-pitch-fonts))
          (mono (my/org--first-available-font my/org-preferred-fixed-pitch-fonts)))
      (when sans
        (dolist (face '((org-level-1 . 1.50)
                        (org-level-2 . 1.40)
                        (org-level-3 . 1.30)
                        (org-level-4 . 1.20)
                        (org-level-5 . 1.15)
                        (org-level-6 . 1.15)
                        (org-level-7 . 1.15)
                        (org-level-8 . 1.15)))
          (set-face-attribute (car face) nil
                              :family sans :weight 'bold :height (cdr face)))
        (set-face-attribute 'org-document-title nil
                            :family sans :weight 'bold :height 2.0))
      (when mono
        (set-face-attribute 'org-block nil
                            :inherit 'fixed-pitch :family mono
                            :foreground nil :height 1.0)
        (set-face-attribute 'org-code nil
                            :inherit '(shadow fixed-pitch)
                            :family mono :height 1.0)
        (set-face-attribute 'org-verbatim nil
                            :inherit '(shadow fixed-pitch)
                            :family mono :height 1.0)
        (set-face-attribute 'org-indent nil
                            :inherit '(org-hide fixed-pitch)
                            :family mono :height 1.0)
        (set-face-attribute 'org-meta-line nil
                            :inherit '(font-lock-comment-face fixed-pitch)
                            :family mono)
        (set-face-attribute 'org-special-keyword nil
                            :inherit '(font-lock-comment-face fixed-pitch)
                            :family mono)
        (set-face-attribute 'org-checkbox nil
                            :inherit 'fixed-pitch :family mono)
        (set-face-attribute 'org-table nil
                            :inherit 'fixed-pitch :family mono)))))

(defun my/org-buffer-local-settings ()
  "Local visual tweaks for Org buffers."
  (setq-local line-spacing 0.3)
  (variable-pitch-mode 1)
  (visual-line-mode 1)
  (my/org-ensure-emphasis-markers-hidden)
  (face-remap-add-relative 'variable-pitch
                           :width my/org-letter-spacing-stretch)
  (unless (bound-and-true-p my/org-text-scale-applied)
    (setq-local my/org-text-scale-applied t)
    (text-scale-increase 1)))

(with-eval-after-load 'org
  (require 'org-indent)
  (my/org-apply-face-customizations)
  (when (fboundp 'advice-add)
    (advice-add 'load-theme :after
                (lambda (&rest _) (my/org-apply-face-customizations))))

  ;; Install visual helper packages on demand
  ;; (my/require-or-install 'doom-themes 'doom-themes)
  (ignore-errors
    (when (featurep 'doom-themes)
      (doom-themes-org-config)))
  (when (my/require-or-install 'ligature 'ligature)
    (ligature-set-ligatures 'org-mode '("->" "<-" "=>" "<=" ">=" "=/=" "=="))
    (add-hook 'org-mode-hook #'ligature-mode))
  (when (my/require-or-install 'olivetti 'olivetti)
    (add-hook 'org-mode-hook #'olivetti-mode))
  (when (my/require-or-install 'org-modern 'org-modern)
    (setq org-auto-align-tags t
          org-tags-column 0
          org-fold-catch-invisible-edits 'show-and-error
          org-special-ctrl-a/e t
          org-insert-heading-respect-content t
          org-modern-star nil
          org-modern-tag nil
          org-modern-priority nil
          org-modern-todo nil
          org-modern-table nil
          org-agenda-tags-column 0
          org-agenda-block-separator ?─
          org-agenda-time-grid
          '((daily today require-timed)
            (800 1000 1200 1400 1600 1800 2000)
            " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
          org-agenda-current-time-string
          "⭠ now ─────────────────────────────────────────────────")
    (global-org-modern-mode 1))
  (when (my/require-or-install 'org-superstar 'org-superstar)
    (setq org-superstar-leading-bullet " "
          org-superstar-headline-bullets-list (make-list 8 " ")
          org-superstar-remove-leading-stars t
          org-superstar-special-todo-items t
          org-superstar-todo-bullet-alist
          '(("TODO" . 9744)
            ("PROJ" . 9744)
            ("READ" . 9744)
            ("CHECK" . 9744)
            ("IDEA" . 9744)
            ("DONE" . 9744)))
    (add-hook 'org-mode-hook #'org-superstar-mode))

  ;; Org appearance + behavior tweaks
  (setq org-adapt-indentation t
        org-hide-leading-stars t
        org-hide-emphasis-markers t
        org-pretty-entities t
        org-ellipsis "  ·"
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-edit-src-content-indentation 0
        org-lowest-priority ?F
        org-default-priority ?E)
  (setq org-priority-faces
        '((65 . "#BF616A")
          (66 . "#EBCB8B")
          (67 . "#B48EAD")
          (68 . "#81A1C1")
          (69 . "#5E81AC")
          (70 . "#4C566A")))
  (setq org-todo-keywords
        '((sequence "TODO" "PROJ" "READ" "CHECK" "IDEA" "|" "DONE")))
  (setq org-todo-keyword-faces
        '(("TODO" . (:foreground "#A3BE8C" :weight bold))
          ("PROJ" . (:foreground "#88C0D0" :weight bold))
          ("READ" . (:foreground "#8FBCBB" :weight bold))
          ("CHECK" . (:foreground "#81A1C1" :weight bold))
          ("IDEA" . (:foreground "#EBCB8B" :weight bold))
          ("DONE" . (:foreground "#30343d" :weight bold))))

  (let ((latex-opts (copy-sequence org-format-latex-options)))
    (setq org-format-latex-options
          (plist-put latex-opts :scale 2.0)))

  (add-hook 'org-mode-hook #'my/org-buffer-local-settings))

;; Org: inline file-name completion (same UI as C-x C-f)
(defun my/org-file-path-capf ()
  "Offer filesystem completion for the filename at point in Org buffers."
  (when (derived-mode-p 'org-mode)
    (let* ((end (point))
           (start (save-excursion
                    (skip-chars-backward "-._~/A-Za-z0-9:+")
                    (point))))
      (when (< start end)
        (list start end #'completion-file-name-table
              :exclusive 'no)))))

(defun my/org-enable-file-path-capf ()
  "Enable inline file-path completion via `completion-at-point'."
  (add-hook 'completion-at-point-functions #'my/org-file-path-capf nil t))

(add-hook 'org-mode-hook #'my/org-enable-file-path-capf)

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c C-c") #'org-latex-export-to-pdf)
  (define-key org-mode-map (kbd "C-c C-´") #'my/md-to-org-region)
  (define-key org-mode-map (kbd "C-c x") #'org-toggle-checkbox)
  ;; C-c C-g removed - use C-c m p for marsch-publish-site instead
  )

 ;; Stenkoll org-mode integration
(with-eval-after-load 'org
  ;; Load stenkoll integration
  (require 'stenkoll)

  ;; The stenkoll integration now loads automatically - no hook needed
  ;; The new implementation uses defensive loading that won't break org-mode

  (define-key org-mode-map (kbd "C-c C-p") #'my-org-prompt)
  (define-key org-mode-map (kbd "C-c C-+") #'my-org-literal)


  (require 'org-side-tree)
  (require 'org-beautify-theme)

  ;; Optional: Global keybinding for stenkoll commands
  (global-set-key (kbd "C-c s e") 'stenkoll-edit-issue-at-point)
  (global-set-key (kbd "C-c s l") 'stenkoll-list-issues))

(provide 'my-org)
