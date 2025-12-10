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
