;; Basic environment ----------------------------------------------------------

(setq emacs-dir "~/src/mine/emacs.d")
(add-to-list 'load-path emacs-dir)
(add-to-list 'load-path (concat emacs-dir "/site-lisp"))

(require 'here-env)

(require 'base)
(require 'functions)
(if (eq system-type 'darwin) (osx-support) )
(require 'magit)

(require 'basic-extras)

;; Desktop mode & bookmarks
;;(require 'bookmark+)
(require 'desktop)
(desktop-save-mode 1)

(global-set-key (kbd "C-c y") 'yas/reload-all)

;; White-space trim
(require 'ws-trim)
(setq-default ws-trim-level 1)
(global-ws-trim-mode t)

;; Load Par Edit --------------------------------------------------------------

(require 'paredit)

;; Use multiple-cursors -------------------------------------------------------

(require 'multiple-cursors)
(global-set-key (kbd "M-<SPC>") 'mc/edit-lines)
(global-set-key (kbd "M-p") 'mc/mark-previous-like-this)
(global-set-key (kbd "M-n") 'mc/mark-next-like-this)


;; Scheme settings ------------------------------------------------------------

(require 'xscheme)

;; Load custom functions ------------------------------------------------------
;; (require 'functions)
;; web-mode -------------------------------------------------------------------

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode)) ;; Override?
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))

(setq web-mode-content-types-alist
      '(("jsx"  . "/Users/jacob/src/lab/miveo/roc-0/.*\\.js[x]?\\'")
        ("jsx"  . "/Users/jacob/src/lab/cra-pre/.*\\.js[x]?\\'")
        ("jsx"  . "/Users/jacob/src/oss/cra-0/.*\\.js[x]?\\'")
        ("jsx"  . "/Users/jacob/src/miveo/.*\\.js[x]?\\'")
        ))

(defun my-web-mode-hook ()
  "web-mode settings"
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (local-set-key (kbd "C-x C-m") 'web-mode-fold-or-unfold)
  (hs-minor-mode) )

(add-hook 'web-mode-hook  'my-web-mode-hook)

(setq auto-mode-alist
      (append '(("\\.json$" . web-mode)) auto-mode-alist))

;; Javascript settings --------------------------------------------------------

(add-hook 'js-mode-hook
          '(lambda ()
             (interactive)
             (highlight-lines-matching-regexp "debugger" 'hi-red-b)
             (highlight-lines-matching-regexp "console\.log" 'hi-red-b)
             (highlight-lines-matching-regexp "alert\(" 'hi-red-b)
             )
          )

;; elm-mode settings  ---------------------------------------------------------

(require 'let-alist)
(require 's)
(require 'f)
(require 'elm-mode)

;; elixir-mode settings  ------------------------------------------------------

(unless (package-installed-p 'elixir-mode)
  (package-install 'elixir-mode))

;; swift-mode settings  -------------------------------------------------------

(unless (package-installed-p 'swift-mode)
  (package-install 'swift-mode))

(add-hook 'swift-mode-hook (lambda () (subword-mode 1)))

;; Python settings ------------------------------------------------------------

(unless (package-installed-p 'yapfify)
  (package-refresh-contents)
  (package-install 'yapfify))

(add-hook 'python-mode-hook
          '(lambda ()
             (interactive)
             (setq outline-regexp "[^ \t\n]\\|[ \t]*\\(if[ \t]+\\|elif[ \t]+\\|else[ \t]+\\|for[ \t]+\\|while[ \t]+\\|with[ \t]+\\|def[ \t]+\\|class[ \t]+\\)")
             (outline-minor-mode t)
             (require 'yapfify)
             (define-key python-mode-map (kbd "C-x C-m")
               'outline-toggle-children)
             (define-key python-mode-map (kbd "s-+")
               'outline-show-all)
             (highlight-lines-matching-regexp ".set_trace" 'hi-red-b)
             )
          )

(defun my-yapf-mode-check-buffers ()
  "Conditionally enable `rjsx-mode' based on file contents."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "yapf" nil t)
      (yapf-mode))
    ))

(add-hook 'find-file-hook #'my-yapf-mode-check-buffers)

(setq python-indent 4)

(setq auto-mode-alist
      (append '(("\\.wsgi$" . python-mode)
                ("\\.pyx$" . python-mode)) auto-mode-alist))

(setq auto-mode-alist
      (append '(("Pipfile*" . conf-mode)) auto-mode-alist))

;; EIN settings ------------------------------------------------------------

(unless (package-installed-p 'ein)
  (package-install 'ein))

(require 'ein)
;; (require 'ein-loaddefs)
(require 'ein-subpackages)

;; CoffeScript settings -------------------------------------------------------

(autoload 'coffee-mode "coffee-mode" "CoffeScript editing mode." t)

(defun cs-compile-and-run ()
  (interactive)
  (save-buffer)
  (coffe-compile-buffer) )

(setq auto-mode-alist
      (append '(("\\.coffee$" . coffee-mode)) auto-mode-alist))

(add-hook 'coffee-mode-hook
          '(lambda()
             (interactive)
             (local-set-key (kbd "s-r") 'cs-compile-and-run)
             (set (make-local-variable 'tab-width) 2) )
          )

;; Objective C settings -------------------------------------------------------

(add-hook 'objc-mode-hook (lambda () (subword-mode 1)))

;; Apache settings ------------------------------------------------------------

(autoload 'apache-mode "apache-mode" "Apache config edit mode." t)

(setq auto-mode-alist
      (append '(("\\.htaccess$" . apache-mode)) auto-mode-alist))

;; Shell settings -------------------------------------------------------------

(setq auto-mode-alist
      (append '(("bash-fc-*" . sh-mode)) auto-mode-alist))

;; Lisp settings --------------------------------------------------------------

(setq auto-mode-alist
      (append '(("\\.nu$" . lisp-mode)) auto-mode-alist))

;; Markdown settings ----------------------------------------------------------

(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)

(setq auto-mode-alist
      (append '(("\\.md$" . markdown-mode)) auto-mode-alist))

;; Ruby settings --------------------------------------------------------------

(setq auto-mode-alist
      (append '(("\\.po$" . ruby-mode)       ;; until I get po-mode sorted

                ("Podfile" . ruby-mode)      ;; Cocoapods
                ("\\.podspec$" . ruby-mode)  ;; Cocoapods

                ("Gemfile" . ruby-mode)      ;; Bundler (for Heroku et al)
                ("\\.ru$" . ruby-mode)        ;; Rack apps

                ("Rakefile" . ruby-mode)) auto-mode-alist))

(add-hook 'ruby-mode-hook
          '(lambda ()
             (local-set-key "\r" 'newline-and-indent)
             (local-set-key (kbd "C-j") 'my-greedy-joinlines) )
          )

;; PHP settings ---------------------------------------------------------------

(setq auto-mode-alist
      (append '(("\\.php$" . web-mode)) auto-mode-alist))

;; YAM-mode ------------------------------------------------------------------

(require 'yaml-mode)

(setq auto-mode-alist
      (append '(("\\.yml$" . yaml-mode)
                ("\\.yaml$" . yaml-mode)) auto-mode-alist))

;; SASS -----------------------------------------------------------------------

(setq auto-mode-alist
      (append '(("\\.sass$" . sass-mode)) auto-mode-alist))

(require 'scss-mode)

(setq auto-mode-alist
      (append '(("\\.scss$" . scss-mode)) auto-mode-alist))

(add-hook 'scss-mode-hook
          '(lambda ()
             (setq scss-compile-at-save nil) ) )

;; Nix expressions ------------------------------------------------------------

(autoload 'nix-mode "nix-mode"
  "Major mode for editing NGINX configuration files" t)

(setq auto-mode-alist
      (append '(("\\.nix$" . nix-mode)) auto-mode-alist))

;; Nginx configuration --------------------------------------------------------

(autoload 'nginx-mode "nginx-mode"
  "Major mode for editing NGINX configuration files" t)

(setq auto-mode-alist
      (append '(("\\nginx*.conf$" . conf-mode)) auto-mode-alist))

;; Various configuration ------------------------------------------------------

(setq auto-mode-alist
      ;; for now, save installation work
      (append '(("\\.gitignore$" . conf-mode)) auto-mode-alist))

;; Text-mode ------------------------------------------------------------------

(require 'wc-mode)

(defun text-env ()
  (interactive)
  (local-set-key (kbd "C-c m") 'oe-commit-message)
  (local-set-key (kbd "<f7>") 'ispell)
  (local-set-key (kbd "C-c s s") 'my-ispell-use-sv)
  (local-set-key (kbd "C-c s e") 'my-ispell-use-en)
  (local-set-key (kbd "C-c c") 'ispell-complete-word)
  (local-set-key (kbd "C-c w") 'count-words)
)

(defun text-mode-env ()
  (interactive)
  (turn-on-auto-fill)
  (text-env)
)

(setq auto-mode-alist
      (append '(("\\.tmp$" . text-mode)) auto-mode-alist))

(setq auto-mode-alist
      (append '(("\\.eml$" . mail-mode)) auto-mode-alist))

(add-hook 'text-mode-hook 'text-mode-env)
(add-hook 'mail-mode-hook 'text-env)

;; nodejs-repl ----------------------------------------------------------------

(require 'nodejs-repl)

;; Tuareg (OCaml) configuration -----------------------------------------------

(load "~/src/ext/ocaml/tuareg/tuareg-site-file")

;; org-mode settings  ---------------------------------------------------------

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

;; http://orgmode.org/manual/Conflicts.html
(add-hook 'org-mode-hook
          (lambda ()
            (local-set-key (kbd "M-<up>") 'windmove-up)
            (local-set-key (kbd "M-<left>") 'windmove-left)
            (local-set-key (kbd "M-<right>") 'windmove-right)
            (local-set-key (kbd "M-<down>") 'windmove-down)
            (local-set-key (kbd "C-<tab>") 'dabbrev-expand)
            (local-set-key (kbd "C-.") 'scroll-down-one-line)
            (local-set-key (kbd "C-,") 'scroll-up-one-line)
            (local-set-key (kbd "C-j") 'my-greedy-joinlines)
            (define-key yas/keymap [tab] 'yas/next-field-or-maybe-expand)))
;; [return tab backspace backspace ?- ?  ?\M-8 ?  ?\M-9 ? ]



(setq org-file-apps
    '(("\\.docx?\\'" . default)
      ("\\.xlsx?\\'" . default)
      ("\\.x?html?\\'" . default)
      ("\\.pdf\\'" . default)
      ("\\.log\\'" . emacs)
      (".*::\\(editme\\)\\'" . (find-file file))
      (auto-mode . emacs)))

;; (setq org-src-fontify-natively t)

;; expand-region --------------------------------------------------------------

(require 'expand-region)

;; dockerfile-mode4 -----------------------------------------------------------

(require 'dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

;; prog-fill ------------------------------------------------------------------

(require 'prog-fill)

;; Magit  ---------------------------------------------------------------------

(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x ?") 'magit-diff-buffer-file)

;; Custom ---------------------------------------------------------------------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(case-fold-search t)
 '(css-indent-offset 2 t)
 '(desktop-path (quote ("~/src/mine/emacs/desktop" "~")))
 '(elm-format-command "/usr/local/bin/elm-format" t)
 '(elm-format-on-save (quote t))
 '(elm-interactive-command "/usr/local/bin/elm-repl")
 '(js-indent-level 2)
 '(make-backup-files nil)
 '(nginx-indent-level 2)
 '(nodejs-repl-command "/usr/local/bin/node")
 '(package-selected-packages
   (quote
    (pymacs drag-stuff highlight-current-line bookmark+ applescript-mode ein-loaddefs "ein" ein w3m swift-mode elixir-mode)))
 '(rst-level-face-base-light 38)
 '(safe-local-variable-values (quote ((encoding . utf-8))))
 '(swift-mode:basic-offset 2)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(yas/field-highlight-face ((t (:background "gray35" :underline t))))
 '(yas/trigger-key "SPC"))

;; Host specific stuff - should always have the last word ---------------------

(cond
 ((equal (string-match "superfly" system-name) 0)
  (require 'superfly))

 ((equal system-name "medea")
  (require 'medea))

 ((string/starts-with system-name "goldskip")
  (require 'goldskip))

 ((string/starts-with system-name "cecilia")
  (require 'cecilia))

 ((string/starts-with system-name "SEGOTLPMRD006")
  (require 'sugarline))

 ((equal system-name "stevie.local")
  (require 'stevie))

 (t
  (require 'generic)) )

;; Load my own keybindings (last to win) --------------------------------------

(require 'custom-keybindings)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 115 :family "Operator Mono"))))
 '(cursor ((t (:background "red" :foreground "red"))))
 '(highlight-current-line-face ((t (:background "gray35"))))
 '(linum ((t (:inherit (shadow default) :height 0.9))))
 '(org-link ((t (:foreground "#d0bf8f" :underline t)))))

