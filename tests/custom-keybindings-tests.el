;;; custom-keybindings-tests.el --- Tests for custom key bindings -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'ert)
(require 'org)
(require 'custom-keybindings)

(ert-deftest my/org-open-at-point-here-is-bound-in-org-mode ()
  "C-c C-g opens Org targets in the selected window."
  (with-temp-buffer
    (org-mode)
    (should (eq (key-binding (kbd "C-c C-g"))
                #'my/org-open-at-point-here))
    (should-not (eq (key-binding (kbd "C-c g"))
                    #'my/org-open-at-point-here))))

(ert-deftest my/org-open-at-point-here-forces-current-window-open ()
  "The wrapper asks Org to visit file links through the selected window."
  (let (captured-arg
        captured-frame-setup
        captured-display-action)
    (cl-letf (((symbol-function 'org-open-at-point)
               (lambda (&optional arg)
                 (setq captured-arg arg)
                 (setq captured-frame-setup org-link-frame-setup)
                 (setq captured-display-action display-buffer-overriding-action))))
      (my/org-open-at-point-here))
    (should (eq captured-arg 'emacs))
    (should (eq (cdr (assq 'file captured-frame-setup)) #'find-file))
    (should (equal captured-display-action
                   '((display-buffer-same-window)
                     (inhibit-switch-frame . t))))))

(provide 'custom-keybindings-tests)

;;; custom-keybindings-tests.el ends here
