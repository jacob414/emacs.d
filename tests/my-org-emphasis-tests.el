;;; my-org-emphasis-tests.el --- Tests for Org emphasis visibility -*- lexical-binding: t; -*-

(require 'ert)
(require 'org)

(defun my/org-test--marker-positions (chars)
  "Return buffer positions containing characters from CHARS.
CHARS is a list of character codes."
  (let (positions)
    (save-excursion
      (goto-char (point-min))
      (while (< (point) (point-max))
        (when (memq (char-after) chars)
          (push (point) positions))
        (forward-char 1)))
    (nreverse positions)))

(ert-deftest my/org-buffer-local-settings-hide-emphasis-markers ()
  "Ensure Org literals/bold/italic markers remain hidden."
  (let ((org-hide-emphasis-markers t))
    (with-temp-buffer
      (insert "*bold* /italic/ =code=")
      (org-mode)
      ;; Simulate another package resetting invisibility spec.
      (setq buffer-invisibility-spec nil)
      (font-lock-flush)
      (font-lock-ensure)
      (let ((marker-positions
             (my/org-test--marker-positions (string-to-list "*=/"))))
        (dolist (pos marker-positions)
          (should-not (invisible-p pos)))
        ;; Apply our local customizations which should re-enable hiding.
        (my/org-buffer-local-settings)
        (font-lock-flush)
        (font-lock-ensure)
        (dolist (pos marker-positions)
          (should (invisible-p pos)))))))

(provide 'my-org-emphasis-tests)

;;; my-org-emphasis-tests.el ends here
