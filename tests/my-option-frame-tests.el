;;; my-option-frame-tests.el --- Tests for frame option picker -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'ert)
(require 'my-option-frame)

(ert-deftest my-option-frame-normalizes-alist-options ()
  "Alist OPTIONS get stable numeric keys and return their cdr values."
  (should
   (equal (my-option-frame--choices '(("Archive" . archive)
                                      ("Delete" . delete)))
          '((:key ?1 :label "Archive" :value archive)
            (:key ?2 :label "Delete" :value delete)))))

(ert-deftest my-option-frame-normalizes-list-valued-options ()
  "Alist option values may be lists usable as function arguments."
  (should
   (equal (my-option-frame--choices '(("Args" . (:limit 10 :scope project))))
          '((:key ?1 :label "Args" :value (:limit 10 :scope project))))))

(ert-deftest my-option-frame-normalizes-plain-options ()
  "Plain OPTIONS display as strings and return the original value."
  (should
   (equal (my-option-frame--choices '(small "medium"))
          '((:key ?1 :label "small" :value small)
            (:key ?2 :label "medium" :value "medium")))))

(ert-deftest my-option-frame-selects-value-by-key ()
  "Key presses map directly to option values."
  (let ((choices (my-option-frame--choices '(("One" . 1)
                                             ("Two" . 2)))))
    (should (= (my-option-frame--value-for-key choices ?2) 2))
    (should (null (my-option-frame--value-for-key choices ?x)))))

(ert-deftest my-option-frame-fallback-returns-selected-value ()
  "The minibuffer fallback returns the value for the selected label."
  (cl-letf (((symbol-function 'completing-read)
             (lambda (&rest _)
               "Remote")))
    (should
     (eq (my-option-frame--read-with-completing-read
          "Target: "
          (my-option-frame--choices '(("Local" . local)
                                      ("Remote" . remote))))
         'remote))))

(ert-deftest my-option-frame-fallback-allows-cancel ()
  "An empty fallback selection returns nil instead of an unusable value."
  (cl-letf (((symbol-function 'completing-read)
             (lambda (&rest _)
               "")))
    (should
     (null (my-option-frame--read-with-completing-read
            "Target: "
            (my-option-frame--choices '(("Local" . local))))))))

(provide 'my-option-frame-tests)

;;; my-option-frame-tests.el ends here
