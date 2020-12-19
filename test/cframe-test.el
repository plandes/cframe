;;; package --- Summary
;;; Commentary:
;;
;; Unit tests of cframe-test.el
;;
;;; Code:

(require 'ert)
(require 'dash)
(require 'cframe)

(ert-deftest test-cframe-settings ()
  "Settings creation"
  (should (< 0 (length (cl-prin1-to-string (cframe-setting))))))

(provide 'cframe-test)

;;; cframe-test ends here
