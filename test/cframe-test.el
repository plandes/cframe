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
  (should (< 0 (length (object-format (cframe-setting))))))

(provide 'cframe-test)

;;; cframe-test ends here
