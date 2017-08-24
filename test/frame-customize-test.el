;;; package --- Summary
;;; Commentary:
;;
;; Unit tests of frame-customize-test.el
;;
;;; Code:

(require 'ert)
(require 'dash)
(require 'frame-customize)

(ert-deftest test-cframe-settings ()
  "Settings creation"
  (should (< 0 (length (object-format (cframe-setting))))))

(provide 'frame-customize-test)

;;; frame-customize-test ends here
