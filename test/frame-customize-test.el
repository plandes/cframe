;;; package --- Summary
;;; Commentary:
;;
;; Unit tests of frame-customize-test.el
;;
;;; Code:

(require 'ert)
(require 'dash)
(require 'frame-customize)

(ert-deftest test-load ()
  "Test successful evaluation of frame-customize"
  (should t))

(provide 'frame-customize-test)

;;; frame-customize-test ends here
