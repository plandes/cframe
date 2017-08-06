;; -*- emacs-lisp -*-
(source gnu)
(source melpa)

(depends-on "buffer-manage"
	    :git "https://github.com/plandes/buffer-manage"
	    :files ("*.el"))

(package-file "frame-customize.el")

(files "*.el" "doc/*.texi" (:exclude ".dir-locals.el"))

(development
 (depends-on "dash")
 (depends-on "ert-runner"))
