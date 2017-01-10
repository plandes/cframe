;;; frame-customize.el --- customize a frame and fast switch size and positions

;; Copyright (C) 2015 - 2017 Paul Landes

;; Version: 0.1
;; Author: Paul Landes
;; Maintainer: Paul Landes
;; Keywords: frame customize
;; URL: https://github.com/plandes/frame-customize
;; Package-Requires: ((emacs "25") (noflet) (dash))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Allows for customization of frame types, which include among other
;; things, height and width of new Emacs frames.  Options included
;; are all of those which are included from `make-frame'.

;;; Code:

(require 'eieio)
;(require 'eieio-base)
(require 'time-stamp)
(require 'dash)
(require 'noflet)

;; EIEIO list types can't unpersist as they produce this error:
;;   eieio-persistent-validate/fix-slot-value: In save file, list of object
;;   constructors found, but no :type specified for slot displays of type nil
(defclass cframe-persistent ()
  ((slots :initarg :slots
	  :initform nil
	  :type list))
  :documentation "\
Super class for objects that want to persist to the file system.")

(cl-defmethod cframe-persistent-persist ((this cframe-persistent))
  "Persist an object"
  (with-slots (slots) this
    (->> slots
	 (-map #'(lambda (slot)
		   (cons slot (slot-value this slot)))))))


(defclass cframe-setting (cframe-persistent)
  ((name :initarg :name
	 :initform "narrow"
	 :type string)
   (width :initarg :width
	  :initform 80
	  :type integer
	  :documentation "Width of the frame.")
   (height :initarg :height
	   :initform 120
	   :type integer
	   :documentation "Height of the frame.")
   (position :initarg :position
	     :initform (0 . 0)
	     :type cons
	     :documentation "Top/left position of the frame."))
  :documentation "A frame settings: size and location.")

(cl-defmethod cframe-setting-frame ((this cframe-setting))
  "Return the setting's frame."
  (selected-frame))

(cl-defmethod cframe-setting-save ((this cframe-setting))
  "Save the current frame configuration."
  (let ((frame (cframe-setting-frame this)))
    (with-slots (width height position) this
      (setq width (frame-width frame)
	    height (frame-height frame)
	    position (frame-position)))))

(cl-defmethod cframe-setting-restore ((this cframe-setting))
  "Restore the frame to the current state of the setting."
  (let ((frame (cframe-setting-frame this)))
    (with-slots (name width height position) this
      (set-frame-width frame width)
      (set-frame-height frame height)
      (set-frame-position frame (car position) (cdr position)))))

(cl-defmethod cframe-setting-set-name ((this cframe-setting)
				       &optional new-name)
  "Set the name of the setting."
  (with-slots (name width) this
    (let ((new-name (or new-name (cond ((<= width 80) "narrow")
				       ((<= width 140) "wide")
				       (t "huge")))))
      (setq name new-name))))

(cl-defmethod object-format ((this cframe-setting))
  (with-slots (name width height position) this
    (format "%s: [top: %d, left: %d, width: %d, height: %d]"
	    name width height (car position) (cdr position))))

(cl-defmethod initialize-instance ((this cframe-setting) &rest rest)
  (cframe-setting-save this)
  (cframe-setting-set-name this)
  (with-slots (slots) this
    (setq slots '(name width height position)))
  (apply #'cl-call-next-method this rest))



(defun cframe-display-id ()
  "Create a key for a display."
  (cons (display-pixel-height)
	(display-pixel-width)))

(defclass cframe-display (cframe-persistent)
  ((name :initarg :name
	 :initform "untitled"
	 :type string
	 :documentation "Name of this custom frame configuration.")
   (id :initarg :id
       :initform (cframe-display-id)
       :type cons
       :documentation "Identifies this display.")
   (settings :initarg :settings
	     :initform nil;(list (cframe-setting))
	     :type list
	     :documentation "List of the settings for this frame.")
   (sindex :initarg :sindex
	   :initform 0
	   :type integer
	   :documentation "Index of current setting."))
  :documentation "Represents a monitor display.")

(defun cframe-display-insert-at-position (seq elt pos)
  "Return SEQ with ELT inserted at position POS."
  (append (subseq seq 0 pos)
	  (list elt)
	  (subseq seq pos)))

(cl-defmethod cframe-display-insert-setting ((this cframe-display)
					     &optional setting)
  "Add and optionally create first a new setting if SETTING is nil."
  (with-slots (settings sindex) this
    (setq settings
	  (cframe-display-insert-at-position settings
				     (or setting (cframe-setting))
				     sindex))
    (incf sindex)))

(cl-defmethod cframe-display-set-name ((this cframe-display)
				       &optional new-name)
  "Set the name of the display."
  (with-slots (name id) this
    (let ((new-name (or new-name
			(format "(%d X %d)" (car id) (cdr id)))))
      (setq name new-name))))

(cl-defmethod object-format ((this cframe-display))
  (with-slots (name id settings sindex) this
    (format "%s [%s]: %d settings, index: %d"
	    name id (length settings) sindex)))

(cl-defmethod initialize-instance ((this cframe-display) &rest rest)
  (with-slots (slots) this
    (setq slots '(name id sindex)))
  (apply #'cl-call-next-method this rest)
  (cframe-display-set-name this))

(cl-defmethod cframe-persistent-persist ((this cframe-display))
  (with-slots (settings) this
    (->> settings
	 (-map #'(lambda (setting)
		   (cframe-persistent-persist setting)))
	 (cons 'settings)
	 (append (cl-call-next-method this)))))



(defclass cframe-manager (cframe-persistent)
  ((displays :initarg :displays
	     :initform nil;(list (cframe-display))
	     :type list
	     :documentation "Manages all displays."))
  :documentation "Manages displays.")

(cl-defmethod cframe-persistent-save ((this cframe-persistent))
  "Persist manager and compiler configuration."
  (let ((fname compile-flex-persistency-file-name))
    (with-temp-buffer
      (insert
       ";; -*- emacs-lisp -*-"
       (condition-case nil
	   (progn
	     (format
	      " <%s %s>\n"
	      (time-stamp-string "%02y/%02m/%02d %02H:%02M:%02S")
	      fname))
	 (error "\n"))
       ";; Frame Customize.  Don't change this file.\n"
       (with-output-to-string
	 ;; save manager configuration for future iterations
	 (pp (append (list (cons 'manager nil))
		     (list (cons 'compilers
				 (flex-compile-manager-config this)))))))
      (write-region (point-min) (point-max) fname)
      (message "Wrote %s" compile-flex-persistency-file-name))))

(cl-defmethod cframe-manager-file-header ((this cframe-manager))
  (with-temp-buffer
    (-> (format "\
;; -*- emacs-lisp -*-<%s %s>
;; Flex compiler configuration.  Don't change this file.\n"
		(time-stamp-string "%02y/%02m/%02d %02H:%02M:%02S")
		(oref this file))
	insert)
    (buffer-string)))

(cl-defmethod object-write ((this cframe-manager) &optional comment)
  (cl-call-next-method this (or comment (oref this file-header-line))))

;; (cl-defmethod initialize-instance ((this cframe-manager) &rest rest)
;;   (with-slots (slots) this
;;     (setq slots '(name id sindex)))
;;   (apply #'cl-call-next-method this rest)
;;   (cframe-display-set-name this))

(cl-defmethod cframe-manager-display ((this cframe-manager)
				      &optional no-create-p id)
  (with-slots (displays) this
    (let* ((id (or id (cframe-display-id)))
	   (display (->> displays
			 (remove-if #'(lambda (display)
					(not (equal id (oref display :id)))))
			 car)))
      (when (and (null display) (not no-create-p))
	(setq display (cframe-display)
	      displays (append displays (list display))))
      display)))

(cl-defmethod cframe-manager-insert-setting ((this cframe-manager)
					     &optional setting)
  (-> (cframe-manager-display this)
      (cframe-display-insert-setting setting)))

(cl-defmethod cframe-persistent-persist ((this cframe-manager))
  (with-slots (displays) this
    (->> displays
	 (-map #'(lambda (setting)
		   (cframe-persistent-persist setting)))
	 (cons 'displays)
	 (append (cl-call-next-method this)))))

(defun a ()
  (interactive)
  (let ((this (cframe-manager)))
    (cframe-manager-insert-setting this)
    (->> (cframe-persistent-persist this)
	 quote
	 prettyprint)))




(defgroup cframe nil
  "Customize a frame and fast switch size and positions."
  :group 'cframe
  :prefix "cframe-")

(defcustom cframe-persistency-file-name
  (expand-file-name "cframe" user-emacs-directory)
  "File containing the Flex compile configuration data."
  :type 'file
  :group 'cframe
  :set #'(lambda (sym val)
	   (set-default sym val)
	   (if (boundp 'the-cframe-manager)
	       (oset the-cframe-manager :file val))))

(defvar the-cframe-manager nil
  "The singleton manager instance.")

(defun cframe-setting-insert ()
  "Add the current setting to the display."
  (interactive)
  (-> the-cframe-manager
      cframe-manager-insert-setting)
  (cframe-save))

(defun cframe-manager-reset ()
  "Reset the state of the custom frame manager."
  (interactive)
  (setq the-cframe-manager
	(cframe-manager :file cframe-persistency-file-name)))

(defun cframe-display-list ()
  "Display a list of displays and their settings."
  (interactive)
  (with-current-buffer (get-buffer-create "*Frame Customize*")
    (erase-buffer)
    (insert "Custom Frame Settings:\n\n")
    (with-slots (displays) the-cframe-manager
      (dolist (display displays)
	(with-slots (settings) display
	  (insert (format "- %s\n" (object-format display)))
	  (dolist (setting settings)
	    (insert (format "   - %s\n" (object-format setting)))))))
    (display-buffer (current-buffer))))

(defun cframe-save ()
  "Save the state of all custom frame settings."
  (interactive)
  (-> the-cframe-manager
      eieio-persistent-save))

(defun cframe-restore ()
  "Restore the state of all custom frame settings."
  (interactive)
  ;; hack to avoid list types not unpersisting:
  ;;   eieio-persistent-validate/fix-slot-value: In save file, list of object
  ;;   constructors found, but no :type specified for slot displays of type nil
  ;;
  ;; `flet' needed to bind past lexical scope
  (flet ((eieio-persistent-validate/fix-slot-value
	  (&rest rest)
	  (apply #'eieio-persistent-validate/fix-slot-value-hack rest)))
    (eieio-persistent-read cframe-persistency-file-name cframe-manager)))

;(cframe-manager-reset)
;(cframe-display-list)
;(cframe-setting-insert)
;(cframe-save)
;(cframe-restore)

(provide 'frame-customize)

;;; frame-customize.el ends here
