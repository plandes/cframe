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

(require 'cl-lib)
(require 'eieio)
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

(defvar cframe-settings-restore-hooks nil
  "Functions to call with `cframae-settings-restore' is called.
When hook functions are called `setting' is bound to an instance
of `cframe-settings'.")

(cl-defmethod cframe-persistent-persist-value ((this cframe-persistent) val)
  (or (and (consp val)
	   (or (let ((fval (car val)))
		 (and fval
		      (eieio-object-p fval)
		      (object-of-class-p fval cframe-persistent)
		      (-map #'(lambda (val)
				(cframe-persistent-persist val))
			    val)))))
      val))

(cl-defmethod cframe-persistent-persist-slots ((this cframe-persistent))
  "Persist the slots of the instance."
  (with-slots (slots) this
    (->> slots
	 (-map #'(lambda (slot)
		   (let ((val (->> (slot-value this slot)
				   (cframe-persistent-persist-value this))))
		     (cons slot val)))))))

(cl-defmethod cframe-persistent-persist ((this cframe-persistent))
  "Persist an object."
  (append `((class . ,(eieio-object-class this))
	    (slots . ,(cframe-persistent-persist-slots this)))
	  (condition-case nil
	      (cl-call-next-method this)
	    (cl-no-next-method))))

(cl-defmethod cframe-persistent-unpersist-value ((this cframe-persistent) val)
  (or (and (consp val)
	   (or (let ((fval (car val)))
		 (and (consp fval)
		      (consp (car fval))
		      (eq 'class (caar fval))
		      (-map #'(lambda (val)
				(cframe-persistent-unpersist val))
			    val)))))
      val))

(cl-defmethod cframe-persistent-unpersist ((this cframe-persistent) vals)
  "Persist an object."
  (with-slots (slots) this
    (->> slots
	 (-map #'(lambda (slot)
		   (let ((val (->> (cdr (assq slot vals))
				   (cframe-persistent-unpersist-value this))))
		     (setf (slot-value this slot) val)))))))

(cl-defmethod cframe-persistent-unpersist ((vals list))
  (let* ((class (cdr (assq 'class vals)))
	 (slots (cdr (assq 'slots vals)))
	 (obj (make-instance class)))
    (cframe-persistent-unpersist obj slots)
    obj))


(defclass cframe-persistable (cframe-persistent)
  ((file :initarg :file
	 :initform nil
	 :type (or null string)
	 :documentation "The file to persist the state of the object."))
  :documentation "Subclasses that can persist to a file.")

(cl-defmethod cframe-persistable-save ((this cframe-persistable))
  "Persist manager and compiler configuration."
  (with-slots (file) this
    (let ((save-class-name (->> this eieio-object-class class-name))
	  (state (cframe-persistent-persist this)))
      (with-temp-buffer
	(insert (format "\
;; -*- emacs-lisp -*- <%s %s>
;; Object: %s.  Don't change this file.\n"
			(time-stamp-string "%02y/%02m/%02d %02H:%02M:%02S")
			file save-class-name))
	(insert (with-output-to-string
		  (pp state)))
	(write-region (point-min) (point-max) file))
      (message "Wrote %s" file))))



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
      (set-frame-position frame (car position) (cdr position)))
    (let ((setting this))
      (run-hooks 'cframe-settings-restore-hooks))))

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
	    name (car position) (cdr position) width height)))

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
  (append (cl-subseq seq 0 pos)
	  (list elt)
	  (cl-subseq seq pos)))

(cl-defmethod cframe-display-index ((this cframe-display) &optional index)
  (with-slots (settings sindex) this
    (let ((slen (length settings)))
      (if (= 0 slen)
	  (error "No settings exist--use `cframe-add-or-advance-setting'"))
      (-> (or index sindex)
	  (mod slen)))))

(cl-defmethod cframe-display-set-index ((this cframe-display) index)
  (with-slots (sindex) this
    (->> (cframe-display-index this index)
	 (setq sindex))))

(cl-defmethod cframe-display-increment-index ((this cframe-display)
					      &optional num)
  (with-slots (sindex) this
    (cframe-display-set-index this (+ sindex (or num 1)))))

(cl-defmethod cframe-display-setting ((this cframe-display) &optional index)
  (with-slots (settings) this
    (nth (cframe-display-index this index) settings)))

(cl-defmethod cframe-display-insert-setting ((this cframe-display)
					     &optional setting)
  "Add and optionally create first a new setting if SETTING is nil."
  (with-slots (settings sindex) this
    (setq settings
	  (cframe-display-insert-at-position settings
					     (or setting (cframe-setting))
					     sindex))
    (cl-incf sindex)))

(cl-defmethod cframe-display-set-name ((this cframe-display)
				       &optional new-name)
  "Set the name of the display."
  (with-slots (name id) this
    (let ((new-name (or new-name
			(format "(%d X %d)" (car id) (cdr id)))))
      (setq name new-name))))

(cl-defmethod cframe-display-setting-restore ((this cframe-display)
					      &optional setting)
  (let ((setting (or setting (cframe-display-setting this))))
    (cframe-setting-restore setting)))

(cl-defmethod object-format ((this cframe-display))
  (with-slots (name id settings sindex) this
    (format "%s [%s]: %d settings, index: %d"
	    name id (length settings)
	    (cframe-display-index this))))

(cl-defmethod initialize-instance ((this cframe-display) &rest rest)
  (with-slots (slots lists) this
    (setq slots '(name id sindex settings)))
  (apply #'cl-call-next-method this rest)
  (cframe-display-set-name this))



(defclass cframe-manager (cframe-persistent cframe-persistable)
  ((displays :initarg :displays
	     :initform nil
	     :type list
	     :documentation "Displays that have settings."))
  :documentation "Manages displays.")

(cl-defmethod initialize-instance ((this cframe-manager) &rest rest)
  (with-slots (slots) this
    (setq slots '(displays file)))
  (apply #'cl-call-next-method this rest))

(cl-defmethod cframe-manager-display ((this cframe-manager)
				      &optional no-create-p id)
  (with-slots (displays) this
    (let* ((id (or id (cframe-display-id)))
	   (display (->> displays
			 (cl-remove-if #'(lambda (display)
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

(cl-defmethod cframe-manager-advance-display ((this cframe-manager)
					      &optional index)
  "Iterate the settings index in the current display and restore it.

This modifies the frame settings."
  (let ((display (cframe-manager-display this)))
    (if index (cframe-display-set-index display index))
    (cframe-display-increment-index display)
    (cframe-display-setting-restore display)))



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

;;;###autoload
(defun cframe-current-setting ()
  "Get the current frame setting."
  (interactive)
  (let* ((display (-> the-cframe-manager
		      cframe-manager-display))
	 (setting (cframe-display-setting display)))
    (-> setting object-format message)))

;;;###autoload
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

;;;###autoload
(defun cframe-add-or-advance-setting (addp)
  "Either add with ADDP the current frame setting advance the next."
  (interactive (list current-prefix-arg))
  (if addp
      (progn
	(-> the-cframe-manager
	    cframe-manager-insert-setting)
	(cframe-save)
	(message "Added setting and saved"))
    (-> the-cframe-manager
	cframe-manager-advance-display))
  (cframe-current-setting))

;;;###autoload
(defun cframe-set-index-and-advance-setting (index)
  "Set the display's setting to INDEX and refresh the frame."
  (interactive (list (or (if (consp current-prefix-arg)
			     0
			   current-prefix-arg)
			 0)))
  (-> the-cframe-manager
      (cframe-manager-advance-display index))
  (cframe-current-setting))

;;;###autoload
(defun cframe-reset ()
  "Reset the state of the custom frame manager.

This blows away all frame settings configuration in memory.  To
wipe the state on the storage call `cframe-restore' or
`cframe-add-or-advance-setting' after calling this."
  (interactive)
  (setq the-cframe-manager
	(cframe-manager :file cframe-persistency-file-name)))

;;;###autoload
(defun cframe-save ()
  "Save the state of all custom frame settings."
  (interactive)
  (-> the-cframe-manager
      cframe-persistable-save))

;;;###autoload
(defun cframe-restore ()
  "Restore the state of all custom frame settings."
  (interactive)
  (let* ((file (expand-file-name "cframe" user-emacs-directory))
	 (mng (if (file-exists-p file)
		  (with-temp-buffer
		    (insert-file-contents file)
		    (->> (read (buffer-string))
			 cframe-persistent-unpersist))
		(cframe-reset))))
    (oset mng :file file)
    (setq the-cframe-manager mng)
    (cframe-manager-advance-display mng)
    mng))

(provide 'frame-customize)

;;; frame-customize.el ends here
