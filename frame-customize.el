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
(require 'eieio-base)
(require 'time-stamp)
(require 'dash)
(require 'noflet)



(defclass cframe-setting ()
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
  (selected-frame))

(cl-defmethod cframe-setting-save ((this cframe-setting))
  (let ((frame (cframe-setting-frame this)))
    (with-slots (width height position) this
      (setq width (frame-width frame)
	    height (frame-height frame)
	    position (frame-position)))))

(cl-defmethod cframe-setting-restore ((this cframe-setting))
  (let ((frame (cframe-setting-frame this)))
    (with-slots (name width height position) this
      (set-frame-width frame width)
      (set-frame-height frame height)
      (set-frame-position frame (car position) (cdr position)))))

(cl-defmethod cframe-setting-set-name ((this cframe-setting)
				       &optional new-name)
  (with-slots (name width) this
    (let ((new-name (or new-name (cond ((<= width 80) "narrow")
				       ((<= width 140) "wide")
				       (t "huge")))))
      (setq name new-name))))

(cl-defmethod initialize-instance ((this cframe-setting) &rest rest)
  (cframe-setting-save this)
  (cframe-setting-set-name this)
					;(oset this :persistable-slots '(:name))
  (apply #'cl-call-next-method this rest))



(defclass cframe-display ()
  ((name :initarg :name
	 :initform "untitled"
	 :type string
	 :documentation "Name of this custom frame configuration.")
   (display :initarg :display
	    :initform (cons (display-pixel-height)
			    (display-pixel-width))
	    :type cons
	    :documentation "Display configuration.")
   (settings :initarg :settings
	     :initform (list (cframe-setting))
	     :type list
	     :documentation "List of the settings for this frame.")
   )
  :documentation "Represents a frame configuration including settings.")

(cl-defmethod cframe-display-add-setting ((this cframe-display)
					  &optional settings)
  (with-slots (settings) this
    (setq settings
	  (-> (or settings (cframe-setting))
	      list
	      (append settings)))))

(cl-defmethod cframe-display-set-name ((this cframe-display)
				       &optional new-name)
  (with-slots (name display) this
    (let ((new-name (or new-name
			(format "(%d %d)" (car display) (cdr display)))))
      (setq name new-name))))

(cl-defmethod initialize-instance ((this cframe-display) &rest rest)
  (cframe-display-add-setting this)
  (apply #'cl-call-next-method this rest)
  (cframe-display-set-name this))



(defclass cframe-manager (eieio-persistent)
  ((displays :initarg :displays
	     :initform (list (cframe-display))
	     :type list
	     :documentation "Manages all displays.")))

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

(defvar the-cframe-manager
  (cframe-manager :file cframe-persistency-file-name)
  "THE singleton manager instance.")

(-> (cframe-manager :file cframe-persistency-file-name)
    eieio-persistent-save)

(flet ((eieio-persistent-validate/fix-slot-value
	(class slot proposed-value)
	(message "REST: %S" (list class slot proposed-value))
	proposed-value))
  (-> (eieio-persistent-read cframe-persistency-file-name cframe-manager)))

(provide 'frame-customize)

;;; frame-customize.el ends here
