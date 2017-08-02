(require 'cl-lib)
(require 'eieio)
(require 'time-stamp)
(require 'dash)
(require 'noflet)
(require 'config-manage)

(defvar tframe-settings-restore-hooks nil
  "Functions to call with `cframae-settings-restore' is called.
When hook functions are called `setting' is bound to an instance
of `cframe-settings'.")

(defclass tframe-setting (config-entry)
  ((width :initarg :width
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

(cl-defmethod tframe-setting-frame ((this tframe-setting))
  "Return the setting's frame."
  (selected-frame))

(cl-defmethod config-entry-description ((this config-entry))
  "Get the description of the configuration entry."
  (with-slots (width height) this
    (format "w: %d, h: %d" width height)))

(cl-defmethod config-entry-save ((this tframe-setting))
  "Save the current frame configuration."
  (let ((frame (tframe-setting-frame this)))
    (with-slots (width height position) this
      (setq width (frame-width frame)
	    height (frame-height frame)
	    position (frame-position)))))

(cl-defmethod config-entry-restore ((this tframe-setting))
  "Restore the frame to the current state of the setting."
  (let ((frame (tframe-setting-frame this)))
    (with-slots (name width height position) this
      (set-frame-width frame width)
      (set-frame-height frame height)
      (set-frame-position frame (car position) (cdr position)))
    (let ((setting this))
      (run-hooks 'tframe-settings-restore-hooks))))

(cl-defmethod tframe-setting-set-name ((this tframe-setting)
				       &optional new-name)
  "Set the name of the `tframe-setting'."
  (with-slots (name width) this
    (let ((new-name (or new-name (cond ((<= width 80) "narrow")
				       ((<= width 140) "wide")
				       (t "huge")))))
      (setq name new-name))))

(cl-defmethod object-format ((this tframe-setting))
  (with-slots (name width height position) this
    (format "%s: [top: %d, left: %d, width: %d, height: %d]"
	    name (car position) (cdr position) width height)))

(cl-defmethod initialize-instance ((this tframe-setting) &rest rest)
  (config-entry-save this)
  (tframe-setting-set-name this)
  (with-slots (slots description width height) this
    (setq slots '(name width height position)))
  (apply #'cl-call-next-method this rest))



(defun tframe-display-id ()
  "Create a key for a display."
  (cons (display-pixel-height)
	(display-pixel-width)))

(defclass tframe-display (config-manager)
  ((id :initarg :id
       :initform (tframe-display-id)
       :type cons
       :documentation "Identifies this display."))
  :documentation "Represents a monitor display.")

(cl-defmethod config-manager-entry-default-name ((this tframe-display))
  (with-slots (id) this
    (format "(%d X %d)" (car id) (cdr id))))

(cl-defmethod config-manager-create-default ((this tframe-display))
  (tframe-setting))

(cl-defmethod object-format ((this tframe-display))
  (with-slots (name id entries) this
    (format "%s [%s]: %d entries"
	    name id (length entries))))

(cl-defmethod initialize-instance ((this tframe-display) &rest rest)
  (with-slots (slots list-header-fields cycle-method) this
    (setq slots (append slots '(id))
	  cycle-method 'next
	  list-header-fields '("C" "Name" "Dimensions")))
  (apply #'cl-call-next-method this rest))



(defclass tframe-manager (config-persistent config-persistable)
  ((displays :initarg :displays
	     :initform nil
	     :type list
	     :documentation "Displays that have settings."))
  :documentation "Manages displays.")

(cl-defmethod tframe-manager-display ((this tframe-manager)
				      &optional no-create-p id)
  "Get display with index ID.
If the dipslay doesn't exist create a new display if NO-CREATE-P is non-nil."
  (with-slots (displays) this
    (let* ((find-id (or id (tframe-display-id)))
	   (display (->> displays
			 (cl-remove-if (lambda (display)
					 (with-slots (id) display
					   (not (equal find-id id)))))
			 car)))
      (when (and (null display) (not no-create-p))
	(setq display (tframe-display)
	      displays (append displays (list display))))
      display)))

(cl-defmethod tframe-manager-advance-display ((this tframe-manager))
  "Iterate the settings for the current display and restore it."
  (let ((display (tframe-manager-display this)))
    (config-manager-switch display 'cycle)))

(cl-defmethod initialize-instance ((this tframe-manager) &rest rest)
  (with-slots (slots) this
    (setq slots '(displays file)))
  (apply #'cl-call-next-method this rest))


;; funcs

(defgroup tframe nil
  "Customize a frame and fast switch size and positions."
  :group 'tframe
  :prefix "tframe-")

(defcustom tframe-persistency-file-name
  (expand-file-name "tframe" user-emacs-directory)
  "File containing the Flex compile configuration data."
  :type 'file
  :group 'tframe
  :set (lambda (sym val)
	 (set-default sym val)
	 (if (and (boundp 'the-tframe-manager)
		  the-tframe-manager)
	     (oset the-tframe-manager :file val))))

(defvar the-tframe-manager nil
  "The singleton manager instance.")

;;;###autoload
(defun tframe-current-setting (&optional include-display-p)
  "Get the current frame setting.

If INCLUDE-DISPLAY-P is non-nil, or provided interactively with
\\[universal-argument]]."
  (interactive "P")
  (let* ((display (-> the-tframe-manager
		      tframe-manager-display))
	 (setting (config-manager-current-instance display)))
    (-> (if include-display-p
	    (concat (object-format display) ", "))
	(concat (object-format setting))
	message)))

;;;###autoload
(defun tframe-add-or-advance-setting (addp)
  "Either add with ADDP the current frame setting advance the next."
  (interactive (list current-prefix-arg))
  (if addp
      (progn
	(-> the-tframe-manager
	    tframe-manager-display
	    config-manager-insert-entry)
	(tframe-save)
	(message "Added setting and saved"))
    (-> the-tframe-manager
	tframe-manager-advance-display))
  (tframe-current-setting))

;;;###autoload
(defun tframe-set-index-setting (index)
  "Set the display's setting to INDEX and refresh the frame."
  (interactive (list (or (if (consp current-prefix-arg)
			     0
			   current-prefix-arg)
			 0)))
  (-> the-tframe-manager
      tframe-manager-advance-display)
  (tframe-current-setting))

;;;###autoload
(defun tframe-save ()
  "Save the state of all custom frame settings."
  (interactive)
  (-> the-tframe-manager
      config-persistable-save))

;;;###autoload
(defun tframe-restore ()
  "Restore the state of all custom frame settings."
  (interactive)
  (let* ((file (expand-file-name "tframe" user-emacs-directory))
	 (mng (if (file-exists-p file)
		  (with-temp-buffer
		    (insert-file-contents file)
		    (->> (read (buffer-string))
			 config-persistent-unpersist))
		(tframe-reset))))
    (oset mng :file file)
    (setq the-tframe-manager mng)
    (tframe-manager-advance-display mng)
    mng))

;;;###autoload
(defun tframe-reset ()
  "Reset the state of the custom frame manager.

This blows away all frame settings configuration in memory.  To
wipe the state on the storage call `tframe-restore' or
`tframe-add-or-advance-setting' after calling this."
  (interactive)
  (setq the-tframe-manager
	(tframe-manager :file tframe-persistency-file-name)))

;;;###autoload
(defun tframe-list ()
  "List settings for current display."
  (interactive)
  (-> the-tframe-manager
      (tframe-manager-display t)
      config-manager-list-entries-buffer))

(global-set-key "\C-x9" 'tframe-reset)
(global-set-key "\C-\\" 'tframe-add-or-advance-setting)
;(global-set-key "\C-x9" 'cframe-set-index-setting)
;(global-set-key "\C-\\" 'cframe-add-or-advance-setting)
