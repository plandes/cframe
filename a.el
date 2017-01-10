(defun eieio-persistent-validate/fix-slot-value-hack (class slot proposed-value)
  "Validate that in CLASS, the SLOT with PROPOSED-VALUE is good, then fix.
A limited number of functions, such as quote, list, and valid object
constructor functions are considered valid.
Second, any text properties will be stripped from strings."
  (cond ((consp proposed-value)
	 ;; Lists with something in them need special treatment.
	 (let* ((slot-idx (- (eieio--slot-name-index class slot)
                             (eval-when-compile
                               (length (cl-struct-slot-info 'eieio--object)))))
                (type (cl--slot-descriptor-type (aref (eieio--class-slots class)
                                                      slot-idx)))
                (classtype (eieio-persistent-slot-type-is-class-p type)))

	   (cond ((eq (car proposed-value) 'quote)
		  (car (cdr proposed-value)))

		 ;; An empty list sometimes shows up as (list), which is dumb, but
		 ;; we need to support it for backward compat.
		 ((and (eq (car proposed-value) 'list)
		       (= (length proposed-value) 1))
		  nil)

		  ;; We have a slot with a single object that can be
		  ;; saved here.  Recurse and evaluate that
		  ;; sub-object.
		 ((and classtype (class-p classtype)
		       (child-of-class-p (car proposed-value) classtype))
		  (eieio-persistent-convert-list-to-object
		   proposed-value))

		 ;; List of object constructors.
		 ((and (eq (car proposed-value) 'list)
		       ;; 2nd item is a list.
		       (consp (car (cdr proposed-value)))
		       ;; 1st elt of 2nd item is a class name.
		       (class-p (car (car (cdr proposed-value))))
		       )

		  ;; Check the value against the input class type.
		  ;; If something goes wrong, issue a smart warning
		  ;; about how a :type is needed for this to work.
		  ;; (unless (and
		  ;; 	   ;; Do we have a type?
		  ;; 	   (consp classtype) (class-p (car classtype)))
		  ;;   (error "In save file, list of object constructors found, but no :type specified for slot %S of type %S"
		  ;; 	   slot classtype))

		  ;; ;; We have a predicate, but it doesn't satisfy the predicate?
		  ;; (dolist (PV (cdr proposed-value))
		  ;;   (unless (child-of-class-p (car PV) (car classtype))
		  ;;     (error "Corrupt object on disk")))

		  ;; We have a list of objects here.  Lets load them
		  ;; in.
		  (let ((objlist nil))
		    (dolist (subobj (cdr proposed-value))
		      (push (eieio-persistent-convert-list-to-object subobj)
			    objlist))
		    ;; return the list of objects ... reversed.
		    (nreverse objlist)))
		 (t
		  proposed-value))))

	 ((stringp proposed-value)
	  ;; Else, check for strings, remove properties.
	  (substring-no-properties proposed-value))

	 (t
	  ;; Else, just return whatever the constant was.
	  proposed-value))
  )
