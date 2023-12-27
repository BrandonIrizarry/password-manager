;;; password-manager.el --- Manage usernames and passwords using Org Mode -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; The goal is to (more or less) implement a replacement (but not
;; necessarily a workalike) for the 'pass' program in pure Org Mode.
;;
;; The idea is that you write your username and password data as Org
;; data, and you can then later query a given service for the username
;; and password you have on file for it, without having to, for
;; example, manually highlight and copy the data.

(require 'org-element)

;;; Code:

(defun pm--derive-raw-name (key)
  "Get a simplified string representation of keyword symbol KEY.

For example, :USERNAME becomes \"USERNAME\"."
  (substring (symbol-name key) 1))

(defun pm--user-property-p (key)
  "Return non-nil if KEY represents a user-defined drawer property.

Return NIL otherwise.

KEY is a keyword symbol.  The assumption is that KEY is a
user-defined drawer property if and only if it's all uppercase.

We refer to a keyword symbol as a \"user-key\" whenever it
represents a user-defined drawer property."
  (let ((raw-key-name (pm--derive-raw-name key)))
    (string= raw-key-name
             (upcase raw-key-name))))

(defun pm--filter-user-keys (properties)
  "Filter PROPERTIES for its list of user-defined drawer properties.

This works for any plist, not just ones associated with Org
drawer properties."
  (seq-filter #'pm--user-property-p
              (seq-filter #'keywordp properties)))

(defun pm--extract-user-data (properties)
  "Extract user-defined drawer properties from PROPERTIES.

PROPERTIES is a plist of the kind that should be the second
element of an Org-element object, for example one returned by
`org-element-parse-buffer'."
  (let ((user-keys (pm--filter-user-keys properties))
        (headline (plist-get properties :raw-value)))
    (cons headline
          ;; Keep the relevant "slice" of the plist for later use.
          (apply #'append (mapcar (lambda (property)
                                    (list property (plist-get properties property)))
                                  user-keys)))))

(defun pm--pretty-print-user-key (user-key)
  "Create an aesthetic version of USER-KEY.

We refer to this aesthetic version as a \"pretty key\"."
  (replace-regexp-in-string "-" " " (capitalize (pm--derive-raw-name user-key))))

(defun pm--canonicalize-pretty-key (pretty-key)
  "Convert PRETTY-KEY into the equivalent user key."
  (intern (concat ":" (replace-regexp-in-string " " "-" (upcase pretty-key)))))

(defun pm--copy-user-property (user-data)
  (let* ((name (completing-read "Service: " (mapcar #'car user-data) nil t))
         (current-properties (cdr (assoc name user-data))))
    (let ((field (pm--canonicalize-pretty-key
                  (completing-read "Copy what: "
                                   (mapcar #'pm--pretty-print-user-key
                                           (pm--filter-user-keys current-properties))
                                   nil t))))
      (if (gui-set-selection 'CLIPBOARD (plist-get current-properties field))
          (message "Copied data to clipboard")
        (user-error "Drawer property missing")))))

(defun pm--set-username (user-data)
  (let ((headline (completing-read "Service: " (mapcar #'car user-data) nil t))
        (username (read-string "Username: ")))
    (save-excursion
      (goto-char (point-min))
      (search-forward headline nil t)
      (org-set-property "username" username))))

(defun pm--set-password (user-data)
  (let ((headline (completing-read "Service: " (mapcar #'car user-data) nil t)))
    (if (and (memq :PASSWORD (assoc headline user-data))
             (y-or-n-p "Password exists; overwrite? "))
        (save-excursion
          (goto-char (point-min))
          (search-forward headline nil t)
          (org-set-property "password" (pm--generate-password 20))
          (message "Password for '%s' set!" headline))
      (user-error "Aborted setting password"))))

(defun pm-do-action (fn)
  "Run an action FN on some user-data.

This user-data takes the form of drawer properties.  Specifically,
it's a plist mapping the property names to their values."
  (interactive
   (let ((actions `(("Set username" . ,#'pm--set-username)
                    ("Set password" . ,#'pm--set-password)
                    ("Copy property to clipboard" . ,#'pm--copy-user-property))))
     (list (cdr (assoc (completing-read "Do what: " actions) actions)))))
  ;; Note that `org-element-map' uses an indent setting of 2.
  (let* ((user-data (org-element-map (org-element-parse-buffer 'headline) 'headline
                      (lambda (element)
                        (pm--extract-user-data (cadr element))))))
    (funcall fn user-data)))


;;; Password generation goodies.

(defun pm--shuffle-array (array)
  "Shuffle an array ARRAY in place, using Fisher-Yates.

Return the shuffled array."
  (let ((len (length array)))
    (dotimes (i (- len 2))
      (let ((j (+ i (random (- len i)))))
        (let ((tmp (aref array i)))
          (aset array i (aref array j))
          (aset array j tmp)))))
  array)

(defun pm--generate-password (len)
  "Return a string of random characters of length LEN.

For now, the set of characters is drawn from ASCII
33 (exclamation point) to 126 (tilde)."
  (let ((all-chars (number-sequence 33 126)))
    (seq-take (pm--shuffle-array (apply #'string all-chars))
              len)))

(provide 'password-manager)

;; Local Variables:
;; read-symbol-shorthands: (("pm-" . "password-manager-"))
;; End:

;;; password-manager.el ends here
