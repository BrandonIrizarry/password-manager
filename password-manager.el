;; -*- lexical-binding: t; -*-

;;; Commentary
;;
;; The goal is to (more or less) implement a replacement (but not
;; necessarily a workalike) for the 'pass' program in pure Org Mode.
;;
;; The idea is that you write your username and password data as Org
;; data, and you can then later query a given service for the username
;; and password you have on file for it, without having to, for
;; example, manually highlight and copy the data.

(require 'org-element)
(defun pm--derive-raw-name (key)
  "Get a simplified string representation of a keyword symbol.

For example, :USERNAME becomes \"USERNAME\"."
  (substring (symbol-name key) 1))

(defun pm--user-property-p (key)
  "Return true if keyword symbol KEY is a user-defined drawer
property.

We assume that such properties are exclusively defined as
uppercase keyword symbols."
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
  "Extract fields given by FIELDNAMES from ELEMENT, an Org
element (as returned by `org-element-parse-buffer'.)"
  (let ((user-keys (pm--filter-user-keys properties))
        (headline (plist-get properties :raw-value)))
    (cons headline
          ;; Keep the relevant "slice" of the plist for later use.
          (apply #'append (mapcar (lambda (property)
                                    (list property (plist-get properties property)))
                                  user-keys)))))

(defun pm--pretty-print-user-key (user-key)
  "Create an aesthetic version of USER-KEY."
  (replace-regexp-in-string "-" " " (capitalize (pm--derive-raw-name user-key))))

(defun pm--canonicalize-pretty-key (pretty-key)
  "Reverse pretty printing of USER-KEY."
  (intern (concat ":" (replace-regexp-in-string " " "-" (upcase pretty-key)))))

(defun pm-compile-data ()
  "Compile an alist mapping headlines (which should be the names of
the various services you use) to username and password data.

return the interactive command used for copying a username or
password to the clipboard."
  (let* ((headline-tree (org-element-parse-buffer 'headline))
         (user-data (org-element-map headline-tree 'headline
                      (lambda (element)
                        (pm--extract-user-data (cadr element))))))
    (lambda ()
      "Interactive command for copying a username or password to the
clipboard."
      (interactive)
      (let* ((name (completing-read "Service: " (mapcar #'car user-data) nil t))
             (current-properties (cdr (assoc name user-data))))
        (let ((field (pm--canonicalize-pretty-key
                      (completing-read "Copy what: "
                                       (mapcar #'pm--pretty-print-user-key
                                               (pm--filter-user-keys current-properties))
                                       nil t))))
          (gui-set-selection 'CLIPBOARD (plist-get current-properties field)))))))

(defun pm-lookup ()
  "The driver for looking up usernames and passwords."
  (interactive)
  (let ((result (call-interactively (pm-compile-data))))
    (if result
        (message "Copied data to clipboard")
      (user-error "Drawer property missing"))))

(provide 'password-manager)

;; Local Variables:
;; read-symbol-shorthands: (("pm-" . "password-manager-"))
;; End:
