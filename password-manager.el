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

(defun pm--user-property-p (key)
  "Return true if keyword symbol KEY is a user-defined drawer
property."
  (let ((raw-key-name (substring (symbol-name key) 1)))
    (string= raw-key-name
             (upcase raw-key-name))))

(defun pm--extract-fields (properties)
  "Extract fields given by FIELDNAMES from ELEMENT, an Org
element (as returned by `org-element-parse-buffer'.)"
  (let ((user-keys (seq-filter #'pm--user-property-p
                               (seq-filter #'keywordp properties)))
        (headline (plist-get properties :raw-value)))
    (cons headline
          ;; Keep the relevant "slice" of the plist for later use.
          (apply #'append (mapcar (lambda (property)
                                    (list property (plist-get properties property)))
                                  user-keys)))))


(defun pm-compile-data ()
  "Compile an alist mapping headlines (which should be the names of
the various services you use) to username and password data.

Return the interactive command used for copying a username or
password to the clipboard."
  (let* ((headline-tree (org-element-parse-buffer 'headline))
         (service-data (org-element-map headline-tree 'headline
                (lambda (element)
                  (pm--extract-fields element
                                      :raw-value
                                      :USERNAME
                                      :PASSWORD)))))
    (lambda (service-name field)
      "Interactive command for copying a username or password to the
clipboard."
      (interactive
       (let ((names (mapcar #'car service-data)))
         (list (completing-read "Service: " names nil t)
               (completing-read "Copy what: " '(username password)))))
      (seq-let (_ username password) (assoc service-name service-data)
        (let ((response (pcase field
                          ("username" username)
                          ("password" password))))
          (gui-set-selection 'CLIPBOARD response))))))

(defun pm-lookup ()
  "The driver for looking up usernames and passwords."
  (interactive)
  (let ((result (call-interactively (pm-compile-data))))
    (or result
        (user-error "Drawer property missing"))))

(provide 'password-manager)

;; Local Variables:
;; read-symbol-shorthands: (("pm-" . "password-manager-"))
;; End:
