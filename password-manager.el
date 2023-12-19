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

(defun pm--extract-fields (element &rest fieldnames)
  "Extract fields given by FIELDNAMES from ELEMENT, an Org
element (as returned by `org-element-parse-buffer'.)"
  (mapcar (lambda (property)
            (plist-get (cadr element) property))
          fieldnames))


(defun pm-compile-data ()
  "Compile an alist mapping headlines (which should be the names of
the various services you use) to username and password data.

Return a function that returns username-password data, given the
name of a service."
  (let* ((headline-tree (org-element-parse-buffer 'headline))
         (service-data (org-element-map headline-tree 'headline
                (lambda (element)
                  (pm--extract-fields element
                                      :raw-value
                                      :USERNAME
                                      :PASSWORD)))))
    (lambda (service-name field)
      (interactive
       (let ((names (mapcar #'car service-data)))
         (list (completing-read "Service: " names nil t)
               (completing-read "Copy what: " '(username password)))))
      (seq-let (_ username password) (assoc service-name service-data)
        (let ((response (pcase field
                          ("username" username)
                          ("password" password))))
          (gui-set-selection 'CLIPBOARD response))))))

(provide 'password-manager)

;; Local Variables:
;; read-symbol-shorthands: (("pm-" . "password-manager-"))
;; End:
