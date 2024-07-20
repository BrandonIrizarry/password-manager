;; -*- lexical-binding: t; -*-

(require 'org-element)

;; Private functions

(defun pm--get-property (headline property)
  "Get PROPERTY from property drawer beneath HEADLINE."
  (save-excursion
    (pm--goto-headline headline)
    (let ((drawer-properties (cadr (org-element-headline-parser))))
      (plist-get drawer-properties property))))


(defun pm--get-all-headline-titles ()
  "Return a list of all headline titles in this Org buffer."
  (let ((tree (org-element-parse-buffer 'headline)))
    (org-element-map tree 'headline
      (lambda (headline)
	(org-element-property :title headline)))))

(defun pm--goto-headline (headline)
  "Move to the relevant HEADLINE.

Headline text in this context is the name of a service.

Point is placed at the beginning of the line where the headline
appears."
  (goto-char 1)
  (re-search-forward (rx-to-string `(seq bol "*" (+ space) ,headline)))
  (beginning-of-line))

;; Password generation goodies.

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

(cl-defun pm--generate-random-password (&optional (len 20))
  "Return a string of random characters of length LEN.

LEN defaults to 20.

For now, the set of characters is drawn from ASCII
33 (exclamation point) to 126 (tilde)."
  ;; 1. Generate a string (array) consisting of all possible characters.
  ;; 2. Shuffle this array.
  ;; 3. Take the first LEN characters as the new password.
  (let ((all-chars (number-sequence 33 126)))
    (seq-take (pm--shuffle-array (apply #'string all-chars))
              len)))


;; Public functions

(defun pm-copy-property-to-keyboard (service property)
  "Copy PROPERTY, filed in SERVICE's property drawer, to the system clipboard.

When called interactively, SERVICE and PROPERTY are prompted for
from the user."
  (interactive
   (let ((properties-readable-to-internal '(("username" . :USERNAME)
					    ("password" . :PASSWORD))))
     (list
      (completing-read "Service: " (pm--get-all-headline-titles))
      (let ((readable (completing-read "Which property: " properties-readable-to-internal)))
	(cdr (assoc readable properties-readable-to-internal))))))
  (if (gui-set-selection 'CLIPBOARD (pm--get-property service property))
      (message "Copied data to clipboard")
    (user-error "Drawer property missing")))

(defun pm-set-username (service username)
  "Set SERVICE username, defined in the service's property drawer, to USERNAME.

When called interactively, SERVICE is prompted for from the user."
  (interactive
   (list
    (completing-read "Service: " (pm--get-all-headline-titles))
    (read-string "New username: ")))
  (save-excursion
    (pm--goto-headline service)
    (org-set-property "username" username)))

(defun pm-set-password (service &optional password)
  "Set SERVICE password, defined in the service's property drawer, to PASSWORD.

If PASSWORD is nil, then a random password is generated."
  (interactive
   (list (completing-read "Service: " (pm--get-all-headline-titles))
	 (let ((new-password (read-string "New password: ")))
	   (if (string-blank-p new-password)
	       (pm--generate-random-password)
	     new-password))))
  (save-excursion
    (pm--goto-headline service)
    (org-set-property "password" password)))


(provide 'password-manager)

;; Local Variables:
;; read-symbol-shorthands: (("pm-" . "password-manager-"))
;; End:
