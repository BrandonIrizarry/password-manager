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

(cl-defun pm--generate-random-password (blacklist-chars &optional (len 20))
  "Return a string of random characters of length LEN.

LEN defaults to 20.

If BLACKLIST-CHARS is nil, then the set of characters is drawn
from ASCII 33 (exclamation point) to 126 (tilde). Otherwise,
blacklist the chars found in BLACKLIST-CHARS from this range."
  ;; 1. Generate a string (array) consisting of all possible
  ;; characters, removing the blacklisted ones.
  ;;
  ;; 2. Shuffle this array.
  ;;
  ;; 3. Take the first LEN characters as the new password.

  ;; Note that if BLACKLIST-CHARS is nil, this falls through nicely:
  ;; ALL-CHARS is set to the widest range of characters possible.
  (let ((all-chars (seq-difference (number-sequence 33 126)
				   blacklist-chars)))
    (seq-take (pm--shuffle-array (apply #'string all-chars))
              len)))

;; This function comes in handy, since our blacklist-chars can come
;; from more than one source (currently either a property drawer, or
;; else from user input.)
(defun pm--process-new-password-entry (new-password blacklist-chars)
  "Generate a new password under certain conditions.

If NEW-PASSWORD is not `string-blank-p' (neither blank nor
empty), simply return the new password.

Otherwise, return a randomly generated password using the given
BLACKLIST-CHARS string."
  (if (string-blank-p new-password)
      (pm--generate-random-password blacklist-chars)
    new-password))

;; Public functions

;;;###autoload
(defun pm-copy-property-to-clipboard (service property)
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
      (message "Copied data to clipboard!")
    (user-error "Drawer property missing")))

;;;###autoload
(defun pm-set-username (service username)
  "Set SERVICE username, defined in the service's property drawer, to USERNAME.

When called interactively, SERVICE is prompted for from the user."
  (interactive
   (list
    (completing-read "Service: " (pm--get-all-headline-titles))
    (read-string "New username: ")))
  (save-excursion
    (pm--goto-headline service)
    (org-set-property "username" username)
    (message "Username for '%s' set!" service)))

;;;###autoload
(defun pm-set-password (service password)
  "Set SERVICE password, defined in the service's property drawer, to PASSWORD.

If PASSWORD is nil, then a random password is generated."
  (interactive
   (let* ((new-service (completing-read "Service: " (pm--get-all-headline-titles)))
	  (new-password (read-string "New password (leave blank for a random password of 20 characters): "))
	  (blacklist-chars (pm--get-property new-service :BLACKLIST)))
     (list new-service
	   (pm--process-new-password-entry new-password blacklist-chars))))
  (save-excursion
    (pm--goto-headline service)
    (org-set-property "password" password)
    (message "Password for '%s' set!" service)))

(defun pm-add-full-entry (service username password blacklist)
  "Add a fully populated username-password entry to the current Org buffer."
  (interactive
   (let* ((new-service (read-string "New service name: " ))
	  (new-username (read-string "Username for this service: "))
	  (new-password (read-string "New password (leave blank for a random password of 20 characters): "))
	  ;; NEW-PASSWORD being blank implies that a random password
	  ;; will be generated later. Hence, let's ask the user for
	  ;; any blacklisted characters for their random passwords.
	  (new-blacklist (when (string-blank-p new-password)
			   (read-string "Enter any blacklisted chars for password generation (leave blank if you don't want this): "))))
     (list new-service
	   new-username
	   (pm--process-new-password-entry new-password new-blacklist)
	   (if (or (null new-blacklist)
		   (string-blank-p new-blacklist))
	       nil
	     new-blacklist))))
  (let ((buffer-read-only nil))
    (save-excursion
      (goto-char (point-max))
      (newline)
      (insert (format "* %s" service))
      (org-set-property "username" username)
      (org-set-property "password" password)
      (when blacklist
	(org-set-property "blacklist" blacklist)))
    (save-buffer)))


(provide 'password-manager)

;; Local Variables:
;; read-symbol-shorthands: (("pm-" . "password-manager-"))
;; End:
