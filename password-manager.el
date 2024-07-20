;; -*- lexical-binding: t; -*-

(require 'org-element)

;; Private functions

(defun pm--get-property (headline property)
  "Get PROPERTY from property drawer beneath HEADLINE."
  (save-excursion
    (pm--goto-headline)
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


(provide 'password-manager)

;; Local Variables:
;; read-symbol-shorthands: (("pm-" . "password-manager-"))
;; End:
