;; google-contacts-wl.el
;; -*- coding: utf-8 -*-
;;
;; Author: Youhei SASAKI <uwabami@gfd-dennou.org>
;; License: WTFPL
;;
;; Install:
;;   Put this file into load-path'ed directory, and byte compile it if
;;   desired. And put the following expression into your ~/.wl
;;
;;    (require 'google-contacts-wl)
;;    ;; Optional
;;    ;; (setq google-contacts-email "Your GMail Address")
;;    ;; (setq google-contacts-password "Your GMail Password")
;;
;; Usage:
;;    M-x google-contacts-wl-update-address
;;
;;** BACKUP YOUR Wanderlust Address BEFORE TESTING THIS FUNCTION **
;;
;; Code:
;;
(require 'google-contacts)

(defvar google-contacts-wl-ask t
  "*If *non-nil*, ask whether or not you really renew your WL address.")

(defun google-contacts-wl-update-address ()
  "Overwrite `wl-address-file' from Google Contacts.

The format of Address file is as follows:
<address1>	<nickname>	<fullname>
<address2>	<nickname>	<fullname>

|--------------------+-------------------------|
| Wanderlust Address | google-contacts-retrive |
|--------------------+-------------------------|
| address1~n         | emails                  |
| nickname           | aka (if exists) or name |
| fullname           | name                    |
|--------------------+-------------------------|
"
  (interactive)
  (if (and google-contacts-wl-ask
           (not (y-or-n-p "Renew Your WL Address file? ")))
      (message "Your WL Address file is not renewed.")
    (condition-case err
        (let ((google-contacts-email
               (or google-contacts-email (read-string "GMail address: ")))
              (google-contacts-passwd
               (or google-contacts-passwd (read-passwd "GMail password: "))))
          (if (or (string= google-contacts-email "")
                  (string= google-contacts-passwd ""))
              (message "Your Gmail Address or Password are empty.")
            (with-temp-buffer
              (dolist (contact (google-contacts-retrieve))
                (let ((shortname (cdr (assoc 'aka contact)))
                      (emails (cadr (assoc 'emails contact)))
                      (name (cdr (assoc 'name contact))))
                  (when emails
                    (while emails
                      (insert (concat (car emails)) "\t")
                      (setq emails (cdr emails))
                      (if name (insert "\t\"" name "\"\t\"" name "\""))
                      (insert "\n"))
                    )
                  ))
              (write-region
               (point-min) (point-max)
               (expand-file-name wl-address-file)))))
      ;; (message "Your WL Address file is n.")
      (quit (message "Your WL Address file is updated!."))))
  ;; nil)))
  (set-file-modes (expand-file-name wl-address-file) 384)
  )

(provide 'google-contacts-wl)
