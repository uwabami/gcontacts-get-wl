;;; google-contacts-wl.el --- 1way converter GoogleContacts to Wanderlust Address
;; -*- mode:emacs-lisp; coding: utf-8 -*-
;;
;; Copyright(C) 2012-2014 Youhei SASAKI <uwabami@gfd-dennou.org>
;;
;; Author: Youhei SASAKI <uwabami@gfd-dennou.org>
;; Version: 0.0.3
;; Package-Requires: ((google-contacts "0.0"))
;; Keywords:
;; License: MIT/X11
;;
;;; Commentary:
;;
;; This file is NOT part of GNU Emacs.
;;
;; The `google-contacts-async-api' is defined at `google-contacts.el`,
;; written by Julien Danjou <julien@danjou.info>
;; @see https://julien.danjou.info/projects/emacs-packages#google-contacts
;;
;;** BACKUP YOUR Wanderlust Address BEFORE TESTING THIS FUNCTION **
;;
;;; Code:
;;
(require 'google-contacts)

(defvar google-contacts-wl-ask t
  "*If *non-nil*, ask whether or not you really renew your WL address.")

(unless wl-address-file
  (setq wl-address-file (expand-file-name "~/.address")))

;;;#autoload
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
    (google-contacts-async-api
     ""
     #'(lambda (contacts)
         (with-temp-buffer
           (dolist (contact contacts)
             (let* ((fullname (cdr (assq 'fullname contact)))
                    (nickname (cdr (assq 'nickname contact))))
               (dolist (email (cdr (assq 'emails contact)))
                 (insert (format "%s\t" (cdr email)))
                 (cond
                  ((not (string= "" nickname))
                   (insert (format "\"%s\"\t\"%s\"\n" fullname nickname)))
                  (t
                   (insert (format "\"%s\"\t\"%s\"\n" fullname fullname)))
                  ))))
           (write-region (point-min) (point-max) (expand-file-name wl-address-file) 0)))))
  )

(provide 'google-contacts-wl)

;;; google-contacts-wl.el ends here
