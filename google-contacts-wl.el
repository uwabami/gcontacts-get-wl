;;; google-contacts-wl.el --- 1way converter GoogleContacts to Wanderlust Address
;; -*- mode:emacs-lisp; coding: utf-8 -*-
;;
;; Copyright(C) 2012-2014 Youhei SASAKI <uwabami@gfd-dennou.org>
;; Author: Youhei SASAKI <uwabami@gfd-dennou.org>
;; Version: 0.0.2
;; Package-Requires: ((google-contacts))
;; Keywords:
;; License: WTFPL
;;
;;; Commentary:
;;
;; This file is NOT part of GNU Emacs.
;;
;; The `google-contacts-retrieve' is written by Øyvind Stegard
;; @see https://github.com/oyvindstegard/emacs-misc
;; Original license is `Public Domain'.
;;
;; Emacs (>= 24) recommended. I don't test Emacs <= 23 anymore.
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
;; Acknowledgements: This file is based on `google-contacts-mew'.
;;
;; I deeply thanks to Takashi Masuda
;;   https://github.com/masutaka/google-contacts-mew/
;;
;;; Code:
;;
(require 'auth-source)
(require 'json)
(require 'url)
(require 'cl)

(defvar google-contacts-email nil "GMail address")
(defvar google-contacts-passwd nil "GMail password")
(defvar google-contacts-wl-ask t
  "*If *non-nil*, ask whether or not you really renew your WL address.")
(defvar google-contacts-system-group-names
  '((friends . "Friends")
    (coworkers . "Coworkers")
    (family . "Family")) "Local names for Google fixed system groups")

(defun google-contacts-get-credentials ()
  (let (email passwd)
    (setq email google-contacts-email passwd google-contacts-passwd)
    (if (and email passwd)
        (cons email passwd)
      (error "Username[/password] not set or not found in auth source for host 'www.google.com'."))))

(defun google-contacts-clientlogin ()
  "Login to Google contacts service, obtain auth cookie which is returned as a string."
  (let* ((creds (google-contacts-get-credentials))
         (email-encoded (url-hexify-string (car creds)))
         (passwd-encoded (url-hexify-string (cdr creds)))
         (url-request-method "POST")
         (url-request-extra-headers '(("Content-type" . "application/x-www-form-urlencoded")))
         (url-request-data (concat "Email=" email-encoded "&Passwd=" passwd-encoded "&service=cp&source=Emacs"))
         auth-cookie)
    (with-current-buffer
        (with-timeout (10 (error "Google contacts: ClientLogin authentication took too long, aborting"))
          (url-retrieve-synchronously "https://www.google.com/accounts/ClientLogin"))
      (goto-char (point-min))
      (re-search-forward "^Auth=\\(.*\\)" nil t)
      (setq auth-cookie (match-string-no-properties 1))
      (set-buffer-modified-p nil)
      (kill-buffer (current-buffer)))
    auth-cookie))

(defun google-contacts-retrieve-all-as-json (auth-cookie)
  "Fetch all contacts from Google and return as a parsed JSON object (Lisp structure)"
  (let ((url-request-extra-headers (list (cons "Authorization" (concat "GoogleLogin auth=" auth-cookie))
                                         (cons "GData-Version" "3.0"))) ;; Use GData version 3 to get nick-names
        json)

    (with-current-buffer
        (with-timeout (10 (error "Google contacts: connection or transfer to took too long, aborting"))
          (url-retrieve-synchronously "http://www.google.com/m8/feeds/contacts/default/full?alt=json&max-results=1000"))
      (declare (special url-http-end-of-headers))
      (set-buffer-multibyte t)
      (decode-coding-region (1+ url-http-end-of-headers) (point-max) 'utf-8)
      (goto-char (1+ url-http-end-of-headers))
      (setq json (json-read-object))
      (set-buffer-modified-p nil)
      (kill-buffer (current-buffer)))
    json))

(defun google-contacts-retrieve-groups (auth-cookie)
  (let ((url-request-extra-headers (list (cons "Authorization" (concat "GoogleLogin auth=" auth-cookie))
                                         (cons "GData-Version" "2.0"))) ;; Use GData version 2.0 to get system groups.
                                                                      ;; They aren't returned with version 3.0, for some unknown reason.
        json)
    (with-current-buffer
        (with-timeout (10 (error "Google contacts: connection or transfer to took too long, aborting."))
          (url-retrieve-synchronously "http://www.google.com/m8/feeds/groups/default/full?alt=json&max-results=1000"))
      (declare (special url-http-end-of-headers))
      (set-buffer-multibyte t)
      (decode-coding-region (1+ url-http-end-of-headers) (point-max) 'utf-8)
      (goto-char (1+ url-http-end-of-headers))
      (setq json (json-read-object))
      (set-buffer-modified-p nil)
      (kill-buffer (current-buffer)))
    (when (not json)
        (error "Could not retrieve contact groups as JSON"))
    (let ((entries (cdr (assoc-string 'entry (assoc-string 'feed json))))
          groups-alist
          entrynode)
      (loop for entrynode across entries
            do
            (let ((title (cdr (assoc-string "$t" (cdr (assoc-string "title" entrynode)))))
                  (id (cdr (assoc-string "$t" (cdr (assoc-string "id" entrynode)))))
                  )
              (when (string-match "^System Group:" title)
                (cond
                 ((string-match "^System Group: My Contacts" title)
                  (setq title nil))
                 ((string-match "^System Group: Friends" title)
                  (setq title (cdr (assoc 'friends google-contacts-system-group-names))))
                 ((string-match "^System Group: Family" title)
                  (setq title (cdr (assoc 'family google-contacts-system-group-names))))
                 ((string-match "^System Group: Coworkers" title)
                  (setq title (cdr (assoc 'coworkers google-contacts-system-group-names))))
                 nil)
               )
              (when title
                (setq groups-alist (cons (cons id title) groups-alist)))
              )
            )
      groups-alist)))

(defun google-contacts-normalize-whitespace(s)
  (and s (replace-regexp-in-string "^ \\| $" "" (replace-regexp-in-string "[ \t]+" " " s))))

;; Used for finding common email address(es) between two sets/bags in contact
;; matching between Google contacts and BBDB.
(defun google-contacts-intersection-ignore-case(list1 list2)
  (cond
   ((not list1) nil)
   ((member-ignore-case (car list1) list2)
    (cons (car list1) (google-contacts-intersection-ignore-case (cdr list1) list2)))
   (t (google-contacts-intersection-ignore-case (cdr list1) list2))))

;; Mappings from Google location type schema to symbol (used for addresses and phone numbers)
(setq google-contacts-location-schema-mapping
      '(("http://schemas.google.com/g/2005#main" . main)
        ("http://schemas.google.com/g/2005#work" . work)
        ("http://schemas.google.com/g/2005#home" . home)
        ("http://schemas.google.com/g/2005#mobile" . mobile)
        ("http://schemas.google.com/g/2005#other" . other)))

;; Get plain phone number structure from phone-node in JSON structure
(defun google-contacts-get-phone-numbers(phone-node)
  (when phone-node
    (map 'list
         (lambda(phone-number)
           (let ((number (cdr (assoc '$t phone-number)))
                 (location (cdr (assoc-string (cdr (assoc 'rel phone-number))
                                          google-contacts-location-schema-mapping))))
             (when (not location) (setq location 'other))
             (cons location number)))
         (cdr phone-node))))

;; Get plain address structure from address-node in JSON structure
(defun google-contacts-get-addresses(address-node)
  (when address-node
    (map 'list
         (lambda(address)
           (let ((location (cdr (assoc-string (cdr (assoc 'rel address))
                                          google-contacts-location-schema-mapping)))
                 (formatted-address (cdr (assoc '$t (assoc 'gd$formattedAddress address)))))
             (when (not location) (setq location 'other))
             (cons location formatted-address)))
         (cdr address-node))))

;; Get list of group names (strings) from group membership node in JSON structure
(defun google-contacts-get-groups(group-membership-node groups-id-name-alist)
  (delete nil (mapcar (lambda(e)
                        (cdr (assoc-string (cdr (assoc 'href e)) groups-id-name-alist)))
                      (cdr group-membership-node))))

;; Get company name from organization-node in JSON structure
(defun google-contacts-get-company (organization-node)
  (when organization-node
    (setq organization-node (cdr organization-node))
    (when (> (length organization-node) 0)
      (cdr (assoc '$t (assoc 'gd$orgName (aref organization-node 0)))))))

;; Retrieves contacts from GMail and returns simple Lisp structure.
(defun google-contacts-retrieve ()
  "Returns Google contacts as alist"
  (let ((auth-cookie (google-contacts-clientlogin))
        google-contacts-alist
        groups-id-name-alist
        json)
    (when auth-cookie
      (setq json (google-contacts-retrieve-all-as-json auth-cookie))
      (when (not json)
        (error "Could not retrieve contacts as JSON"))
      (setq groups-id-name-alist (google-contacts-retrieve-groups auth-cookie))
      (setq json (cdr (car (cdr (car json))))) ;; Untangle top structural elements of JSON object
      (loop for contactnode across json
            do
            (let* ((email-node (assoc 'gd$email contactnode))
                   (title-node (assoc 'title contactnode)) ; consider using gd$fullName node instead
                   (address-node (assoc 'gd$structuredPostalAddress contactnode))
                   (phone-node (assoc 'gd$phoneNumber contactnode))
                   (organization-node (assoc 'gd$organization contactnode))
                   (nickname-node (assoc 'gContact$nickname contactnode))
                   (birthday-node (assoc 'gContact$birthday contactnode))
                   (group-membership-node (assoc 'gContact$groupMembershipInfo contactnode))
                   (content-node (assoc 'content contactnode)) ; contact notes-field

                   (name
                    (and title-node (google-contacts-normalize-whitespace (cdr (assoc-string '$t title-node)))))
                   (emails
                    (and email-node (map 'list (lambda(e) (cdr (assoc-string 'address e))) (cdr email-node))))

                   contact addresses
                   phone-numbers company
                   nickname birthday groups notes)
              ;; Ignore all contacts with no emails and no name:
              (when (or emails (and name (> (length name) 0)))
                (setq phone-numbers (google-contacts-get-phone-numbers phone-node))
                (setq company (google-contacts-get-company organization-node))
                (setq addresses (google-contacts-get-addresses address-node))
                (setq nickname (cdr (assoc-string '$t nickname-node)))
                (setq birthday (cdr (assoc-string 'when birthday-node)))
                (setq groups (google-contacts-get-groups group-membership-node groups-id-name-alist))
                (setq notes (cdr (assoc '$t (cdr content-node))))
                (when (and nickname (> (length nickname) 0))
                  (setq contact (cons (cons 'aka nickname) contact)))
                (when birthday
                  (setq contact (cons (cons 'birthday birthday) contact)))
                (when notes
                  ;; HTC Android phones store additional tagged metadata in the notes field, strip that away.
                  (setq notes (replace-regexp-in-string "<HTCData>\\(.\\|\n\\)*?</HTCData>" "" notes))
                  (setq notes (replace-regexp-in-string "\\`\\( \\|\n\\|\t\\)+\\|\\( \\|\n\\|\t\\)+\\'" "" notes))
                  (when (> (length notes) 0)
                    (setq contact (cons (cons 'notes notes) contact))))
                (when addresses
                  (setq contact (cons (cons 'formatted-addresses addresses) contact)))
                (when phone-numbers
                  (setq contact (cons (cons 'phone-numbers phone-numbers) contact)))
                (when company
                  (setq contact (cons (cons 'company company) contact)))
                (when groups
                  (setq contact (cons (cons 'groups (list groups)) contact)))
                (when emails
                  (setq contact (cons (cons 'emails (list emails)) contact)))
                (when (and name (> (length name) 0))
                  (setq contact (cons (cons 'name name) contact)))

                (setq google-contacts-alist (cons contact google-contacts-alist)))))
      google-contacts-alist)))

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
                      (cond
                       ((and name shortname)
                        (insert "\t\"" shortname "\"\t\"" name "\""))
                       (name
                        (insert "\t\"" name "\"\t\"" name "\"")))
                      (insert "\n"))
                    )
                  ))
              (write-region
               (point-min) (point-max)
               (expand-file-name wl-address-file)))))
      (quit (message "Your WL Address file is updated!."))))
  (set-file-modes (expand-file-name wl-address-file) 384)
  )


(provide 'google-contacts-wl)
;;; google-contacts-wl.el ends here
