;;; gcontacts-wl.el --- 1-way converter GoogleContacts to Wanderlust Address file
;; -*- mode:emacs-lisp; coding: utf-8 -*-
;;
;; Copyright(C) 2012-2015 Youhei SASAKI <uwabami@gfd-dennou.org>
;; Author: Youhei SASAKI <uwabami@gfd-dennou.org>
;; Version: 0.0.6
;; Package-Requires: ((json "1.4") (oauth2 "0.10"))
;; Keywords: net, mail
;; License: WTFPL
;;
;;; Commentary:
;;
;; This file is NOT part of GNU Emacs.
;;
;; The `gcontacts-get' is written by Øyvind Stegard <oyvind.stegard@ifi.uio.no>.
;; @see https://github.com/oyvindstegard/emacs-misc
;; Original license is `Public Domain'.
;;
;; Install & Setup
;;
;;   Put this file into load-path'ed directory, and byte compile it if
;;   desired. And put the following expression into your ~/.wl
;;
;;    (require 'gcontacts-get-wl)
;;    (setq gcontacts-get-wl-oauth-client-ID "Your Client ID"
;;          gcontacts-get-wl-oauth-client-secret "Your Client Secret")
;;
;; Usage:
;;    M-x gcontacts-update-wl-address
;;
;; ** BACKUP YOUR Wanderlust Address BEFORE TESTING THIS FUNCTION **
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
(require 'oauth2)
(eval-when-compile
  (require 'cl))

;; OAuth2 constants
(defvar gcontacts-get-wl-oauth-client-ID nil
  "Set your oauth client ID")
(defvar gcontacts-get-wl-oauth-client-secret nil
  "Set your oauth client Secret")
(defvar gcontacts-get-wl-max-results 1000
  "Maximum number of contacts to fetch from Google.
   Should be some integer value higher than your total number of contacts.")
(defvar gcontacts-get-wl-system-group-names
  '((friends . "Friends")
    (coworkers . "Coworkers")
    (family . "Family"))
  "Alist mapping Google fixed system group ids to your preferred local name (string)")

(defvar gcontacts-get-wl-ask t
  "*If *non-nil*, ask whether or not you really renew your WL address.")

;; End of user variables.

(defconst gcontacts-get-wl-oauth-scope-uris "https://www.google.com/m8/feeds/")
(defconst gcontacts-get-wl-oauth-redirect-uri "urn:ietf:wg:oauth:2.0:oob")
(defconst gcontacts-get-wl-oauth-auth-uri "https://accounts.google.com/o/oauth2/auth")
(defconst gcontacts-get-wl-oauth-token-uri "https://accounts.google.com/o/oauth2/token")

(defun gcontacts-get-wl-url-retrieve (url session &optional extra-headers timeout)
  "Retrieves URL using whatever authentication that is
configured, optionally adding EXTRA-HEADERS to the request and
with timeout TIMEOUT (seconds). The variable SESSION should
satisfy `consp' and will be used to store session state between
calls to this function."
  (progn
    (let ((token (or (car session)
                     (setcar session
                             (oauth2-auth-and-store gcontacts-get-wl-oauth-auth-uri
                                                    gcontacts-get-wl-oauth-token-uri
                                                    gcontacts-get-wl-oauth-scope-uris
                                                    gcontacts-get-wl-oauth-client-ID
                                                    gcontacts-get-wl-oauth-client-secret)))))
      (if (integerp timeout)
          (with-timeout (timeout (error (concat "gcontacts-get: timed out requesting URL: " url)))
            (oauth2-url-retrieve-synchronously token url nil nil extra-headers))
        (oauth2-url-retrieve-synchronously token url nil nil extra-headers)))))

(defun gcontacts-get-wl-all-as-json (session)
  "Fetch all contacts from Google and return as a parsed JSON object (Lisp structure)"
  (let (json)
    (with-current-buffer
        (gcontacts-get-wl-url-retrieve
         (format "https://www.google.com/m8/feeds/contacts/default/full?alt=json&max-results=%d" gcontacts-get-wl-max-results) session '(("GData-Version" . "3.0")) 20) ;; Use GData version 3 to get nick-names
      (declare (special url-http-end-of-headers))
      (set-buffer-multibyte t)
      (decode-coding-region (1+ url-http-end-of-headers) (point-max) 'utf-8)
      (goto-char (1+ url-http-end-of-headers))
      (setq json (json-read-object))
      (set-buffer-modified-p nil)
      (kill-buffer (current-buffer)))
    json))

(defun gcontacts-get-wl-groups (session)
  (let (json)
    (with-current-buffer
        (gcontacts-get-wl-url-retrieve
         (format "https://www.google.com/m8/feeds/groups/default/full?alt=json&max-results=%d" gcontacts-get-wl-max-results) session '(("GData-Version" . "2.0")) 20) ;; Use GData version 2.0 to get system groups.                                                                    ;; They aren't returned with version 3.0, for some unknown reason.
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
                  (setq title (cdr (assoc 'friends gcontacts-get-wl-system-group-names))))
                 ((string-match "^System Group: Family" title)
                  (setq title (cdr (assoc 'family gcontacts-get-wl-system-group-names))))
                 ((string-match "^System Group: Coworkers" title)
                  (setq title (cdr (assoc 'coworkers gcontacts-get-wl-system-group-names)))))
               )
              (when title
                (setq groups-alist (cons (cons id title) groups-alist)))
              )
            )
      groups-alist)))

(defun gcontacts-get-wl-normalize-whitespace(s)
  (and s (replace-regexp-in-string "^ \\| $" "" (replace-regexp-in-string "[ \t]+" " " s))))

;; Used for finding common email address(es) between two sets/bags in contact
;; matching between Google contacts and BBDB.
(defun gcontacts-get-wl-intersection-ignore-case(list1 list2)
  (cond
   ((not list1) nil)
   ((member-ignore-case (car list1) list2)
    (cons (car list1) (gcontacts-get-wl-intersection-ignore-case (cdr list1) list2)))
   (t (gcontacts-get-wl-intersection-ignore-case (cdr list1) list2))))

;; Mappings from Google location type schema to symbol (used for addresses and phone numbers)
(defconst gcontacts-get-wl-location-schema-mapping
      '(("http://schemas.google.com/g/2005#main" . main)
        ("http://schemas.google.com/g/2005#work" . work)
        ("http://schemas.google.com/g/2005#home" . home)
        ("http://schemas.google.com/g/2005#mobile" . mobile)
        ("http://schemas.google.com/g/2005#other" . other)))

;; Get plain phone number structure from phone-node in JSON structure
(defun gcontacts-get-wl-get-phone-numbers(phone-node) ; expect vector of alists as cdr
  (when phone-node
    (map 'list
         (lambda(phone-number)
           (let ((number (cdr (assoc '$t phone-number)))
                 (location (cdr (assoc-string (cdr (assoc 'rel phone-number))
                                          gcontacts-get-wl-location-schema-mapping))))
             (when (not location) (setq location 'other))
             (cons location number)))
         (cdr phone-node))))

(defun gcontacts-get-wl-get-websites(website-node) ; expect vector of alists as cdr
  (when website-node
    (map 'list
         (lambda(website)
           (let ((rel (cdr (assoc 'rel website)))
                 (href (cdr (assoc 'href website))))
             (unless rel (setq rel "other"))
             (cons rel href))) (cdr website-node))))

;; Get plain address structure from address-node in JSON structure
(defun gcontacts-get-wl-get-addresses(address-node)
  (when address-node
    (map 'list
         (lambda(address)
           (let ((location (cdr (assoc-string (cdr (assoc 'rel address))
                                              gcontacts-get-wl-location-schema-mapping)))
                 (formatted-address (cdr (assoc '$t (assoc 'gd$formattedAddress address)))))
             (when (not location) (setq location 'other))
             (cons location formatted-address)))
         (cdr address-node))))

;; Get list of group names (strings) from group membership node in JSON structure
(defun gcontacts-get-wl-get-groups(group-membership-node groups-id-name-alist)
  (delete nil (mapcar (lambda(e)
                        (cdr (assoc-string (cdr (assoc 'href e)) groups-id-name-alist)))
                      (cdr group-membership-node))))

;; Get company name from organization-node in JSON structure
(defun gcontacts-get-wl-get-company (organization-node)
  (when organization-node
    (setq organization-node (cdr organization-node))
    (when (> (length organization-node) 0)
      (cdr (assoc '$t (assoc 'gd$orgName (aref organization-node 0)))))))

;; Retrieves contacts from GMail and returns simple Lisp structure.
(defun gcontacts-get ()
  "Returns Google contacts as alist"
  (let (google-contacts-alist
        groups-id-name-alist
        json
        (session (cons nil nil)))
    (setq json (gcontacts-get-wl-all-as-json session))
    (when (not json)
      (error "Could not retrieve contacts as JSON"))
    (setq groups-id-name-alist (gcontacts-get-wl-groups session))
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
                 (website-node (assoc 'gContact$website contactnode))

                 (name
                  (and title-node (gcontacts-get-wl-normalize-whitespace (cdr (assoc-string '$t title-node)))))
                 (emails
                  (and email-node (map 'list (lambda(e) (cdr (assoc-string 'address e))) (cdr email-node))))

                 contact addresses
                 phone-numbers company websites
                 nickname birthday groups notes)
            ;; Ignore all contacts with no emails and no name:
            (when (or emails (and name (> (length name) 0)))
              (setq phone-numbers (gcontacts-get-wl-get-phone-numbers phone-node))
              (setq company (gcontacts-get-wl-get-company organization-node))
              (setq websites (gcontacts-get-wl-get-websites website-node))
              (setq addresses (gcontacts-get-wl-get-addresses address-node))
              (setq nickname (cdr (assoc-string '$t nickname-node)))
              (setq birthday (cdr (assoc-string 'when birthday-node)))
              (setq groups (gcontacts-get-wl-get-groups group-membership-node groups-id-name-alist))
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
              (when websites
                (setq contact (cons (cons 'websites websites) contact)))
              (when groups
                (setq contact (cons (cons 'groups (list groups)) contact)))
              (when emails
                (setq contact (cons (cons 'emails (list emails)) contact)))
              (when (and name (> (length name) 0))
                (setq contact (cons (cons 'name name) contact)))

              (setq google-contacts-alist (cons contact google-contacts-alist)))))
    google-contacts-alist))

;;;###autoload
(defun gcontacts-update-wl-address ()
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
  (if (and gcontacts-get-wl-ask
           (not (y-or-n-p "Renew Your WL Address file? ")))
      (message "Your WL Address file is not renewed.")
    (with-temp-buffer
      (dolist (contact (gcontacts-get))
        (let ((emails (cadr (assq 'emails contact)))
              (fullname (cdr (assq 'name contact)))
              (nickname (cdr (assq 'aka contact))))
          ;; (substring firstemail 0 (string-match "@" firstemail))
          ;; firstemail))
          (dolist (email emails)
            (insert (format "%s\t" email))
            (cond
             ((not fullname)
              (insert (format "\"%s\"\t\"%s\"\n" email email)))
             ((and fullname nickname)
              (insert (format "\"%s\"\t\"%s\"\n" nickname fullname)))
             (t
              (insert (format "\"%s\"\t\"%s\"\n" fullname fullname)))))))
      (write-region
       (point-min) (point-max)
       (expand-file-name wl-address-file 0)))
    ))
(provide 'gcontacts-get-wl)
;;; gcontacts-wl.el ends here.
