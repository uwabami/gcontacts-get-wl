1-way converter: `Google Contacts` → Wanderlust `Address` file
-------------------------------------------------------------

*Warning* : This emacs-lisp completely update your `Address`
file. Please backup before use this program.

Install/Usage
=============

* Put `google-contacts.el` and `google-contacts-wl.el` into load-path'ed
  direcotry, and byte-compile it if desired.
  * Or use [el-get recipe](hoge)

* Put following code into your wanderlust init file(`~/.wl`):

        ```emacs-lisp
         (require 'google-contacts-wl)
         ;; Optional
         ;; (setq google-contacts-email "Your GMail Address")
         ;; (setq google-contacts-password "Your GMail Password")


* run `M-x google-contacts-wl`         

Customize
=========

        ```emacs-lisp
        (setq google-contacts-wl-ask nil)

License
=======

  * google-contacts.el: Public domain. please see original file in detail
  * google-contacts-wl.el: WTFPL.

Special Thanks to:
==================

  * Takashi Masuda aka matsutaka: https://github.com/masutaka/google-contacts-mew        