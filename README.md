1-way converter: `Google Contacts` → Wanderlust `Address` file
---------------------------------------------------------------

*Warning* : This emacs-lisp completely update your `Address`
file. Please backup before use this program.

Install/Usage
=============

* Put `google-contacts.el` and `google-contacts-wl.el` into load-path'ed
  direcotry, and byte-compile it if desired. Or use this [el-get recipe](https://github.com/uwabami/google-contacts-wl/blob/master/google-contacts-wl.rcp)

* Put following code into your wanderlust init file(`~/.wl`):

             
         ```lisp
         (require 'google-contacts-wl)
         ;; Optional
         ;; (setq google-contacts-email "Your GMail Address")
         ;; (setq google-contacts-password "Your GMail Password")
             

* run `M-x google-contacts-wl`         

Customize
=========

        (setq google-contacts-wl-ask nil)

License
=======

  * google-contacts.el: Public domain. please see [original file](https://github.com/uwabami/google-contacts-wl/blob/master/google-contacts.el) in detail
  * google-contacts-wl.el: WTFPL. see [this file](https://github.com/uwabami/google-contacts-wl/blob/master/WTFPL)

Special Thanks to:
==================

  * Takashi Masuda ([masutaka](https://github.com/masutaka/)): [google-contacts-mew](https://github.com/masutaka/google-contacts-mew/)
