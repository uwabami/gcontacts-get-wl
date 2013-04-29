1-way converter: `Google Contacts` → Wanderlust `Address` file
---------------------------------------------------------------

*Warning* : This emacs-lisp completely update your `Address`
file. Please backup before use this program.

Install/Usage
=============

* Put `google-contacts-wl.el` into load-path
  direcotry, and byte-compile it if desired. Or use this
  [el-get recipe](https://github.com/uwabami/google-contacts-wl/blob/master/google-contacts-wl.rcp)

* Put following code into your wanderlust init file(`~/.wl`):

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

  * google-contacts-wl.el: WTFPL. @see
    [this file](https://github.com/uwabami/google-contacts-wl/blob/master/WTFPL)
    * Original `google-contacts.el` and `google-contacts-mew.el` are
      Public Domain. Thus I set `google-concacts-wl.el` is under WTFPL.

ToDo
====

  * Support OAuth.

Special Thanks to:
==================

  * Øÿyvind Stegard
     ([ Øÿyvind Stegard](https://github.com/oyvindstegard)):
     [emacs-misc](https://github.com/oyvindstegard/emacs-misc/)
  * Takashi Masuda ([masutaka](https://github.com/masutaka/)):
    [google-contacts-mew](https://github.com/masutaka/google-contacts-mew/)
