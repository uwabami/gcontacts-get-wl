1-way converter: `Google Contacts` to  Wanderlust `Address` file
================================================================

*Warning* : This emacs-lisp completely update your `Address`
file. Please backup before use this program.

Install
-------

Install [google-contacts.el](https://julien.danjou.info/projects/emacs-packages#google-contacts) and Put `google-contacts-wl.el` into load-path directory, and byte-compile it if desired. 

If you use `el-get`, please check my [el-get recipe](https://github.com/uwabami/google-contacts-wl/blob/master/google-contacts-wl.rcp).

Usage
-----

* Put following code into your wanderlust init file(`~/.wl`):

         (require 'google-contacts-wl)
         ;; Optional: If non-nil, ask whether or not you really renew your WL address.
         ;; (setq google-contacts-wl-ask t)

* run `M-x google-contacts-wl`

License
=======

  * google-contacts-wl.el: MIT/X11. @see
    [this file](https://github.com/uwabami/google-contacts-wl/blob/master/COPYRIGHT)

Special Thanks to:
==================

* Julien Danjou ([jd (Julien Danjou)](https://github.com/jd/)):
  * [jd/google-contacts.el](https://github.com/jd/google-contacts.el/)

* Takashi Masuda ([masutaka](https://github.com/masutaka/)):
  * [google-contacts-mew](https://github.com/masutaka/google-contacts-mew/)
