1-way converter: `Google Contacts` → Wanderlust `Address` file
===============================================================

*Warning* : This emacs-lisp completely update your `Address` file. Please backup before use this program.

Install/Usage
-------------

* Put `gcontacts-get-wl.el` into load-path direcotry, and byte-compile it if desired. Or use this
  [el-get recipe](https://github.com/uwabami/google-contacts-wl/blob/master/google-contacts-wl.rcp)

* Put following code into your wanderlust init file(`~/.wl`):

        (require 'gcontacts-get-wl)
        ;; Optional - application login
        ;; (setq gcontacts-get-email "Your GMail Address")
        ;; (setq gcontacts-get-passwd "Your GMail Password")
        ;; Optional - use auth-source
        ;; (setq gcontacts-get-passwd-use-auth-source "somewhere")
        ;; Optional - use Oauth2  (Required package `oauth2')
        ;; (setq gcontacts-get-use-oauth2 t)
        ;; (setq gcontacts-get-oauth-client-ID nil)
        ;; (setq gcontacts-get-oauth-client-secret "Your Client Secret")

* run `M-x gcontacts-update-wl-address`

License
-------

  * google-contacts-wl.el: WTFPL. @see
    [this file](https://github.com/uwabami/google-contacts-wl/blob/master/WTFPL)
    * Original `gcontacts-get.el` and `google-contacts-mew.el` are
      Public Domain. Thus I set `google-concacts-wl.el` is under WTFPL.

Special Thanks to...
--------------------

  * Øÿyvind Stegard
     ([ Øÿyvind Stegard](https://github.com/oyvindstegard)):
     [emacs-misc](https://github.com/oyvindstegard/emacs-misc/)
  * Takashi Masuda ([masutaka](https://github.com/masutaka/)):
    [google-contacts-mew](https://github.com/masutaka/google-contacts-mew/)
