1-way converter: `Google Contacts` → Wanderlust `Address` file
===============================================================

*Warning* : This emacs-lisp completely update your `Address` file. Please backup before use this program.

Install/Usage
-------------

* Put `gcontacts-get-wl.el` into load-path direcotry, and byte-compile it if desired. Or use this
  [el-get recipe](https://github.com/uwabami/google-contacts-wl/blob/master/google-contacts-wl.rcp)

* Create Your own Project, setup Contacts API and OAuth2 Access.
  * Go to [Google Developer Console](https://console.developers.google.com/project)
  * Create a project (with any name)
  * Click on the project
  * Click on *APIs & Auth* then *Credentials*
  * Click on *Create New Client ID* with Application type Installed application, Installed application type Other
  * Click on Create Client ID
  * Record the Client ID and Client secret for setup.
  * Under the same *APIs & Auth* menu section, select *APIs*
  * Scroll down to *Contacts API*. Click the *Status* button to enable calendar API access to the app you created in steps 5 & 6.

* Put following code into your wanderlust init file(`~/.wl`):

        (require 'gcontacts-get-wl)
        (setq gcontacts-get-wl-oauth-client-ID "Your Client ID"
              gcontacts-get-wl-oauth-client-secret "Your Client Secret")

* run `M-x gcontacts-update-wl-address`

License
-------

  * google-contacts-wl.el: WTFPL. @see
    [this file](https://github.com/uwabami/google-contacts-wl/blob/master/COPYRIGHT)
    * Original `gcontacts-get.el` and `google-contacts-mew.el` are
      Public Domain. Thus I set `google-concacts-wl.el` is under WTFPL.

Special Thanks to...
--------------------

  * Øÿyvind Stegard
     ([ Øÿyvind Stegard](https://github.com/oyvindstegard)):
     [emacs-misc](https://github.com/oyvindstegard/emacs-misc/)
  * Takashi Masuda ([masutaka](https://github.com/masutaka/)):
    [google-contacts-mew](https://github.com/masutaka/google-contacts-mew/)
