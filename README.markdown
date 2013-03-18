gitit2
======

gitit2 is a rewrite of [gitit] using the [Yesod] web framework.
My intention is that it will eventually replace gitit.

[gitit]: http://gitit.net

To install:

    cabal install

If you run gitit2 from the source directory, a default
`wikidata` directory will be automatically created, based
on the settings in `settings.yaml`.  You will then be able
to view your wiki at <http://localhost:3000>.

The basic wiki functions have all been implemented. You can
create, edit, and modify pages.  You can search, view
history, and comment.  You can export in any format that
pandoc supports.

What is still missing?

* User login -- currently there is no access control.
* Preview in edit.
* Loading plugins (the plugin API works, but there is not
  yet support for specifying plugins in the config file).
* Page locking (no-delete, no-edit).
* Command-line option to specify a config file.
* Site initialization wizard.
* Documentation.
* Multiple options for treatment of math in HTML.
* Configurable session timeout.
* Option to show bird tracks in LHS.
* Log level options.
* Configurabel mime types file.
* 'absolute-urls' option for wikilinks?
* 'feed-refresh-time' option?
* Option to disable PDF export?
* Option for pandoc user data directory
* Option to disable sanitization?
* System for customizing CSS

[Yesod]: http://www.yesodweb.com/

