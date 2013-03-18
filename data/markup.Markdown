# Markdown

This wiki's pages are written in [pandoc]'s extended form of [markdown].
If you're not familiar with markdown, you should start by looking
at the [markdown "basics" page] and the [markdown syntax description].
Consult the [pandoc User's Guide] for information about pandoc's syntax
for footnotes, tables, description lists, and other elements not present
in standard markdown.

[pandoc]: http://johnmacfarlane.net/pandoc
[pandoc User's Guide]: http://johnmacfarlane.net/pandoc/README.html
[markdown]: http://daringfireball.net/projects/markdown
[markdown "basics" page]: http://daringfireball.net/projects/markdown/basics
[markdown syntax description]: http://daringfireball.net/projects/markdown/syntax 

## Wiki links

Links to other wiki pages are formed this way:  `[Page Name]()`.
(Gitit converts markdown links with empty targets into wikilinks.)

To link to a wiki page using something else as the link text:
`[something else](Page Name)`.

Note that page names may contain spaces and some special characters.
They need not be CamelCase.  CamelCase words are *not* automatically
converted to wiki links.

Wiki pages may be organized into directories.  So, if you have
several pages on wine, you may wish to organize them like so:

    Wine/Pinot Noir
    Wine/Burgundy
    Wine/Cabernet Sauvignon

Note that a wiki link `[Burgundy]()` that occurs inside the `Wine`
directory will link to `Wine/Burgundy`, and not to `Burgundy`.
To link to a top-level page called `Burgundy`, you'd have to use
`[Burgundy](/Burgundy)`.

To link to a directory listing for a subdirectory, use a trailing
slash: `[Wine/]()` will link to a listing of the `Wine` subdirectory.
