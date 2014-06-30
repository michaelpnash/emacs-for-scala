# ENSIME

ENhanced Scala Interaction Mode for [Emacs](http://www.gnu.org/software/emacs/).

This project provides the Emacs support for the ENSIME server and
currently shares the same issue tracker. This project is actively
community maintained, and we are very pleased to see contributions
from new members. Please visit the
[server's github page](/ensime/ensime-server)
to find out more about how you can help.

ENSIME brings IDE-like features to your favourite text editor, such as:

- Show the `type` of the symbol under the cursor.
- Contextual completion for `var`s, `val`s and `def`s.
- Add an import for the symbol under the cursor.
- Fast classpath search (types and members).
- Jump to source code or documentation.
- Browse packages and type hierarchies.
- Find all references to a symbol.
- Refactorings (rename, organize imports, extract method).
- REPL with stack trace highlighting.
- Errors and warnings in your code: *red squigglies*.
- Debugging

and many more.


# Quick Start

There are two ways to install this extension. You can use MELPA:

```elisp
;; if you're new to the MELPA package manager, this is how to add it
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)
;; and then do a M-x package-install RET "ensime" RET
```

**WARNING**: there is [a bug that means](https://github.com/ensime/ensime-server/issues/310)
byte compiled files from MELPA are not currently working. As a workaround, delete all your
elpa/ensime `.elc` files before loading ENSIME.

Or fork and clone this repository into a directory of your choice and
add it explicitly into your `~/.emacs`

```elisp
;; assuming you put the repository in ~/.emacs.d/ensime
(add-to-list 'load-path (concat user-emacs-directory "ensime"))
```


In either case, enable ensime with the following:

```elisp
(require 'ensime)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
```

Much of the actual text editing is provided by the excellent
[scala-mode2](https://github.com/hvesalai/scala-mode2), which can
be customised.


## Getting Started

A project needs to have a `.ensime` configuration file. Luckily we
have a [plugin for SBT projects](https://github.com/ensime/ensime-sbt/)
and [`maker` provides out-of-the-box support](https://github.com/cage433/maker),
which will automatically create an appropriate `.ensime` config.
We would love to receive a user-contributed maven or ant generator.

For best behaviour, the ENSIME server needs to be running the same
version of scala that your project uses. The default version of scala
is defined by `ensime-default-scala-version` but you can specify
a per-project version by setting, e.g. `:scala-version "2.9.3"` in
`.ensime`.

Then simply `M-x ensime` and point it at your project config.

The first time you run this command (for incompatible versions of scala), it will download the
relevant ENSIME server.
If the download fails (or freezes emacs, which can happen behind some firewalls)
then you can manually install the server following the instructions on
[the ENSIME server github page](https://github.com/ensime/ensime-server#quick-start).


Once the server is available, wait for the analyzer to complete and
enjoy editing with the ENSIME commands that are conveniently
summarised in our
[ENSIME Quick command reference](https://github.com/ensime/ensime-emacs/wiki/Quick-command-reference)
(or [read it straight from the source](https://github.com/ensime/ensime-emacs/blob/master/ensime.el#L393)).


Keeping up to date with releases is recommended. Melpa manages upgrading of
packages and if you're running from source you will need to
`git pull --rebase upstream master` regularly.

The server can also be upgraded very easily: we create a new binary release every time a
pull request is merged. Simply type `M-x ensime-upgrade-server`. Your old servers will
remain in your `.emacs.d/ensime-servers` directory.


## Further Information

Although the ENSIME's options are fully documented in the emacs
help pages (`C-h P RET "ensime"`), you may also wish to read the [ENSIME User
Manual](http://ensime.github.io/).

[Older releases](https://www.dropbox.com/sh/ryd981hq08swyqr/V9o9rDvxkS/ENSIME%20Releases)
are bundled with the server.
