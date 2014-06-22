emacs-for-scala
===============

My Emacs setup for Scala development.

The .emacs.d contained in this repo has a specific set of keybindings set up for Emacs. Mostly these are the defaults from whatever included modules have been used, but in a few cases they've been customized to play well together.

Please see the full list of keybindings at <a href="http://jglobal.com/emacs-for-scala-keybindings/" target="_new">http://jglobal.com/emacs-for-scala-keybindings/</a>

Please see <a href="http://jglobal.com/emacs-for-scala/" target="_new">http://jglobal.com/emacs-for-scala/</a> for a series of articles describing the Emacs config I've published here.

For faster startup of Emacs, you can do two things: Compile the elisp, or use Emacs in client-server mode.

For client-server mode, use the script emacsserver.sh once at startup to launch the background server, then use the script "emacs" to launch the client. Remove the "-nw" if you want GUI mode emacs for some reason.

