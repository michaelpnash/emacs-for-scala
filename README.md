emacs-for-scala
===============

My Emacs setup for Scala development, including a fully-configured Ensime.

The .emacs.d contained in this repo has a specific set of keybindings set up for Emacs. Mostly these are the defaults from whatever included modules have been used, but in a few cases they've been customized to play well together.

Please see the full list of keybindings at <a href="http://jglobal.com/emacs-for-scala-keybindings/" target="_new">http://jglobal.com/emacs-for-scala-keybindings/</a>

Please see <a href="http://jglobal.com/emacs-for-scala/" target="_new">http://jglobal.com/emacs-for-scala/</a> for a series of articles describing the Emacs config I've published here.

For faster startup of Emacs, you can do two things: Compile the elisp, or use Emacs in client-server mode.

For client-server mode, put the scripts 'emacsserver' and 'emacs' found in this repo in your path (before any other emacs), and then run "emacs ." - if the server is not running, it will start it, then subsequent launches of emacs will be very quick.

The scripts are intended for OSX, but could easily be adapted.

I've modified my keybindings to not use the "Super" key, so that text-mode emacs works without doing anything special within iTerm. For me, much of the power of Emacs is due to being able to combine it with Tmux and such, so text-mode is a big deal for me.

