emacs-for-scala
===============

My Emacs setup for Scala development, including a fully-configured Ensime.

The .emacs.d contained in this repo has a specific set of keybindings set up for Emacs. Mostly these are the defaults from whatever included modules have been used, but in a few cases they've been customized to play well together.

Please see the full list of keybindings at <a href="http://jglobal.com/emacs-for-scala-keybindings/" target="_new">http://jglobal.com/emacs-for-scala-keybindings/</a>

Please see <a href="http://jglobal.com/emacs-for-scala/" target="_new">http://jglobal.com/emacs-for-scala/</a> for a series of articles describing the Emacs config I've published here.

By default, the "emacs" launch script here is set up to launch Emacs in GUI mode, using the Emacs for OSX, and not to use client-server mode. If you want to use character-mode, add the param "-nw" to the startup script. This is useful if you want to use Emacs on a remote system, or to use it with Tmux or Screen.
  
For faster startup of Emacs, you can do two things: Compile the elisp, or use Emacs in client-server mode.

For client-server mode, put the scripts 'emacsserver' and 'emacsclient' found in this repo in your path (before any other emacs), and then run "emacs ." - if the server is not running, it will start it, then subsequent launches of emacs will be very quick.

The scripts are intended for OSX, but could easily be adapted.

-----------------------------------
# Keybindings

Shorthand | Press | On OSX
  --------|-------|---------
C-x | Control-x | Control-x
M-x |  Meta-x | Alt/Option-x
s-x | Super-x | Command-x

## Finding Files

Keys    | Description | Command
--------|-------------|-------
C-x C-f | Prompt to open file | ido-find-file
S-d     | Start dirtree, prompts for directory |       dirtree
C-x C-M-f |          Find file in project (current dir up to the .git directory) |          find-name-dired
 |          Find file by regex | find-grep-dired
C-x C-v |          Revert file to version on disk |          revert-buffer

## Movement

Keys | Description | Command
  ----|--------|-----
 C-n |          Next line |          next-line
M-f |          Forward a word |          forward-word
          M-b |          Backwards a word |          backward-word
          C-M-b |  Go to previous open brace/comma/bracket |    backward-sexp      
          C-M-f |   Go to next ending brace/comma/bracket  |          forward-sexp
          C-M-u |   Move up in parenthesis structure |  backward-up-list
          C-M-d |   Move down in parenthesis structure |          down-list
          M-< |  Beginning of buffer |          beginning-of-buffer
          M-> |          End of buffer |           end-of-buffer
          M-m |           Back to indendation |          back-to-indentation
          Shift-TAB |          Undent |          scala-indent:indent-with-reluctant-strategy
          C-u C-x TAB |          Indent region 4 spaces |          universal-argument indent-rigidly
          C-v |           Scroll up |          scroll-down-command
          C-k |          Kill remainder of line |        kill-line
          <f7> |           Kill whole line |          kill-whole-line
          C-a |           Beginning of line |          move-beginning-of-line
          C-e |           Ending of line |  move-end-of-line
          M-v |           Scroll down |           scroll-down-command
          C-l |           Goto line |           goto-line
          C-c SPACE |           Ace Jump to a word |          ace-jump-word-mode
          C-u C-c SPACE |           Ace Jump Character Mode |          ace-jump-char-mode
          C-u C-u C-c SPACE <i>OR</i> <f6>  |           Ace Jump Line Mode |          ace-jump-line-mode
          M-s } |           Jump to next } |           search-to-close-brace
          M-S } |           Jump to previous } |           search-to-prev-close-brace
          M-s { |           Jump to next { |           search-to-brace
          M-S { |  Jump to previous { |           search-to-prev-brace
          M-S d |  Jump to previous &#8220;def &#8220; |          search-to-next-def
          M-s d |           Jump to next &#8220;def &#8220; |           search-to-next-def
  C-c SPC | Ace Jump Mode | ace-jump-mode
  [f6] | Ace Jump Line Mode | ace-jump-line-mode
  C-x SPC | Ace Jump Mode Pop Mark | ace-jump-mode-pop-mark

## General
  
  Keys | Description | Command
  ----|--------|-----
M-s M-s OR S-s | Save all modified buffers | save-silently
C-u 3 |  Repeat next command 3 times | universal-argument
 |    Prompt for input mode (tex for unicode) |       toggle-input-mode
  M-t |   Transpose words| transpose-words
 C-x C-t |  Transpose lines |          transpose-lines
  C-t | Transpose characters | transpose-chars
 C-M-o | Open line at insertion point | split-line
 M-^ |  Join the current and previous line | delete-indentation
 C-x C-b | List all buffers |  list-buffers
 C-x k | Kill buffer | ido-kill-buffer
 C-h c | Show command run by given key sequence | describe-key-briefly
 C-x b |  Select another buffer |  ido-switch-buffer
 M-up | Move current line or selection up | move-text-up
 M-down | Move current line or selection down | move-text-down
 C-s | Start an incremental search forward. | isearch-forward
 C-r |  Start an incremental search backwards | isearch-backward
 C-w | Kill (Cut) | kill
 M-W | Kill Ring Save (Copy) | kill-ring-save
 C-y | Yank (Paste) | yank
M-^ | Delete Indentation | delete-indentation
  S-z | Undo | undo
  M-s { | Search to brace | search-to-brace
  M-S { | Search to prev brace | search-to-prev-brace
  M-s } | Search to close brace | search-to-close-brace
  M-S } | Search to previous close brace | search-to-prev-brace
  M-s d | Search to next def | search-to-next-def
  M-S d | Search to previous def | search-to-prev-def

## Ensime

[Full Ensime Manual](http://ensime.github.io/)

Keys | Description | Command
----|--------|-----
S-e | Start Ensime | ensime
C-c C-b b |  Rebuild entire project (clean build) | ensime-builder-build
 C-c C-b r | Rebuild project incrementally | ensime-builder-rebuild
 |  Reload the .ensime file and recompile the project | ensime-reload
 | Start the automatic configuration file generator | ensime-config-get
C-c C-v z |  Launch REPL| ensime-inf-switch
 C-c C-v i |  Launch type inspector on symbol under cursor (,=back .=forward) | ensime-inspect-type-at-point
   C-c C-v 5 i |  Launch type inspector on symbol under cursor in other frame |  ensime-inspect-type-at-point
  C-c C-v o |  Open the inspector on the current project&#8217;s main package |
        |  Open inspector on arbitrary type or package |  ensime-inspect-by-path
  C-s | Search through completion candidates |
  C-c C-o i |   Organize imports |  ensime-refactor-organize-imports
 C-c C-v v or s-o |   Search globally for methods or types | ensime-search
  C-c C-v . |  Select the surrounding syntactic context. Use . and , to grow and shrink selection |  ensime-expand-selection-command
 M-, |  Pop back to previously visited position |  ensime-pop-find-definition-stack
  M-. |  Jump to definition of symbol under cursor |  ensime-edit-definition
  C-c C-v i |  Inspect type under cursor (.=forward page ,=back page C-n/TAB=forward link C-p=backward link) | ensime-inspect-type-at-point
 TAB | Start completion |
 C-c C-v v |  Global type and method search (type uppercase for case-sensitive) | ensime-search
 C-c C-v a | Typecheck all files in the project | ensime-typecheck-all
  C-c C-v c |  Typecheck the current file |  ensime-typecheck-current-file
  C-c C-b s | Switch to SBT | ensime-sbt-switch
 C-c C-b s |  SBT do compile | ensime-sbt-do-compile
 C-c C-b n | SBT do clean | ensime-sbt-do-clean
 C-c C-b p | SBT do package |  ensime-sbt-do-package
 C-c C-v r | List all references to the symbol under the cursor. Find Usages | ensime-show-uses-of-symbol-at-point
 C-c C-v p | Inspect the package of the current source file. | ensime-inspect-package-at-point
 M-p | Go to the previous compilation note in the current buffer | ensime-backward-note
 M-n | Go to the next compilation note in current buffer |  ensime-forward-note
 C-c C-v u |  Undo a refactoring or formatting change | ensime-undo-peek
  C-c C-v f |  Format the current Scala file |  ensime-format-source
 C-c C-v e |  Show all errors and warnings in the project |  ensime-show-all-errors-and-warnings
  C-c C-v x | Scalex Documentation Search | ensime-scalex
  M-s M-t | Test-Only Current Buffer | scala-test-only
  M-s M-T OR S-t | Jump to test | jump-to-test
  S-n | Scala Find Name | scala-find-name
  S-N | Scala Find class/trait | scala-find-class
  
## Ensime Refactoring

  Keys | Description | Command
  ----|--------|-----
 C-c C-r t | Add import for type at point | ensime-import-type-at-point
 C-c C-r r | Rename | ensime-refactor-rename
 C-c C-r m | Extract method (C-space at beginning, move to end) | ensime-refactor-extract-method
  C-c C-r i | Inline Local | ensime-refactor-inline-local
 C-c C-r l | Extract local | ensime-refactor-extract-local
 C-c C-r o |  Organize imports |  ensime-refactor-organize-imports

## Ensime Debugger
  
    Keys | Description | Command
  ----|--------|-----
 C-c C-d d |  Start debugger | ensime-db-start
 C-c C-d b | Set breakpoint | ensime-db-set-break
 C-c C-d u |  Clear breakpoint |  ensime-db-clear-break
 C-c C-d s | Step |  ensime-db-step
  C-c C-d n | Step over | ensime-db-next
  C-c C-d o | Step out |  ensime-db-step-out
 C-c C-d r | Run | ensime-db-run
 C-c C-d c | Continue from breakpoint|  ensime-db-continue
  C-c C-d q |  Quit debug session | ensime-db-quit
 C-c C-d i | Inspect variable at cursor in debugger | ensime-db-inspect-value-at-point
  C-c C-d t |  Show backtrace in debugger | ensime-db-backtrace
 C-c C-d a |  Clear all breakpoints | ensime-db-clear-all-breaks

## Magit
  
 <a href="http://magit.github.io/magit/magit.html" target="_new">Magit User Manual</a>
  
  Keys | Description | Command
 ----|--------|-----  
     |     Enter magit mode, view status | magit-status
 S-TAB | Toggle visibility of current section |
 M-S | Show all sections |      
   l l |  Show history, short form |
  l L | Show history, more verbose form |
   s |  Move hunk into staging area |
   u | Move hunk out of staging area (unstage) |
   S | Move all hunks into staging |
  U | Move all hunks out of staging |
  c | Prompt for Commit message |      
   C-c C-c | Commit |
 P P | Push |      
 F F |  Pull |

## Ensime Sbt Window

      Keys | Description | Command
  ----|--------|-----    
 M-p | Previously entered command |
 M-n |  Next entered command |

## Shell Commands

      Keys | Description | Command
  ----|--------|-----  
 M-! | Execute a shell command |  shell-command
  M-| | Run shell command on region | shell-command-on-region
 C-u M-| |  Filter region through shell command |     
  |  Start shell in window *shell* | shell


## Org Mode


[Org-Mode Full Manual](http://orgmode.org/org.html)

    Keys | Description | Command
----|--------|-----  
 TAB |  Expand or contract current selection |
  <backtab> | Scala indent with reluctant strategy | scala-indent:indent-with-reluctant-strategy
 C-c C-t |  Rotate TODO state | org-todo
 s-T |  Show TODO list in prio order for current file | todo-agenda-current-file

## DirTree

         Keys | Description | Command
  ----|--------|-----  
   s-d | Start dirtree in it's own buffer |  dirtree
  D | Delete tree | tree-mode-delete-tree
   p |  Previous node | tree-mode-previous-node
   n |  Next node |  tree-mode-next-node
   j |  Next sibling |   tree-mode-next-sib
  k |   Previous sibling |   tree-mode-previous-sib
     C-r |  Search backwards |  tree-mode-isearch-backward
      C-s |   Search forward |   tree-mode-isearch-forward
     ! | Collapse other except |  tree-mode-collaps-other-except
  / |   Keep match | tree-mode-keep-match
    |          Start dirtree in this buffer |  dirtree-in-buffer
   s |  Sort by tag |   tree-mode-sort-by-tag
  e |  Toggle expand |  tree-mode-toggle-expand
  E |  Expand level |   tree-mode-expand-level
  g | Reflesh tree |    tree-mode-reflesh
 r |   Goto root |   tree-mode-goto-root
   u |  Goto parent |  tree-mode-got-parent

### Table Mode

[Sourceforge Page for Table Mode](http://table.sourceforge.net/)

       Keys | Description | Command
  ----|--------|-----  
  |         Insert new table |  table-insert
 C-+ |  Insert row | table-insert-row
  C-+ | Insert column | table-insert-column
 |  Delete row | table-delete-row
  |   Delete column |  table-delete-column
|     Recognize all table in current buffer | table-recognize
     |     Unrecognize tables in current buffer |    table-unrecognize
 |  Recognize in region | table-recognize-region
| Unrecognize in region | table-unrecognize-region
| Recognize single table |  table-recognize-table
| Unrecognize single table | table-unrecognize-table
|  Recognize a cell at current point | table-recognize-cell
|  Unrecognize cell at point |  table-unrecognize-cell
| Move forward to next Nth cell | table-forward-cell
|  Move previous Nth cell | table-backward-cell
 C-* | Span current cell in specified direction |  table-span-cell
 C&#8211; | Split current cell vertically | table-split-cell-vertically
 C-| |  Split current cell horizontally | table-split-cell-horizontally
 |  Split current cell vertically or horizontally | table-split-cell
  C-} |  Increase height of current cell | table-heighten-cell
  C-{ |  Decrease height of current cell | table-shorten-cell
 C-< |  Narrow current cell | table-narrow-cell
  C-> |  Widen current cell |  table-widen-cell
  C-! | Toggle fixed width mode |  table-fixed-width-mode
  C-# |  Compute and report current table dimension |  table-query-dimension
 C-^ |  Generate source in specified language andinsert into specified buffer | table-generate-source
  |   Travel forward inserting specified sequencein cells |  table-insert-sequence
 |     Convert plant text into table by capturingtext in the region | table-capture
 |  Convert table into plain text |  table-release
  C-: |  Justify contents of cells |  table-justify
|   Disable all table advice | table-disable-advice
 | Enable table advice |  table-enable-advice
|  Show version of table mode |  table-version
 TAB |  Move point to beginning of next cell |

## Window Commands

  Keys | Description | Command
  -----|--------|-----  
            C-x 2 |  Divide the current window horizontally in two |  split-window-horizontally
  [f2] | Split window vertically | split-window-vertically
  [f1] | Remove split | remove-split
 C-x 5 |  Divide the current window vertically in two. | split-windws-vertically
  C-x > |  Scroll the window right. |  scroll-right
  C-x < | Scroll the window left.|  scroll-left
 C-x 0 |  Delete the current window. | delete-window
 C-x 1 |  Delete all the windows except this one. | delete-other-windows
 |  Delete all windows open to a particular buff. |  delete-windows-on
 C-x ^ |  Make the current window taller. | enlarge-window
 |    Make the current window smaller. |  shrink-window
  C-x } |  Make the window wider. |  enlarge-window-horizontally
 C-x { |  Make the window less wide. |  shrink-window-horizontally
  M-C-v |  Scroll the other window. | scroll-other-window
 C-x 4 f | Find a file in the other window. | find-file-other-window
 C-x 4 b |  Select a buffer in the other window. |  switch-to-buffer-other-window
 |  Compare two buffers and show the first diff. | compare-windows

## Capitalization

    Keys | Description | Command
  -----|--------|-----  
  M-c |  Capitalize the first letter of current word. |  capitalize-word
  M-u |  Make the word all uppercase. |  upcase-word
 M-l |  Make the word all lowercase. |  downcase-word
 C-x C-l | Make the region all lowercase. |  downcase-region
  C-x C-u |  Make the region all uppercase. |  uppercase-region

## Getting Help

      Keys | Description | Command
  -----|--------|-----  
 C-h a | What commands work like this&#8230;? |  command-apropos
 |  What functions and variables work like this.? | apropos
  C-h c |  What command does this key sequence do? |  describe-key-briefly
  C-h b |  What are the key bindings for this buffer? |  describe-bindings
 C-h k |  What command does this sequence do,and tell me about it. |  describe-key
 C-h l |  What are the last 100 characters typed? | view-lossage
 C-h w |  What is the key binding for this? | where-is
  C-h f |   What does this function do? |  describe-function
  C-h v |   What is this variable? | describe-variable
 C-h m |  Tell me about this mode. |  describe-mode
 C-h s | What is the syntax table for this buffer? | describe-syntax

## Tmux

[Tmux Sourceforge Page](http://tmux.sourceforge.net/)

    Keys | Description | Command
  -----|--------|-----  
  C-b d |  Detach Session |
   C-b ? | Show Keybindings |
  tmux -S /tmp/xyz | Start session in file /tmp/xyz (must chmod to 777 to share) |
  tmux -S /tmp/xyz attach | Attach to an existing session in /tmp/xyz |

## Dired
  
[Dired Manual](http://www.gnu.org/software/emacs/manual/html_node/emacs/Dired.html#Dired)
    
     Keys | Description | Command
-----|--------|-----  
 C-x d |  Start dired in a specified directory (prompts for directory) |  dired
  C-n |   Next node |
  <space> |  Next node |
     n | Next node |
     C-p |  Previous node |
    <del> |  Move up and unflag |
       j | Prompt for filename, move to the line with that file name|  dired-goto-file
    i | Insert contents of directory at point |      dired-maybe-insert-subdir
   l |  Refresh directory contents | dired-do-redisplay
  ^ |  Move point to parent directory entry |
   $ |  Hide/Unhide subdirectory, leaving only header line visible |
   M-$ |  Hide/Unhide all subdirectories, leaving only header lines visible |
  C-M-n |  Go to next subdirectory header line, regardless of level | dired-next-subdir
 C-M-p | Go to previous subdirectory header line, regardless of level | dired-prev-subdir
 C-M-u | Go up to the parent directory&#8217;s header line | dired-tree-up
   C-M-d |  Go down in the directory tree, to the first subdirectory&#8217;s header line | dired-tree-down
   M-x image-dired | Enter image-dired mode, show thumbnails of images in this directory |
    C-t d |  Display thumbnails of marked images in this diredtory | image-dired-display-thumbs
  < | Move up to the previous directory-file line |   dired-prev-dirline
  > | Move down to the next directory-file line | dired-prev-dirline
  C-x C-q | Toggle WDired mode, [Dired Manual](http://www.gnu.org/software/emacs/manual/html_node/emacs/Wdired.html#Wdired) | dired-toggle-read-only
 d | Flag this file for deletion | dired-flag-file-deletion
  u |  Remove the deletion flag | dired-unmark
 <DEL> |  Move point to previous line and remove the deletion flag on that line | dired-unmark-backward
  x | Delete files flagged for deletion | dired-do-flagged-delete
  * % regexp <RET> |  Mark with a &#8216;*&#8217; all files whose name matches the specified regexp | dired-mark-files-regexp
  % g regexp <RET> |  Mark with a &#8216;*&#8217; all files whose <i>contents</i> match the specified regexp | dired-mark-files-containing-regexp
  C-/ |  Undo changes in the dired buffer |  dired-undo
  C |   Copy File |  dired-do-copy
  s-j |  Jump to dired of directory containing the current file | dired-jump
  D |  Delete file |  dired-do-delete
  R |  Rename file |  dired-do-rename
  M  Change mode of file | dired-do-chmod
  A regexp <RET> |  Search all the specified files for the regular expression regexp |  dired-do-search
  m | Mark current file/directory |
  u |  Unmark current file/directory
  U |  Unmark all marked in current directory |
  M&#8211;|  Toggle Tree View  |      [Dired Details on EmacsWiki](http://www.emacswiki.org/emacs/dired-details.el)
  ) | Show more details on each file/dir |
  ( | Show less details on each file/dir |

## Multiple Cursors

Keys | Description | Command
-----|--------|-----  
 C-S-c C-S-c | Multiple cursors on lines in active region | mc/edit-lines
 C-> | Mark next line like this |  mc/mark-next-like-this
 C-< | Mark previous line like this | mc/mark-previous-like-this
 C-c C-< |   Mark all like this (select a region to match first) | mc/mark-all-like-this
 C-g |  Cancel multiple cursor mode |
 RET | Exit multiple cursor mode |
 C-j | Insert a newline in multiple-cursor mode |
  
