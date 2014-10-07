emacs-for-scala
===============

My Emacs setup for Scala development, including a fully-configured Ensime.

The .emacs.d contained in this repo has a specific set of keybindings set up for Emacs. Mostly these are the defaults from whatever included modules have been used, but in a few cases they've been customized to play well together.

Please see the full list of keybindings at <a href="http://jglobal.com/emacs-for-scala-keybindings/" target="_new">http://jglobal.com/emacs-for-scala-keybindings/</a>

Please see <a href="http://jglobal.com/emacs-for-scala/" target="_new">http://jglobal.com/emacs-for-scala/</a> for a series of articles describing the Emacs config I've published here.

For faster startup of Emacs, you can do two things: Compile the elisp, or use Emacs in client-server mode.

For client-server mode, put the scripts 'emacsserver' and 'emacs' found in this repo in your path (before any other emacs), and then run "emacs ." - if the server is not running, it will start it, then subsequent launches of emacs will be very quick.

The scripts are intended for OSX, but could easily be adapted.

-----------------------------------
# Keybindings

Shorthand | Press | On OSX
  --------|-------|---------
C-x | Control-x | Control-x
M-x |  Meta-x | Alt/Option-x
s-x | Super-x | Command-x

## Finding Files

Keys | Description | Command
-----|-------------|-------
C-x C-f | Prompt to open file | ido-find-file
 |        Start dirtree, prompts for directory |       dirtree
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

## General
  
  Keys | Description | Command
  ----|--------|-----
M-s M-s | Save all modified buffers | save-silently
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

## Ensime
  
      <a href="http://ensime.github.io/" target="_new">Full Ensime Manual</a>

  Keys | Description | Command
  ----|--------|-----
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
      
      <tr>
        <td align="left" valign="top">
          P P
        </td>
        
        <td align="left" valign="top">
          Push
        </td>
        
        <td align="left" valign="top">
        </td>
      </tr>
      
      <tr>
        <td align="left" valign="top">
          F F
        </td>
        
        <td align="left" valign="top">
          Pull
        </td>
        
        <td align="left" valign="top">
        </td>
      </tr>
    </table>
  </div></p>
</div>

<div class="clear">
</div>

<p class="trigger ">
  <a href="#toggle_153406473753f1f8e6979a1">Ensime Sbt Window</a>
</p>

<div class="toggle_container" style="display:none;">
  <div class="block">
    <table border="1">
      <tr>
        <td align="left" valign="top">
          M-p
        </td>
        
        <td align="left" valign="top">
          Previously entered command
        </td>
        
        <td align="left" valign="top">
        </td>
      </tr>
      
      <tr>
        <td align="left" valign="top">
          M-n
        </td>
        
        <td align="left" valign="top">
          Next entered command
        </td>
        
        <td align="left" valign="top">
        </td>
      </tr>
    </table>
  </div></p>
</div>

<div class="clear">
</div>

<p class="trigger ">
  <a href="#toggle_179736096853f1f8e697a6d">Shell Commands</a>
</p>

<div class="toggle_container" style="display:none;">
  <div class="block">
    <table border="1">
      <tr>
        <td align="left" valign="top">
          M-!
        </td>
        
        <td align="left" valign="top">
          Execute a shell command
        </td>
        
        <td align="left" valign="top">
          shell-command
        </td>
      </tr>
      
      <tr>
        <td align="left" valign="top">
          M-|
        </td>
        
        <td align="left" valign="top">
          Run shell command on region
        </td>
        
        <td align="left" valign="top">
          shell-command-on-region
        </td>
      </tr>
      
      <tr>
        <td align="left" valign="top">
          C-u M-|
        </td>
        
        <td align="left" valign="top">
          Filter region through shell command
        </td>
        
        <td align="left" valign="top">
        </td>
      </tr>
      
      <tr>
        <td align="left" valign="top">
        </td>
        
        <td align="left" valign="top">
          Start shell in window *shell*
        </td>
        
        <td align="left" valign="top">
          shell
        </td>
      </tr>
    </table>
  </div></p>
</div>

<div class="clear">
</div>

<p class="trigger ">
  <a href="#toggle_12896647053f1f8e697b39">Org Mode</a>
</p>

<div class="toggle_container" style="display:none;">
  <div class="block">
    <p>
      <a href="http://orgmode.org/org.html" target="_new">Org-Mode Full Manual</a>
    </p>
    
    <table border="1">
      <tr>
        <td align="left" valign="top">
          TAB
        </td>
        
        <td align="left" valign="top">
          Expand or contract current selection
        </td>
        
        <td align="left" valign="top">
        </td>
      </tr>
      
      <tr>
        <td align="left" valign="top">
          C-c C-t
        </td>
        
        <td align="left" valign="top">
          Rotate TODO state
        </td>
        
        <td align="left" valign="top">
          org-todo
        </td>
      </tr>
      
      <tr>
        <td align="left" valign="top">
          s-T
        </td>
        
        <td align="left" valign="top">
          Show TODO list in prio order for current file
        </td>
        
        <td align="left" valign="top">
          todo-agenda-current-file
        </td>
      </tr>
    </table>
  </div></p>
</div>

<div class="clear">
</div>

<p class="trigger ">
  <a href="#toggle_16351606253f1f8e697c06">DirTree</a>
</p>

<div class="toggle_container" style="display:none;">
  <div class="block">
    <table border="1">
      <tr>
        <td align="left" valign="top">
          s-d
        </td>
        
        <td align="left" valign="top">
          Start dirtree in it&#8217;s own buffer
        </td>
        
        <td align="left" valign="top">
          dirtree
        </td>
      </tr>
      
      <tr>
        <td align="left" valign="top">
          D
        </td>
        
        <td align="left" valign="top">
          Delete tree
        </td>
        
        <td align="left" valign="top">
          tree-mode-delete-tree
        </td>
      </tr>
      
      <tr>
        <td align="left" valign="top">
          p
        </td>
        
        <td align="left" valign="top">
          Previous node
        </td>
        
        <td align="left" valign="top">
          tree-mode-previous-node
        </td>
      </tr>
      
      <tr>
        <td align="left" valign="top">
          n
        </td>
        
        <td align="left" valign="top">
          Next node
        </td>
        
        <td align="left" valign="top">
          tree-mode-next-node
        </td>
      </tr>
      
      <tr>
        <td align="left" valign="top">
          j
        </td>
        
        <td align="left" valign="top">
          Next sibling
        </td>
        
        <td align="left" valign="top">
          tree-mode-next-sib
        </td>
      </tr>
      
      <tr>
        <td align="left" valign="top">
          k
        </td>
        
        <td align="left" valign="top">
          Previous sibling
        </td>
        
        <td align="left" valign="top">
          tree-mode-previous-sib
        </td>
      </tr>
      
      <tr>
        <td align="left" valign="top">
          C-r
        </td>
        
        <td align="left" valign="top">
          Search backwards
        </td>
        
        <td align="left" valign="top">
          tree-mode-isearch-backward
        </td>
      </tr>
      
      <tr>
        <td align="left" valign="top">
          C-s
        </td>
        
        <td align="left" valign="top">
          Search forward
        </td>
        
        <td align="left" valign="top">
          tree-mode-isearch-forward
        </td>
      </tr>
      
      <tr>
        <td align="left" valign="top">
          !
        </td>
        
        <td align="left" valign="top">
          Collapse other except
        </td>
        
        <td align="left" valign="top">
          tree-mode-collaps-other-except
        </td>
      </tr>
      
      <tr>
        <td align="left" valign="top">
          /
        </td>
        
        <td align="left" valign="top">
          Keep match
        </td>
        
        <td align="left" valign="top">
          tree-mode-keep-match
        </td>
      </tr>
      
      <tr>
        <td align="left" valign="top">
        </td>
        
        <td align="left" valign="top">
          Start dirtree in this buffer
        </td>
        
        <td align="left" valign="top">
          dirtree-in-buffer
        </td>
      </tr>
      
      <tr>
        <td align="left" valign="top">
          s
        </td>
        
        <td align="left" valign="top">
          Sort by tag
        </td>
        
        <td align="left" valign="top">
          tree-mode-sort-by-tag
        </td>
      </tr>
      
      <tr>
        <td align="left" valign="top">
          e
        </td>
        
        <td align="left" valign="top">
          Toggle expand
        </td>
        
        <td align="left" valign="top">
          tree-mode-toggle-expand
        </td>
      </tr>
      
      <tr>
        <td align="left" valign="top">
          E
        </td>
        
        <td align="left" valign="top">
          Expand level
        </td>
        
        <td align="left" valign="top">
          tree-mode-expand-level
        </td>
      </tr>
      
      <tr>
        <td align="left" valign="top">
          g
        </td>
        
        <td align="left" valign="top">
          Reflesh tree
        </td>
        
        <td align="left" valign="top">
          tree-mode-reflesh
        </td>
      </tr>
      
      <tr>
        <td align="left" valign="top">
          r
        </td>
        
        <td align="left" valign="top">
          Goto root
        </td>
        
        <td align="left" valign="top">
          tree-mode-goto-root
        </td>
      </tr>
      
      <tr>
        <td align="left" valign="top">
          u
        </td>
        
        <td align="left" valign="top">
          Goto parent
        </td>
        
        <td align="left" valign="top">
          tree-mode-got-parent
        </td>
      </tr>
    </table>
  </div></p>
</div>

<div class="clear">
</div>

<p class="trigger ">
### Table Mode
      <a href="http://table.sourceforge.net/" target="_new">Sourceforge Page for Table Mode</a>

        <tr>
        <td align="left" valign="top">
        </td>
        
        <td align="left" valign="top">
          Insert new table
        </td>
        
        <td align="left" valign="top">
          table-insert
        </td>
      </tr>
      
      <tr>
        <td align="left" valign="top">
          C-+
        </td>
        
        <td align="left" valign="top">
          Insert row
        </td>
        
        <td align="left" valign="top">
          table-insert-row
        </td>
      </tr>
      
      <tr>
        <td align="left" valign="top">
          C-+
        </td>
        
        <td align="left" valign="top">
          Insert column
        </td>
        
        <td align="left" valign="top">
          table-insert-column
        </td>
      </tr>
      
      <tr>
        <td align="left" valign="top">
        </td>
        
        <td align="left" valign="top">
          Delete row
        </td>
        
        <td align="left" valign="top">
          table-delete-row
        </td>
      </tr>
      
      <tr>
        <td align="left" valign="top">
        </td>
        
        <td align="left" valign="top">
          Delete column
        </td>
        
        <td align="left" valign="top">
          table-delete-column
        </td>
      </tr>
      
      <tr>
        <td align="left" valign="top">
        </td>
        
        <td align="left" valign="top">
          Recognize all table in current buffer
        </td>
        
        <td align="left" valign="top">
          table-recognize
        </td>
      </tr>
      
      <tr>
        <td align="left" valign="top">
        </td>
        
        <td align="left" valign="top">
          Unrecognize tables in current buffer
        </td>
        
        <td align="left" valign="top">
          table-unrecognize
        </td>
      </tr>
      
      <tr>
        <td align="left" valign="top">
        </td>
        
        <td align="left" valign="top">
          Recognize in region
        </td>
        
        <td align="left" valign="top">
          table-recognize-region
        </td>
      </tr>
      
      <tr>
        <td align="left" valign="top">
        </td>
        
        <td align="left" valign="top">
          Unrecognize in region
        </td>
        
        <td align="left" valign="top">
          table-unrecognize-region
        </td>
      </tr>
      
      <tr>
        <td align="left" valign="top">
        </td>
        
        <td align="left" valign="top">
          Recognize single table
        </td>
        
        <td align="left" valign="top">
          table-recognize-table
        </td>
      </tr>
      
      <tr>
        <td align="left" valign="top">
        </td>
        
        <td align="left" valign="top">
          Unrecognize single table
        </td>
        
        <td align="left" valign="top">
          table-unrecognize-table
        </td>
      </tr>
      
      <tr>
        <td align="left" valign="top">
        </td>
        
        <td align="left" valign="top">
          Recognize a cell at current point
        </td>
        
        <td align="left" valign="top">
          table-recognize-cell
        </td>
      </tr>
      
      <tr>
        <td align="left" valign="top">
        </td>
        
        <td align="left" valign="top">
          Unrecognize cell at point
        </td>
        
        <td align="left" valign="top">
          table-unrecognize-cell
        </td>
      </tr>
      
      <tr>
        <td align="left" valign="top">
        </td>
        
        <td align="left" valign="top">
          Move forward to next Nth cell
        </td>
        
        <td align="left" valign="top">
          table-forward-cell
        </td>
      </tr>
      
      <tr>
        <td align="left" valign="top">
        </td>
        
        <td align="left" valign="top">
          Move previous Nth cell
        </td>
        
        <td align="left" valign="top">
          table-backward-cell
        </td>
      </tr>
      
      <tr>
        <td align="left" valign="top">
          C-*
        </td>
        
        <td align="left" valign="top">
          Span current cell in specified direction
        </td>
        
        <td align="left" valign="top">
          table-span-cell
        </td>
      </tr>
      
      <tr>
        <td align="left" valign="top">
          C&#8211;
        </td>
        
        <td align="left" valign="top">
          Split current cell vertically
        </td>
        
        <td align="left" valign="top">
          table-split-cell-vertically
        </td>
      </tr>
      
      <tr>
        <td align="left" valign="top">
          C-|
        </td>
        
        <td align="left" valign="top">
          Split current cell horizontally
        </td>
        
        <td align="left" valign="top">
          table-split-cell-horizontally
        </td>
      </tr>
      
      <tr>
        <td align="left" valign="top">
        </td>
        
        <td align="left" valign="top">
          Split current cell vertically or horizontally
        </td>
        
        <td align="left" valign="top">
          table-split-cell
        </td>
      </tr>
      
      <tr>
        <td align="left" valign="top">
          C-}
        </td>
        
        <td align="left" valign="top">
          Increase height of current cell
        </td>
        
        <td align="left" valign="top">
          table-heighten-cell
        </td>
      </tr>
      
      <tr>
        <td align="left" valign="top">
          C-{
        </td>
        
        <td align="left" valign="top">
          Decrease height of current cell
        </td>
        
        <td align="left" valign="top">
          table-shorten-cell
        </td>
      </tr>
      
      <tr>
        <td align="left" valign="top">
          C-<
        </td>
        
        <td align="left" valign="top">
          Narrow current cell
        </td>
        
        <td align="left" valign="top">
          table-narrow-cell
        </td>
      </tr>
      
      <tr>
        <td align="left" valign="top">
          C->
        </td>
        
        <td align="left" valign="top">
          Widen current cell
        </td>
        
        <td align="left" valign="top">
          table-widen-cell
        </td>
      </tr>
      
      <tr>
        <td align="left" valign="top">
          C-!
        </td>
        
        <td align="left" valign="top">
          Toggle fixed width mode
        </td>
        
        <td align="left" valign="top">
          table-fixed-width-mode
        </td>
      </tr>
      
      <tr>
        <td align="left" valign="top">
          C-#
        </td>
        
        <td align="left" valign="top">
          Compute and report current table dimension
        </td>
        
        <td align="left" valign="top">
          table-query-dimension
        </td>
      </tr>
      
      <tr>
        <td align="left" valign="top">
          C-^
        </td>
        
        <td align="left" valign="top">
          Generate source in specified language andinsert into specified buffer
        </td>
        
        <td align="left" valign="top">
          table-generate-source
        </td>
      </tr>
      
      <tr>
        <td align="left" valign="top">
        </td>
        
        <td align="left" valign="top">
          Travel forward inserting specified sequencein cells
        </td>
        
        <td align="left" valign="top">
          table-insert-sequence
        </td>
      </tr>
      
      <tr>
        <td align="left" valign="top">
        </td>
        
        <td align="left" valign="top">
          Convert plant text into table by capturingtext in the region
        </td>
        
        <td align="left" valign="top">
          table-capture
        </td>
      </tr>
      
      <tr>
        <td align="left" valign="top">
        </td>
        
        <td align="left" valign="top">
          Convert table into plain text
        </td>
        
        <td align="left" valign="top">
          table-release
        </td>
      </tr>
      
      <tr>
        <td align="left" valign="top">
          C-:
        </td>
        
        <td align="left" valign="top">
          Justify contents of cells
        </td>
        
        <td align="left" valign="top">
          table-justify
        </td>
      </tr>
      
      <tr>
        <td align="left" valign="top">
        </td>
        
        <td align="left" valign="top">
          Disable all table advice
        </td>
        
        <td align="left" valign="top">
          table-disable-advice
        </td>
      </tr>
      
      <tr>
        <td align="left" valign="top">
        </td>
        
        <td align="left" valign="top">
          Enable table advice
        </td>
        
        <td align="left" valign="top">
          table-enable-advice
        </td>
      </tr>
      
      <tr>
        <td align="left" valign="top">
        </td>
        
        <td align="left" valign="top">
          Show version of table mode
        </td>
        
        <td align="left" valign="top">
          table-version
        </td>
      </tr>
      
      <tr>
        <td align="left" valign="top">
          TAB
        </td>
        
        <td align="left" valign="top">
          Move point to beginning of next cell
        </td>
        
        <td align="left" valign="top">
        </td>
      </tr>
    </table>
  </div></p>
</div>

<div class="clear">
</div>

<p class="trigger ">
  <a href="#toggle_166401666553f1f8e697daf">Window Commands</a>
</p>

<div class="toggle_container" style="display:none;">
  <div class="block">
    <table border="1">
      <tr>
        <td align="left" valign="top">
          C-x 2
        </td>
        
        <td align="left" valign="top">
          Divide the current window horizontally in two
        </td>
        
        <td align="left" valign="top">
          split-window-horizontally
        </td>
      </tr>
      
      <tr>
        <td align="left" valign="top">
          C-x 5
        </td>
        
        <td align="left" valign="top">
          Divide the current window vertically in two.
        </td>
        
        <td align="left" valign="top">
          split-windws-vertically
        </td>
      </tr>
      
      <tr>
        <td align="left" valign="top">
          C-x >
        </td>
        
        <td align="left" valign="top">
          Scroll the window right.
        </td>
        
        <td align="left" valign="top">
          scroll-right
        </td>
      </tr>
      
      <tr>
        <td align="left" valign="top">
          C-x <
        </td>
        
        <td align="left" valign="top">
          Scroll the window left.
        </td>
        
        <td align="left" valign="top">
          scroll-left
        </td>
      </tr>
      
      <tr>
        <td align="left" valign="top">
          C-x 0
        </td>
        
        <td align="left" valign="top">
          Delete the current window.
        </td>
        
        <td align="left" valign="top">
          delete-window
        </td>
      </tr>
      
      <tr>
        <td align="left" valign="top">
          C-x 1
        </td>
        
        <td align="left" valign="top">
          Delete all the windows except this one.
        </td>
        
        <td align="left" valign="top">
          delete-other-windows
        </td>
      </tr>
      
      <tr>
        <td align="left" valign="top">
        </td>
        
        <td align="left" valign="top">
          Delete all windows open to a particular buff.
        </td>
        
        <td align="left" valign="top">
          delete-windows-on
        </td>
      </tr>
      
      <tr>
        <td align="left" valign="top">
          C-x ^
        </td>
        
        <td align="left" valign="top">
          Make the current window taller.
        </td>
        
        <td align="left" valign="top">
          enlarge-window
        </td>
      </tr>
      
      <tr>
        <td align="left" valign="top">
        </td>
        
        <td align="left" valign="top">
          Make the current window smaller.
        </td>
        
        <td align="left" valign="top">
          shrink-window
        </td>
      </tr>
      
      <tr>
        <td align="left" valign="top">
          C-x }
        </td>
        
        <td align="left" valign="top">
          Make the window wider.
        </td>
        
        <td align="left" valign="top">
          enlarge-window-horizontally
        </td>
      </tr>
      
      <tr>
        <td align="left" valign="top">
          C-x {
        </td>
        
        <td align="left" valign="top">
          Make the window less wide.
        </td>
        
        <td align="left" valign="top">
          shrink-window-horizontally
        </td>
      </tr>
      
      <tr>
        <td align="left" valign="top">
          M-C-v
        </td>
        
        <td align="left" valign="top">
          Scroll the other window.
        </td>
        
        <td align="left" valign="top">
          scroll-other-window
        </td>
      </tr>
      
      <tr>
        <td align="left" valign="top">
          C-x 4 f
        </td>
        
        <td align="left" valign="top">
          Find a file in the other window.
        </td>
        
        <td align="left" valign="top">
          find-file-other-window
        </td>
      </tr>
      
      <tr>
        <td align="left" valign="top">
          C-x 4 b
        </td>
        
        <td align="left" valign="top">
          Select a buffer in the other window.
        </td>
        
        <td align="left" valign="top">
          switch-to-buffer-other-window
        </td>
      </tr>
      
      <tr>
        <td align="left" valign="top">
        </td>
        
        <td align="left" valign="top">
          Compare two buffers and show the first diff.
        </td>
        
        <td align="left" valign="top">
          compare-windows
        </td>
      </tr>
    </table>
  </div></p>
</div>

<div class="clear">
</div>

<p class="trigger ">
  <a href="#toggle_174292440053f1f8e697e7d">Capitalization</a>
</p>

<div class="toggle_container" style="display:none;">
  <div class="block">
    <table border="1">
      <tr>
        <td align="left" valign="top">
          M-c
        </td>
        
        <td align="left" valign="top">
          Capitalize the first letter of current word.
        </td>
        
        <td align="left" valign="top">
          capitalize-word
        </td>
      </tr>
      
      <tr>
        <td align="left" valign="top">
          M-u
        </td>
        
        <td align="left" valign="top">
          Make the word all uppercase.
        </td>
        
        <td align="left" valign="top">
          upcase-word
        </td>
      </tr>
      
      <tr>
        <td align="left" valign="top">
          M-l
        </td>
        
        <td align="left" valign="top">
          Make the word all lowercase.
        </td>
        
        <td align="left" valign="top">
          downcase-word
        </td>
      </tr>
      
      <tr>
        <td align="left" valign="top">
          C-x C-l
        </td>
        
        <td align="left" valign="top">
          Make the region all lowercase.
        </td>
        
        <td align="left" valign="top">
          downcase-region
        </td>
      </tr>
      
      <tr>
        <td align="left" valign="top">
          C-x C-u
        </td>
        
        <td align="left" valign="top">
          Make the region all uppercase.
        </td>
        
        <td align="left" valign="top">
          uppercase-region
        </td>
      </tr>
    </table>
  </div></p>
</div>

<div class="clear">
</div>

<p class="trigger ">
  <a href="#toggle_45759399353f1f8e697f77">Getting Help</a>
</p>

<div class="toggle_container" style="display:none;">
  <div class="block">
    <table border="1">
      <tr>
        <td align="left" valign="top">
          C-h a
        </td>
        
        <td align="left" valign="top">
          What commands work like this&#8230;?
        </td>
        
        <td align="left" valign="top">
          command-apropos
        </td>
      </tr>
      
      <tr>
        <td align="left" valign="top">
        </td>
        
        <td align="left" valign="top">
          What functions and variables work like this.?
        </td>
        
        <td align="left" valign="top">
          apropos
        </td>
      </tr>
      
      <tr>
        <td align="left" valign="top">
          C-h c
        </td>
        
        <td align="left" valign="top">
          What command does this key sequence do?
        </td>
        
        <td align="left" valign="top">
          describe-key-briefly
        </td>
      </tr>
      
      <tr>
        <td align="left" valign="top">
          C-h b
        </td>
        
        <td align="left" valign="top">
          What are the key bindings for this buffer?
        </td>
        
        <td align="left" valign="top">
          describe-bindings
        </td>
      </tr>
      
      <tr>
        <td align="left" valign="top">
          C-h k
        </td>
        
        <td align="left" valign="top">
          What command does this sequence do,and tell me about it.
        </td>
        
        <td align="left" valign="top">
          describe-key
        </td>
      </tr>
      
      <tr>
        <td align="left" valign="top">
          C-h l
        </td>
        
        <td align="left" valign="top">
          What are the last 100 characters typed?
        </td>
        
        <td align="left" valign="top">
          view-lossage
        </td>
      </tr>
      
      <tr>
        <td align="left" valign="top">
          C-h w
        </td>
        
        <td align="left" valign="top">
          What is the key binding for this?
        </td>
        
        <td align="left" valign="top">
          where-is
        </td>
      </tr>
      
      <tr>
        <td align="left" valign="top">
          C-h f
        </td>
        
        <td align="left" valign="top">
          What does this function do?
        </td>
        
        <td align="left" valign="top">
          describe-function
        </td>
      </tr>
      
      <tr>
        <td align="left" valign="top">
          C-h v
        </td>
        
        <td align="left" valign="top">
          What is this variable?
        </td>
        
        <td align="left" valign="top">
          describe-variable
        </td>
      </tr>
      
      <tr>
        <td align="left" valign="top">
          C-h m
        </td>
        
        <td align="left" valign="top">
          Tell me about this mode.
        </td>
        
        <td align="left" valign="top">
          describe-mode
        </td>
      </tr>
      
      <tr>
        <td align="left" valign="top">
          C-h s
        </td>
        
        <td align="left" valign="top">
          What is the syntax table for this buffer?
        </td>
        
        <td align="left" valign="top">
          describe-syntax
        </td>
      </tr>
    </table>
  </div></p>
</div>

<div class="clear">
</div>

<p class="trigger ">
  <a href="#toggle_108693708453f1f8e698065">Tmux</a>
</p>

<div class="toggle_container" style="display:none;">
  <div class="block">
    <p>
      <a href="http://tmux.sourceforge.net/" target="_new">Tmux Sourceforge Page</a>
    </p>
    
    <table border="1">
      <tr>
        <td align="left" valign="top">
          Keys
        </td>
        
        <td align="left" valign="top">
          Description
        </td>
      </tr>
      
      <tr>
        <td align="left" valign="top">
          C-b d
        </td>
        
        <td align="left" valign="top">
          Detach Session
        </td>
      </tr>
      
      <tr>
        <td align="left" valign="top">
          C-b ?
        </td>
        
        <td align="left" valign="top">
          Show Keybindings
        </td>
      </tr>
    </table>
    
    <table border="1">
      <tr>
        <td align="left" valign="top">
          tmux -S /tmp/xyz
        </td>
        
        <td align="left" valign="top">
          Start session in file /tmp/xyz (must chmod to 777 to share)
        </td>
      </tr>
      
      <tr>
        <td align="left" valign="top">
          tmux -S /tmp/xyz attach
        </td>
        
        <td align="left" valign="top">
          Attach to an existing session in /tmp/xyz
        </td>
      </tr>
    </table>
  </div></p>
</div>

<div class="clear">
</div>

<p class="trigger ">
  <a href="#toggle_38825162153f1f8e69813e">Dired</a>
</p>

<div class="toggle_container" style="display:none;">
  <div class="block">
    <p>
      <a href="http://www.gnu.org/software/emacs/manual/html_node/emacs/Dired.html#Dired" target="_new">Dired Manual</a>
    </p>
    
    <p>
      Perform query-replace-regexp on each of the specified files, replacing matches for regexp with the string todired-do-query-replace-regexptToggle all marks in current directory
    </p>
    
    <table>
      <tr>
        <td>
          C-x d
        </td>
        
        <td>
          Start dired in a specified directory (prompts for directory)
        </td>
        
        <td>
          dired
        </td>
      </tr>
      
      <tr>
        <td>
          C-n
        </td>
        
        <td>
          Next node
        </td>
        
        <td>
        </td>
      </tr>
      
      <tr>
        <td>
          <space>
        </td>
        
        <td>
          Next node
        </td>
        
        <td>
        </td>
      </tr>
      
      <tr>
        <td>
          n
        </td>
        
        <td>
          Next node
        </td>
        
        <td>
        </td>
      </tr>
      
      <tr>
        <td>
          C-p
        </td>
        
        <td>
          Previous node
        </td>
        
        <td>
        </td>
      </tr>
      
      <tr>
        <td>
          <del>
        </td>
        
        <td>
          Move up and unflag
        </td>
        
        <td>
        </td>
      </tr>
      
      <tr>
        <td>
          j
        </td>
        
        <td>
          Prompt for filename, move to the line with that file name
        </td>
        
        <td>
          dired-goto-file
        </td>
      </tr>
      
      <tr>
        <td>
          i
        </td>
        
        <td>
          Insert contents of directory at point
        </td>
        
        <td>
          dired-maybe-insert-subdir
        </td>
      </tr>
      
      <tr>
        <td>
          l
        </td>
        
        <td>
          Refresh directory contents
        </td>
        
        <td>
          dired-do-redisplay
        </td>
      </tr>
      
      <tr>
        <td>
          ^
        </td>
        
        <td>
          Move point to parent directory entry
        </td>
        
        <td>
        </td>
      </tr>
      
      <tr>
        <td>
          $
        </td>
        
        <td>
          Hide/Unhide subdirectory, leaving only header line visible
        </td>
        
        <td>
        </td>
      </tr>
      
      <tr>
        <td>
          M-$
        </td>
        
        <td>
          Hide/Unhide all subdirectories, leaving only header lines visible
        </td>
        
        <td>
        </td>
      </tr>
      
      <tr>
        <td>
          C-M-n
        </td>
        
        <td>
          Go to next subdirectory header line, regardless of level
        </td>
        
        <td>
          dired-next-subdir
        </td>
      </tr>
      
      <tr>
        <td>
          C-M-p
        </td>
        
        <td>
          Go to previous subdirectory header line, regardless of level
        </td>
        
        <td>
          dired-prev-subdir
        </td>
      </tr>
      
      <tr>
        <td>
          C-M-u
        </td>
        
        <td>
          Go up to the parent directory&#8217;s header line
        </td>
        
        <td>
          dired-tree-up
        </td>
      </tr>
      
      <tr>
        <td>
          C-M-d
        </td>
        
        <td>
          Go down in the directory tree, to the first subdirectory&#8217;s header line
        </td>
        
        <td>
          dired-tree-down
        </td>
      </tr>
      
      <tr>
        <td>
          M-x image-dired
        </td>
        
        <td>
          Enter image-dired mode, show thumbnails of images in this directory
        </td>
      </tr>
      
      <tr>
        <td>
          C-t d
        </td>
        
        <td>
          Display thumbnails of marked images in this diredtory
        </td>
        
        <td>
          image-dired-display-thumbs
        </td>
      </tr>
      
      <tr>
        <td>
          <
        </td>
        
        <td>
          Move up to the previous directory-file line
        </td>
        
        <td>
          dired-prev-dirline
        </td>
      </tr>
      
      <tr>
        <td>
          >
        </td>
        
        <td>
          Move down to the next directory-file line
        </td>
        
        <td>
          dired-prev-dirline
        </td>
      </tr>
      
      <tr>
        <td>
          C-x C-q
        </td>
        
        <td>
          Toggle WDired mode, <a href="http://www.gnu.org/software/emacs/manual/html_node/emacs/Wdired.html#Wdired" target="_new">see this manual for details</a>
        </td>
        
        <td>
          dired-toggle-read-only
        </td>
      </tr>
      
      <tr>
        <td>
          d
        </td>
        
        <td>
          Flag this file for deletion
        </td>
        
        <td>
          dired-flag-file-deletion
        </td>
      </tr>
      
      <tr>
        <td>
          u
        </td>
        
        <td>
          Remove the deletion flag
        </td>
        
        <td>
          dired-unmark
        </td>
      </tr>
      
      <tr>
        <td>
          <DEL>
        </td>
        
        <td>
          Move point to previous line and remove the deletion flag on that line
        </td>
        
        <td>
          dired-unmark-backward
        </td>
      </tr>
      
      <tr>
        <td>
          x
        </td>
        
        <td>
          Delete files flagged for deletion
        </td>
        
        <td>
          dired-do-flagged-delete
        </td>
      </tr>
      
      <tr>
        <td>
          * % regexp <RET>
        </td>
        
        <td>
          Mark with a &#8216;*&#8217; all files whose name matches the specified regexp
        </td>
        
        <td>
          dired-mark-files-regexp
        </td>
      </tr>
      
      <tr>
        <td>
          % g regexp <RET>
        </td>
        
        <td>
          Mark with a &#8216;*&#8217; all files whose <i>contents</i> match the specified regexp
        </td>
        
        <td>
          dired-mark-files-containing-regexp
        </td>
      </tr>
      
      <tr>
        <td>
          C-/
        </td>
        
        <td>
          Undo changes in the dired buffer
        </td>
        
        <td>
          dired-undo
        </td>
      </tr>
      
      <tr>
        <td>
          C
        </td>
        
        <td>
          Copy File
        </td>
        
        <td>
          dired-do-copy
        </td>
      </tr>
      
      <tr>
        <td>
          s-j
        </td>
        
        <td>
          Jump to dired of directory containing the current file
        </td>
        
        <td>
          dired-jump
        </td>
      </tr>
      
      <tr>
        <td>
          D
        </td>
        
        <td>
          Delete file
        </td>
        
        <td>
          dired-do-delete
        </td>
      </tr>
      
      <tr>
        <td>
          R
        </td>
        
        <td>
          Rename file
        </td>
        
        <td>
          dired-do-rename
        </td>
      </tr>
      
      <tr>
        <td>
          M
        </td>
        
        <td>
          Change mode of file
        </td>
        
        <td>
          dired-do-chmod
        </td>
      </tr>
      
      <tr>
        <td>
          A regexp <RET>
        </td>
        
        <td>
          Search all the specified files for the regular expression regexp
        </td>
        
        <td>
          dired-do-search
        </td>
      </tr>
      
      <tr>
        <td>
          Q regexp <RET> to <RET>
        </td>
      </tr>
      
      <tr>
        <td>
          m
        </td>
        
        <td>
          Mark current file/directory
        </td>
        
        <td>
        </td>
      </tr>
      
      <tr>
        <td>
          u
        </td>
        
        <td>
          Unmark current file/directory
        </td>
        
        <td>
        </td>
      </tr>
      
      <tr>
        <td>
          U
        </td>
        
        <td>
          Unmark all marked in current directory
        </td>
        
        <td>
        </td>
      </tr>
      
      <tr>
        <td align="left" valign="top">
          M&#8211;
        </td>
        
        <td align="left" valign="top">
          Toggle Tree View
        </td>
        
        <td align="left" valign="top">
        </td>
      </tr>
      
      <tr>
        <td align="left" valign="top">
          m
        </td>
        
        <td align="left" valign="top">
          Mark one file
        </td>
        
        <td align="left" valign="top">
        </td>
      </tr>
    </table>
    
    <p>
      <a href="http://www.emacswiki.org/emacs/dired-details.el" target="_new">Dired Details on EmacsWiki</a>
    </p>
    
    <table>
      <tr>
        <td>
          )
        </td>
        
        <td>
          Show more details on each file/dir
        </td>
        
        <td>
        </td>
      </tr>
      
      <tr>
        <td>
          (
        </td>
        
        <td>
          Show less details on each file/dir
        </td>
        
        <td>
        </td>
      </tr>
    </table>
  </div></p>
</div>

<div class="clear">
</div>

<p class="trigger ">
  <a href="#toggle_86341151153f1f8e698213">Multiple Cursors</a>
</p>

<div class="toggle_container" style="display:none;">
  <div class="block">
    <table>
      <tr>
        <td>
          C-S-c C-S-c
        </td>
        
        <td>
          Multiple cursors on lines in active region
        </td>
        
        <td>
          mc/edit-lines
        </td>
      </tr>
      
      <tr>
        <td>
          C->
        </td>
        
        <td>
          Mark next line like this
        </td>
        
        <td>
          mc/mark-next-like-this
        </td>
      </tr>
      
      <tr>
        <td>
          C-<
        </td>
        
        <td>
          Mark previous line like this
        </td>
        
        <td>
          mc/mark-previous-like-this
        </td>
      </tr>
      
      <tr>
        <td>
          C-c C-<
        </td>
        
        <td>
          Mark all like this (select a region to match first)
        </td>
        
        <td>
          mc/mark-all-like-this
        </td>
      </tr>
      
      <tr>
        <td>
          C-g
        </td>
        
        <td>
          Cancel multiple cursor mode
        </td>
        
        <td>
        </td>
      </tr>
      
      <tr>
        <td>
          RET
        </td>
        
        <td>
          Exit multiple cursor mode
        </td>
        
        <td>
        </td>
      </tr>
      
      <tr>
        <td>
          C-j
        </td>
        
        <td>
          Insert a newline in multiple-cursor mode
        </td>
        
        <td>
        </td>
      </tr>
    </table>
  </div></p>
</div>

<div class="clear">
</div>
