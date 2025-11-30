;;; zlisp-vtkeyboard-goldmap.el --- VT keyboard `gold' keymap.  -*- mode: emacs-lisp; lexical-binding: t; -*-
;;
;; Copyright (c) 2025 Paul Ward <paul@lisphacker.uk>
;;
;; Author:     Paul Ward <paul@lisphacker.uk>
;; Maintainer: Paul Ward <paul@lisphacker.uk>
;; Created:    29 Nov 2025 03:42:29
;; URL:        not distributed yet
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY  WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; The meanings of what these mean to DEC software are based on the document:
;;
;; AA-5267C-TA -- WPS-8 Word Processing System Reference Manual.
;;                Versions 2.7 and 3.0
;;                Dec 1978.
;;
;; As an aside, I was 8 months old when that document was published.
;;
;; +==========================================================================+
;; | `Programmable Function` keys.                                            |
;; |------+-----------------+-------------------------------------------------|
;; | PF1  | Gold            | Bucky bit for WPS / DECwrite / et al.           |
;; | PF2  | Page            | Display page endings.                           |
;; | PF3  | Del Word        | Delete word under cursor.                       |
;; | PF4  | Del Char        | Delete character under cursor.                  |
;; |------+-----------------+-------------------------------------------------|
;; | NOTES:                                                                   |
;; |                                                                          |
;; | * DEL WORD has the same semantics as `kill-word'.                        |
;; |                                                                          |
;; | * DEL CHAR has the same semantics as `delete-char'.                      |
;; |                                                                          |
;; | * GOLD+PAGE will go to the end of a page.                                |
;; |                                                                          |
;; | * PAGE will display page endings, just like any other word processor.    |
;; |   There is no real parallel in Emacs.  Maybe an `org-mode' hack?         |
;; |                                                                          |
;; +==========================================================================+
;; | `Function' keys.                                                         |
;; |------+-----------------+-------------------------------------------------|
;; | F6   | Inter/Halt      | Interrupt or halt an action.                    |
;; | F7   | Resume          | Resume an interrupted action.                   |
;; | F8   | Cancel          | Cancel something.                               |
;; | F9   | Main Screen     | Return to application's main screen.            |
;; | F10  | Exit            | Get the hell out of Dodge.                      |
;; | F11  | Alt Char        | Possibly enter Alternate Character Set mode.    |
;; | F12  | Pr. Fld Hyp Psh | either Print Field, or Hyphen Push.             |
;; | F13  | Er Line Rub Wrd | Erase line / Rub word.                          |
;; | F14  | Insert/Ovrstke  | Toggle insert/overstrike mode.                  |
;; | F15  | Help            | Get help.  Seems to be for FMS, not WPS.        |
;; | F16  | Do              | Execute some form of command.                   |
;; |------+-----------------+-------------------------------------------------|
;; | NOTES:                                                                   |
;; |                                                                          |
;; | * HYPH PUSH is something I think Emacs lacks.                            |
;; |                                                                          |
;; | * The PR. FIELD label is for VAX FORMS.  As that doesn't do hypen        |
;; |   madness.                                                               |
;; |                                                                          |
;; | * The HELP function key /probably/ does what PF2 would do on a VT100.    |
;; |   That is, be used in FMS to get context help.  It was probably moved    |
;; |   to where it is later on for the VT200 keyboards.                       |
;; |                                                                          |
;; | * DO is probably the least-documented key of them all.  Chances are that |
;; |   it does something specific depending on which application is being     |
;; |   used.  I.e. on some it might pop up a `Command:' prompt, on others     |
;; |   it might do some specific action.  On Emacs, this might just be the    |
;; |   same as pressing `M-x'.                                                |
;; |                                                                          |
;; +==========================================================================+
;; | Numeric keypad -- `mini' in PDP-8 parlance.                              |
;; |------+-----------------+-------------------------------------------------|
;; | NP-  | Cut             | Cut into pasteboard.                            |
;; | NP9  | Under Line      | Mark selected text as underlined.               |
;; | NP8  | Tab Pos         | Move to next/previous tab stop position.        |
;; | NP7  | Sent            | Move to next/previous sentence.                 |
;; | NP+  | Paste           | Paste from pasteboard.                          |
;; | NP6  | Bold            | Mark selected text as bold.                     |
;; | NP5  | Para            | Move to next/previous paragraph.                |
;; | NP4  | Word            | Move to next/previous word.                     |
;; | NPE  | Enter           | See notes.                                      |
;; | NP3  | Upper Case      | Mark selected text as upper case.               |
;; | NP2  | Line            | Move to next/previous line.                     |
;; | NP1  | Back Up         | Move to previous item or character.             |
;; | NP.  | Sel             | Insert selection mark.                          |
;; | NP0  | Advance         | Move to next item or character.                 |
;; |------+-----------------+-------------------------------------------------|
;; | NOTES:                                                                   |
;; |                                                                          |
;; | * ADVANCE / BACK UP affect the "direction" of the cursor for the other   |
;; |   syntactic editing keys.  That is, if you press BACK UP to move the     |
;; |   cursor to the previous character, then PARA, WORD, and LINE will move  |
;; |   to the previous syntactic entity.  The opposite occurs if ADVANCE is   |
;; |   used.  I assume the default is to advance.                             |
;; |                                                                          |
;; | * Pressing GOLD + <motion> would enter `continuous scroll' mode.  The    |
;; |   cursor would scroll until either the end of the document or until the  |
;; |   INTER/HALT key was pressed. This is evil and Emacs should not do this. |
;; |                                                                          |
;; | * Pressing PARA would, as well as causing a navigation jump, would also  |
;; |   reformat the text in the current paragraph.  In Emacs, this is silly.  |
;; |                                                                          |
;; | * SEL equates to Emacs' `set-mark-command'.  Pressing SEL again clears   |
;; |   the selection mark.  With Emacs, `C-SPC' will create a new mark at the |
;; |   cursor location.                                                       |
;; |                                                                          |
;; | * CUT and PASTE equate to `kill-region' and `yank'.                      |
;; |                                                                          |
;; | * There is no UNDO or anything like `yank-pop' from what I see.  Man,    |
;; |   WPS was harsh if you blindly cut muiltiple things.                     |
;; |                                                                          |
;; | * GOLD+CUT worked like `kill-ring-save'.                                 |
;; |                                                                          |
;; | * GOLD+PASTE would insert text but with the original ruler (margins) and |
;; |   not the ruler of the new location.                                     |
;; |                                                                          |
;; | * UNDER LINE and BOLD should make use of modes that support text attrs   |
;; |   such as `org-mode'.                                                    |
;; |                                                                          |
;; | * NPE is dual-labelled -- "Enter" and "< >".  "Enter" is what you think  |
;; |   it is, and "< >" moves from the first character after a `>' to         |
;; |   another.  This could probably be used for lots of things, not just     |
;; |   angle brackets.                                                        |
;; |                                                                          |
;; | * UPPER CASE is applied to a selection made with SEL, it simply converts |
;; |   the region to uppercase.  Emacs can do this.                           |
;; |                                                                          |
;; | * GOLD + attribute will remove the specified attribute.                  |
;; |                                                                          |
;; +==========================================================================+
;; | `Application keys' -- known as `editing keys' in PC parlance.            |
;; |------+-----------------+-------------------------------------------------|
;; | Ins  | Find            | Find for text.                                  |
;; | Home | Insert Here     | Paste (presumably the deleted) block.           |
;; | PgUp | Remove          | Kill the selected block.                        |
;; | Del  | Select          | Mark / start a selection.                       |
;; | End  | Prev            | Next search result.                             |
;; | PgDn | Next            | Previous search result                          |
;; |------+-----------------+-------------------------------------------------|
;; | NOTES:                                                                   |
;; |                                                                          |
;; | * FIND simply enters Swiper / isearch.                                   |
;; |                                                                          |
;; | * PREV - Same as BACK UP, jumps to previous match.                       |
;; |                                                                          |
;; | * NEXT - Same as ADVANCE, jumps to next match.                           |
;; |                                                                          |
;; | * SELECT - Accept the current Swiper match.                              |
;; |                                                                          |
;; | * REMOVE - `kill-region'.                                                |
;; |                                                                          |
;; | * INSERT HERE - `yank'.                                                  |
;; |                                                                          |
;; +==========================================================================+
;; | `Gold' action keycaps on all keys.                                       |
;; |------+-----------------+-------------------------------------------------|
;; | F11  | Lock            | Screen lock? Mode lock? Door lock?              |
;; | F12  | Hyph Pull       | Does the inverse of HYPH PUSH.                  |
;; | F13  | Rub Sent        | Erases a sentence to the left of the cursor.    |
;; |  -   | Print Hyph      | Insert a breaking hyphen.                       |
;; |  =   | Abbrv           | Insert an abbreviation.                         |
;; | BS   | Rub Line        | Erases a line to the left of the cursor.        |
;; | Tab  | Indent Tab      | Context-aware indent.                           |
;; |  q   | Super Script    | Apply superscript formatting to a selection.    |
;; |  w   | Write Docmt     | Copy document to a new document.                |
;; |  r   | Ruler           | Display a ruler.                                |
;; |  t   | Top Docmt       | Move to top of the document.                    |
;; |  y   | Footnote        | Create a footnote.                              |
;; |  u   | UDP             | I think this turns F row to F17-F20.            |
;; |  p   | Page Marker     | Inserts a page marker, denoting a page.         |
;; |  [   | Cmnd            | Insert a printer command/control.               |
;; |  ]   | Change Editor   | Presumably changes which editor to be used.     |
;; |  a   | Sub Script      | Apply subscript formatting to a selection.      |
;; |  s   | Spell Check     | Check document for speeling masterks.           |
;; |  d   | Dead Key        | Used to produce composites.                     |
;; |  f   | File Docmt      | Saves the file and then exits to main screen.   |
;; |  g   | Get Docmt       | Load a document into the current document.      |
;; |  h   | Help            | Get help.                                       |
;; |  j   | Lang Aids       | Language aids tooling - dict/thesaurus etc.     |
;; |  k   | Quit Docmt      | Exit current editing buffer.                    |
;; |  l   | Libry           | Insert a paragraph from the library.            |
;; |  ;   | Global Replc    | Accept a replacement in SRCH mode.              |
;; |  '   | Recalc          | Recalculate.                                    |
;; |  \   | Date & Time     | Insert a date/time stamp.                       |
;; |Return| Para Marker     | Indent to paragraph marker on ruler.            |
;; |  z   | Status          | Show document status.                           |
;; |  c   | Center          | Center text using ruler configuration.          |
;; |  v   | View            | View applied formatting inline. (print preview) |
;; |  b   | Bot Docmt       | Move to the bottom of the document.             |
;; |  n   | New Page        | Insert a new page marker.                       |
;; |  m   | Menu            | Displays the editing menu.                      |
;; |  ,   | Srch            | Start a search.                                 |
;; |  .   | Cont Srch       | Continue search (next match)                    |
;; |  /   | Cont Srch & Sel | Search and replace with selection.              |
;; | NPE  | Swap            | Interchange two characters.                     |
;; |------+-----------------+-------------------------------------------------|
;; | NOTES:                                                                   |
;; |                                                                          |
;; | * Pressing GOLD + TOP DOCMT would perform actions similar to theese:     |
;; |   `write-file', `revert-buffer', `beginning-of-buffer'.                  |
;; |                                                                          |
;; | * Pressing GOLD + BOT DOCMT would essentially show you the last screen   |
;; |   page of the document (24 lines, or 11 if the right margin was > 79)    |
;; |   with the cursor set to the top of that region.                         |
;; |                                                                          |
;; | * In Emacs, TOP DOCMT can just be `beginning-of-buffer' and BOT DOCMT    |
;; |   can be `end-of-buffer' -- no silly save/revert/jump nonsense, and no   |
;; |   overlay last screen page (with caveats) etc.                           |
;; |                                                                          |
;; | * SWAP worked in the same way as Emacs `transpose-character'.            |
;; |                                                                          |
;; | * RULER can invoke `ruler-mode' in Emacs.  It was probably slightly more |
;; |   complicated on WPS-8.                                                  |
;; |                                                                          |
;; | * PARA MARKER works using the ruler's `p'aragraph marker.  This equates  |
;; |   to the paragraph tab marker in a modern word processor.  I am not 100% |
;; |   if `ruler-mode', `org' etc support this.  Could use the rather unused  |
;; |   `paragraph-indent-minor-mode' thing.  This would change PARA MARKER    |
;; |   into a key to set the tab stop used for paragraph leading indents.     |
;; |                                                                          |
;; | * SRCH and CONT SRCH are equivalent to ISearch in Emacs.                 |
;; |                                                                          |
;; | * CONT SRCH & SEL is a search/replace mechanic within search mode.       |
;; |   This isn't similar to Emacs, in that you're replacing within a search  |
;; |   operation.  But, this could be used along with REPLC / GLOBAL REPLC    |
;; |   within a Swiper search & replace operation.                            |
;; |                                                                          |
;; | * SRCH mode in WPS can also use ADVANCE / BACK UP / HALT to navigate and |
;; |   exit.                                                                  |
;; |                                                                          |
;; | * CENTER uses a specific mark on the ruler (called "center point") to    |
;; |   determine where it should be centered.  This could be simplified to    |
;; |   just center within the current ruler region or whatnot.                |
;; |                                                                          |
;; | * VIEW is basically an old-school text-mode word processor kludge that   |
;; |   allows authors to view formatting on-screen.  A poor-man's "print      |
;; |   preview".  Not entirely sure how this could map to Emacs.  This could  |
;; |   potentially toggle `whitespace-mode' or `global-whitespace-mode'.      |
;; |                                                                          |
;; | * The COMPOSE key on a modern DEC keyboard isn't a substitute for the    |
;; |   DEAD key composition method.  COMPOSE starts a process that starts on  |
;; |   the terminal, DEAD starts a process that is on the host.               |
;; |                                                                          |
;; | * PRINT HYPH is interesting.  Without GOLD, you get `-', which is a      |
;; |   non-breaking hyphen.  With GOLD, you also get `-', but the system      |
;; |   treats it as a breaking hyphen.  There is probably no parallel with    |
;; |   Emacs, so it should just insert a hyphen (`-')                         |
;; |                                                                          |
;; | * HYPH PULL is, I think, something Emacs lacks.                          |
;; |                                                                          |
;; | * NEW PAGE will force a new page to be created.  This is the same as any |
;; |   other word processing system.  It doesn't really have any parallels in |
;; |   Emacs.  Maybe something for `org-mode'?                                |
;; |                                                                          |
;; | * PAGE MARKER allows you to insert pagination into a document.  Emacs    |
;; |   could just simply insert a `^L' to simulate page breaks?               |
;; |                                                                          |
;; | * CMND seems to be for setting up printer controls to allow such things  |
;; |   as printing multiple columns.  Emacs lacks this.  So, either have it   |
;; |   emit `M-x', or some kind of `org-mode' hack as this key also sets up   |
;; |   headers and footers.                                                   |
;; |                                                                          |
;; | * In ALL-IN-1, FILE DOCMT performs file-based actions (such as open)     |
;; |                                                                          |
;; | * FOOTNOTE would probably only really work in  `org-mode'.               |
;; |                                                                          |
;; | * HELP is potentially different from the F-key HELP.  So, in this case   |
;; |   the gold key should describe the mode, and the function key should     |
;; |   load Emacs help.                                                       |
;; |                                                                          |
;; | * INDENT TAB would align inside some sort of context.  That is, if you   |
;; |   were using a programmer's editor, that could indent based on syntax.   |
;; |   I'd imagine a forms editor could use this to line up fields too.       |
;; |                                                                          |
;; | * From what I can find, RECALC would do things like recompute formulae   |
;; |   in DECwrite tables, or recalculate mail merge templates in ALL-IN-1.   |
;; |                                                                          |
;; +==========================================================================+
;;
;; Hyphen pushing and pulling.
;; --------------------------
;;
;;     Press the HYPH PUSH key.  The first
;;     character from the bottom line is moved
;;     ^cursor
;;     up to the top, and a hyphen mark is
;;     automatically inserted after it.  If you
;;     like the results, proceed to another
;;     line.
;;
;; HYPH PUSH a few times.
;;
;;     Press the HYPH PUSH key.  The first char-
;;     acter from the bottom line moves up to
;;     the top, and a hyphen mark is
;;     automatically inserted after it.  If you
;;     ^ cursor
;;     like the results, proceed to
;;     another line.
;;
;; HYPH PUSH a few times
;;
;;     Press the HYPH PUSH key.  The first char-
;;     acter from the bottom line moves up to
;;     the top, and a hyphen mark is automati-
;;     cally inserted after it.  If you like the
;;     results, proceed to another line.
;;
;; HYPH PULL does the inverse, it pulls from the line above to the line with
;; the cursor.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'zlisp-features))

;; TODO: Move this elsewhere - whitespace mode with unicode paragraph and tab.
(setq whitespace-display-mappings
      `((newline-mark 10  [182 10] [36 10])
        (tab-mark     9   [187 9]  [92 9])
        (space-mark   32  [183]    [46])
        (space-mark   160 [164]    [95])))

(defun zlisp-vtkeyboard//setup-goldmap ()
  "Set up the global map for the `gold' key."
  (when (zlisp/feature-p :vt-keyboard)
    (progn
      (define-prefix-command 'gold-map)
      (global-set-key [gold] 'gold-map)

      ;; Define the keymap.
      (define-key gold-map (kbd "-")   #'zlisp/insert-hyphen)
      (define-key gold-map (kbd "\\")  #'zlisp/insert-date-and-time)
      (define-key gold-map (kbd "h")   #'zlisp-vtkeyboard/describe-bindings))))

(defun zlisp-vtkeyboard/describe-bindings ()
  "Describe the key bindings in the `gold' key map."
  (interactive)
  (describe-bindings [gold]))

(defun zlisp-vtkeyboard/toggle-ruler ()
  "Toggle the display of a ruler in the current buffer."
  (interactive)
  (if (bound-and-true-p ruler-mode)
      (ruler-mode -1)
    (ruler-mode 1)))

(defun zlisp-vtkeyboard/toggle-whitespace ()
  "Toggle the display of whitespace in the current buffer."
  (interactive)
  (if (bound-and-true-p whitespace-mode)
      (whitespace-mode -1)
    (whitespace-mode 1)))

(provide 'zlisp-vtkeyboard-goldmap)

;;; zlisp-vtkeyboard-goldmap.el ends here.
