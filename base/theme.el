;;; -*- Mode: Emacs-Lisp -*-
;;;
;;; theme.el --- Emacs colour theme.
;;;
;;; Time-stamp: <Wednesday Sep  5, 2012 16:48:05 asmodai>
;;; Revision:   93
;;;
;;; Copyright (c) 2011-2012 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    21 Feb 2011 22:48:34
;;; Keywords:   
;;; URL:        not distributed yet
;;;
;;;{{{ License:
;;;
;;; This program is free software: you can redistribute it
;;; and/or modify it under the terms of the GNU General Public
;;; Licenseas published by the Free Software Foundation,
;;; either version 3 of the License, or (at your option) any
;;; later version.
;;;
;;; This program isdistributed in the hope that it will be
;;; useful, but WITHOUT ANY  WARRANTY; without even the implied
;;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;; PURPOSE.  See the GNU General Public License for more
;;; details.
;;;
;;; You should have received a copy of the GNU General Public
;;; License along with this program.  If not, see
;;; <http://www.gnu.org/licenses/>.
;;;
;;;}}}
;;;{{{ Commentary:
;;;
;;;
;;;
;;;}}}

;;; ==================================================================
;;;{{{ Basic UI elements:

(when (fboundp 'set-background-color)
  (set-background-color "black"))

(when (fboundp 'set-cursor-color)
  (set-cursor-color "green3"))

(when (fboundp 'set-foreground-color)
  (set-foreground-color "white"))

(when (fboundp 'set-border-color)
  (set-border-color "black"))

;;;}}}
;;; ==================================================================

;;; ==================================================================
;;;{{{ Faces:

;;; The following faces are missing in Emacs 21.
(when emacs=21-p
  (defvar font-lock-comment-delimiter-face
    'font-lock-comment-delimiter-face
    "Face name used for comment delimiters.")
  (defface font-lock-comment-delimiter-face
      '((((class color) (background dark))
         (:foreground "steelblue"))
        (((class color) (background light))
         (:foreground "steelblue"))
        (((class grayscale) (background light))
         (:foreground "steelblue"))
        (((class grayscale) (background dark))
         (:foreground "steelblue")))
    "Font Lock mode face used to highlight comment delimiters."
    :group 'font-lock-faces))

;;;}}}
;;; ==================================================================

;;; ==================================================================
;;;{{{ Theme:

;;; Only if we're on Emacs>=20
(when emacs>=20-p
  (custom-set-faces
   ;; NOTE: NeXTSTEP is not supported here as it stores most of
   ;; its face information in the Defaults database and I am at a
   ;; complete loss as to how to override that.  It appears that
   ;; Emacs.app also ignores some of the font-lock details that
   ;; are written out to Defaults by `ns-save-preferences',
   ;; irritating.
   (cond (terminal-p
          ;; If we're running in a terminal, we want sane values that
          ;; are sane.  Black and white will do.
          '(default
            ((t (:background "black"
                 :foreground "white")))))
         (windows-nt-p
          ;; Consolas first appeared in Windows XP as far as I recall,
          ;; so it might not be available in earlier versions, that's
          ;; a risk I'm willing to take for now. So, just assume we
          ;; have Consolas.
          '(default
            ((t (:family "DejaVu Sans Mono"
                 :size 9
                 :height 90
                 :background "#0e0412"
                 :foreground "#c4b1cb")))))
         ((and windows-p (not windows-nt-p))
          ;; Given that we are on a Windows that is /not/ Windows NT,
          ;; we probably do not have Consolas, so just go for Courier
          ;; New and be done with it.  This can always be fine-tuned
          ;; later on.
          '(default
            ((t (:family "Courier New"
                 :size 9
                 :height 90
                 :background "#0e0412"
                 :foreground "#c4b1cb")))))
         ((and unix-p                   ; Unix
               (not mac-os-x-p)         ; ... but not Mac OS X
               (not next-mach-p))       ; ... but not NeXTSTEP
          ;; On Unix we use a different font depending on whether
          ;; Emacs has been compiled with Motif widgets or not.
          (if motif-p
              ;; We're using Motif, so use the font the user specified
              ;; in the CDE/Motif preferences.  TODO: would be nice to
              ;; find a way to determine if Emacs is running in CDE
              ;; rather than just assuming it is CDE.
              '(default
                ((t (:family "Interface User"
                     :size 10
                     :height 100
                     :background "#0e0412"
                     :foreground "#c4b1cb"))))
              ;; We're not using Motif, so let's try our luck with
              ;; DejaVu Sans Mono.  Probably a Bad Thing(tm) given
              ;; that not all Unix or Unix-like systems will have this
              ;; font.
              '(default
                ((t (:family "DejaVu Sans Mono"
                     :size 9
                     :height 90
                     :background "#0e0412"
                     :foreground "#c4b1cb"))))))
         (mac-os-x-p
          ;; Use Monaco and be done with it.
          '(default
            ((t (:family "Monaco"
                 :size 11
                 :height 110
                 :background "#0e0412"
                 :foreground "#c4b1cb")))))
         (presentation-manager-p
          ;; Use System VIO as the font on OS/2.
          '(default
            ((t (:family "System VIO"
                 :size 9
                 :height 90
                 :background "#0e0412"
                 :foreground "#c4b1cb")))))
         (t
          ;; Give up and let Emacs decide.
          '(default
            ((t (:size 9
                 :height 90
                 :background "#0e0412"
                 :foreground "#c4b1cb"))))))
   ;;;
   ;;; And now the rest
   ;;;
   ;;;{{{ UI elements:
   '(blank-space-face
     ((t (nil))))
   '(blank-tab-face
     ((t (nil))))
   '(border
     ((t (nil))))
   '(button
     ((t (:inherit custom-button))))
   '(border-glyph
     ((t (nil))))
   '(buffers-tab
     ((t (:background "gray65"
          :foreground "black"
          :bold t))))
   '(cursor
     ((t (:background "green3"))))
   '(error
     ((t (:foreground "darkred" :weight bold))))
   '(fringe
     ((t (:background "grey7"))))
   '(gui-button-face
     ((t (:inherit custom-button))))
   '(gui-element
     ((t (:background "gray80"))))
   '(highlight
     ((t (:inherit secondary-selection))))
   '(highlight-changes-delete-face
     ((t (:underline t
          :foreground "red"))))
   '(highlight-changes-face
     ((t (:foreground "red"))))
   '(highline-face
     ((t (:background "grey95"))))
   '(lazy-highlight
     ((t (:background "gray20"
          :foreground "aquamarine"))))
   '(linemenu-face
     ((t (nil))))
   '(minibuffer-prompt
     ((t (:foreground "purple"))))
   '(modeline
     ((t (:insert mode-line))))
   '(mode-line
     ((((min-colors 256))
       (:background "#2b113d"
        :foreground "#b8b66c"
        :box (:line-width 1 :color "#230b2f" :style released-button)))
      (((min-colors 8))
       (:background "blue"
        :foreground "orange"))))
   '(header-line
     ((t (:inherit mode-line
          :foreground "white"))))
   '(menu
     ((t (:inherit header-line))))
   '(mode-line-inactive
     ((t (:background "#160a1e"
          :foreground "#51502f"
          :box (:line-width 1 :color "#230b2f" :style released-button)))))
   '(modeline-buffer-id
     ((t (:foreground "#35dc5d"))))
   '(modeline-mousable
     ((t (:background "#1a0a25"
          :foreground "#61331a"))))
   '(modeline-mousable-minor-mode
     ((t (:background "#1a0a25"
          :foreground "#611a1a"))))
   '(mode-line-highlight
     ((t (:inherit link :box nil :underline nil))))
   '(primary-selection
     ((t (:background "#171725" 
          :foreground "turquoise"))))
   '(region
     ((t (:background "#171725"
          :foreground "yellow"))))
   '(right-margin
     ((t (nil))))
   `(secondary-selection
     ((t (:background "#171725"
          :foreground "paleturquoise"))))
   '(text-cursor
     ((t (:background "green3"))))
   '(tool-bar
     ((t (:inherit header-line))))
   '(toolbar
     ((t (:inherit toolbar))))
   '(tooltip
     ((t (:background "lightyellow"
          :foreground "black"))))
   '(vcursor
     ((t (:background "lawngreen"))))
   '(vertical-divider
     ((t (:background "gray70"
          :foreground "black"))))
   '(zmacs-region
     ((t (:background "gray20"
          :foreground "yellow"))))
   ;;;}}}
   ;;;{{{ Standard font-lock faces:
   '(bold
     ((t (:bold t))))
   '(bold-italic
     ((t (:italic t
          :bold t))))
   '(italic
     ((t (:italic t))))
   '(underline
     ((t (:underline t))))
   ;;;}}}
   ;;;{{{ Common colours:
   '(blue
     ((t (:foreground "blue"))))
   '(cyan
     ((t (:foreground "cyan"))))
   '(green
     ((t (:foreground "green"))))
   '(magenta
     ((t (:foreground "magenta"))))
   '(red
     ((t (:foreground "red"))))
   '(white
     ((t (:foreground "white"))))
   '(yellow
     ((t (:foreground "yellow"))))
   ;;;}}}
   ;;;{{{ Font-lock colours:
   '(font-lock-builtin-face
     ((t (:foreground "#af51dc"))))
   '(font-lock-comment-delimiter-face
     ((t (:foreground "#2c435f"))))
   '(font-lock-comment-face
     ((t (:foreground "#669bcd"))))
   '(font-lock-constant-face
     ((t (:foreground "#bc962e"))))
   '(font-lock-doc-face
     ((t (:foreground "#669bcd"))))
   '(font-lock-function-name-face
     ((t (:foreground "#c4a259"
          :bold t))))
   '(font-lock-keyword-face
     ((t (:foreground "#e99487"))))
   '(font-lock-negation-char-face
     ((t (:background "grey10"
          :foreground "firebrick"))))
   '(font-lock-preprocessor-face
     ((t (:foreground "#7e2e89"))))
   '(font-lock-regexp-grouping-backslash
     ((t (:foreground "grey50"
          :bold t))))
   '(font-lock-string-face
     ((t (:foreground "#87b53c"))))
   '(font-lock-type-face
     ((t (:foreground "#d18649"))))
   '(font-lock-variable-name-face
     ((t (:foreground "#ede769"
          :bold t))))
   '(font-lock-warning-face
     ((t (:foreground "red"
          :bold t))))
   ;;;}}}
   ;;;{{{ Widget faces:
   '(widget-button
     ((t (:inherit custom-button))))
   '(widget-button-pressed
     ((t (:inherit custom-button-pressed))))
   '(widget-documentation
     ((t (:inherit custom-documentation))))
   '(widget-field
     ((t (:background "gray15"))))
   '(widget-inactive
     ((t (:foreground "gray40"
          :background "gray10"))))
   '(widget-single-line-field
     ((t (:background "gray15"))))
   ;;;}}}
   ;;;{{{ Customize faces:
   '(custom-button
     ((t (:background "#3b124e"
          :foreground "#a2a244"
          :box (:line-width 1 :color "#230b2f" :style released-button)))))
   '(custom-button-mouse
     ((t (:background "#651f86"
          :foreground "white"
          :box (:line-width 1 :style released-button)))))
   '(custom-button-pressed
     ((t (:inherit custom-button
          :foreground "white"
          :box (:line-width 1 :style pressed-button)))))
   '(custom-button-pressed-unraised
     ((t (:inherit custom-button-unraised
          :foreground "white"))))
   '(custom-button-unraised
     ((t (:inherit custom-button
          :box nil))))
   '(custom-changed
     ((t (:foreground "white"
          :background "royalblue"
          :bold t))))
   '(custom-comment
     ((t (:inherit font-lock-comment-face))))
   '(custom-comment-tag
     ((t (:inherit font-lock-comment-face 
          :underline t))))
   '(custom-documentation
     ((t (:inherit font-lock-comment-face
          :slant italic))))
   '(custom-face-tag
     ((t (:foreground "royalblue"))))
   '(custom-group-tag
     ((t (:underline t
          :foreground "cadetblue"))))
   '(custom-group-tag-1
     ((t (:foreground "royalblue"
          :weight bold
          :height 1.2))))
   '(custom-invalid
     ((t (:foreground "yellow"
          :background "red"))))
   '(custom-modified
     ((t (:foreground "white"
          :background "blue"))))
   '(custom-rogue
     ((t (:foreground "pink"
          :background "black"))))
   '(custom-saved
     ((t (:underline t))))
   '(custom-set
     ((t (:foreground "blue"
          :background "white"))))
   '(custom-state
     ((t (:foreground "goldenrod3"))))
   '(custom-variable-button
     ((t (:inherit font-lock-variable-name-face))))
   '(custom-variable-tag
     ((t (:underline t
          :foreground "mediumorchid"))))
   ;;;}}}
   ;;;{{{ ISearch faces:
   '(isearch
     ((t (:background "gray10"
          :foreground "azure4"))))
   '(isearch-secondary
     ((t (:foreground "red3"))))
   '(isearch-fail
     ((t (:foreground "red"
          :bold t))))
   ;;;}}}
   ;;;{{{ CPerl faces:
   '(cperl-array-face
     ((t (:foreground "skyblue"
          :bold t))))
   '(cperl-hash-face
     ((t (:foreground "hotpink"
          :bold t))))
   '(cperl-nonoverridable-face
     ((t (:foreground "chartreuse3"))))
   '(cperl-here-face
     ((t (:foreground "green"
          :background "grey25"
          :bold t))))
   '(cperl-pod-head-face
     ((t (:foreground "green3"))))
   '(cperl-pod-face
     ((t (:foreground "firebrick"))))
   ;;;}}}
   ;;;{{{ Dired faces:
   '(dired-face-directory ((t (:bold t))))
   ;;;}}}
   ;;;{{{ ERC faces:
   '(erc-capab-identify-unidentified
     ((t (:foreground "firebrick"))))
   '(erc-current-nick-face
     ((t (:foreground "turquoise"
          :bold t))))
   '(erc-header-line
     ((t (:background "gray30"
          :foreground "gray70"))))
   '(erc-input-face
     ((t (:foreground "cadetblue"))))
   '(erc-my-nick-face 
     ((t (:foreground "cadetblue"
          :bold t))))
   '(erc-nick-default-face
     ((t (:bold t))))
   '(erc-pal-face
     ((t (:foreground "darkmagenta"
          :bold t))))
   '(erc-prompt-face
     ((t (:background "black"
          :foreground "lightblue2"
          :bold t))))
   '(erc-timestamp-face
     ((t (:foreground "green4"
          :bold t))))
   ;;;}}}
   ;;;{{{ Paren faces:
   '(show-paren-match
     ((((min-colors 256))
       (:background "gray15"
        :foreground "lawngreen"
        :bold t))
      (((min-colors 8))
       (:foreground "green"
        :bold t))))
   '(show-paren-mismatch
     ((((min-colors 256))
       (:background "gray15"
        :foreground "red"
        :bold t))
      (((min-colors 8))
       (:foreground "red"
        :bold t))))
   ;;;}}}
   ;;;{{{ Browser faces:
   '(link
     ((t (:foreground "slateblue"
          :underline t))))
   '(link-visited
     ((t (:foreground "magenta3"
          :underline t))))
   ;;;}}}
   ;;;{{{ IBuffer faces:
   '(ibuffer-occur-match-face
     ((t (:foreground "firebrick"))))
   ;;;}}}
   ;;;{{{ Hyper-Apropos faces:
   '(hyper-apropos-major-heading
     ((t (:foreground "steelblue"
          :bold t))))
   '(hyper-apropos-section-heading
     ((t (:foreground "cadetblue"
          :bold t))))
   '(hyper-apropos-heading
     ((t (:bold t
          :foreground "skyblue"))))
   '(hyper-apropos-documentation
     ((t (:foreground "forestgreen"))))
   '(hyper-apropos-hyperlink
     ((t (:foreground "slateblue"
          :underline t))))
   '(hyper-apropos-warning
     ((t (:foreground "red"
          :bold t))))
   ;;;}}}
   ;;;{{{ Info faces:
   '(info-menu-header
     ((t (:inherit info-title-4
          :weight bold))))
   '(info-node
     ((t (:foreground "steelblue"))))
   '(info-title-1
     ((t (:inherit info-title-2
          :height 1.2))))
   '(info-title-2
     ((t (:inherit info-title-3
          :height 1.2))))
   '(info-title-3
     ((t (:inherit info-title-4
          :height 1.2))))
   '(info-title-4
     ((t (:inherit default
          :weight bold
          :height 1.2))))
   '(info-xref
     ((t (:inherit link))))
   '(info-xref-visited
     ((t (:inherit link-visited))))
   ;;;}}}
   ;;;{{{ Man viewer faces:   (n.b. not woman)
   '(man-italic
     ((t (:foreground "blanchedalmond"
          :italic t))))
   ;;;}}}
   ;;;{{{ SLIME faces:
   '(sldb-restart-number-face
     ((t (:bold t))))
   '(slime-inspector-value-face
     ((t (:foreground "mediumblue"
          :bold t))))
   '(slime-reader-conditional-face
     ((t (:foreground "DimGray"
          :bold t))))
   '(slime-repl-input-face
     ((t (:bold t))))
   '(slime-repl-output-mouseover-face
     ((t (:bold t))))
   ;;;}}}
   ;;;{{{ `About' faces:
   '(about-headling-face
     ((t (:foreground "hotpink"
          :bold t))))
   '(about-link-face
     ((t (:foreground "steelblue"
          :underline t))))
   ;;;}}}
   ;;;{{{ Calendar faces:
   '(calendar-today-face
     ((t (:foreground "hotpink"
          :bold t))))
   ;;;}}}
   ;;;{{{ LaTeX faces:
   '(font-latex-verbatim-face
     ((t (:foreground "salmon"
          :bold t))))
   ;;;}}}
   ;;;{{{ Semantic faces:
   '(semantic-decoration-on-unknown-includes
     ((t (:background "#2f0101"))))
   '(semantic-decoration-on-fileless-includes
     ((t (:background "#13191c"))))
   '(semantic-decoration-on-unparsed-includes
     ((t (:background "#1c151e"))))
   '(semantic-decoration-on-private-members-face
     ((t (:background "#231021"))))
   '(semantic-decoration-on-protected-members-face
     ((t (:background "#010520"))))
   '(semantic-highlight-edits-face
     ((t (:background "#001205"))))
   '(semantic-highlight-func-current-tag-face
     ((t (:background "#171717"))))
   '(senator-momentary-highlight-face
     ((t (:background "#202020"))))
   '(senator-read-only-face
     ((t (:background "#190102"))))
   ;;;}}}
   ;;;{{{ Speedbar:
   '(speedbar-button-face
     ((t (:inherit custom-button))))
   '(speedbar-directory-face
     ((t (:foreground "royalblue"))))
   '(speedbar-file-face
     ((t (:foreground "cadetblue"))))
   '(speedbar-highlight-face
     ((t (:inherit secondary-selection))))
   '(speedbar-selected-face
     ((t (:inherit custom-button-mouse))))
   '(speedbar-separator-face
     ((t (:background "blue"
          :foreground "white"
          :box (:line-width 1 :color "darkblue" :style released-button)))))
   ;;;}}}
   ;;;{{{ Company:
   '(company-tooltip
     ((t (:background "gray15"
          :box (:line-width 1 :color "grey5" :style released-button)))))
   '(company-tooltip-common
     ((t (:inherit company-tooltip
          :foreground "orange4"))))
   '(company-tooltip-common-selection
     ((t (:inherit company-tooltip-selection
          :foreground "goldenrod"))))
   '(company-tooltip-selection
     ((t (:inherit company-tooltip
          :foreground "white"))))
   ;;;}}}
   ))

;;;------------------------------------------------------------------
;;;{{{ Paren highlighting:
;;;
;;; This isn't set via `custom-set-face', but rather
;;;`custom-set-variable'.

(if terminal-p
    (custom-set-variables
     '(hl-paren-colors (quote ("magenta" "cyan" "green"
                               "red" "blue" "white"))))
    (custom-set-variables
     '(hl-paren-colors (quote ("firebrick1" "DarkRed" "IndianRed"
                               "LightCoral" "Salmon" "DarkSalmon")))))

;;;}}}
;;;------------------------------------------------------------------

;;;}}}
;;; ==================================================================

;;; theme.el ends here

  
