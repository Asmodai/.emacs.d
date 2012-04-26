;;; -*- Mode: Emacs-Lisp -*-
;;;
;;; theme.el --- Emacs colour theme.
;;;
;;; Time-stamp: <Thursday Apr 26, 2012 20:27:44 asmodai>
;;; Revision:   31
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
      '((((class color) (background dark)) (:foreground "steelblue"))
        (((class color) (background light)) (:foreground "steelblue"))
        (((class grayscale) (background light)) (:foreground "steelblue"))
        (((class grayscale) (background dark)) (:foreground "steelblue")))
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
   (cond (windows-nt-p
          ;; Consolas first appeared in Windows XP as far as I recall,
          ;; so it might not be available in earlier versions, that's
          ;; a risk I'm willing to take for now. So, just assume we
          ;; have Consolas.
          '(default ((t (:family "DejaVu Sans Mono"
                         :size 9
                         :height 90
                         :background "black"
                         :foreground "white")))))
         ((and windows-p (not windows-nt-p))
          ;; Given that we are on a Windows that is /not/ Windows NT,
          ;; we probably do not have Consolas, so just go for Courier
          ;; New and be done with it.  This can always be fine-tuned
          ;; later on.
          '(default ((t (:family "Courier New"
                         :size 9
                         :height 90
                         :background "black"
                         :foreground "white")))))
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
              '(default ((t (:family "Interface User"
                             :size 10
                             :height 100
                             :background "black"
                             :foreground "white"))))
              ;; We're not using Motif, so let's try our luck with
              ;; DejaVu Sans Mono.  Probably a Bad Thing(tm) given
              ;; that not all Unix or Unix-like systems will have this
              ;; font.
              '(default ((t (:family "DejaVu Sans Mono"
                             :size 9
                             :height 90
                             :background "black"
                             :foreground "white"))))))
         (mac-os-x-p
          ;; Use Monaco and be done with it.
          '(default ((t (:family "Monaco"
                         :size 11
                         :height 110
                         :background "black"
                         :foreground "white")))))
         (presentation-manager-p
          ;; Use System VIO as the font on OS/2.
          '(default ((t (:family "System VIO"
                         :size 9
                         :height 90
                         :background "black"
                         :foreground "white")))))
         (t
          ;; Give up and let Emacs decide.
          '(default ((t (:size 9
                         :height 90
                         :background "black"
                         :foreground "white"))))))
   ;;;{{{ UI elements:
   '(blank-space-face ((t (nil))))
   '(blank-tab-face ((t (nil))))
   '(border ((t (nil))))
   '(border-glyph ((t (nil))))
   '(buffers-tab ((t (:background "gray65"
                      :foreground "black"
                      :bold t))))
   '(cursor ((t (:background "green3"))))
   '(fringe ((t (:background "grey3"))))
   '(gui-button-face ((t (:background "gray75"))))
   '(gui-element ((t (:background "gray80"))))
   '(highlight ((t (:background "gray20" :foreground "firebrick"))))
   '(highlight-changes-delete-face ((t (:underline t
                                        :foreground "red"))))
   '(highlight-changes-face ((t (:foreground "red"))))
   '(highline-face ((t (:background "grey95"))))
   '(lazy-highlight ((t (:background "gray20"
                         :foreground "aquamarine"))))
   '(linemenu-face ((t (nil))))
   '(minibuffer-prompt ((t (:foreground "purple"))))
   '(modeline ((t (:foreground "black" :background "gray70"))))
   '(modeline-buffer-id ((t (:background "gray70"
                             :foreground "darkgreen"))))
   '(modeline-mousable ((t (:background "gray70"
                            :foreground "darkblue"))))
   '(modeline-mousable-minor-mode ((t (:background "gray70"
                                       :foreground "firebrick"))))
   '(primary-selection ((t (:background "gray20"
                            :foreground "turquoise"))))
   '(region ((t (:background "gray20" :foreground "yellow"))))
   '(right-margin ((t (nil))))
   `(secondary-selection ((t (:background "gray20"
                              :foreground "paleturquoise"))))
   '(text-cursor ((t (:background "green3"))))
   '(tool-bar ((t (:background "grey" :foreground "black"))))
   '(toolbar ((t (:background "grey" :foreground "black"))))
   '(tooltip ((t (:background "lightyellow" :foreground "black"))))
   '(vcursor ((t (:background "lawngreen"))))
   '(vertical-divider ((t (:background "gray70" :foreground "black"))))
   '(zmacs-region ((t (:background "gray20" :foreground "yellow"))))
   ;;;}}}
   ;;;{{{ Standard font-lock faces:
   '(bold ((t (:bold t))))
   '(bold-italic ((t (:italic t
                      :bold t))))
   '(italic ((t (:italic t))))
   '(underline ((t (:underline t))))
   ;;;}}}
   ;;;{{{ Common colours:
   '(blue ((t (:foreground "blue"))))
   '(cyan ((t (:foreground "cyan"))))
   '(green ((t (:foreground "green"))))
   '(magenta ((t (:foreground "magenta"))))
   '(red ((t (:foreground "red"))))
   '(white ((t (:foreground "white"))))
   '(yellow ((t (:foreground "yellow"))))
   ;;;}}}
   ;;;{{{ Font-lock colours:
   '(font-lock-builtin-face ((t (:foreground "mediumorchid"))))
   '(font-lock-comment-delimiter-face ((t (:foreground "steelblue"))))
   '(font-lock-comment-face ((t (:foreground "mediumaquamarine"))))
   '(font-lock-constant-face ((t (:foreground "goldenrod3"))))
   '(font-lock-doc-face ((t (:foreground "forestgreen"))))
   '(font-lock-function-name-face ((t (:foreground "yellow" :bold t))))
   '(font-lock-keyword-face ((t (:foreground "salmon"))))
   '(font-lock-negation-char-face ((t (:background "grey10"
                                       :foreground "firebrick"))))
   '(font-lock-preprocessor-face ((t (:foreground "magenta4"))))
   '(font-lock-regexp-grouping-backslash ((t (:foreground "grey50"
                                              :bold t))))
   '(font-lock-string-face ((t (:foreground "lawngreen"))))
   '(font-lock-type-face ((t (:foreground "chocolate"))))
   '(font-lock-variable-name-face ((t (:foreground "orangered"
                                       :bold t))))
   '(font-lock-warning-face ((t (:foreground "red" :bold t))))
   ;;;}}}
   ;;;{{{ Widget faces:
   '(widget-button ((t (:bold t))))
   '(widget-button-face ((t (:background "gray10" :bold t))))
   '(widget-button-pressed-face ((t (:foreground "red"))))
   '(widget-documentation ((t (:foreground "green4" :italic t))))
   '(widget-documentation-face ((t (:foreground "green4" :italic t))))
   '(widget-field-face ((t (:background "gray15"))))
   '(widget-inactive-face ((t (:foreground "gray40" :background "gray10"))))
   '(widget-single-line-field-face ((t (:background "gray15"))))
   ;;;}}}
   ;;;{{{ Customize faces:
   '(custom-button ((t (:bold t))))
   '(custom-comment ((t (:background "gray15"))))
   '(custom-documentation ((t (:foreground "green2"))))
   '(custom-documentation-face ((t (:foreground "green2"))))
   '(custom-button-face ((t (:foreground "black"
                             :background "grey75"))))
   '(custom-button-unraised ((t (:background "grey65"
                                 :foreground "black"))))
   '(custom-variable-tag-face ((t (:underline t
                                   :foreground "mediumorchid"))))
   '(custom-group-tag-face ((t (:underline t
                                :foreground "cadetblue"))))
   '(custom-state-face ((t (:foreground "goldenrod3"))))
   '(custom-face-tag-face ((t (:foreground "royalblue"))))
   '(custom-variable-button-face ((t (:bold t
                                      :underline t
                                      :foreground "firebrick"))))
   '(custom-set-face ((t (:foreground "blue" :background "white"))))
   '(custom-changed-face ((t (:foreground "white"
                              :background "royalblue"
                              :bold t))))
   '(custom-modified-face ((t (:foreground "white" :background "blue"))))
   '(custom-invalid-face ((t (:foreground "yellow" :background "red"))))
   '(custom-saved-face ((t (:underline t))))
   '(custom-rogue-face ((t (:foreground "pink" :background "black"))))
   ;;;}}}
   ;;;{{{ ISearch faces:
   '(isearch ((t (:background "gray10" :foreground "azure"))))
   '(isearch-secondary ((t (:foreground "red3"))))
   '(isearch-fail ((t (:foreground "red" :bold t))))
   ;;;}}}
   ;;;{{{ CPerl faces:
   '(cperl-array-face ((t (:foreground "skyblue" :bold t))))
   '(cperl-hash-face ((t (:foreground "hotpink" :bold t))))
   '(cperl-nonoverridable-face ((t (:foreground "chartreuse3"))))
   '(cperl-here-face ((t (:foreground "green" :background "grey25"
                          :bold t))))
   '(cperl-pod-head-face ((t (:foreground "green3"))))
   '(cperl-pod-face ((t (:foreground "firebrick"))))
   ;;;}}}
   ;;;{{{ Dired faces:
   '(dired-face-directory ((t (:bold t))))
   ;;;}}}
   ;;;{{{ ERC faces:
   '(erc-capab-identify-unidentified ((t (:foreground "firebrick"))))
   '(erc-current-nick-face ((t (:foreground "turquoise" :bold t))))
   '(erc-header-line ((t (:background "gray30" :foreground "gray70"))))
   '(erc-input-face ((t (:foreground "cadetblue"))))
   '(erc-my-nick-face ((t (:foreground "cadetblue" :bold t))))
   '(erc-nick-default-face ((t (:bold t))))
   '(erc-pal-face ((t (:foreground "darkmagenta" :bold t))))
   '(erc-prompt-face ((t (:background "black"
                          :foreground "lightblue2"
                          :bold t))))
   '(erc-timestamp-face ((t (:foreground "green4" :bold t))))
   ;;;}}}
   ;;;{{{ Paren faces:
   '(show-paren-match ((t (:background "gray15"
                           :foreground "lawngreen"
                           :bold t))))
   '(show-paren-mismatch ((t (:background "gray15"
                              :foreground "red"
                              :bold t))))
   ;;;}}}
   ;;;{{{ Browser faces:
   '(link ((t (:foreground "slateblue" :underline t))))
   '(link-visited ((t (:foreground "magenta3" :underline t))))
   ;;;}}}
   ;;;{{{ IBuffer faces:
   '(ibuffer-occur-match-face ((t (:foreground "firebrick"))))
   ;;;}}}
   ;;;{{{ Hyper-Apropos faces:
   '(hyper-apropos-major-heading ((t (:foreground "steelblue"
                                      :bold t))))
   '(hyper-apropos-section-heading ((t (:foreground "cadetblue"
                                        :bold t))))
   '(hyper-apropos-heading ((t (:bold t :foreground "skyblue"))))
   '(hyper-apropos-documentation ((t (:foreground "forestgreen"))))
   '(hyper-apropos-hyperlink ((t (:foreground "slateblue"
                                  :underline t))))
   '(hyper-apropos-warning ((t (:foreground "red" :bold t))))
   ;;;}}}
   ;;;{{{ Info faces:
   '(info-xref ((t (:foreground "slateblue" :bold t :underline t))))
   '(info-node ((t (:foreground "steelblue" :bold t))))
   ;;;}}}
   ;;;{{{ Man viewer faces:   (n.b. not woman)
   '(man-italic ((t (:foreground "blanchedalmond" :italic t))))
   ;;;}}}
   ;;;{{{ SLIME faces:
   '(sldb-restart-number-face ((t (:bold t))))
   '(slime-inspector-value-face ((t (:foreground "mediumblue"
                                     :bold t))))
   '(slime-reader-conditional-face ((t (:foreground "DimGray"
                                        :bold t))))
   '(slime-repl-input-face ((t (:bold t))))
   '(slime-repl-output-mouseover-face ((t (:bold t))))
   ;;;}}}
   ;;;{{{ `About' faces:
   '(about-headling-face ((t (:foreground "hotpink" :bold t))))
   '(about-link-face ((t (:foreground "steelblue" :underline t))))
   ;;;}}}
   ;;;{{{ Calendar faces:
   '(calendar-today-face ((t (:foreground "hotpink" :bold t))))
   ;;;}}}
   ;;;{{{ LaTeX faces:
   '(font-latex-verbatim-face ((t (:foreground "salmon" :bold t))))
   ;;;}}}
   ))

;;;}}}
;;; ==================================================================

;;; theme.el ends here
