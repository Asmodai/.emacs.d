;;; zmacs-themes.el --- ZMACS themes  -*- mode: emacs-lisp; lexical-binding: t; -*-
;;
;; Copyright (c) 2024 Paul Ward <paul@lisphacker.uk>
;;
;; Author:     Paul Ward <paul@lisphacker.uk>
;; Maintainer: Paul Ward <paul@lisphacker.uk>
;; Created:    17 Oct 2024 19:48:42
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
;; You want to `M-x rainbow-mode' for the best visuals.

;;; Code:

(require 'cl-lib)
(require 'zlisp-colour)

;;;; Hooks:

(defvar zmacs-themes-after-load-theme-hook nil
  "Hook that ise run after zmacs-theme is loaded using `load-theme`.")

;;;; Customize group:

(defgroup zmacs-faces nil
  "Faces for ZMACS' theme."
  :group 'faces)

;;;; Colour tables:
;;;;; UI table:

(let ((kde-background "#131117")
      (base-colour    "#8253ca")
      (base-grey      "#808080")
      (base-black     "#100d13")
      (base-white     "#ffffff")
      (base-derive    "#A98EDF"))
  (defconst zmacs-ui-colours
    `((:base  . ,base-colour)
      (:black . ,base-black)
      (:grey  . ,base-grey)
      (:white . ,base-white)
      ;;
      ;; KDE theme.
      (:view-background      . ,kde-background)
      (:view-text            . "#d4d2cf")
      (:window-background    . "#2b2a33")
      (:window-text          . "#e0dedb")
      (:button-background    . "#3a3946")
      (:button-text          . "#e8e6e3")
      (:highlight-background . "#222027")
      (:selection-background . "#422e5e")
      (:selection-text       . ,base-white)
      (:cursor               . "#a23ffe")
      ;;
      ;; Tints:  base -> white.
      ,@(zlisp/list->alist "tint"
                           (zlisp/gradient-oklab base-colour
                                                 base-white
                                                 14
                                                 :inclusive nil
                                                 :safe      t))
      ;;
      ;; Tones: base -> background.
      ,@(zlisp/list->alist "tone"
                           (zlisp/gradient-oklab base-colour
                                                 kde-background
                                                 14
                                                 :inclusive nil
                                                 :safe      t))
      ;;
      ;; Shades: base -> grey.
      ,@(zlisp/list->alist "shade"
                           (zlisp/gradient-oklab base-colour
                                                 base-grey
                                                 14
                                                 :inclusive nil
                                                 :safe      t))
      ;;
      ;; Greys.
      ,@(zlisp/list->alist "grey"
                           (zlisp/gradient-oklab
                            (zlisp/adjust-brightness-oklab base-grey +5)
                            (zlisp/adjust-brightness-oklab base-grey -75)
                            14
                            :inclusive nil
                            :safe t))
      ;;
      ;; Base colours.
      ,@(zlisp/palette-to-list (zlisp/palette-generate base-derive
                                                       :include-extras t))
      ;;
      ;; Primary colours.

      )
    "Base UI colour ladders generated from #8353ca in OKLab."))

(defsubst zmacs--get-ui-colour (name)
  "Find NAME in the list of ZMACS UI colours.

If NAME is not found, then the default face foreground is returned."
  (let ((found (assoc name zmacs-ui-colours)))
    (if (null found)
        (face-foreground 'default)
      (cdr found))))

(defconst zmacs-primary-colours
  (zlisp/palette-to-list (zlisp/palette-generate
                          (zmacs--get-ui-colour :base)))
  "Primary colours.")

(defsubst zmacs--get-primary-colour (name)
  "Find NAME in the list of primary colours.

If NAME is not found, then the default face foreground is returned."
  (let ((found (assoc name zmacs-primary-colours)))
    (if (null found)
        (face-foreground 'default)
      (cdr found))))


;;;; Gradients:
;;;;; Comments:

(defconst zmacs-ui-comment-colours
  `(,@(zlisp/gradient-ladder-square-complements-oklab
       (zmacs--get-ui-colour :bright-steel)
       8                                ; steps
       -2))                             ; amount
  "Colours used by comment-like things that require gradient variation.

This includes things like `outline-mode', `outline-minor-mode' et al.")

(defsubst zmacs--get-ui-comment-level (level)
  "Return the colour for the given comment LEVEL.

If LEVEL is greater than the number of comment levels available, then the
colour returned is that of `:bright-steel'."
  (let ((len (length zmacs-ui-comment-colours)))
    (if (>= level len)
        (zmacs--get-ui-colour :bright-steel)
      (nth level zmacs-ui-comment-colours))))

;;;; Default colours:

(defcustom zmacs-colour-background (zmacs--get-ui-colour :view-background)
  "Default background colour."
  :type 'string
  :group 'zmacs-faces)

(defcustom zmacs-colour-text-dark (zmacs--get-ui-colour :black)
  "Default dark text colour."
  :type 'string
  :group 'zmacs-faces)

(defcustom zmacs-colour-text-light (zmacs--get-ui-colour :view-text)
  "Default light text colour."
  :type 'string
  :group 'zmacs-faces)

;;;; Fonts:
;;;;; Utility functions:

(defsubst zmacs--x-list-fonts (pattern)
  "Return a list of the names of available fonts matching PATTERN.

If we are not using a graphical display, then NIL will be returned.
Otherwise, the result of `x-list-fonts' for the pattern is returned."
  (if (and (display-graphic-p)
           (fboundp 'x-list-fonts))
      (x-list-fonts pattern)
    nil))

(defsubst zmacs--x-family-fonts (family)
  "Return a list of available fonts of family FAMILY.

If we are not using a graphical display, then NIL will be returned.
Otherwise, the result of `x-family-fonts' for the family is returned."
  (if (and (display-graphic-p)
           (fboundp 'x-family-fonts))
      (x-family-fonts family)
    nil))

;;;;; Custom variables:

(defcustom zmacs-font-size-fixed (if (eq window-system 'ns)
                                     130
                                   100)
  "Default fixed font size."
  :type 'number
  :group 'zmacs-faces)

(defcustom zmacs-font-size-variable (if (eq window-system 'ns)
                                        130
                                      100)
  "Default variable font size."
  :type 'number
  :group 'zmacs-faces)

(defcustom zmacs-face-variable
  (cond ((zmacs--x-list-fonts "ETBembo")         '(:font   "ETBembo"))
        ((zmacs--x-list-fonts "Source Sans Pro") '(:font   "Source Sans Pro"))
        ((zmacs--x-list-fonts "Lucida Grande")   '(:font   "Lucida Grande"))
        ((zmacs--x-list-fonts "Verdana")         '(:font   "Verdana"))
        ((zmacs--x-list-fonts "DejaVu Sans")     '(:font   "DejaVu Sans"))
        ((zmacs--x-list-fonts "Sans Serif")      '(:family "Sans Serif"))
        (nil
         (warn "Could not find a suitable sans-serif font.")
         '(:font "Helvetica")))
  "Font to use for variable-pitch faces.

The font is determined by checking the availability of various fonts on your
system.  You can override this behaviour by setting this custom variable."
  :type 'list
  :group 'zmacs-faces)

(defcustom zmacs-face-fixed
  (cond ((zmacs--x-list-fonts "Source Code Pro for Powerline")
         '(:font "Source Code Pro for Powerline"))
        ((zmacs--x-list-fonts "Sauce Code Powerline")
         '(:font "Sauce Code Powerline"))
        ((zmacs--x-list-fonts "Source Code Pro")
         '(:font "Source Code Pro"))
        ((zmacs--x-list-fonts "DejaVu Sans Mono for Powerline")
         '(:font "DejaVu Sans Mono for Powerline"))
        ((zmacs--x-list-fonts "DejaVu Sans Mono")
         '(:font "DejaVu Sans Mono"))
        ((zmacs--x-list-fonts "Droid Sans Mono for Powerline")
         '(:font "Droid Sans Mono for Powerline"))
        ((zmacs--x-list-fonts "Droid Sans Mono")
         '(:font "Droid Sans Mono"))
        ((zmacs--x-list-fonts "Consolas")
         '(:font "Consolas"))
        ((zmacs--x-list-fonts "Menlo")
         '(:font "Menlo"))
        ((zmacs--x-list-fonts "Courier New")
         '(:font "Courier New"))
        ((zmacs--x-family-fonts "Monospace")
         '(:family "Monospace"))
        (nil
         (warn "Could not find a suitable monospace font.")
         '(:font "Monospace")))
  "Font to use for fixed-pitch faces.

The font is determined by checking the availability of various fonts on your
system.  You can override this behaviour by setting this custom variable."
  :type 'list
  :group 'zmacs-faces)

;;;; Default faces:

(custom-set-faces
 ;;
 ;; Default face:
 `(default
    ;;
    ;; Terminal:
    ((((type tty))
      (:width          normal
       :weight         normal
       :slant          normal
       :underline      nil
       :overline       nil
       :extend         nil
       :strike-through nil
       :box            nil
       :inverse-video  nil
       :foreground     ,zmacs-colour-text-light
       :background     unspecified      ; So terminal background is used.
       :stipple        nil
       :inherit        nil))
     ;;
     ;; Everything else:
     (t
      (,@zmacs-face-fixed
       :width          normal
       :height         ,zmacs-font-size-fixed
       :weight         normal
       :slant          normal
       :underline      nil
       :overline       nil
       :extend         nil
       :strike-through nil
       :box            nil
       :inverse-video  nil
       :foreground     ,zmacs-colour-text-light
       :background     ,zmacs-colour-background
       :stipple        nil
       :inherit        nil))))
 ;;
 ;; Variable pitch face:
 `(variable-pitch
   ((t (,@zmacs-face-variable
        :width   normal
        :height  ,zmacs-font-size-variable
        :weight  normal
        :slant   normal
        :inherit nil))))
 ;;
 ;; Fixed pitch face:
 `(fixed-pitch
   ((t (,@zmacs-face-fixed
        :width   normal
        :height  ,zmacs-font-size-fixed
        :weight  normal
        :slant   normal
        :inherit nil)))))

;;;; Faces:
;;;;; Background and foreground:

(defface zmacs-background nil
  "Background face."
  :group 'zmacs-faces)

(defface zmacs-foreground nil
  "Foreground face."
  :group 'zmacs-faces)

;;;;; Highlight faces:

(defface zmacs-light-ultra nil
  "Important structural information such as line numbers."
  :group 'zmacs-faces)

(defface zmacs-light-high nil
  "Useful structural information such as fringes."
  :group 'zmacs-faces)

(defface zmacs-light-low nil
  "Useful for less important structural information."
  :group 'zmacs-faces)

;;;;; Priority faces:

(defface zmacs-priority-critical nil
  "A face for elements that absolutely require your attention."
  :group 'zmacs-faces)

(defface zmacs-priority-urgent nil
  "A face for elements that require your attention.

It should stick out from any other faces currently displayed."
  :group 'zmacs-faces)

(defface zmacs-priority-crucial nil
  "A face for displaying important information.

It does not have to stick out as much as `urgent', but should still grab your
attention."
  :group 'zmacs-faces)

(defface zmacs-priority-focus nil
  "A face for displaying information that might need attention."
  :group 'zmacs-faces)

(defface zmacs-priority-ok nil
  "A face for displaying affirmative information."
  :group 'zmacs-faces)

;;;;; Structural faces:

(defface zmacs-priority-strong nil
  "A face for a strong structural accent in contrast with the normal face."
  :group 'zmacs-faces)

(defface zmacs-priority-meek nil
  "A face for structural elements that are useful but less important."
  :group 'zmacs-faces)

(defface zmacs-priority-mild nil
  "A face for shading to differentiate from the background."
  :group 'zmacs-faces)

(defface zmacs-priority-faint nil
  "A face for very slicht accenting or shading."
  :group 'zmacs-faces)

;;;;; ZMACS Org style faces:

(defgroup zmacs-org-style-faces nil
  "Faces for ZMACS Org styles."
  :group 'zmacs-faces)

;;;;;; Org Present

(defgroup zmacs-org-style-present-faces nil
  "Faces for `org-present'."
  :group 'zmacs-org-style-faces)

(defface zmacs-org-style-present-default nil
  "Default face for `org-present'."
  :group 'zmacs-org-style-present-faces)

(defface zmacs-org-style-present-fixed-pitch nil
  "Fixed pitch face for `org-present'."
  :group 'zmacs-org-style-present-faces)

(defface zmacs-org-style-present-header-line nil
  "Header line face for `org-present'."
  :group 'zmacs-org-style-present-faces)

(defface zmacs-org-style-present-document-info nil
  "Document info face for `org-present'."
  :group 'zmacs-org-style-present-faces)

(defface zmacs-org-style-present-document-title nil
  "Document title face for `org-present'."
  :group 'zmacs-org-style-present-faces)

(defface zmacs-org-style-present-level-1 nil
  "Level 1 heading face for `org-present'."
  :group 'zmacs-org-style-present-faces)

(defface zmacs-org-style-present-level nil
  "Heading face for `org-present'."
  :group 'zmacs-org-style-present-faces)

(defface zmacs-org-style-present-code nil
  "Code face for `org-present'."
  :group 'zmacs-org-style-present-faces)

(defface zmacs-org-style-present-verbatim nil
  "Verbatim face for `org-present'."
  :group 'zmacs-org-style-present-faces)

(defface zmacs-org-style-present-block nil
  "Block face for `org-present'."
  :group 'zmacs-org-style-present-faces)

(defface zmacs-org-style-present-block-begin-line nil
  "Block begin line face for `org-present'."
  :group 'zmacs-org-style-present-faces)

(defface zmacs-org-style-present-block-end-line nil
  "Block end line face for `org-present'."
  :group 'zmacs-org-style-present-faces)

;;;;;; Org Agenda:

(defgroup zmacs-org-style-agenda-faces nil
  "Faces for `org-agenda'."
  :group 'zmacs-org-style-faces)

(defface zmacs-org-style-agenda-level-1 nil
  "Level 1 heading face for `org-agenda'."
  :group 'zmacs-org-style-agenda-faces)

(defface zmacs-org-style-agenda-level-2 nil
  "Level 2 heading face for `org-agenda'."
  :group 'zmacs-org-style-agenda-faces)

(defface zmacs-org-style-agenda-level-3 nil
  "Level 3 heading face for `org-agenda'."
  :group 'zmacs-org-style-agenda-faces)

(defface zmacs-org-style-agenda-level nil
  "Heading face for `org-agenda'."
  :group 'zmacs-org-style-agenda-faces)

;;;;;; Denote:

(defgroup zmacs-org-style-denote-faces nil
  "Faces for `denote'."
  :group 'zmacs-org-style-faces)

(defface zmacs-org-style-denote-level-1 nil
  "Level 1 heading face for `denote'."
  :group 'zmacs-org-style-denote-faces)

(defface zmacs-org-style-denote-level-2 nil
  "Level 2 heading face for `denote'."
  :group 'zmacs-org-style-denote-faces)

(defface zmacs-org-style-denote-level-3 nil
  "Level 3 heading face for `denote'."
  :group 'zmacs-org-style-denote-faces)

(defface zmacs-org-style-denote-level nil
  "Heading face for `denote'."
  :group 'zmacs-org-style-denote-faces)

;;;;; Colours:

(defface zmacs-colour-black     nil "Black face."      :group 'zmacs-faces)
(defface zmacs-colour-white     nil "White face."      :group 'zmacs-faces)
(defface zmacs-colour-red       nil "Red face."        :group 'zmacs-faces)
(defface zmacs-colour-rose      nil "Rose face."       :group 'zmacs-faces)
(defface zmacs-colour-umber     nil "Umber face."      :group 'zmacs-faces)
(defface zmacs-colour-orange    nil "Orange face."     :group 'zmacs-faces)
(defface zmacs-colour-yellow    nil "Yellow face."     :group 'zmacs-faces)
(defface zmacs-colour-green     nil "Green face."      :group 'zmacs-faces)
(defface zmacs-colour-teal      nil "Teal face."       :group 'zmacs-faces)
(defface zmacs-colour-turquoise nil "Turquoise face."  :group 'zmacs-faces)
(defface zmacs-colour-cyan      nil "Cyan face."       :group 'zmacs-faces)
(defface zmacs-colour-blue      nil "Blue face."       :group 'zmacs-faces)
(defface zmacs-colour-violet    nil "Violet face."     :group 'zmacs-faces)
(defface zmacs-colour-magenta   nil "Magenta face."    :group 'zmacs-faces)
(defface zmacs-colour-steel     nil "Steel face."      :group 'zmacs-faces)

;;;;; Tag faces:

(defface zmacs-tag-todo nil
  "A face for `todo' tags."
  :group 'zmacs-faces)

(defface zmacs-tag-doing nil
  "A face for `doing' tags."
  :group 'zmacs-faces)

(defface zmacs-tag-done nil
  "A face for `done' tags."
  :group 'zmacs-faces)

(defface zmacs-tag-wait nil
  "A face for `wait' tags."
  :group 'zmacs-faces)

(defface zmacs-tag-hold nil
  "A face for `hold' tags."
  :group 'zmacs-faces)

(defface zmacs-tag-cancelled nil
  "A face for `cancelled' tags."
  :group 'zmacs-faces)

;;;;; Custom Org faces:

(defface zmacs-org-list-symbol nil
  "Face used for `org-mode' list symbols."
  :group 'zmacs-faces)

;;;;; Meta faces:
;;;;;; REPLs:

(defface zmacs-repl-prompt-face
  `((t (:foreground ,(zmacs--get-ui-colour :blue)
        :background unspecified)))
  "Face for REPL prompts."
  :group 'zmacs-faces)

(defface zmacs-repl-input-face
  `((t (:foreground ,zmacs-colour-text-light
        :background unspecified)))
  "Face for REPL input text."
  :group 'zmacs-faces)

(defface zmacs-repl-output-face
  `((t (:foreground ,(zmacs--get-ui-colour :orange)
        :background unspecified)))
  "Face for REPL output text."
  :group 'zmacs-faces)

(defface zmacs-repl-inline-ui-face
  `((t (:foreground ,(zmacs--get-ui-colour :bright-orange)
        :box (:line-width (1 . 1)
              :color      ,(zmacs--get-ui-colour :base)
              :style      flat-button))))
  "Face for inline UI elements that might be used by a REPL."
  :group 'zmacs-faces)

;;;; Theme definition:

(defun zmacs-themes-create (_ theme-name)
  (let ((class '((class color) (min-colors 89)))
;;;;; Colour definitions:
        ;;
        ;; Basic
        (colour-background zmacs-colour-background)
        (colour-text-light zmacs-colour-text-light)
        (colour-text-dark  zmacs-colour-text-dark)
        (colour-window     (zmacs--get-ui-colour :window-background))
        ;;
        ;; Highlighting.
        (colour-ultralight (zmacs--get-ui-colour :grey-9))
        (colour-highlight  (zmacs--get-ui-colour :grey-10))
        (colour-lowlight   (zmacs--get-ui-colour :grey-13))
        ;;
        ;; Headings.
        (colour-heading1 (zmacs--get-ui-colour :tint-10))
        (colour-heading2 (zmacs--get-ui-colour :tint-8))
        (colour-heading3 (zmacs--get-ui-colour :tint-6))
        (colour-heading4 (zmacs--get-ui-colour :tint-4))
        (colour-heading? (zmacs--get-ui-colour :tint-2))
        ;;
        ;; Comments:
        (colour-comment1 (zmacs--get-ui-comment-level 0))
        (colour-comment2 (zmacs--get-ui-comment-level 1))
        (colour-comment3 (zmacs--get-ui-comment-level 2))
        (colour-comment4 (zmacs--get-ui-comment-level 3))
        (colour-comment5 (zmacs--get-ui-comment-level 4))
        (colour-comment6 (zmacs--get-ui-comment-level 5))
        (colour-comment7 (zmacs--get-ui-comment-level 6))
        (colour-comment8 (zmacs--get-ui-comment-level 7))
        ;;
        ;; Priorities:
        (colour-critical-bg (zmacs--get-ui-colour :dim-red))
        (colour-critical-fg (zmacs--get-ui-colour :bright-red))
        (colour-urgent-bg   (zmacs--get-ui-colour :dim-orange))
        (colour-urgent-fg   (zmacs--get-ui-colour :bright-orange))
        (colour-crucial-bg  (zmacs--get-ui-colour :dim-yellow))
        (colour-crucial-fg  (zmacs--get-ui-colour :bright-yellow))
        (colour-focus-bg    (zmacs--get-ui-colour :dim-blue))
        (colour-focus-fg    (zmacs--get-ui-colour :bright-blue))
        (colour-ok-bg       (zmacs--get-ui-colour :dim-green))
        (colour-ok-fg       (zmacs--get-ui-colour :bright-green))
        ;;
        ;; Structural priority:
        (colour-strong (zmacs--get-ui-colour :tint-3))
        (colour-meek   (zmacs--get-ui-colour :grey-0))
        (colour-mild   (zmacs--get-ui-colour :grey-2))
        (colour-faint  (zmacs--get-ui-colour :grey-4))
        )
;;;;; Faces:
    (custom-set-faces
;;;;;; Defaults:
     `(cursor
       ((,class (:background ,(zmacs--get-ui-colour :cursor)))))
     `(fringe
       ((,class (:background ,(zmacs--get-ui-colour :view-background)
                 :foreground ,colour-highlight))))
     `(highlight
       ((,class (:background ,(zmacs--get-ui-colour :button-background)))))
     `(hl-line
       ((,class (:background ,(zmacs--get-ui-colour :highlight-background)))))
     `(region
       ((,class (:background ,(zmacs--get-ui-colour :selection-background)))))
     `(secondary-selection
       ((,class (:background ,colour-highlight))))
     `(buffer-menu-buffer
       ((,class (:foreground ,(zmacs--get-ui-colour :blue)
                 :background unspecified))))
     `(minibuffer-prompt
       ((,class (:inherit zmacs-repl-prompt-face))))
     `(link
       ((,class (:foreground ,(zmacs--get-ui-colour :bright-blue)
                 :background unspecified
                 :underline  t))))
     `(shadow
       ((,class (:foreground ,colour-meek
                 :weight     light))))
;;;;;; ZMACS faces:
;;;;;;; Foreground and background faces:
     `(zmacs-background
       ((,class (:background ,colour-background))))
     `(zmacs-foreground
       ((,class (:foreground ,colour-text-light))))
;;;;;;; Highlighting:
     `(zmacs-light-ultra
       ((,class (:background ,colour-ultralight))))
     `(zmacs-light-high
       ((,class (:foreground ,colour-highlight))))
     `(zmacs-light-low
       ((,class (:foreground ,colour-lowlight))))
;;;;;;; Priorities:
     `(zmacs-priority-critical
       ((,class (:foreground ,colour-critical-fg
                 :weight     bold))))
     `(zmacs-priority-urgent
       ((,class (:foreground ,colour-urgent-fg
                 :weight     bold))))
     `(zmacs-priority-crucial
       ((,class (:foreground ,colour-crucial-fg
                 :weight     semi-bold))))
     `(zmacs-priority-focus
       ((,class (:foreground ,colour-focus-fg))))
     `(zmacs-priority-ok
       ((,class (:foreground ,colour-ok-fg))))
;;;;;;; Structural priorities:
     `(zmacs-priority-strong
       ((,class (:foreground ,colour-strong
                 :weight     semi-bold))))
     `(zmacs-priority-mild
       ((,class (:foreground ,colour-mild))))
     `(zmacs-priority-meek
       ((,class (:foreground ,colour-meek))))
     `(zmacs-priority-faint
       ((,class (:foreground ,colour-faint))))
;;;;;;; Primary colours:
     `(zmacs-colour-black
       ((,class (:foreground ,(zmacs--get-ui-colour :black)))))
     `(zmacs-colour-white
       ((,class (:foreground ,(zmacs--get-ui-colour :white)))))
     `(zmacs-colour-red
       ((,class (:foreground ,(zmacs--get-ui-colour :red)))))
     `(zmacs-colour-rose
       ((,class (:foreground ,(zmacs--get-ui-colour :rose)))))
     `(zmacs-colour-umber
       ((,class (:foreground ,(zmacs--get-ui-colour :umber)))))
     `(zmacs-colour-orange
       ((,class (:foreground ,(zmacs--get-ui-colour :orange)))))
     `(zmacs-colour-yellow
       ((,class (:foreground ,(zmacs--get-ui-colour :yellow)))))
     `(zmacs-colour-green
       ((,class (:foreground ,(zmacs--get-ui-colour :green)))))
     `(zmacs-colour-teal
       ((,class (:foreground ,(zmacs--get-ui-colour :teal)))))
     `(zmacs-colour-turquoise
       ((,class (:foreground ,(zmacs--get-ui-colour :turquoise)))))
     `(zmacs-colour-teal
       ((,class (:foreground ,(zmacs--get-ui-colour :teal)))))
     `(zmacs-colour-blue
       ((,class (:foreground ,(zmacs--get-ui-colour :blue)))))
     `(zmacs-colour-cyan
       ((,class (:foreground ,(zmacs--get-ui-colour :cyan)))))
     `(zmacs-colour-violet
       ((,class (:foreground ,(zmacs--get-ui-colour :violet)))))
     `(zmacs-colour-magenta
       ((,class (:foreground ,(zmacs--get-ui-colour :magenta)))))
     `(zmacs-colour-steel
       ((,class (:foreground ,(zmacs--get-ui-colour :steel)))))
;;;;;;; Todo lists:
     `(zmacs-tag-todo
       ((,class (:foreground ,(zmacs--get-ui-colour :bright-yellow)
                 :background unspecified
                 :weight     bold))))
     `(zmacs-tag-doing
       ((,class (:foreground ,(zmacs--get-ui-colour :bright-cyan)
                 :background unspecified
                 :weight     bold))))
     `(zmacs-tag-done
       ((,class (:foreground ,(zmacs--get-ui-colour :bright-green)
                 :background unspecified
                 :weight     bold))))
     `(zmacs-tag-wait
       ((,class (:foreground ,(zmacs--get-ui-colour :bright-orange)
                 :background unspecified
                 :weight     bold))))
     `(zmacs-tag-hold
       ((,class (:foreground ,(zmacs--get-ui-colour :bright-magenta)
                 :background unspecified
                 :weight     bold))))
     `(zmacs-tag-cancelled
       ((,class (:foreground ,(zmacs--get-ui-colour :bright-)
                 :background unspecified
                 :weight     bold))))
;;;;;; Basic faces:
     `(error
       ((,class (:foreground ,colour-critical-fg
                 :background unspecified
                 :bold       t))))
     `(success
       ((,class (:foreground ,colour-ok-fg
                 :background unspecified
                 :bold       t))))
     `(warning
       ((,class (:foreground ,colour-crucial-fg
                 :background unspecified
                 :bold       t))))
     `(alert-low-face
       ((,class (:foreground ,colour-urgent-fg
                 :background unspecified))))
     `(escape-glyph
       ((,class (:foreground ,(zmacs--get-ui-colour :red)
                 :background unspecified))))
     `(homoglyph
       ((,class (:foreground ,colour-focus-fg
                 :background unspecified))))
     `(match
       ((,class (:foreground ,colour-lowlight
                 :background ,colour-focus-fg))))
;;;;;; Font Lock faces:
     `(font-lock-builtin-face
       ((,class (:foreground ,(zmacs--get-ui-colour :bright-magenta)
                 :background unspecified))))
     `(font-lock-comment-delimiter-face
       ((,class (:foreground ,(zmacs--get-ui-colour :dim-steel)
                 :background unspecified))))
     `(font-lock-comment-face
       ((,class (:foreground ,(zmacs--get-ui-colour :bright-steel)
                 :background unspecified))))
     `(font-lock-constant-face
       ((,class (:foreground ,(zmacs--get-ui-colour :bright-cyan)
                 :background unspecified))))
     `(font-lock-string-face
       ((,class (:foreground ,(zmacs--get-ui-colour :green)
                 :background unspecified))))
     `(font-lock-number-face
       ((,class (:foreground ,(zmacs--get-ui-colour :cyan)
                 :background unspecified))))
     `(font-lock-function-name-face
       ((,class (:foreground ,(zmacs--get-ui-colour :blue)
                 :background unspecified))))
     `(font-lock-function-call-face
       ((,class (:inherit font-lock-function-name-face))))
     `(font-lock-variable-name-face
       ((,class (:foreground ,(zmacs--get-ui-colour :teal)
                 :background unspecified))))
     `(font-lock-variable-use-face
       ((,class (:inherit font-lock-variable-name-face))))
     `(font-lock-property-name-face
       ((,class (:foreground ,(zmacs--get-ui-colour :yellow)
                 :background unspecified))))
     `(font-lock-property-use-face
       ((,class (:inherit font-lock-property-name-face))))
     `(font-lock-keyword-face
       ((,class (:foreground ,(zmacs--get-ui-colour :bright-magenta)
                 :background unspecified))))
     `(font-lock-operator-face
       ((,class (:foreground ,(zmacs--get-ui-colour :steel)
                 :background unspecified))))
     `(font-lock-type-face
       ((,class (:foreground ,(zmacs--get-ui-colour :bright-violet)
                 :background unspecified))))
     `(font-lock-preprocessor-face
       ((,class (:foreground ,(zmacs--get-ui-colour :bright-orange)
                 :background unspecified))))
     `(font-lock-punctuation-face
       ((,class (:foreground ,(zmacs--get-ui-colour :bright-black)
                 :background unspecified))))
     `(font-lock-bracket-face
       ((,class (:foreground ,(zmacs--get-ui-colour :steel)
                 :background unspecified))))
     `(font-lock-delimiter-face
       ((,class (:foreground ,(zmacs--get-ui-colour :bright-black)
                 :background unspecified))))
     `(font-lock-misc-punctuation-face
       ((,class (:foreground ,(zmacs--get-ui-colour :dim-steel)
                 :background unspecified))))
     `(font-lock-doc-face
       ((,class (:foreground ,(zmacs--get-ui-colour :bright-umber)
                 :background unspecified))))
     `(font-lock-doc-markup-face
       ((,class (:inherit font-lock-doc-face))))
     `(font-lock-warning-face
       ((,class (:foreground ,colour-critical-fg
                 :background unspecified))))
     `(font-lock-negation-char-face
       ((,class (:foreground ,colour-urgent-fg
                 :background unspecified))))
     `(font-lock-regexp-grouping-construct
       ((,class (:foreground ,colour-crucial-fg
                 :background unspecified
                 :weight     bold))))
     `(font-lock-regexp-grouping-backslash
       ((,class (:foreground ,colour-urgent-fg
                 :background unspecified
                 :weight     bold))))
     `(font-lock-escape-face
       ((,class (:inherit font-lock-regexp-grouping-backslash))))
     `(font-lock-regexp-grouping-backslash
       ((,class (:foreground ,(zmacs--get-ui-colour :bright-red)
                 :weight     semi-bold))))
     `(font-lock-regexp-grouping-construct
       ((,class (:foreground ,(zmacs--get-ui-colour :bright-yellow)
                 :weight     semi-bold))))
;;;;;; Widget:
     `(widget-button
       ((,class (:foreground ,(zmacs--get-ui-colour :button-face)
                 :background ,(zmacs--get-ui-colour :button-background)))))
     `(widget-field
       ((, class (:foreground ,colour-text-light
                  :background ,(zmacs--get-ui-colour :grey-9)
                  :box (:line-width (1 . -1)
                        :color      ,(zmacs--get-ui-colour :grey-11)
                        :style      flat-button)))))
     `(widget-single-line-field
       ((, class (:foreground ,colour-text-light
                  :background ,(zmacs--get-ui-colour :grey-9)
                  :box (:line-width (1 . -1)
                        :color      ,(zmacs--get-ui-colour :grey-11)
                        :style      flat-button)))))
;;;;;; Customize:
     `(custom-button
       ((,class (:foreground ,(zmacs--get-ui-colour :button-face)
                 :background ,(zmacs--get-ui-colour :button-background)))))
     `(custom-button-unraised
       ((,class (:foreground ,(zmacs--get-ui-colour :button-face)
                 :background ,(zmacs--get-ui-colour :button-background)))))
;;;;;; Line Numbering:
     `(line-number
       ((,class (:foreground ,(zmacs--get-ui-colour :grey-5)
                 :background unspecified))))
     `(line-number-current-line
       ((,class (:foreground ,(zmacs--get-ui-colour :dim-steel)
                 :background ,(zlisp/adjust-brightness-oklab
                               (zmacs--get-ui-colour :highlight-background)
                               20)))))
;;;;;; Parens:
     `(show-paren-match
       ((,class (:foreground ,(zmacs--get-ui-colour :white)
                 :background ,(zmacs--get-ui-colour :dim-green)
                 :bold       t))))
     `(show-paren-mismatch
       ((,class (:foreground ,(zmacs--get-ui-colour :white)
                 :background ,(zmacs--get-primary-colour :dim-red)
                 :bold       t))))
     `(sp-pair-overlay-face
       ((,class (:bold    nil))))
     `(sp-show-pair-match-face
       ((,class (:bold    nil))))
     `(sp-show-pair-match-face
       ((,class (:bold    nil))))
     `(highlight-parentheses-highlight
       ((,class (:bold nil))))
;;;;;; Indicators:
     `(fill-column-indicator
       ((,class (:foreground ,(zmacs--get-ui-colour :grey-5)
                 :background unspecified))))
     `(indent-guide-face
       ((,class (:inherit fill-column-indicator))))
     `(whitespace-tab
       ((,class (:inherit fill-column-indicator))))
     `(whitespace-tabmark
       ((,class (:inherit fill-column-indicator))))
     `(whitespace-line
       ((,class (:inherit fill-column-indicator))))
     `(whitespace-space
       ((,class (:inherit fill-column-indicator))))
     `(whitespace-space-after-tab
       ((,class (:inherit fill-column-indicator))))
     `(whitespace-hspace
       ((,class (:inherit fill-column-indicator))))
     `(whitespace-newline
       ((,class (:inherit fill-column-indicator))))
     `(whitespace-indentation
       ((,class (:inherit fill-column-indicator))))
     `(whitespace-trailing
       ((,class (:background ,(zmacs--get-primary-colour :bright-red)))))
;;;;;; Highlight Indentation:
     `(highlight-indentation-current-column-face
       ((,class (:inherit fill-column-indicator
                 :background unspecified))))
     `(highlight-indentation-face
       ((,class (:inherit fill-column-indicator
                 :background unspecified))))
;;;;;; Highlight indentation guides:
     `(highlight-indent-guides-character-face
       ((,class (:inherit fill-column-indicator
                 :background unspecified))))
     `(highlight-indent-guides-top-character-face
       ((,class (:foreground ,(zmacs--get-ui-colour :dim-steel)
                 :background unspecified))))
     `(highlight-indent-guides-stack-character-face
       ((,class (:inherit line-number-current-line
                 :background unspecified))))
     `(highlight-indent-guides-stack-odd-face
       ((,class (:foreground ,(zmacs--get-ui-colour :silver)
                 :background unspecified))))
     `(highlight-indent-guides-stack-even-face
       ((,class (:foreground ,(zmacs--get-ui-colour :dim-steel)
                 :background unspecified))))
     `(highlight-indent-guides-top-odd-face
       ((,class (:foreground ,(zmacs--get-ui-colour :dim-silver)
                 :background unspecified))))
     `(highlight-indent-guides-top-even-face
       ((,class (:foreground ,(zmacs--get-ui-colour :dim-steel)
                 :background unspecified))))
     `(highlight-indent-guides-odd-face
       ((,class (:foreground ,(zmacs--get-ui-colour :dim-silver)
                 :background unspecified))))
     `(highlight-indent-guides-even-face
       ((,class (:foreground ,(zmacs--get-ui-colour :dim-steel)
                 :background unspecified))))
;;;;;; Auto Highlight Symbol:
     `(ahs-plugin-default-face
       ((,class (:foreground ,(zmacs--get-ui-colour :black)
                 :background ,(zmacs--get-ui-colour :bright-steel)))))
     `(ahs-plugin-default-face-unfocused
       ((,class (:foreground ,(zmacs--get-ui-colour :black)
                 :background ,(zmacs--get-ui-colour :steel)))))
     `(ahs-face
       ((,class (:foreground ,(zmacs--get-ui-colour :black)
                 :background ,(zmacs--get-ui-colour :silver)))))
     `(ahs-face-unfocused
       ((,class (:foreground ,(zmacs--get-ui-colour :black)
                 :background ,(zmacs--get-ui-colour :dim-silver)))))
;;;;;; Header line:
     `(header-line
       ((,class (:foreground ,(zmacs--get-ui-colour :window-text)
                 :background ,(zmacs--get-ui-colour :window-background)))))
;;;;;; Modeline:
     `(mode-line
       ((,class (:foreground ,(zmacs--get-ui-colour :tint-10)
                 :background ,(zmacs--get-ui-colour :tone-9)
                 :box (:color ,(zmacs--get-ui-colour :tone-3)
                       :line-width 1)))))
     `(mode-line-inactive
       ((,class (:foreground ,(zmacs--get-ui-colour :grey-0)
                 :background ,(zmacs--get-ui-colour :window-background)
                 :box (:color ,(zmacs--get-ui-colour :grey-7)
                       :line-width 1)))))
     `(mode-line-buffer-id
       ((,class (:inherit bold
                 :foreground ,(zmacs--get-ui-colour :yellow)))))
     `(mode-line-highlight
       ((,class (:inherit bold
                 :foreground ,(zmacs--get-ui-colour :orange)))))
;;;;;; Breadcrumbs:
     `(breadcrumb-face
       ((,class (:foreground ,(zmacs--get-ui-colour :grey)
                 :background ,(zmacs--get-ui-colour :window-background)))))
     `(breadcrumb-project-leaf-face
       ((,class (:foreground ,(zmacs--get-ui-colour :bright-yellow)
                 :background ,(zmacs--get-ui-colour :window-background)))))
;;;;;; Child frames:
     `(mini-popup-background
       ((,class (:background ,colour-faint))))
     `(mini-popup-border
       ((,class (:background ,colour-faint))))
;;;;;; Posframe:
     `(which-key-posframe
       ((,class (:background ,colour-faint))))
     `(which-key-posframe-border
       ((,class (:background ,colour-faint))))
     `(transient-posframe
       ((,class (:foreground ,colour-strong
                 :background ,colour-faint))))
     `(transient-posframe-border
       ((,class (:background ,colour-faint))))
;;;;;; General completion:
     `(completions-annotations
       ((,class (:foreground ,colour-strong))))
;;;;;; Company:
     `(company-scrollbar-bg
       ((,class (:inherit fringe))))
     `(company-scrollbar-fg
       ((,class (:background ,colour-mild))))
     `(company-tooltip
       ((,class (:background ,colour-lowlight))))
     `(company-tooltip-annotation
       ((,class (:foreground ,(zmacs--get-ui-colour :green)))))
     `(company-tooltip-annotation-selection
       ((,class (:inherit company-tooltip-annotation))))
     `(company-tooltip-selection
       ((,class (:foreground ,colour-text-light
                 :background ,(zmacs--get-ui-colour :window-background)))))
     `(company-tooltip-common
       ((,class (:foreground ,(zmacs--get-ui-colour :blue)
                 :underline t))))
     `(company-tooltip-common-selection
       ((,class (:foreground ,(zmacs--get-ui-colour :blue)
                 :underline t))))
     `(company-preview-common
       ((,class (:foreground ,colour-highlight))))
     `(company-preview
       ((,class (:background unspecified))))
     `(company-preview-search
       ((,class (:background ,(zmacs--get-ui-colour :cyan)))))
     `(company-template-field
       ((,class (:foreground ,(zmacs--get-ui-colour :black)
                 :background ,(zmacs--get-ui-colour :yellow)))))
     `(company-echo-common
       ((,class (:foreground ,(zmacs--get-ui-colour :red)))))
;;;;;; Dired:
     `(dired-broken-symlink
       ((,class (:foreground ,(zmacs--get-ui-colour :bright-yellow)
                 :background ,(zmacs--get-primary-colour :dim-red)
                 :weight     bold))))
;;;;;; Org:
;;;;;;; Text attributes:
     `(org-hide
       ((,class (:foreground ,colour-background))))
     `(org-code
       ((,class (:foreground ,(zmacs--get-ui-colour :bright-cyan)
                 :weight     bold))))
     `(org-verbatim
       ((,class (:foreground ,(zmacs--get-ui-colour :bright-green)
                 :weight     bold))))
     `(org-link
       ((,class (:foreground ,(zmacs--get-ui-colour :bright-blue)
                 :background unspecified
                 :underline  t))))
;;;;;;; Metadata:
     `(org-drawer
       ((,class (:foreground ,(zmacs--get-ui-colour :dim-steel)
                 :background unspecified))))
     `(org-meta-line
       ((,class (:foreground ,(zmacs--get-ui-colour :dim-silver)
                 :background unspecified))))
     `(org-special-keyword
       ((,class (:foreground ,(zmacs--get-ui-colour :dim-silver)
                 :background unspecified))))
     `(org-property-value
       ((,class (:inherit    font-lock-property-name-face
                 :background unspecified))))
     `(org-document-info
       ((,class (:foreground ,colour-comment2
                 :background unspecified))))
     `(org-document-info-keyword
       ((,class (:inherit    font-lock-keyword-face
                 :background unspecified))))
     `(org-document-title
       ((,class (:foreground ,(zmacs--get-ui-colour :bright-orange)
                 :background unspecified
                 :weight     bold
                 :underline  nil
                 :height     1.2))))
;;;;;;; Lists:
     `(zmacs-org-list-symbol
       ((,class (:foreground ,(zmacs--get-ui-colour :bright-magenta)
                 :background unspecified
                 :weight     bold))))
     `(org-checkbox
       ((,class (:foreground ,(zmacs--get-ui-colour :bright-green)
                 :background unspecified
                 :weight     bold))))
;;;;;;; Tables:
     `(org-table
       ((,class (:foreground ,colour-text-light
                 :background ,(zmacs--get-ui-colour :tone-13)))))
     `(org-table-header
       ((,class (:inherit    org-table
                 :foreground ,colour-text-light
                 :background ,(zmacs--get-ui-colour :tint-5)
                 :underline  nil
                 :overline   nil
                 :weight     bold))))
     `(org-table-row
       ((,class (:inherit org-table))))
;;;;;;; Blocks:
     `(org-block
       ((,class (:foreground unspecified
                 :background ,(zmacs--get-ui-colour :black)))))
     `(org-block-begin-line
       ((,class (:foreground ,(zmacs--get-ui-colour :bright-green)
                 :background unspecified
                 :weight     bold
                 :underline  t
                 :extend     t))))
     `(org-block-end-line
       ((,class (:foreground ,(zmacs--get-ui-colour :bright-red)
                 :background unspecified
                 :weight     bold
                 :overline   t
                 :extend     t))))
     `(org-verse
       ((,class (:inherit    org-block
                 :foreground ,(zmacs--get-ui-colour :bright-steel)
                 :slant      italic))))
     `(org-quote
       ((,class (:inherit    org-block
                 :foreground ,(zmacs--get-ui-colour :bright-silver)))))
;;;;;;; Levels:
     `(org-level-1
       ((,class (:inherit    fixed-pitch
                 :background unspecified ;;,colour-heading1
                 :foreground ,(zmacs--get-ui-colour :tint-10)
                 :weight     bold
                 :height     1.5
                 :extend     t))))
     `(org-level-2
       ((,class (:inherit    fixed-pitch
                 :background unspecified
                 :foreground ,colour-heading2
                 :weight     bold
                 :height     1.4
                 :extend     t))))
     `(org-level-3
       ((,class (:inherit    fixed-pitch
                 :background unspecified
                 :foreground ,colour-heading3
                 :weight     bold
                 :height     1.3
                 :extend     t))))
     `(org-level-4
       ((,class (:inherit    fixed-pitch
                 :background unspecified
                 :foreground ,colour-heading4
                 :weight     bold
                 :height     1.2
                 :extend     t))))
     `(org-level-5
       ((,class (:inherit    fixed-pitch
                 :background unspecified
                 :foreground ,colour-heading?
                 :weight     bold
                 :height     1.1
                 :extend     t))))
     `(org-level-6
       ((,class (:inherit    fixed-pitch
                 :background unspecified
                 :foreground ,colour-heading?
                 :weight     bold
                 :height     1.1
                 :extend     t))))
     `(org-level-7
       ((,class (:inherit    fixed-pitch
                 :background unspecified
                 :foreground ,colour-heading?
                 :weight     bold
                 :height     1.1
                 :extend     t))))
     `(org-level-8
       ((,class (:inherit    fixed-pitch
                 :background unspecified
                 :foreground ,colour-heading?
                 :weight     bold
                 :height     1.1
                 :extend     t))))
;;;;;;; Todo:
     `(org-todo
       ((,class (:inherit    zmacs-tag-todo
                 :weight     bold))))
     `(org-done
       ((,class (:inherit    zmacs-tag-done
                 :weight     bold))))
;;;;;;; Modern:
     `(org-modern-label
       ((,class (:foreground ,colour-text-light
                 :height     0.8
                 :width      condensed
                 :weight     regular
                 :underline  nil))))
     `(org-modern-tag
       ((,class (:inherit    (secondary-selection org-modern-label)
                 :background nil
                 :foreground ,(zmacs--get-ui-colour :bright-orange)
                 :height     0.8
                 :weight     semi-bold))))
     `(org-modern-progress-complete
       ((,class (:inherit    org-modern-label
                 :foreground ,(zmacs--get-ui-colour :white)
                 :background ,(zmacs--get-primary-colour :green)
                 :weight     bold))))
     `(org-modern-progress-incomplete
       ((,class (:inherit    org-modern-label
                 :foreground ,(zmacs--get-ui-colour :white)
                 :background ,colour-highlight
                 :weight     bold))))
     `(org-modern-date-active
       ((,class (:inherit    org-modern-label
                 :foreground ,(zmacs--get-ui-colour :button-text)
                 :background ,(zmacs--get-ui-colour :button-background)
                 :weight     bold))))
     `(org-modern-date-inactive
       ((,class (:inherit    org-modern-label
                 :foreground ,(zlisp/adjust-brightness-oklab
                               (zmacs--get-ui-colour :button-text)
                               -20)
                 :background ,(zmacs--get-ui-colour :button-background)
                 :weight     semi-bold))))
     `(org-modern-time-active
       ((,class (:inherit            org-modern-label
                 :foreground         ,(zmacs--get-ui-colour :white)
                 :distant-foreground ,(zmacs--get-ui-colour :white)
                 :background         ,(zmacs--get-primary-colour :blue)
                 :weight             bold))))
     `(org-modern-time-inactive
       ((,class (:inherit            org-modern-label
                 :foreground         ,(zmacs--get-ui-colour :white)
                 :distant-foreground ,(zmacs--get-ui-colour :white)
                 :background         ,(zmacs--get-primary-colour :dim-blue)
                 :weight             semi-bold))))
;;;;;;; Agenda:
     `(org-agenda-date
       ((,class (:foreground ,(zmacs--get-ui-colour :tint-8)
                 :weight     bold
                 :overline   t
                 :height     1.2))))
     `(org-agenda-date-today
       ((,class (:foreground ,(zmacs--get-ui-colour :tint-12)
                 :weight     bold
                 :overline   t
                 :height     1.5))))
     `(org-time-grid
       ((,class (:foreground ,(zmacs--get-ui-colour :blue)))))
     `(org-agenda-current-time
       ((,class (:foreground ,(zmacs--get-ui-colour :bright-steel)
                 :weight     bold))))
    `(org-agenda-diary
       ((,class (:foreground ,(zmacs--get-ui-colour :bright-magenta)))))
    `(org-agenda-calendar-event
      ((,class (:foreground ,(zmacs--get-ui-colour :bright-violet)))))
    `(org-agenda-structure
       ((,class (:foreground ,(zmacs--get-ui-colour :tint-6)
                 :weight     bold
                 :height     1.3))))
     `(org-super-agenda-header
       ((,class (:foreground ,(zmacs--get-ui-colour :tint-4)
                 :height    1.1
                 :weight     bold))))
;;;;;;; Pomodoro
     `(org-pomodoro-mode-line
       ((,class (:foreground ,(zmacs--get-ui-colour :bright-orange)))))
     `(org-pomodoro-mode-line-overtime
       ((,class (:foreground ,(zmacs--get-ui-colour :bright-red)
                 :weight     bold))))
     `(org-pomodoro-mode-line-break
       ((,class (:foreground ,(zmacs--get-ui-colour :bright-silver)))))
;;;;;; Outline:
;;;;;;; `outline-mode':
     `(outline-1
       ((,class (:foreground ,colour-comment1
                 :background unspecified
                 :weight     bold
                 :extend     t))))
    `(outline-2
      ((,class (:foreground ,colour-comment2
                :background unspecified
                :weight     bold
                :extend     t))))
    `(outline-3
      ((,class (:foreground ,colour-comment3
                :background unspecified
                :weight     bold
                :extend     t))))
    `(outline-4
      ((,class (:foreground ,colour-comment4
                :background unspecified
                :weight     bold
                :extend     t))))
    `(outline-5
      ((,class (:foreground ,colour-comment5
                :background unspecified
                :weight     bold
                :extend     t))))
    `(outline-6
      ((,class (:foreground ,colour-comment6
                :background unspecified
                :weight     bold
                :extend     t))))
    `(outline-7
      ((,class (:foreground ,colour-comment7
                :background unspecified
                :weight     bold
                :extend     t))))
    `(outline-8
      ((,class (:foreground ,colour-comment1
                :background unspecified
                :weight     bold
                :extend     t))))
;;;;;;; `outline-minor-mode':
     ;; These simply mirror what `outline-mode' has, even though they are
     ;; slightly different names.
     ;;
     ;; Don't try to be clever here, or comments will suddenly look different
     ;; within source code.
     `(outline-minor-0 ((,class (:inherit outline-1))))
     `(outline-minor-1 ((,class (:inherit outline-2))))
     `(outline-minor-2 ((,class (:inherit outline-3))))
     `(outline-minor-3 ((,class (:inherit outline-4))))
     `(outline-minor-4 ((,class (:inherit outline-5))))
     `(outline-minor-5 ((,class (:inherit outline-6))))
     `(outline-minor-6 ((,class (:inherit outline-7))))
     `(outline-minor-7 ((,class (:inherit outline-8))))
;;;;;; Flycheck:
     `(flycheck-warning
       ((,class (:underline (:style wave :color ,colour-crucial-fg)))))
     `(flycheck-error
       ((,class (:underline (:style wave :color ,colour-critical-fg)))))
     `(flycheck-info
       ((,class (:underline (:style wave :color ,colour-focus-fg)))))
     `(flycheck-fringe-warning
       ((,class (:foreground ,colour-crucial-fg))))
     `(flycheck-fringe-error
       ((,class (:foreground ,colour-critical-fg))))
     `(flycheck-fringe-info
       ((,class (:foreground ,colour-focus-fg))))
     `(flycheck-error-list-warning
       ((,class (:foreground ,colour-crucial-fg
                 :bold       t))))
     `(flycheck-error-list-error
       ((,class (:foreground ,colour-critical-fg
                 :bold       t))))
     `(flycheck-error-list-info
       ((,class (:foreground ,colour-focus-fg
                 :bold       t))))
;;;;;; Flyspell:
     `(flyspell-incorrect
       ((,class (:underline (:color ,(zmacs--get-primary-colour :red)
                             :style line)))))
     `(flyspell-duplicate
       ((,class (:underline (:color ,(zmacs--get-primary-colour :orange)
                             :style line)))))
;;;;;; Magit:
     `(magit-header-line
       ((,class (:foreground ,colour-text-light
                 :background ,colour-heading1))))
;;;;;; Dashboard:
     `(dashboard-text-banner
       ((,class (:foreground ,(zmacs--get-ui-colour :bright-blue)
                 :bold       t))))
     `(dashboard-banner-logo-title
       ((,class (:foreground ,(zmacs--get-ui-colour :bright-silver)))))
     `(dashboard-heading
       ((,class (:foreground ,colour-strong))))
     `(dashboard-items-face
       ((,class (:foreground ,(zmacs--get-ui-colour :base)
                 :bold       t))))
     `(dashboard-no-items-face
       ((,class (:foreground ,colour-text-light
                 :bold       t))))
     `(dashboard-footer-face
       ((,class (:foreground ,(zmacs--get-ui-colour :bright-steel)))))
     `(dashboard-init-info-face
       ((,class (:foreground ,colour-faint))))
;;;;;; Calfs:
     `(cfw:face-title
       ((,class (:foreground ,colour-urgent-fg
                 :weight     bold
                 :height     2.0))))
     `(cfw:face-header
       ((,class (:foreground ,(zmacs--get-ui-colour :bright-green)
                 :weight     bold))))
     `(cfw:face-saturday
       ((,class (:foreground ,(zmacs--get-ui-colour :bright-yellow)
                 :background ,(zmacs--get-ui-colour :grey-10)
                 :weight     bold))))
     `(cfw:face-sunday
       ((,class (:foreground ,(zmacs--get-ui-colour :bright-red)
                 :background ,(zmacs--get-ui-colour :grey-10)
                 :weight     bold))))
     `(cfw:face-holiday
       ((,class (:background ,(zmacs--get-ui-colour :grey-10)
                 :foreground ,(zmacs--get-ui-colour :bright-steel)
                 :weight     bold))))
     `(cfw:face-grid
       ((,class (:inherit fill-column-indicator))))
     `(cfw:face-default-content
       ((,class (:foreground ,colour-text-light))))
     `(cfw:face-periods
       ((,class (:foreground ,(zmacs--get-ui-colour :bright-cyan)))))
     `(cfw:face-day-title
       ((,class (:background ,(zmacs--get-ui-colour :grey-10)))))
     `(cfw:face-default-day
       ((,class (:weight  bold
                 :inherit cfw:face-day-title))))
     `(cfw:face-select
       ((,class (:background ,(zmacs--get-ui-colour :tone-6)))))
     `(cfw:face-toolbar
       ((,class (:background ,(zmacs--get-ui-colour :base)
                 :foreground ,colour-heading1))))
     `(cfw:face-toolbar-button-on
       ((,class (:foreground ,colour-text-light
                 :background ,(zmacs--get-ui-colour :base)))))
     `(cfw:face-toolbar-button-off
       ((,class (:foreground ,colour-background
                 :background ,(zmacs--get-ui-colour :base)))))
     `(cfw:face-today
       ((,class (:background ,(zmacs--get-ui-colour :tone-12)
                 :weight     bold))))
     `(cfw:face-today-title
       ((,class (:background ,(zmacs--get-ui-colour :tint-0)
                 :foreground ,(zmacs--get-ui-colour :tint-12)
                 :weight     bold))))
     `(cfw:face-disable
       ((,class (:foreground ,(zmacs--get-ui-colour :grey-0)
                 :inherit    cfw:face-day-title))))
     `(cfw:face-annotation
       ((,class (:foreground ,(zmacs--get-ui-colour :bright-violet)
                 :background ,(zmacs--get-ui-colour :grey-10)
                 :inherit    cfw:face-day-title))))
;;;;;; Slime:
     `(slime-inspector-action-face
       ((,class (:foreground ,(zmacs--get-ui-colour :bright-blue)
                 :background unspecified))))
     `(slime-inspector-topline-face
       ((,class (:foreground ,colour-background
                 :background unspecified
                 :weight     bold))))
     `(slime-inspector-value-face
       ((,class (:foreground ,(zmacs--get-ui-colour :bright-orange)
                 :background unspecified))))
     `(slime-inspector-label-face
       ((,class (:foreground ,(zmacs--get-ui-colour :bright-magenta)
                 :background unspecified))))
     `(slime-inspector-type-face
       ((,class (:foreground ,(zmacs--get-ui-colour :bright-violet)
                 :background unspecified))))
     `(slime-apropos-label
       ((,class (:foreground ,(zmacs--get-ui-colour :bright-blue)))))
     `(slime-apropos-symbol
       ((,class (:foreground ,(zmacs--get-ui-colour :bright-orange)))))
     `(slime-repl-prompt-face
       ((,class (:inherit zmacs-repl-prompt-face))))
     `(slime-repl-input-face
       ((,class (:inherit zmacs-repl-input-face))))
     `(slime-repl-output-face
       ((,class (:inherit zmacs-repl-output-face))))
     `(slime-repl-inputed-output-face
       ((,class (:inherit zmacs-repl-output-face))))
     `(slime-repl-output-mouseover-face
       ((,class (:inherit zmacs-repl-inline-ui-face))))
;;;;;; Stripe Buffer:
     `(stripe-highlight
       ((,class (:background ,(zmacs--get-ui-colour :tone-12)))))
     `(stripe-hl-line
       ((,class (:import highlight))))
;;;;;; ZMACS Org Styles:
;;;;;;; Present:
     `(zmacs-org-style-present-default
       ((,class (:inherit default
                 :height  1.3))))
     `(zmacs-org-style-present-fixed-pitch
       ((,class (:inherit fixed-pitch
                 :height  1.3))))
     `(zmacs-org-style-present-header-line
       ((,class (:inherit fixed-pitch
                 :height 2.0))))
     `(zmacs-org-style-present-document-title
       ((,class (:inherit    fixed-pitch
                 :background unspecified
                 :foreground ,(zmacs--get-ui-colour :bright-orange)
                 :weight     bold
                 :height     3.0
                 :underline  t
                 :extend     t))))
     `(zmacs-org-style-present-document-info
       ((,class (:inherit    fixed-pitch
                 :background unspecified
                 :foreground ,(zmacs--get-ui-colour :bright-silver)
                 :weight     bold
                 :height     1.5))))
     `(zmacs-org-style-present-level-1
       ((,class (:inherit    fixed-pitch
                 :background unspecified
                 :foreground ,colour-heading1
                 :weight     bold
                 :height     2.0
                 :underline  t
                 :overline   nil
                 :extend     t))))
     `(zmacs-org-style-present-level
       ((,class (:inherit    fixed-pitch
                 :background unspecified
                 :foreground ,colour-heading1
                 :weight     bold
                 :height     1.3
                 :overline   nil
                 :underline  nil
                 :extend     nil))))
     `(zmacs-org-style-present-code
       ((,class (:inherit org-code
                 :height  1.3))))
     `(zmacs-org-style-present-verbatim
       ((,class (:inherit org-verbatim
                 :height  1.3))))
     `(zmacs-org-style-present-block
       ((,class (:inherit org-block
                 :height 1.3))))
     `(zmacs-org-style-present-block-begin-line
       ((,class (:inherit org-block-begin-line
                 :height 1.0))))
     `(zmacs-org-style-present-block-end-line
       ((,class (:inherit org-block-end-line
                 :height 1.0))))
;;;;;;; Agenda:
     `(zmacs-org-style-agenda-level-1
       ((,class (:inherit    fixed-pitch
                 :background unspecified
                 :foreground ,colour-heading1
                 :weight     bold
                 :height     1.0))))
     `(zmacs-org-style-agenda-level-2
       ((,class (:inherit    fixed-pitch
                 :background unspecified
                 :foreground ,colour-heading2
                 :weight     bold
                 :height     1.0))))
     `(zmacs-org-style-agenda-level-3
       ((,class (:inherit    fixed-pitch
                 :background unspecified
                 :foreground ,colour-heading3
                 :weight     bold
                 :height     1.0))))
     `(zmacs-org-style-agenda-level
       ((,class (:inherit    fixed-pitch
                 :background unspecified
                 :foreground ,colour-heading4
                 :weight     bold
                 :height     1.0))))
;;;;;;; Denote:
     `(zmacs-org-style-denote-level-1
       ((,class (:inherit    fixed-pitch
                 :background unspecified
                 :foreground ,colour-heading1
                 :underline  t
                 :extend     t
                 :weight     bold
                 :height     1.4))))
     `(zmacs-org-style-denote-level-2
       ((,class (:inherit    fixed-pitch
                 :background unspecified
                 :foreground ,colour-heading2
                 :weight     bold
                 :height     1.3))))
     `(zmacs-org-style-denote-level-3
       ((,class (:inherit    fixed-pitch
                 :background unspecified
                 :foreground ,colour-heading2
                 :weight     bold
                 :height     1.2))))
     `(zmacs-org-style-denote-level
       ((,class (:inherit    fixed-pitch
                 :background unspecified
                 :foreground ,colour-heading2
                 :weight     bold
                 :height     1.1))))
;;;;; End of faces.
     )                                  ; `custom-set-faces'
(custom-theme-set-variables
 theme-name
 `(ansi-colour-names-vector
   [,(zmacs--get-ui-colour :black)
    ,(zmacs--get-ui-colour :bright-red)
    ,(zmacs--get-ui-colour :bright-green)
    ,(zmacs--get-ui-colour :bright-yellow)
    ,(zmacs--get-ui-colour :bright-blue)
    ,(zmacs--get-ui-colour :bright-magenta)
    ,(zmacs--get-ui-colour :bright-cyan)
    ,(zmacs--get-ui-colour :bright-white)])))
;;;;; END.
  )

;;;; Highlight parens:

(setq-default highlight-parentheses-colors
              (list (zlisp/adjust-brightness-oklab
                     (zmacs--get-primary-colour :bright-green)  25)
                    (zlisp/adjust-brightness-oklab
                     (zmacs--get-primary-colour :bright-yellow) 25)
                    (zlisp/adjust-brightness-oklab
                     (zmacs--get-primary-colour :bright-orange) 25)
                    (zlisp/adjust-brightness-oklab
                     (zmacs--get-primary-colour :bright-red) 25)))

;;;; Highlight Todo:

(setq-default hl-todo-keyword-faces
              '(("HOLD"       . zmacs-tag-hold)
                ("TODO"       . zmacs-tag-todo)
                ("NEXT"       . zmacs-tag-doing)
                ("OKAY"       . zmacs-tag-done)
                ("DONT"       . zmacs-tag-cancelled)
                ("FAIL"       . zmacs-tag-cancelled)
                ("DONE"       . zmacs-tag-done)
                ("NOTE"       . warning)
                ("KLUDGE"     . warning)
                ("HACK"       . warning)
                ("TEMP"       . warning)
                ("FIXME"      . error)
                ("XXX+"       . error)
                ("BUG"        . error)
                ("REVIEW"     . warning)
                ("DEPRECATED" . warning)))

;;;; Org Todo:

(setq-default org-todo-keyword-faces
              '(("TODO"      . zmacs-tag-todo)
                ("DOING"     . zmacs-tag-doing)
                ("DONE"      . zmacs-tag-done)
                ("WAIT"      . zmacs-tag-wait)
                ("HOLD"      . zmacs-tag-hold)
                ("CANCELLED" . zmacs-tag-cancelled)))

;;;; Autoload:

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

;;;; Provide theme:

(provide 'zmacs-themes)

;;; zmacs-themes.el ends here.
