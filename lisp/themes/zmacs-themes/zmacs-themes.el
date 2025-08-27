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
;;
;;

;;; Code:

(require 'cl-lib)

;;;; Hooks:

(defvar zmacs-themes-after-load-theme-hook nil
  "Hook that ise run after zmacs-theme is loaded using `load-theme`.")

;;;; Colour tables:

(defvar *zmacs--primary-color-table*
  ;; Name        Normal    Bright    Faint
  '((:yellow  . ("#6b5300" "#b2911f" "#fed53f"))
    (:orange  . ("#6b3000" "#b25c1f" "#fe8b3f"))
    (:red     . ("#6b1200" "#b2381f" "#fe5f3f"))
    (:magenta . ("#62006b" "#a520b2" "#ee3ffe"))
    (:violet  . ("#37006b" "#6a20b2" "#a23ffe"))
    (:blue    . ("#00356b" "#1e67b2" "#3f9efe"))
    (:cyan    . ("#005d6b" "#209eb2" "#3fe4fe"))
    (:green   . ("#006b12" "#20b237" "#3ffe5e"))
    (:white   . ("#403f40" "#eae1fe" "#f8f5ff"))
    (:black   . ("#131217" "#131217" "#131217")))
  "Colours used for primary colours within the theme.")

(defvar *zmacs--ascii-color-table*
  ;; Name        Normal    Bright    Faint
  '((:black  . ("#131217" "#2c2b30" "#131217"))
    (:red    . ("#d40131" "#fe4453" "#40000f"))
    (:green  . ("#819501" "#e4fc84" "#384001"))
    (:yellow . ("#ac8300" "#ffd36e" "#403000"))
    (:blue   . ("#2b90d8" "#54b4ff" "#134160"))
    (:purple . ("#8c5bd4" "#b181fe" "#2a1b40"))
    (:cyan   . ("#259d94" "#94fff4" "#0f403c"))
    (:white  . ("#a5a3a9" "#f8f5ff" "#3e3e40")))
  "Colours used for ASCII and ANSI colours.")

(defvar *zmacs--color-table*
  ;;  White    Purp/Wht  Purple    Grey
  '(("#efebf5" "#eae1fe" "#8253ca" "#727272")
    ("#e0dde6" "#d2c9e5" "#744cb3" "#666666")
    ("#d2cfd6" "#bab1cd" "#67459d" "#5b5b5b")
    ("#c4c1c7" "#a29ab5" "#593d88" "#504f50")
    ("#b5b3b9" "#8b839d" "#4d3672" "#454445")
    ("#a7a6aa" "#756d87" "#402f5e" "#3a3a3b")
    ("#9a999c" "#605871" "#34284a" "#302f31")
    ("#8c8b8d" "#4b445b" "#282137" "#262527")
    ("#7f7f80" "#383047" "#1d1925" "#1c1b1d")
    ("#727272" "#251e33" "#131214" "#131214"))
  "Colours that we consider Zmacs signature colours.")

(defvar *zmacs--ui-color-table*
  '((:view-background      . "#1c1b22")
    (:view-text            . "#d4d2cf")
    (:window-background    . "#2b2a33")
    (:window-text          . "#e0dedb")
    (:button-background    . "#3a3946")
    (:button-text          . "#e8e6e3")
    (:selection-background . "#422e5e")
    (:selection-text       . "#ffffff")
    (:cursor               . "#a23ffe"))
  "Colours that are used in my KDE theme.")

(defvar *zmacs-comment-levels-color-table*
  '("#eae1fe" ; 1
    "#d6c7f5" ; 2
    "#c2adec" ; 3
    "#af93e2" ; 4
    "#9c79d8" ; 5
    "#caf1f7" ; 6
    "#a6e8f2" ; 7
    "#7ddeed" ; 8
    )
  "Colours that are used for various comment nesting levels.")

(defsubst zmacs--color-table-list-value (table offset index)
  "Return the colour at index OFFSET in the list at index INDEX in TABLE."
  (nth offset (nth index table)))

(defsubst zmacs--theme-color (type offset)
  "Return the associated RGB value for the OFFSET of sub-colour TYPE.

TYPE is the offset within `*zmacs--color-table*'.

Ex:
     (ZMACS--THEME-COLOR ZMACS--COLOR-PURPLE 0)
  => \"#8253ca\" ;; the third colour in the zeroeth list."
  (zmacs--color-table-list-value *zmacs--color-table* type offset))

(defsubst zmacs--color-alist-value (alist name offset)
  "Return the RGB value for the colour at OFFSET for the sublist NAME in the
associated list ALIST."
  (nth offset (assoc name alist)))

(defvar *zmacs--color-type-normal* 1)
(defvar *zmacs--color-type-bright* 2)
(defvar *zmacs--color-type-faint*  3)

(defvar *zmacs--color-white*  0)
(defvar *zmacs--color-pwhite* 1)
(defvar *zmacs--color-purple* 2)
(defvar *zmacs--color-grey*   3)

(defsubst zmacs--primary-color/normal (name)
  "Return the RGB value for the primary color given in NAME."
  (zmacs--color-alist-value *zmacs--primary-color-table*
                            name
                            *zmacs--color-type-normal*))

(defsubst zmacs--primary-color/bright (name)
  "Return the RGB value for the primary color given in NAME."
  (zmacs--color-alist-value *zmacs--primary-color-table*
                            name
                            *zmacs--color-type-bright*))

(defsubst zmacs--primary-color/faint (name)
  "Return the RGB value for the primary color given in NAME."
  (zmacs--color-alist-value *zmacs--primary-color-table*
                            name
                            *zmacs--color-type-faint*))

(defsubst zmacs--ascii-color/normal (name)
  "Return the RGB value for the ASCII color given in NAME."
  (zmacs--color-alist-value *zmacs--ascii-color-table*
                            name
                            *zmacs--color-type-normal*))

(defsubst zmacs--ascii-color/bright (name)
  "Return the RGB value for the ASCII color given in NAME."
  (zmacs--color-alist-value *zmacs--ascii-color-table*
                            name
                            *zmacs--color-type-bright*))

(defsubst zmacs--ascii-color/faint (name)
  "Return the RGB value for the ASCII color given in NAME."
  (zmacs--color-alist-value *zmacs--ascii-color-table*
                            name
                            *zmacs--color-type-faint*))

(defsubst zmacs--ui-color (name)
  "Return the RGB value for the UI theme colour give in NAME."
  (cdr (assoc name *zmacs--ui-color-table*)))

(defsubst zmacs--comment-color (idx)
  (nth idx *zmacs-comment-levels-color-table*))

;;;; Fonts:

(defvar *zmacs-fixed-font-size*
  (if (eq window-system 'ns)
      130
    100)
  "Fixed font size.")

(defvar *zmacs-variable-font-size*
  (if (eq window-system 'ns)
      130
    100)
  "Variable font size.")

(cl-defun zmacs--x-list-fonts (x)
  "Wrapper around `x-list-fonts'."
  (when (or (not (display-graphic-p))
            (not (fboundp 'x-list-fonts)))
    (cl-return-from zmacs--x-list-fonts nil))
  (x-list-fonts x))

(cl-defun zmacs--x-family-fonts (x)
  "Wrapper around `x-family-fonts'."
  (when (or (not (display-graphic-p))
            (not (fboundp 'x-family-fonts)))
    (cl-return-from zmacs--x-family-fonts nil))
  (x-family-fonts x))

(defvar *zmacs-variable-face*
  (cond ((zmacs--x-list-fonts "ETBembo")
         '(:fond "ETBembo"))
        ((zmacs--x-list-fonts "Source Sans Pro")
         '(:font "Source Sans Pro"))
        ((zmacs--x-list-fonts "Lucida Grande")
         '(:font "Lucida Grande"))
        ((zmacs--x-list-fonts "Verdana")
         '(:font "Verdana"))
        ((zmacs--x-list-fonts "DejaVu Sans")
         '(:font "DejaVu Sans"))
        ((zmacs--x-family-fonts "Sans Serif")
         '(:family "Sans Serif"))
        (nil
         (warn "Could not find a suitable sans-serif font.")
         '(:font "Helvetica")))
  "Font to use for variable faces.  The font is determined by checking the
availability of various fonts on your system.  The first one that is available
shall be used.")

(defvar *zmacs-fixed-face*
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
  "Font to use for fixed faces.  The font is determined by checking the
availability of various fonts on your system.  The first one that is available
shall be used.")

;;;; Default, variable pitch, and fixed pitch faces:

(custom-set-faces
 `(default
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
       :foreground     ,(zmacs--theme-color *zmacs--color-pwhite* 0)
       :background     unspecified
       :stipple        nil
       :inherit        nil))
    (t
     (,@*zmacs-fixed-face*
      :width          normal
      :height         ,*zmacs-fixed-font-size*
      :weight         normal
      :slant          normal
      :underline      nil
      :overline       nil
      :extend         nil
      :strike-through nil
      :box            nil
      :inverse-video  nil
      :foreground     ,(zmacs--theme-color *zmacs--color-pwhite* 0)
      :background     ,(zmacs--ui-color :view-background)
      :stipple        nil
      :inherit        nil))))
 `(variable-pitch
   ((t (,@*zmacs-variable-face*
        :width   normal
        :height  ,*zmacs-variable-font-size*
        :weight  normal
        :slant   normal
        :inherit nil))))
 `(fixed-pitch
   ((t (,@*zmacs-fixed-face*
        :width   normal
        :height  ,*zmacs-fixed-font-size*
        :weight  normal
        :slant   normal
        :inherit nil)))))

;;;; Theme faces:

(defgroup zmacs-themes nil
  "Faces and colors for lambda-themes."
  :group 'faces)

(defface zmacs-bg nil
  "Background face for zmacs-themes."
  :group 'faces)

(defface zmacs-fg nil
  "Foreground face for zmacs-themes."
  :group 'faces)

(defface zmacs-ultralight nil
  "Bright highlight face."
  :group 'faces)

(defface zmacs-highlight nil
  "Highlight face."
  :group 'faces)

(defface zmacs-lowlight nil
  "Dim highlight face."
  :group 'faces)

(defface zmacs-urgent nil
  "Urgent face requires your attention.
It should stick out from any other faces currently displayed."
  :group 'faces)

(defface zmacs-crucial nil
  "Crucial face displays important information."
  :group 'faces)

(defface zmacs-focus nil
  "Focus face display information that is useful or pertinent."
  :group 'faces)

(defface zmacs-strong nil
  "Strong face is for a structural accent in contrast with the normal
foreground face."
  :group 'faces)

(defface zmacs-meek nil
  "Meek face is for information that is useful but less important."
  :group 'faces)

(defface zmacs-mild nil
  "Mild face is for shading that is differentiable from the background but
doesn't stand out."
  :group 'faces)

(defface zmacs-faint nil
  "Faint face is for very slightly accenting or shading information."
  :group 'faces)

(defface zmacs-blue nil
  "A blue accent face."
  :group 'faces)

(defface zmacs-green nil
  "A green accent face."
  :group 'faces)

(defface zmacs-red nil
  "A red accent face."
  :group 'faces)

(defface zmacs-yellow nil
  "A yellow accent face."
  :group 'faces)

(defface zmacs-orange nil
  "An orange accent face."
  :group 'faces)

(defface zmacs-magenta nil
  "A purple accent face."
  :group 'faces)

(defface zmacs-cyan nil
  "An aqua accent face."
  :group 'faces)

(defface zmacs-todo-todo nil
  "A `todo' face for todo lists."
  :group 'faces)

(defface zmacs-todo-doing nil
  "A `doing' face for todo lists."
  :group 'faces)

(defface zmacs-todo-done nil
  "A `done' face for todo lists."
  :group 'faces)

(defface zmacs-todo-wait nil
  "A `wait' face for todo lists."
  :group 'faces)

(defface zmacs-todo-hold nil
  "A `hold' face for todo lists."
  :group 'faces)

(defface zmacs-todo-cancelled nil
  "A `cancelled' face for todo lists."
  :group 'faces)

(defface zmacs-org-list nil
  "Face used for org-mode lists."
  :group 'faces)

;;;; Meta faces:
;; These are not really used, they're just here to define a set of faces that
;; can be used by things like REPLs et al.


(defface meta-repl-prompt-face
  `((t (:inherit    fixed-pitch
        :foreground ,(zmacs--primary-color/faint :blue)
        :background unspecified)))
  "Meta face for REPL prompts.")

(defface meta-repl-input-face
  `((t (:inherit    fixed-pitch
        :foreground ,(zmacs--theme-color *zmacs--color-pwhite* 0)
        :background unspecified)))
  "Meta face for REPL input text.")

(defface meta-repl-output-face
  `((t (:inherit    fixed-pitch
        :foreground ,(zmacs--primary-color/faint :orange)
        :background unspecified)))
  "Meta face for REPL output text.")

(defface meta-repl-inline-ui-face
  `((t (:inherit    meta-repl-input-face
        :foreground ,(zmacs--primary-color/faint :orange)
        :box (:line-width  (1 . 1)
              :color ,(zmacs--theme-color *zmacs--color-purple* 0)
              :style flat-button))))
  "Meta face for inline UI elements that might be used by a REPL.")

;;;; Theme definition:

(defgroup zmacs-themes nil
  "Faces and colours for zmacs-themes."
  :group 'faces)

(defun zmacs-themes-create (_ theme-name)
  (let ((class '((class color) (min-colors 89)))
;;;;; Colour names:
        ;;
        ;; Basic.
        (zmacs-background (zmacs--ui-color :view-background))
        (zmacs-foreground (zmacs--theme-color *zmacs--color-pwhite* 0))
        (zmacs-window     (zmacs--ui-color :window-background))
        ;;
        ;; Highlighting.
        (zmacs-ultralight (zmacs--theme-color *zmacs--color-grey* 2))
        (zmacs-highlight  (zmacs--theme-color *zmacs--color-grey* 5))
        (zmacs-lowlight   (zmacs--theme-color *zmacs--color-grey* 7))
        ;;
        ;; Heading levels.
        (zmacs-l1 (zmacs--theme-color *zmacs--color-purple* 0))
        (zmacs-l2 (zmacs--theme-color *zmacs--color-purple* 2))
        (zmacs-l3 (zmacs--theme-color *zmacs--color-purple* 5))
        (zmacs-l4 (zmacs--theme-color *zmacs--color-purple* 7))
        ;;
        ;; Comment levels.
        (zmacs-comment-1 (zmacs--comment-color 0))
        (zmacs-comment-2 (zmacs--comment-color 1))
        (zmacs-comment-3 (zmacs--comment-color 2))
        (zmacs-comment-4 (zmacs--comment-color 3))
        (zmacs-comment-5 (zmacs--comment-color 4))
        (zmacs-comment-6 (zmacs--comment-color 5))
        (zmacs-comment-7 (zmacs--comment-color 6))
        (zmacs-comment-8 (zmacs--comment-color 7))
        ;;
        ;; Attention.
        (zmacs-critical-bg (zmacs--primary-color/normal :red))
        (zmacs-urgent-bg   (zmacs--primary-color/normal :orange))
        (zmacs-crucial-bg  (zmacs--primary-color/normal :yellow))
        (zmacs-focus-bg    (zmacs--primary-color/normal :blue))
        (zmacs-ok-bg       (zmacs--primary-color/normal :green))
        (zmacs-critical    (zmacs--primary-color/faint  :red))
        (zmacs-urgent      (zmacs--primary-color/faint  :orange))
        (zmacs-crucial     (zmacs--primary-color/faint  :yellow))
        (zmacs-focus       (zmacs--primary-color/faint  :blue))
        (zmacs-ok          (zmacs--primary-color/faint  :green))
        (zmacs-strong      (zmacs--primary-color/faint  :white))
        (zmacs-meek        (zmacs--primary-color/bright :white))
        (zmacs-mild        (zmacs--ascii-color/normal   :white))
        (zmacs-faint       (zmacs--primary-color/normal :white))
        ;;
        ;; Accents.
        (zmacs-yellow  (zmacs--primary-color/faint :yellow))
        (zmacs-orange  (zmacs--primary-color/faint :orange))
        (zmacs-red     (zmacs--primary-color/faint :red))
        (zmacs-magenta (zmacs--primary-color/faint :magenta))
        (zmacs-violet  (zmacs--primary-color/faint :violet))
        (zmacs-blue    (zmacs--primary-color/faint :blue))
        (zmacs-cyan    (zmacs--primary-color/faint :cyan))
        (zmacs-green   (zmacs--primary-color/faint :green))
        (zmacs-white   (zmacs--primary-color/faint :white))
        (zmacs-black   (zmacs--primary-color/faint :black)))
;;;;; Faces:
    (custom-set-faces
;;;;;; Defaults:
     `(cursor
       ((,class (:background ,(zmacs--ui-color :cursor)))))
     `(fringe
       ((,class (:background ,(zmacs--theme-color *zmacs--color-purple* 8)
                 :foreground ,zmacs-highlight))))
     `(highlight
       ((,class (:background ,(zmacs--theme-color *zmacs--color-grey* 7)))))
     `(hl-line
       ((,class (:inherit highlight))))
     `(region
       ((,class (:background ,(zmacs--ui-color :selection-background)))))
     `(secondary-selection
       ((,class (:background ,zmacs-highlight))))
     `(buffer-menu-buffer
       ((,class (:inherit fixed-pitch
                 :foreground ,zmacs-blue
                 :background unspecified))))
     `(minibuffer-prompt
       ((,class (:inherit meta-repl-prompt-face))))
     `(link
       ((,class (:inherit    fixed-pitch
                 :foreground ,zmacs-blue
                 :background unspecified
                 :underline  t))))
     `(shadow
       ((,class (:inherit    fixed-pitch
                 :foreground ,zmacs-meek
                 :background unspecified
                 :weight     light))))
;;;;;; Zmacs faces:
     `(zmacs-fg
       ((,class (:foreground ,zmacs-foreground))))
     `(zmacs-bg
       ((,class (:background ,zmacs-background))))
     `(zmacs-ultralight
       ((,class (:background ,zmacs-ultralight))))
     `(zmacs-highlight
       ((,class (:foreground ,zmacs-highlight))))
     `(zmacs-lowlight
       ((,class (:foreground ,zmacs-lowlight))))
     `(zmacs-urgent
       ((,class (:foreground ,zmacs-urgent))))
     `(zmacs-focus
       ((,class (:foreground ,zmacs-focus))))
     `(zmacs-strong
       ((,class (:foreground ,zmacs-strong
                 :weight semi-bold))))
     `(zmacs-crucial
       ((,class (:foreground ,zmacs-crucial))))
     `(zmacs-mild
       ((,class (:foreground ,zmacs-mild))))
     `(zmacs-faint
       ((,class (:foreground ,zmacs-faint))))
     `(zmacs-blue
       ((,class (:foreground ,zmacs-blue))))
     `(zmacs-green
       ((,class (:foreground ,zmacs-green))))
     `(zmacs-red
       ((,class (:foreground ,zmacs-red))))
     `(zmacs-orange
       ((,class (:foreground ,zmacs-orange))))
     `(zmacs-yellow
       ((,class (:foreground ,zmacs-yellow))))
     `(zmacs-cyan
       ((,class (:foreground ,zmacs-cyan))))
     `(zmacs-magenta
       ((,class (:foreground ,zmacs-magenta))))
     `(zmacs-meek
       ((,class (:foreground ,zmacs-meek))))
;;;;;; Basic faces:
     `(error
       ((,class (:inherit    fixed-pitch
                 :foreground ,zmacs-critical
                 :background unspecified
                 :bold       t))))
     `(success
       ((,class (:inherit    fixed-pitch
                 :foreground ,zmacs-ok
                 :background unspecified
                 :bold       t))))
     `(warning
       ((,class (:inherit    fixed-pitch
                 :foreground ,zmacs-crucial
                 :background unspecified
                 :bold       t))))
     `(alert-low-face
       ((,class (:inherit    fixed-pitch
                 :foreground ,zmacs-urgent
                 :background unspecified))))
     `(escape-glyph
       ((,class (:inherit    fixed-pitch
                 :foreground ,zmacs-red
                 :background unspecified))))
     `(homoglyph
       ((,class (:inherit    fixed-pitch
                 :foreground ,zmacs-focus
                 :background unspecified))))
     `(match
       ((,class (:inherit    fixed-pitch
                 :foreground ,zmacs-lowlight
                 :background ,zmacs-focus))))
;;;;;; Font Lock faces:
     `(font-lock-builtin-face
       ((,class (:inherit    fixed-pitch
                 :foreground ,zmacs-yellow
                 :background unspecified))))
     `(font-lock-comment-delimiter-face
       ((,class (:inherit    fixed-pitch
                 :foreground ,(zmacs--primary-color/bright :cyan)
                 :background unspecified))))
     `(font-lock-comment-face
       ((,class (:inherit    fixed-pitch
                 :foreground ,zmacs-cyan
                 :background unspecified))))
     `(font-lock-constant-face
       ((,class (:inherit    fixed-pitch
                 :foreground ,zmacs-magenta
                 :background unspecified))))
     `(font-lock-string-face
       ((,class (:inherit    fixed-pitch
                 :foreground ,zmacs-green
                 :background unspecified))))
     `(font-lock-number-face
       ((,class (:inherit    fixed-pitch
                 :foreground ,zmacs-violet
                 :background unspecified))))
     `(font-lock-function-name-face
       ((,class (:inherit    fixed-pitch
                 :foreground ,zmacs-yellow
                 :background unspecified))))
     `(font-lock-function-call-face
       ((,class (:inherit font-lock-function-name-face))))
     `(font-lock-variable-name-face
       ((,class (:inherit    fixed-pitch
                 :foreground ,zmacs-orange
                 :background unspecified))))
     `(font-lock-variable-use-face
       ((,class (:inherit font-lock-variable-name-face))))
     `(font-lock-property-name-face
       ((,class (:inherit font-lock-variable-name-face))))
     `(font-lock-property-use-face
       ((,class (:inherit font-lock-property-name-face))))
     `(font-lock-keyword-face
       ((,class (:inherit    fixed-pitch
                 :foreground ,zmacs-blue
                 :background unspecified))))
     `(font-lock-builtin-face
       ((,class (:inherit    fixed-pitch
                 :foreground ,zmacs-yellow
                 :background unspecified))))
     `(font-lock-operator-face
       ((,class (:inherit font-lock-builtin-face))))
     `(font-lock-type-face
       ((,class (:inherit    fixed-pitch
                 :foreground ,zmacs-violet
                 :background unspecified))))
     `(font-lock-preprocessor-face
       ((,class (:inherit    fixed-pitch
                 :foreground ,(zmacs--ascii-color/bright :green)
                 :background unspecified))))
     `(font-lock-punctuation-face
       ((,class (:inherit    fixed-pitch
                 :foreground ,zmacs-lowlight
                 :background unspecified))))
     `(font-lock-bracket-face
       ((,class (:inherit font-lock-punctuation-face))))
     `(font-lock-delimiter-face
       ((,class (:inherit font-lock-punctuation-face))))
     `(font-lock-misc-punctuation-face
       ((,class (:inherit font-lock-punctuation-face))))
     `(font-lock-doc-face
       ((,class (:inherit    fixed-pitch
                 :foreground ,(zmacs--ascii-color/bright :cyan)
                 :background unspecified))))
     `(font-lock-doc-markup-face
       ((,class (:inherit font-lock-constant-face))))
     `(font-lock-warning-face
       ((,class (:inherit    fixed-pitch
                 :foreground ,zmacs-critical
                 :background unspecified))))
     `(font-lock-negation-char-face
       ((,class (:inherit    fixed-pitch
                 :foreground ,zmacs-urgent
                 :background unspecified))))
     `(font-lock-regexp-grouping-construct
       ((,class (:inherit    fixed-pitch
                 :foreground ,zmacs-crucial
                 :background unspecified
                 :weight     bold))))
     `(font-lock-regexp-grouping-backslash
       ((,class (:inherit    fixed-pitch
                 :foreground ,zmacs-urgent
                 :background unspecified
                 :weight     bold))))
     `(font-lock-escape-face
       ((,class (:inherit font-lock-regexp-grouping-backslash))))
;;;;;; Line numbering:
     `(line-number
       ((,class (:inherit    fixed-pitch
                 :foreground ,zmacs-ultralight
                 :background unspecified))))
     `(line-number-current-line
       ((,class (:inherit fixed-pitch
                 :foreground ,(zmacs--theme-color *zmacs--color-white* 3)
                 :background ,(zmacs--theme-color *zmacs--color-grey* 6)))))
;;;;;; Parens:
     `(show-paren-match
       ((,class (:inherit    fixed-pitch
                 :foreground ,zmacs-ok
                 :background ,zmacs-ok-bg
                 :bold       nil))))
     `(show-paren-mismatch
       ((,class (:inherit    fixed-pitch
                 :foreground ,zmacs-critical
                 :background ,zmacs-critical-bg
                 :bold       nil))))
     `(sp-pair-overlay-face
       ((,class (:inherit fixed-pitch
                 :bold    nil))))
     `(sp-show-pair-match-face
       ((,class (:inherit fixed-pitch
                 :bold    nil))))
     `(sp-show-pair-match-face
       ((,class (:inherit fixed-pitch
                 :bold    nil))))
     `(highlight-parentheses-highlight
       ((,class (:inherit fixed-pitch
                 :bold nil))))
;;;;;; Indicators:
     `(fill-column-indicator
       ((,class (:inherit    fixed-pitch
                 :foreground ,zmacs-ultralight
                 :background unspecified))))
     `(indent-guide-face
       ((,class (:inherit    fixed-pitch
                 :foreground ,zmacs-highlight
                 :background unspecified))))
     `(whitespace-tab
       ((,class (:foreground ,zmacs-highlight
                 :background unspecified))))
     `(whitespace-tabmark
       ((,class (:foreground ,zmacs-highlight
                 :background unspecified))))
     `(whitespace-line
       ((,class (:foreground ,zmacs-highlight
                 :background unspecified))))
     `(whitespace-space
       ((,class (:foreground ,zmacs-highlight
                 :background unspecified))))
     `(whitespace-space-after-tab
       ((,class (:foreground ,zmacs-highlight
                 :background unspecified))))
     `(whitespace-hspace
       ((,class (:foreground ,zmacs-highlight
                 :background unspecified))))
     `(whitespace-newline
       ((,class (:foreground ,zmacs-highlight
                 :background unspecified))))
     `(whitespace-indentation
       ((,class (:foreground ,zmacs-highlight
                 :background unspecified))))
     `(whitespace-trailing
       ((,class (:background ,zmacs-red))))
;;;;;; Modeline:
     `(mode-line
       ((,class (:foreground ,(zmacs--theme-color *zmacs--color-pwhite* 2)
                 :background ,(zmacs--theme-color *zmacs--color-purple* 6)
                 :box (:color ,(zmacs--theme-color *zmacs--color-purple* 3)
                       :line-width 1)))))
     `(mode-line-inactive
       ((,class (:foreground ,(zmacs--theme-color *zmacs--color-pwhite* 2)
                 :background ,(zmacs--theme-color *zmacs--color-grey* 6)
                 :box (:color ,(zmacs--theme-color *zmacs--color-grey* 3)
                       :line-width 1)))))
     `(mode-line-buffer-id
       ((,class (:inherit bold
                 :foreground ,zmacs-yellow))))
     `(mode-line-highlight
       ((,class (:inherit bold
                 :foreground ,zmacs-orange))))
;;;;;; Child frames:
     `(mini-popup-background
       ((,class (:background ,zmacs-faint))))
     `(mini-popup-border
       ((,class (:background ,zmacs-faint))))
;;;;;; Posframe
     `(which-key-posframe
       ((,class (:background ,zmacs-faint))))
     `(which-key-posframe-border
       ((,class (:background ,zmacs-faint))))
     `(transient-posframe
       ((,class (:foreground ,zmacs-strong
                 :background ,zmacs-faint))))
     `(transient-posframe-border
       ((,class (:background ,zmacs-faint))))
;;;;;; General completion:
     `(completions-annotations
       ((,class (:foreground ,zmacs-meek))))
;;;;;; Company:
     `(company-scrollbar-bg
       ((,class (:inherit fringe))))
     `(company-scrollbar-fg
       ((,class (:background ,zmacs-mild))))
     `(company-tooltip
       ((,class (:background ,zmacs-lowlight))))
     `(company-tooltip-annotation
       ((,class (:foreground ,zmacs-green))))
     `(company-tooltip-annotation-selection
       ((,class (:inherit company-tooltip-annotation))))
     `(company-tooltip-selection
       ((,class (:foreground ,zmacs-foreground
                 :background ,(zmacs--theme-color *zmacs--color-purple* 5)))))
     `(company-tooltip-common
       ((,class (:foreground ,zmacs-blue
                 :underline t))))
     `(company-tooltip-common-selection
       ((,class (:foreground ,zmacs-blue
                 :underline t))))
     `(company-preview-common
       ((,class (:foreground ,zmacs-highlight))))
     `(company-preview
       ((,class (:background unspecified))))
     `(company-preview-search
       ((,class (:background ,zmacs-cyan))))
     `(company-template-field
       ((,class (:foreground ,zmacs-black
                 :background ,zmacs-yellow))))
     `(company-echo-common
       ((,class (:foreground ,zmacs-red))))
;;;;;; Org:
     `(org-code
       ((,class (:inherit (shadow fixed-pitch)
                 :foreground ,(zmacs--ascii-color/bright :yellow)))))
     `(org-verbatim
       ((,class (:inherit (shadow fixed-pitch)
                 :foreground ,(zmacs--ascii-color/bright :green)))))
     `(org-link
       ((,class (:inherit    fixed-pitch
                 :foreground ,zmacs-blue
                 :background unspecified
                 :underline  t))))
     `(org-drawer
       ((,class (:inherit    fixed-pitch
                 :foreground ,zmacs-cyan
                 :background unspecified))))
     `(org-meta-line
       ((,class (:inherit    fixed-pitch
                 :foreground ,zmacs-magenta
                 :background unspecified))))
     `(org-special-keyword
       ((,class (:inherit    fixed-pitch
                 :foreground ,zmacs-violet
                 :background unspecified))))
     `(org-property-value
       ((,class (:inherit    font-lock-property-name-face
                 :background unspecified))))
     `(org-document-info
       ((,class (:inherit    fixed-pitch
                 :foreground ,zmacs-orange
                 :background unspecified))))
     `(org-document-info-keyword
       ((,class (:inherit    fixed-pitch
                 :foreground ,zmacs-yellow))))
     `(org-document-title
       ((,class (:inherit    fixed-pitch
                 :foreground ,zmacs-foreground
                 :weight     bold
                 :underline  nil
                 :height     1.2))))
     `(org-todo
       ((,class (:inherit    fixed-pitch
                 :foreground ,zmacs-orange
                 :background unspecified
                 :weight     bold))))
     `(org-done
       ((,class (:inherit    fixed-pitch
                 :foreground ,zmacs-green
                 :background unspecified
                 :weight     bold))))
     `(org-checkbox
       ((,class (:inherit    fixed-pitch
                 :foreground ,zmacs-comment-6
                 :background unspecified
                 :weight     bold))))
     `(zmacs-org-list
       ((,class (:inherit    fixed-pitch
                 :foreground ,zmacs-comment-4
                 :background unspecified
                 :weight     bold))))
     `(org-table-header
       ((,class (:inherit    fixed-pitch
                 :foreground ,zmacs-foreground
                 :background ,(zmacs--theme-color *zmacs--color-purple* 7)))))
     `(org-table
       ((,class (:inherit    fixed-pitch
                 :foreground ,zmacs-foreground
                 :background ,(zmacs--theme-color *zmacs--color-grey* 7)))))
     `(org-block
       ((,class (:inherit    fixed-pitch
                 :foreground unspecified
                 :background ,zmacs-black))))
     `(org-block-begin-line
       ((,class (:inherit    fixed-pitch
                 :foreground "SeaGreen3"
                 :background ,zmacs-l4
                 :weight     bold
                 :extend     t))))
     `(org-block-end-line
       ((,class (:inherit    fixed-pitch
                 :foreground "salmon1"
                 :background ,zmacs-l4
                 :weight     bold
                 :extend     t))))
     `(org-verse
       ((,class (:inherit    (fixed-pitch org-block)
                 :foreground ,zmacs-green
                 :slant      italic))))
     `(org-quote
       ((,class (:inherit    (fixed-pitch org-block)
                 :foreground ,zmacs-blue))))
     `(org-level-1
       ((,class (:inherit    fixed-pitch
                 :background ,zmacs-l1
                 :foreground ,zmacs-foreground
                 :weight     bold
                 :height     1.3
                 :extend     t))))
     `(org-level-2
       ((,class (:inherit    fixed-pitch
                 :background ,zmacs-l2
                 :foreground ,zmacs-foreground
                 :weight     bold
                 :height     1.2
                 :extend     t))))
     `(org-level-3
       ((,class (:inherit    fixed-pitch
                 :background ,zmacs-l3
                 :foreground ,zmacs-foreground
                 :weight     bold
                 :height     1.1
                 :extend     t))))
     `(org-level-4
       ((,class (:inherit    fixed-pitch
                 :background ,zmacs-l4
                 :foreground ,zmacs-foreground
                 :weight     bold
                 :extend     t))))
     `(org-level-5
       ((,class (:inherit    fixed-pitch
                 :background ,zmacs-l4
                 :foreground ,zmacs-foreground
                 :weight     bold
                 :extend     t))))
     `(org-level-6
       ((,class (:inherit    fixed-pitch
                 :background ,zmacs-l4
                 :foreground ,zmacs-foreground
                 :weight     bold
                 :extend     t))))
     `(org-level-7
       ((,class (:inherit    fixed-pitch
                 :background ,zmacs-l4
                 :foreground ,zmacs-foreground
                 :weight     bold
                 :extend     t))))
     `(org-level-8
       ((,class (:inherit    fixed-pitch
                 :background ,zmacs-l4
                 :foreground ,zmacs-foreground
                 :weight     bold
                 :extend     t))))
     `(org-modern-tag
       ((,class (:background ,(zmacs--theme-color *zmacs--color-white* 4)
                 :foreground ,(zmacs--ui-color :button-background)
                 :width compressed
                 :height 0.8
                 :weight bold
                 :box (:line-width (-1 . -2)
                       :color ,(zmacs--ui-color :button-background)
                       :style flat-button)))))
     `(org-modern-progress-complete
       ((,class (:inherit org-modern-label
                 :foreground "black"
                 :background ,zmacs-green))))
     `(org-modern-progress-incomplete
       ((,class (:inherit org-modern-label
                 :foreground "black"
                 :background ,zmacs-highlight))))
     `(org-modern-date-active
       ((,class (:inherit org-modern-label
                 :foreground ,zmacs-foreground
                 :background ,zmacs-l2))))
     `(org-modern-date-inactive
       ((,class (:inherit org-modern-label
                 :foreground ,zmacs-foreground
                 :background ,zmacs-l2))))
     `(org-modern-time-active
       ((,class (:inherit org-modern-label
                 :foreground "black"
                 :distant-foreground "black"
                 :background ,zmacs-green))))
     `(org-modern-time-inactive
       ((,class (:inherit org-modern-label
                 :foreground ,zmacs-foreground
                 :distant-foreground ,zmacs-foreground
                 :background ,zmacs-highlight))))
     `(org-time-grid
       ((,class (:inherit fixed-pitch
                 :foreground ,zmacs-comment-8
                 ;;,(zmacs--theme-color *zmacs--color-white* 7)
                 ))))
     `(org-agenda-date-today
       ((,class (:inherit fixed-pitch
                 :foreground ,zmacs-urgent
                 :weight bold
                 :height 1.5))))
     `(org-agenda-current-time
       ((,class (:inherit fixed-pitch
                 :foreground ,zmacs-comment-6
                 :weight bold))))
     `(org-agenda-diary
       ((,class (:inherit fixed-pitch
                 :foreground ,(zmacs--primary-color/faint :magenta)))))
     `(org-agenda-calendar-event
       ((,class (:inherit fixed-pitch
                 :foreground ,(zmacs--primary-color/faint :orange)))))
     `(org-agenda-structure
       ((,class (:inherit fixed-pitch
                 :foreground ,zmacs-comment-3
                 :weight bold
                 :height 1.3))))
     `(org-super-agenda-header
       ((,class (:inherit fixed-pitch
                 :foreground ,zmacs-comment-5
                 :weight bold))))
;;;;;; Slime
     `(slime-inspector-action-face
       ((,class (:inherit    fixed-pitch
                 :foreground ,zmacs-blue
                 :background unspecified))))
     `(slime-inspector-topline-face
       ((,class (:inherit    fixed-pitch
                 :foreground ,zmacs-background
                 :background unspecified
                 :weight     bold))))
     `(slime-inspector-value-face
       ((,class (:inherit    fixed-pitch
                 :foreground ,zmacs-orange
                 :background unspecified))))
     `(slime-inspector-label-face
       ((,class (:inherit    fixed-pitch
                 :foreground ,zmacs-magenta
                 :background unspecified))))
     `(slime-inspector-type-face
       ((,class (:inherit    fixed-pitch
                 :foreground ,zmacs-violet
                 :background unspecified))))
     `(slime-apropos-label
       ((,class (:inherit    fixed-pitch
                 :foreground ,zmacs-blue))))
     `(slime-apropos-symbol
       ((,class (:inherit    fixed-pitch
                 :foreground ,zmacs-orange))))
     `(slime-repl-prompt-face
       ((,class (:inherit meta-repl-prompt-face))))
     `(slime-repl-input-face
       ((,class (:inherit meta-repl-input-face))))
     `(slime-repl-output-face
       ((,class (:inherit meta-repl-output-face))))
     `(slime-repl-inputed-output-face
       ((,class (:inherit meta-repl-output-face))))
     `(slime-repl-output-mouseover-face
       ((,class (:inherit meta-repl-inline-ui-face))))
;;;;;; Flycheck:
     `(flycheck-warning
       ((,class (:underline (:style wave :color ,zmacs-crucial)))))
     `(flycheck-error
       ((,class (:underline (:style wave :color ,zmacs-critical)))))
     `(flycheck-info
       ((,class (:underline (:style wave :color ,zmacs-focus)))))
     `(flycheck-fringe-warning
       ((,class (:foreground ,zmacs-crucial))))
     `(flycheck-fringe-error
       ((,class (:foreground ,zmacs-critical))))
     `(flycheck-fringe-info
       ((,class (:foreground ,zmacs-focus))))
     `(flycheck-error-list-warning
       ((,class (:foreground ,zmacs-crucial
                 :bold       t))))
     `(flycheck-error-list-error
       ((,class (:foreground ,zmacs-critical
                 :bold       t))))
     `(flycheck-error-list-info
       ((,class (:foreground ,zmacs-focus
                 :bold       t))))
;;;;;; Flyspell:
     `(flyspell-duplicate
       ((,class (:underline (:color ,zmacs-red :style line)))))
     `(flyspell-duplicate
       ((,class (:underline (:color ,zmacs-red :style line)))))
;;;;;; Magit:
     `(magit-header-line
       ((,class (:inherit fixed-pitch
                 :foreground ,zmacs-foreground
                 :background ,zmacs-l1))))
;;;;;; Dashboard:
     `(dashboard-text-banner
       ((,class (:inherit    fixed-pitch
                 :foreground ,zmacs-violet
                 :bold       t))))
     `(dashboard-banner-logo-title
       ((,class (:inherit    fixed-pitch
                 :foreground ,zmacs-foreground))))
     `(dashboard-heading
       ((,class (:inherit    fixed-pitch
                 :foreground ,zmacs-blue))))
     `(dashboard-no-items-face
       ((,class (:inherit    fixed-pitch
                 :foreground ,zmacs-foreground
                 :bold       t))))
     `(dashboard-footer-face
       ((,class (:inherit    fixed-pitch
                 :foreground ,zmacs-mild))))
     `(dashboard-init-info-face
       ((,class (:inherit    fixed-pitch
                 :foreground ,zmacs-strong))))
;;;;;; Highlight Indentation:
     `(highlight-indentation-current-column-face
       ((,class (:background ,(zmacs--theme-color *zmacs--color-purple* 6)))))
     `(highlight-indentation-face
       ((,class (:background ,(zmacs--theme-color *zmacs--color-purple* 7)))))
;;;;;; Highlight indentation guides:
     `(highlight-indent-guides-stack-odd-face
       ((,class (:foreground ,zmacs-orange
                 :background unspecified))))
     `(highlight-indent-guides-stack-even-face
       ((,class (:foreground ,zmacs-yellow
                 :background unspecified))))
     `(highlight-indent-guides-top-odd-face
       ((,class (:foreground ,zmacs-orange
                 :background unspecified))))
     `(highlight-indent-guides-top-even-face
       ((,class (:foreground ,zmacs-yellow
                 :background unspecified))))
     `(highlight-indent-guides-odd-face
       ((,class (:foreground ,zmacs-orange
                 :background unspecified))))
     `(highlight-indent-guides-even-face
       ((,class (:foreground ,zmacs-yellow
                 :background unspecified))))
     `(highlight-indent-guides-character-face
       ((,class (:foreground ,zmacs-highlight
                 :background unspecified))))
     `(highlight-indent-guides-top-character-face
       ((,class (:foreground ,zmacs-highlight
                 :background unspecified))))
     `(highlight-indent-guides-stack-character-face
       ((,class (:foreground ,zmacs-highlight
                 :background unspecified))))
;;;;;; Outline:
     `(outline-1
       ((,class (:inherit    fixed-pitch
                 :foreground ,zmacs-comment-1
                 :background unspecified
                 :weight     bold
                 :extend     t))))
     `(outline-2
       ((,class (:inherit    fixed-pitch
                 :foreground ,zmacs-comment-2
                 :background unspecified
                 :weight     bold
                 :extend     t))))
     `(outline-3
       ((,class (:inherit    fixed-pitch
                 :foreground ,zmacs-comment-3
                 :background unspecified
                 :weight     bold
                 :extend     t))))
     `(outline-4
       ((,class (:inherit    fixed-pitch
                 :foreground ,zmacs-comment-4
                 :background unspecified
                 :weight     bold
                 :extend     t))))
     `(outline-5
       ((,class (:inherit    fixed-pitch
                 :foreground ,zmacs-comment-5
                 :background unspecified
                 :weight     bold
                 :extend     t))))
     `(outline-6
       ((,class (:inherit    fixed-pitch
                 :foreground ,zmacs-comment-6
                 :background unspecified
                 :weight     bold
                 :extend     t))))
     `(outline-7
       ((,class (:inherit    fixed-pitch
                 :foreground ,zmacs-comment-7
                 :background unspecified
                 :weight     bold
                 :extend     t))))
     `(outline-8
       ((,class (:inherit    fixed-pitch
                 :foreground ,zmacs-comment-8
                 :background unspecified
                 :weight     bold
                 :extend     t))))
     `(outline-minor-0
       ((,class (:inherit    fixed-pitch
                 :background ,(zmacs--theme-color *zmacs--color-purple* 7)
                 :weight     bold
                 :extend     t))))
;;;;;; Outline minor:
     `(outline-minor-1
       ((,class (:inherit   fixed-pitch
                 :weight    bold
                 :extend    t
                 :underline (:color ,(zmacs--theme-color *zmacs--color-purple* 2)
                             :style line
                             :position nil)))))
     `(outline-minor-2 ((,class (:inherit (outline-minor-0 outline-2)))))
     `(outline-minor-3 ((,class (:inherit (outline-minor-0 outline-3)))))
     `(outline-minor-4 ((,class (:inherit (outline-minor-0 outline-4)))))
     `(outline-minor-5 ((,class (:inherit (outline-minor-0 outline-5)))))
     `(outline-minor-6 ((,class (:inherit (outline-minor-0 outline-6)))))
     `(outline-minor-7 ((,class (:inherit (outline-minor-0 outline-7)))))
     `(outline-minor-8 ((,class (:inherit (outline-minor-0 outline-8)))))
;;;;;; Breadcrumb:
     `(breadcrumb-face
       ((,class (:inherit shadow))))
;;;;;; Calfs:
     `(cfw:face-title
       ((,class (:foreground ,zmacs-urgent
                 :weight bold
                 :height 2.0
                 :inherit variable-pitch))))
     `(cfw:face-header
       ((,class (:foreground ,(zmacs--primary-color/faint :green)
                 :weight bold))))
     `(cfw:face-saturday
       ((,class (:foreground ,(zmacs--primary-color/faint :yellow)
                 :weight bold))))
     `(cfw:face-sunday
       ((,class (:foreground ,(zmacs--primary-color/faint :red)
                 :weight bold))))
     `(cfw:face-holiday
       ((,class (:background "grey10"
                 :foreground ,zmacs-comment-6
                 :weight bold))))
     `(cfw:face-grid
       ((,class (:foreground ,(zmacs--theme-color *zmacs--color-grey* 2)))))
     `(cfw:face-default-content
       ((,class (:foreground "#bfebbf"))))
     `(cfw:face-periods
       ((,class (:foreground "cyan"))))
     `(cfw:face-day-title
       ((,class (:background "grey10"))))
     `(cfw:face-default-day
       ((,class (:weight bold
                 :inherit cfw:face-day-title))))
     `(cfw:face-select
       ((,class (:background ,(zmacs--theme-color *zmacs--color-purple* 2)))))
     `(cfw:face-toolbar
       ((,class (:background ,zmacs-l1
                 :foreground ,zmacs-l1))))
     `(cfw:face-toolbar-button-on
       ((,class (:foreground ,zmacs-foreground
                 :background ,zmacs-l1))))
     `(cfw:face-toolbar-button-off
       ((,class (:foreground ,zmacs-background
                 :background ,zmacs-l1))))
     `(cfw:face-today
       ((,class (:background ,(zmacs--theme-color *zmacs--color-pwhite* 9)
                 :weight bold))))
     `(cfw:face-today-title
       ((,class (:background ,(zmacs--theme-color *zmacs--color-pwhite* 6)
                 :foreground ,zmacs-comment-1
                 :weight bold))))
     `(cfw:face-disable
       ((,class (:foreground ,(zmacs--theme-color *zmacs--color-grey* 1)
                 :inherit cfw:face-day-title))))
     `(cfw:face-annotation
       ((,class (:foreground ,(zmacs--primary-color/bright :magenta)
                 :inherit cfw:face-day-title))))
;;;;;; Todo lists:
     `(zmacs-todo-todo
       ((,class (:foreground ,zmacs-yellow
                 :weight bold))))
     `(zmacs-todo-doing
       ((,class (:foreground ,zmacs-cyan
                 :weight bold))))
     `(zmacs-todo-done
       ((,class (:foreground ,zmacs-green
                 :weight bold))))
     `(zmacs-todo-wait
       ((,class (:foreground ,zmacs-orange
                 :weight bold))))
     `(zmacs-todo-hold
       ((,class (:foreground ,zmacs-magenta
                 :weight bold))))
     `(zmacs-todo-cancelled
       ((,class (:foreground ,zmacs-red
                 :weight bold))))
;;;;; End of faces:
     )                                  ; custom-set-faces
    (custom-theme-set-variables
     theme-name
     `(ansi-color-names-vector
       [,(zmacs--ascii-color/normal :black)
        ,(zmacs--ascii-color/normal :red)
        ,(zmacs--ascii-color/normal :green)
        ,(zmacs--ascii-color/normal :yellow)
        ,(zmacs--ascii-color/normal :blue)
        ,(zmacs--ascii-color/normal :purple)
        ,(zmacs--ascii-color/normal :cyan)
        ,(zmacs--ascii-color/normal :white)]))))

;;;; Highlight parentheses:

(setq-default highlight-parentheses-colors '("#dcff75"
                                             "#f3cc62"
                                             "#fd964f"
                                             "#ff543d"))

;;;; Highlight Todo:

(setq-default hl-todo-keyword-faces
              '(("HOLD"       . zmacs-todo-hold)
                ("TODO"       . zmacs-todo-todo)
                ("NEXT"       . zmacs-todo-doing)
                ("OKAY"       . zmacs-todo-done)
                ("DONT"       . zmacs-todo-cancelled)
                ("FAIL"       . zmacs-todo-cancelled)
                ("DONE"       . zmacs-todo-done)
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
              '(("TODO" . zmacs-todo-todo)
                ("DOING" . zmacs-todo-doing)
                ("DONE" . zmacs-todo-done)
                ("WAIT" . zmacs-todo-wait)
                ("HOLD" . zmacs-todo-hold)
                ("CANCELLED" . zmacs-todo-cancelled)))

;;;; Autoload:

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

;;;; Provide theme:

(provide 'zmacs-themes)

;;; Local Variables:
;;; eval: (when (fboundp 'rainbow-mode) (rainbow-mode +1))
;;; End:
;;; zmacs-theme.el ends here.
