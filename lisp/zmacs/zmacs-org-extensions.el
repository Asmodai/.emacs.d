;;; zmacs-org-extensions.el --- Org extension packages  -*- mode: emacs-lisp; lexical-binding: t; -*-
;;
;; Copyright (c) 2024 Paul Ward <paul@lisphacker.uk>
;;
;; Author:     Paul Ward <paul@lisphacker.uk>
;; Maintainer: Paul Ward <paul@lisphacker.uk>
;; Created:    25 Oct 2024 16:27:17
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

(eval-when-compile
  (require 'cl-lib))

;;;===================================================================
;;;{{{ Org Appearance:

(use-package org-appear
  :after org
  :commands (org-appear-mode)
  :hook (org-mode . org-appear-mode)
  :custom
  (org-appear-autoemphasis   t)
  (org-appear-autolinks      nil)
  (org-appear-autosubmarkers t))

;;;}}}
;;;===================================================================

;;;===================================================================
;;;{{{ Modern:

(use-package org-modern
  :after org
  :hook ((org-mode            . org-modern-mode)
         (org-agenda-finalize . org-modern-agenda))
  :custom
  (org-modern-hide-stars   'leading)
  (org-modern-todo         nil)
  (org-modern-tag          t)
  (org-modern-label-border .25)
  (org-modern-star         ["⦶" "⦷" "⦹" "⊕" "⍟" "⊛" "⏣" "❂"]))

;;;}}}
;;;===================================================================

;;;===================================================================
;;;{{{ Org Autolist:

(use-package org-autolist
  :after org
  :hook (org-mode . org-autolist-mode))

;;;}}}
;;;===================================================================

;;;===================================================================
;;;{{{ Org Babel:

(use-package ob-bitfield
  :ensure t
  :after org
  :defer t
  :init
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((bitfield . t))))

(use-package ob-shell
  :ensure nil
  :after org
  :defer t
  :commands
  (org-babel-execute:sh
   org-babel-expand-body:sh
   org-babel-execute:bash
   org-babel-expand-body:bash))

(use-package ob-lisp
  :ensure nil
  :after org
  :defer t
  :commands (org-babel-execute:lisp))

(use-package ob-latex
  :ensure nil
  :after org
  :defer t
  :commands (org-babel-execute:latex))

;;;}}}
;;;===================================================================

;;;===================================================================
;;;{{{ Org Wild Notifier:

(defvar org-start-notification-daemon-on-startup nil)

(use-package org-wild-notifier
  :after org
  :defer t
  :init
  (when org-start-notification-daemon-on-startup
    (org-wild-notifier-mode)))

;;;}}}
;;;===================================================================

;;;===================================================================
;;;{{{ Org Contacts:

(use-package org-contacts
  :after org
  :ensure nil
  :defer t)

;;;}}}
;;;===================================================================

;;;===================================================================
;;;{{{ Org VCard:

(use-package org-vcard
  :after org
  :defer t)

;;;}}}
;;;===================================================================

;;;===================================================================
;;;{{{ Org Expiry:

(use-package org-expiry
  :ensure nil
  :after org
  :commands (org-expiry-insinuate
             org-expiry-deinsinuate
             org-expiry-insert-created
             org-expiry-insert-expiry
             org-expiry-add-keyword
             org-expiry-archive-subtree
             org-expiry-process-entry
             org-expiry-process-entries))

;;;}}}
;;;===================================================================

;;;===================================================================
;;;{{{ Org Download:

(use-package org-download
  :after org
  :commands (org-download-yank
             org-download-screenshot
             org-download-image)
  :hook (org-mode-hook . org-download-enable)
  :custom
  (org-download-method 'directory)
  (org-download-image-dir (concat *zmacs-org-directory* "pictures/"))
  (org-download-image-latex-width 500)
  (org-download-timestamp "%Y-%m-%d"))

;;;}}}
;;;===================================================================

;;;===================================================================
;;;{{{ Org Export:

(use-package ox-pandoc
  :if (executable-find "pandoc")
  :after ox
  :custom
  (org-pandoc-command (executable-find "pandoc"))
  (org-pandoc-options '((standalone .  t)))
  (org-pandoc-options-for-docx '((standalone . nil)))
  (org-pandoc-options-for-beamer-pdf '((pdf-engine . "xelatex")))
  (org-pandoc-options-for-latex-pdf '((pdf-engine . "xelatex")))
  (org-pandoc-format-extensions '(org+smart)))

(use-package ox-epub
  :after ox
  :defer t
  :config (require 'ox-epub))

(use-package ox-twbs
  :after ox
  :defer t
  :config (require 'ox-twbs))

(use-package ox-gfm
  :after ox
  :defer t
  :config (require 'ox-gfm))

(use-package ox-asciidoc
  :after ox
  :defer t)

(use-package ox-hugo
  :after ox
  :defer t
  :config
  (plist-put org-hugo-citations-plist :bibliography-section-heading "References"))

(setq org-pandoc-menu-entry
      '((?4 "to html5 and open." org-pandoc-export-to-html5-and-open)
        (?$ "as html5." org-pandoc-export-as-html5)
        (?5 "to html5-pdf and open." org-pandoc-export-to-html5-pdf-and-open)
        (?% "to html5-pdf." org-pandoc-export-to-html5-pdf)
        (?8 "to opendocument and open." org-pandoc-export-to-opendocument-and-open)
        (? "as opendocument." org-pandoc-export-as-opendocument)
        (?8 "to opml." org-pandoc-export-to-opml)
        (?9 "to opml and open." org-pandoc-export-to-opml-and-open)
        (?< "to slideous and open." org-pandoc-export-to-slideous-and-open)
        (?, "as slideous." org-pandoc-export-as-slideous)
        (?= "to ms-pdf and open." org-pandoc-export-to-ms-pdf-and-open)
        (?- "to ms-pdf." org-pandoc-export-to-ms-pdf)
        (?a "to asciidoc and open." org-pandoc-export-to-asciidoc-and-open)
        (?A "as asciidoc." org-pandoc-export-as-asciidoc)
        (?b "to beamer-pdf and open." org-pandoc-export-to-beamer-pdf-and-open)
        (?B "to beamer-pdf." org-pandoc-export-to-beamer-pdf)
        (?c "to context-pdf and open." org-pandoc-export-to-context-pdf-and-open)
        (?C "to context-pdf." org-pandoc-export-to-context-pdf)
        (?d "to docbook5 and open." org-pandoc-export-to-docbook5-and-open)
        (?D "as docbook5." org-pandoc-export-as-docbook5)
        (?e "to epub3 and open." org-pandoc-export-to-epub3-and-open)
        (?E "to epub3." org-pandoc-export-to-epub3)
        (?g "to gfm and open." org-pandoc-export-to-gfm-and-open)
        (?G "as gfm." org-pandoc-export-as-gfm)
        (?h "to html4 and open." org-pandoc-export-to-html4-and-open)
        (?H "as html4." org-pandoc-export-as-html4)
        (?i "to icml and open." org-pandoc-export-to-icml-and-open)
        (?I "as icml." org-pandoc-export-as-icml)
        (?j "to json and open." org-pandoc-export-to-json-and-open)
        (?J "as json." org-pandoc-export-as-json)
        (?k "to markdown and open." org-pandoc-export-to-markdown-and-open)
        (?K "as markdown." org-pandoc-export-as-markdown)
        (?l "to latex-pdf and open." org-pandoc-export-to-latex-pdf-and-open)
        (?L "to latex-pdf." org-pandoc-export-to-latex-pdf)
        (?m "to man and open." org-pandoc-export-to-man-and-open)
        (?M "as man." org-pandoc-export-as-man)
        (?n "to native and open." org-pandoc-export-to-native-and-open)
        (?N "as native." org-pandoc-export-as-native)
        (?o "to odt and open." org-pandoc-export-to-odt-and-open)
        (?O "to odt." org-pandoc-export-to-odt)
        (?p "to pptx and open." org-pandoc-export-to-pptx-and-open)
        (?P "to pptx." org-pandoc-export-to-pptx)
        (?r "to rtf and open." org-pandoc-export-to-rtf-and-open)
        (?R "as rtf." org-pandoc-export-as-rtf)
        (?t "to texinfo and open." org-pandoc-export-to-texinfo-and-open)
        (?T "as texinfo." org-pandoc-export-as-texinfo)
        (?u "to dokuwiki and open." org-pandoc-export-to-dokuwiki-and-open)
        (?U "as dokuwiki." org-pandoc-export-as-dokuwiki)
        (?v "to revealjs and open." org-pandoc-export-to-revealjs-and-open)
        (?V "as revealjs." org-pandoc-export-as-revealjs)
        (?w "to mediawiki and open." org-pandoc-export-to-mediawiki-and-open)
        (?W "as mediawiki." org-pandoc-export-as-mediawiki)
        (?x "to docx and open." org-pandoc-export-to-docx-and-open)
        (?X "to docx." org-pandoc-export-to-docx)))

(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "C-+")
    (lambda()
      (interactive)
      (diredp-do-apply/eval 'org-hugo-export-wim-to-md '(4)))))

(defun org-hugo-link-complete ()
  "Create link with Hugo ref shortcode"
  (concat "{{% ref " (file-relative-name (read-file-name "File: ")) " %}}"))

(defun org-hugo-follow (link)
  (find-file (expand-file-name link)))

(with-eval-after-load 'org
  (org-link-set-parameters "hugo"
                           :complete 'org-hugo-link-complete
                           :follow 'org-hugo-follow))

;;;}}}
;;;===================================================================

;;;===================================================================
;;;{{{ Time tracking:

(use-package org-pomodoro
  :after org
  :commands org-pomodoro)

;;;}}}
;;;===================================================================

;;;===================================================================
;;;{{{ Org Mime:

(use-package org-mime
  :after org
  :defer t)

;;;}}}
;;;===================================================================

;;;===================================================================
;;;{{{ Org Present:

(defun zmacs--org-present-start ()
  "Initiate `org-present' mode."
  (org-present-big)
  (org-display-inline-images)
  (org-present-hide-cursor)
  (org-present-read-only))

(defun zmacs--org-present-end ()
  "Terminate `org-present' mode."
  (org-present-small)
  (if (not org-startup-with-inline-images)
      (org-remove-inline-images))
  (org-present-show-cursor)
  (org-present-read-write))

(use-package org-present
  :after org
  :defer t
  :init
  (add-hook 'org-present-mode-hook      #'zmacs--org-present-start)
  (add-hook 'org-present-mode-quit-hook #'zmacs--org-present-end))

;;;}}}
;;;===================================================================

;;;===================================================================
;;;{{{ Org Cliplink:

(use-package org-cliplink
  :after org
  :defer t)

;;;}}}
;;;===================================================================

;;;===================================================================
;;;{{{ Re-reveal:

(use-package org-re-reveal
  :after org
  :defer t
  :config (require 'org-re-reveal))

;;;}}}
;;;===================================================================

;;;===================================================================
;;;{{{ Persp Mode:

(use-package persp-mode
  :after org
  :defer t)

;;;}}}
;;;===================================================================

;;;===================================================================
;;;{{{ Sticky Header:

(use-package org-sticky-header
  :after org
  :defer t
  :hook (org-mode-hook . org-sticky-header-mode))

;;;}}}
;;;===================================================================

;;;===================================================================
;;;{{{ Verb:

(use-package verb
  :after org
  :defer t)

;;;}}}
;;;===================================================================

;;;===================================================================
;;;{{{ VAlign:

(use-package valign
  :after org
  :defer t
  :init
  (zmacs-diminish valign-mode " ⇅" " VA")
  (add-hook 'org-mode-hook 'valign-mode)
  (add-hook 'valign-mode-hook (lambda ()
                                (unless valign-mode
                                  (valign-remove-advice)))))

;;;}}}
;;;===================================================================

;;;===================================================================
;;;{{{ Transclusion:

(use-package org-transclusion
  :after org
  :defer t)

;;;}}}
;;;===================================================================

;;;===================================================================
;;;{{{ Bullets:

(use-package org-bullets
  :after org
  :defer t
  :hook (org-mode-hook . org-bullets-mode))

;;;}}}
;;;===================================================================

;;;===================================================================
;;;{{{ Remark:

(use-package org-remark
  :after org
  :defer t
  :bind
  (("C-c n m" . org-remark-mark)
   ("C-c n l" . org-remark-mark-line)
   :map org-remark-mode-map
   ("C-c n o" . org-remark-open)
   ("C-c n ]" . org-remark-view-next)
   ("C-c n [" . org-remark-view-prev)
   ("C-c n r" . org-remark-remove)
   ("C-c n d" . org-remark-delete))
  :init
  (zmacs-diminish org-remark-global-tracking-mode " ✐" " Or")
  (org-remark-global-tracking-mode +1)
  :config
  (use-package org-remark-info
    :ensure nil
    :after info
    :config (org-remark-info-mode +1))
  (use-package org-remark-eww
    :ensure nil
    :after eww
    :config (org-remark-eww-mode +1))
  (use-package org-remark-nov
    :ensure nil
    :after nov
    :config (org-remark-nov-mode +1)))

;;;}}}
;;;===================================================================

(provide 'zmacs-org-extensions)

;;; zmacs-org-extensions.el ends here.
