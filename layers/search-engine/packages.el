;;; -*- Mode: Emacs-Lisp -*-
;;;
;;; packages.el --- Search engine packages.
;;;
;;; Copyright (c) 2015-2016 Paul Ward <asmodai@gmail.com>
;;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    04 Dec 2015 22:02:23
;;; Keywords:   
;;; URL:        not distributed yet
;;;
;;;{{{ License:
;;;
;;; This program is free software: you can redistribute it
;;; and/or modify it under the terms of the GNU General Public
;;; License as published by the Free Software Foundation,
;;; either version 3 of the License, or (at your option) any
;;; later version.
;;;
;;; This program is distributed in the hope that it will be
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
;;;}}}

(setq search-engine-packages
      '(engine-mode))

(defun search-engine:init-engine-mode ()
  (use-package engine-mode
    :commands (defengine bootstrap:search-engine-select)
    :defines search-engine-alist
    :init
    (setq search-engine-alist
           '((amazon
             :name "Amazon"
             :url "http://www.amazon.com/s/ref=nb_sb_noss?url=search-alias%%3Daps&field-keywords=%s")
            (bing
             :name "Bing"
             :url "http://www.bing.com/search?q=%s")
            (duck-duck-go
             :name "Duck Duck Go"
             :url "https://duckduckgo.com/?q=%s")
            (google
             :name "Google"
             :url "http://www.google.com/search?ie=utf-8&oe=utf-8&q=%s")
            (google-images
             :name "Google Images"
             :url "http://www.google.com/images?hl=en&source=hp&biw=1440&bih=795&gbv=2&aq=f&aqi=&aql=&oq=&q=%s")
            (github
             :name "Github"
             :url "https://github.com/search?ref=simplesearch&q=%s")
            (google-maps
             :name "Google Maps"
             :url "http://maps.google.com/maps?q=%s")
            (twitter
             :name "Twitter"
             :url "https://twitter.com/search?q=%s")
            (project-gutenberg
             :name "Project Gutenberg"
             :url "http://www.gutenberg.org/ebooks/search.html/?format=html&default_prefix=all&sort_order=&query=%s")
            (youtube
             :name "YouTube"
             :url "http://www.youtube.com/results?aq=f&oq=&search_query=%s")
            (stack-overflow
             :name "Stack Overflow"
             :url "https://stackoverflow.com/search?q=%s")
            (spacemacs-issues
             :name "Spacemacs Issues"
             :url "https://github.com/syl20bnr/spacemacs/issues?utf8=%%E2%%9C%%93&q=is%%3Aissue+is%%3Aopen+%s")
            (spacemacs-pullrequests
             :name "Spacemacs Pull Requests"
             :url "https://github.com/syl20bnr/spacemacs/pulls?utf8=%%E2%%9C%%93&q=is%%3Aissue+is%%3Aopen+%s")
            (wikipedia
             :name "Wikipedia"
             :url "http://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s")
            (wolfram-alpha
             :name "Wolfram Alpha"
             :url "http://www.wolframalpha.com/input/?i=%s")))
    :config
    (engine-mode t)
    (mapcar (lambda (engine)
              (let* ((cur-engine (car engine))
                     (engine-url (plist-get (cdr engine) :url)))
                (eval `(defengine ,cur-engine ,engine-url))))
            search-engine-alist)

    (defun bootstrap::search-engine-source (engines)
      `((name . "Search Engines")
        (candidates . ,(mapcar (lambda (engine)
                                 (cons (plist-get (cdr engine) :name)
                                       (intern (format "engine/search-%S"
                                                       (car engine)))))
                               engines
                               ))
        (action . (lambda (candidate)
                    (call-interactively candidate)))))

    (defun bootstrap:search-engine-select ()
      (interactive)
      (helm :sources (list (bootstrap::search-engine-source
                            search-engine-alist))))))

;;; packages.el ends here.
