;;; -*- Mode: Emacs-Lisp -*-
;;;
;;; packages.el --- Markdown packages.
;;;
;;; Copyright (c) 2016 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    11 Mar 2016 19:58:49
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

(setq markdown-packages
      '(emoji-cheat-sheet-plus
        gh-md
        markdown-mode
        markdown-toc
        mmm-mode
        company
        company-emoji
        smartparens))

(defun markdown:post-init-emoji-cheat-sheet-plus ()
  (add-hook 'markdown-mode-hook 'emoji-cheat-sheet-plus-display-mode))

(defun markdown:init-gh-md ()
  (use-package gh-md
    :defer t))

(defun markdown:post-init-smartparens ()
  (add-hook 'markdown-mode-hook 'smartparens-mode))

(defun markdown:init-markdown-mode ()
  (use-package markdown-mode
    :defer t
    :config
    (progn
      (defun bootstrap:insert-keybinding-markdown (key)
        "Ask for a key and then insert its description.

This will work with `org-mode' and any mode that accepts plain HTML."
        (interactive "kType key sequence: ")
        (let* ((tag "~%s~"))
          (if (null (equal key "\r"))
              (insert
               (format tag (help-key-description key nil)))
            (insert (format tag ""))
            (forward-char -6))))

      (define-key markdown-mode-map (kbd "M-h") 'markdown-promote)
      (define-key markdown-mode-map (kbd "M-j") 'markdown-move-down)
      (define-key markdown-mode-map (kbd "M-k") 'markdown-move-up)
      (define-key markdown-mode-map (kbd "M-l") 'markdown-demote))))

(defun markdown:init-markdown-toc ()
  (use-package markdown-toc
    :defer t))

(defun markdown:init-mmm-mode ()
  (use-package mmm-mode
    :commands mmm-parse-buffer
    :config
    (progn
      
      (mmm-add-classes '((markdown-python
                          :submode python-mode
                          :face mmm-declaration-submode-face
                          :front "^```python[\n\r]+"
                          :back "^```$")))
      (mmm-add-classes '((markdown-html
                          :submode web-mode
                          :face mmm-declaration-submode-face
                          :front "^```html[\n\r]+"
                          :back "^```$")))
      (mmm-add-classes '((markdown-java
                          :submode java-mode
                          :face mmm-declaration-submode-face
                          :front "^```java[\n\r]+"
                          :back "^```$")))
      (mmm-add-classes '((markdown-ruby
                          :submode ruby-mode
                          :face mmm-declaration-submode-face
                          :front "^```ruby[\n\r]+"
                          :back "^```$")))
      (mmm-add-classes '((markdown-c
                          :submode c-mode
                          :face mmm-declaration-submode-face
                          :front "^```c[\n\r]+"
                          :back "^```$")))
      (mmm-add-classes '((markdown-c++
                          :submode c++-mode
                          :face mmm-declaration-submode-face
                          :front "^```c\+\+[\n\r]+"
                          :back "^```$")))
      (mmm-add-classes '((markdown-elisp
                          :submode emacs-lisp-mode
                          :face mmm-declaration-submode-face
                          :front "^```elisp[\n\r]+"
                          :back "^```$")))
      (mmm-add-classes '((markdown-javascript
                          :submode javascript-mode
                          :face mmm-declaration-submode-face
                          :front "^```javascript[\n\r]+"
                          :back "^```$")))
      (mmm-add-classes '((markdown-ess
                          :submode R-mode
                          :face mmm-declaration-submode-face
                          :front "^```{?r.*}?[\n\r]+"
                          :back "^```$")))
      (mmm-add-classes '((markdown-rust
                          :submode rust-mode
                          :face mmm-declaration-submode-face
                          :front "^```rust[\n\r]+"
                          :back "^```$")))

      (setq mmm-global-mode t)
      (mmm-add-mode-ext-class 'markdown-mode nil 'markdown-python)
      (mmm-add-mode-ext-class 'markdown-mode nil 'markdown-java)
      (mmm-add-mode-ext-class 'markdown-mode nil 'markdown-ruby)
      (mmm-add-mode-ext-class 'markdown-mode nil 'markdown-c)
      (mmm-add-mode-ext-class 'markdown-mode nil 'markdown-c++)
      (mmm-add-mode-ext-class 'markdown-mode nil 'markdown-elisp)
      (mmm-add-mode-ext-class 'markdown-mode nil 'markdown-html)
      (mmm-add-mode-ext-class 'markdown-mode nil 'markdown-javascript)
      (mmm-add-mode-ext-class 'markdown-mode nil 'markdown-ess)
      (mmm-add-mode-ext-class 'markdown-mode nil 'markdown-rust))))

(when (bootstrap-layer:layer-used-p 'auto-completion)
  (defun markdown:post-init-company ()
    (bootstrap:add-company-hook markdown-mode)
    (push 'company-capf *company-backends-markdown-mode*))

  (defun markdown:post-init-company-emoji ()
    (push 'company-emoji *company-backends-markdown-mode*)))

;;; packages.el ends here
