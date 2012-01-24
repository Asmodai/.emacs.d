;;; -*- Mode: Emacs-Lisp -*-
;;;
;;; licenses.el --- Boilerplate license insertion
;;;
;;; Time-stamp: <Monday Jan 23, 2012 23:41:54 asmodai>
;;; Revision:   2
;;;
;;; Copyright (c) 2005-2012 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    15 Feb 2011 16:52:00
;;; Keywords:   
;;; URL:        not distributed yet
;;;
;;; {{{ License:
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
;;; }}}
;;; {{{ Commentary:
;;;
;;; }}}

(require 'easymenu)
(require 'cl)

(defvar licenses-mode-menu nil)

(defvar licenses-default "gpl3"
  "The default license to insert should none be chosen.")

;;; ==================================================================
;;; {{{ Licenses:

;;; ------------------------------------------------------------------
;;; {{{ GNU General Public License v2.0:

(defconst licenses-gnu-gpl-2.0
  (concat
   "This program is free software; you can redistribute it and/or\n"
   "modify it under the terms of the GNU General Public License\n"
   "as published by the Free Software Foundation; either version 2\n"
   "of the license, or (at your option) any later version.\n"
   "\n"
   "This program is distributed in the hope that it will be useful,\n"
   "but WITHOUT ANY WARRANTY; without even the implied warranty of\n"
   "MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n"
   "GNU General Public License for more details.\n"
   "\n"
   "You should have received a copy of the GNU General Public License\n"
   "along with this program; if not, write to the Free Software\n"
   "Foundation, Inc., 59 Temple Place - Suit 330, Boston, MA\n"
   "02111-1307, USA.\n")
  "GNU GPL v2.0")

;;; }}}
;;; ------------------------------------------------------------------

;;; ------------------------------------------------------------------
;;; {{{ GNU General Public License v3.0:

(defconst licenses-gnu-gpl-3.0
  (concat
   "This program is free software: you can redistribute it\n"
   "and/or modify it under the terms of the GNU General Public\n"
   "License\ as published by the Free Software Foundation,\n"
   "either version 3 of the License, or (at your option) any\n"
   "later version.\n"
   "\n"
   "This program is distributed in the hope that it will be\n"
   "useful, but WITHOUT ANY  WARRANTY; without even the implied\n"
   "warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR\n"
   "PURPOSE.  See the GNU General Public License for more\n"
   "details.\n"
   "\n"
   "You should have received a copy of the GNU General Public\n"
   "License along with this program.  If not, see\n"
   "<http://www.gnu.org/licenses/>.\n")
  "GNU GPL v3.0")

;;; }}}
;;; ------------------------------------------------------------------

;;; ------------------------------------------------------------------
;;; {{{ GNU Lesser General Public License v2.1:

(defconst licenses-gnu-lgpl
  (concat
   "This library is free software; you can redistribute it and/or\n"
   "modify it under the terms of the GNU Lesser General Public\n"
   "License as published by the Free Software Foundation; either\n"
   "version 2.1 of the License, or (at your option) any later version.\n"
   "\n" 
   "This library is distributed in the hope that it will be useful,\n"
   "but WITHOUT ANY WARRANTY; without even the implied warranty of\n"
   "MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU\n"
   "Lesser General Public License for more details.\n"
   "\n" 
   "You should have received a copy of the GNU Lesser General Public\n"
   "License along with this library; if not, write to the Free Software\n"
   "Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA\n"
   "02111-1307  USA\n")
  "GNU LGPL 2.1")

;;; }}}
;;; ------------------------------------------------------------------

;;; ------------------------------------------------------------------
;;; {{{ Franz Lisp GNU Lesser General Public License:

(defconst licenses-franz-llgpl
  (concat
   "This code is free software; you can redistribute it and/or\n"
   "modify it under the terms of the version 2.1 of\n"
   "the GNU Lesser General Public License as published by\n"
   "the Free Software Foundation, as clarified by the Franz\n"
   "preamble to the LGPL found in\n"
   "http://opensource.franz.com/preamble.html.\n"
   "\n"
   "This code is distributed in the hope that it will be useful,\n"
   "but without any warranty; without even the implied warranty of\n"
   "merchantability or fitness for a particular purpose.  See the GNU\n"
   "Lesser General Public License for more details.\n"
   "\n"
   "Version 2.1 of the GNU Lesser General Public License can be\n"
   "found at http://opensource.franz.com/license.html.\n"
   "If it is not present, you can access it from\n"
   "http://www.gnu.org/copyleft/lesser.txt (until superseded by a newer\n"
   "version) or write to the Free Software Foundation, Inc., 59 Temple\n"
   "Place, Suite 330, Boston, MA  02111-1307  USA\n")
  "Franz Lisp LGPL")

;;; }}}
;;; ------------------------------------------------------------------

;;; ------------------------------------------------------------------
;;; {{{ GNU Free Documentation License:

(defconst licenses-gnu-fdl
  (concat
   "Permission is granted to copy, distribute and/or modify this document\n"
   "under the terms of the GNU Free Documentation License, Version 1.1\n"
   "or any later version published by the Free Software Foundation;\n"
   "with the Invariant Sections being LIST THEIR TITLES, with the\n"
   "Front-Cover Texts being LIST, and with the Back-Cover Texts being LIST.\n"
   "A copy of the license is included in the section entitled \"GNU\n"
   "Free Documentation License\".\n")
  "GNU FDL")

;;; }}}
;;; ------------------------------------------------------------------

;;; ------------------------------------------------------------------
;;; {{{ MIT License:

(defconst licenses-mit
  (concat
   "Permission is hereby granted, free of charge, to any person\n"
   "obtaining a copy of this software and associated documentation files\n"
   "(the \"Software\"), to deal in the Software without restriction,\n"
   "including without limitation the rights to use, copy, modify, merge,\n"
   "publish, distribute, sublicense, and/or sell copies of the Software,\n"
   "and to permit persons to whom the Software is furnished to do so,\n"
   "subject to the following conditions:\n"
   "\n"
   "The above copyright notice and this permission notice shall be\n"
   "included in all copies or substantial portions of the Software.\n"
   "\n"
   "THE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND,\n"
   "EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF\n"
   "MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND\n"
   "NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS\n"
   "BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN\n"
   "ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN\n"
   "CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE\n"
   "SOFTWARE.")
  "MIT")

;;; }}}
;;; ------------------------------------------------------------------

;;; ------------------------------------------------------------------
;;; {{{ BSD License:

(defconst licenses-bsd
  (concat
   "Redistribution and use in source and binary forms, with or without\n"
   "modification, are  permitted provided that the following conditions are\n"
   "met:\n"
   "\n"
   "Redistributions of source code must retain the above copyright notice,\n"
   "this list of conditions and the following disclaimer. \n"
   "\n"
   "Redistributions in binary form must reproduce the above copyright\n"
   "notice, this list of conditions and the following disclaimer in the\n"
   "documentation and/or other materials provided with the distribution.\n"
   "\n"
   "THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS \"AS\n"
   "IS\" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED\n"
   "TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A\n"
   "PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER\n"
   "OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,\n"
   "EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,\n"
   "PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR\n"
   "PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF\n"
   "LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING\n"
   "NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS\n"
   "SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.\n")
  "New BSD")

;;; }}}
;;; ------------------------------------------------------------------


;;; }}}
;;; ==================================================================

;;; ==================================================================
;;; {{{ Function:

(defun licenses (lictype)
  "Insert a license into the current buffer at the current point."
  (interactive "P")
  (let ((option (or (if lictype
                        lictype
                        (completing-read
                         "Which license do you wish to use? "
                         '(("gpl2" 1)
                           ("gpl3" 2)
                           ("lgpl" 3)
                           ("llgpl" 4)
                           ("fdl" 5)
                           ("mit" 6)
                           ("bsd" 7))
                         nil t nil))
                    (if (equal licenses-default nil)
                        (error "You need to set a default license.")
                        licenses-default))))
    (when option
      (cond ((string= option "gpl2") (insert-string licenses-gnu-gpl-2.0))
            ((string= option "gpl3") (insert-string licenses-gnu-gpl-3.0))
            ((string= option "lgpl") (insert-string licenses-gnu-lgpl))
            ((string= option "llgpl") (insert-string licenses-franz-llgpl))
            ((string= option "fdl") (insert-string licenses-gnu-fdl))
            ((string= option "mit") (insert-string licenses-mit))
            ((string= option "bsd") (insert-string licenses-bsd))))))


;;; }}}
;;; ==================================================================

(provide 'licenses)

(global-set-key [(control f2)] 'licenses)

;;; licenses.el ends here
