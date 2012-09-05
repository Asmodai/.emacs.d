;;; -*- Mode: Emacs-Lisp -*-

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-default-style (quote ((c-mode . "bsd") (lsl-mode . "bsd") (csharp-mode . "bsd") (c++-mode . "bsd") (cc-mode . "bsd") (objc-mode . "gnu") (java-mode . "java") (other . "gnu"))))
 '(column-number-mode t)
 '(ecb-cache-directory-contents (quote (("^/\\([^:/]*@\\)?\\([^@:/]*\\):.*" . 0) (".*" . 50) ("*.*.~*~" . 1000))))
 '(ecb-options-version "2.40")
 '(ecb-prescan-directories-for-emptyness nil)
 '(ecb-source-file-regexps (quote ((".*" nil nil))))
 '(ecb-source-path (quote (("/Users/asmodai/Documents/Projects/XHack" "XHack"))))
 '(ede-project-directories (quote ("/Users/asmodai/Documents/Projects/XHack" "/Users/asmodai/Documents/Projects/killer-lsm")))
 '(erc-fool-highlight-type (quote all))
 '(erc-fools (quote ("\\\\(echelon\\\\)" "\\\\(silentnoise\\\\)")))
 '(erc-modules (quote (autojoin button capab-identify completion fill irccontrols list match menu move-to-prompt netsplit networks noncommands readonly ring stamp track unmorse)))
 '(erc-nick "AsmoEMACS")
 '(erc-nick-uniquifier "`")
 '(erc-pal-highlight-type (quote all))
 '(erc-pals (quote ("\\\\(booto\\\\)" "\\\\(wa1800z\\\\)" "\\\\(enstyne\\\\)" "\\\\(BSDGurl\\\\)" "\\\\(int16h\\\\)")))
 '(erc-paranoid t)
 '(erc-user-full-name "Paul")
 '(font-lock-mode t t (font-lock))
 '(gud-gdb-command-name "gdb --annotate=1")
 '(gutter-buffers-tab-visible-p t t)
 '(gutters-buffers-tab-visible-p t t)
 '(header-copyright-notice "")
 '(header-date-format t)
 '(hl-paren-background-colors nil)
 '(hl-paren-colors (quote ("firebrick1" "DarkRed" "IndianRed" "LightCoral" "Salmon" "DarkSalmon")))
 '(inhibit-startup-screen t)
 '(large-file-warning-threshold nil)
 '(paren-mode (quote paren) nil (paren))
 '(safe-local-variable-values (quote ((Package . DARK-HERESY) (Package . HUNCHENTOOT) (Package . dark-heresy) (folded-file . t) (Fonts . CPTFONT\,CPTFONTI\,CPTFONTCB) (Package . dw) (Fill . Yes) (auto-fill-mode . Yes) (Package . TERMINAL-DRIVER) (Package . System-internals) (Package . UT) (Package . COMMAND-PROCESSOR) (Package . DYNAMIC-TTY) (Package . XLIB) (Package . NULISP-COLD) (Package ARCH :use CL) (Package . NULISP) (Package . NULISP-INTERNALS) (Package . LISP-MACHINE) (Readtable . ZL) (Package . MACINTOSH-INTERNALS) (Package . RPC) (Syntax . Lisp+C) (Package . TCP) (Default-character-style :fix :roman :normal) (Syntax . ansi-common-lisp) (Patch-File . Yes) (Package . NETWORK-INTERNALS) (Package . FLAVOR) (Package . DIS) (Package . FED) (Package . C-user) (Package . CLtL-Internals) (Package . user) (Syntax . common-lisp) (Package ddex :use SCL) (Package . COMMON-LISP-INTERNALS) (Package . JOSHUA-INTERNALS) (Package . Nsage) (Package . SCT) (Package . NSAGE) (Package . http) (Syntax . Ansi-Common-Lisp) (Package . FTP) (Readtable . CL) (Package WHY-PC GLOBAL) (Package . CL-NAMESPACE) (Package . CL-STARTUP) (Package . DYNAMIC-WINDOWS) (Package . CLUEI) (Fonts CPTFONT HL10B) (Fonts TR12 HL12B TR12I CPTFONT HL10 CPTFONTB) (Cold-Load . t) (Package . SYS) (Cold-load . T) (Fonts Cptfont Cptfontb Hl12bi Cptfont Medfnb) (Fonts Cptfont Cptfontb Hl12bi) (Package . Name) (Cold-Load . T) (Package . NAME) (Package . CL-HACKS-CLOS) (Package . CL-HACKS-FAD) (Package . CL-HACKS-UUID) (Package . CL-FAD) (Package . CL-HACKS-INTERNALS) (rcs-header . "$Header: /hope/lwhope1-cam/hope.0/compound/63/LISPparsergen/RCS/defsys.lisp,v 1.9.13.1 2009/08/21 21:14:37 davef Exp $") (rcs-header . "$Header: /hope/lwhope1-cam/hope.0/compound/63/LISPeditor/RCS/shell-defsys.lisp,v 1.1.15.1 2009/08/21 21:12:51 davef Exp $") (rcs-header . "$Header: /hope/lwhope1-cam/hope.0/compound/42/LISPccl/RCS/prof-defsys.lisp,v 1.7.1.1 2009/08/21 21:12:19 davef Exp $") (Package . CL-HACKS-MOP) (Package . CL-MUD) (Package . DOMAIN-NAME-SYSTEM) (Package . NEKO) (Fonts CPTFONT HL12B HL12I) (Package . FILE-SYSTEM) (Package . DEMO) (Fonts medfnt hl12b) (Font . CPTFONT) (Package REMIND :size 100) (Fonts CPTFONT HL12BI HL12B) (Package . CP) (Package . IVT) (Fonts cptfont HL12B HL12BI) (Package . TELNET) (Fonts MEDFNT MEDFNB HL12B) (Fonts CPTFONT HL10B TR10BI TR10I) (Fonts MEDFNB hl12B hl12bi) (Fonts MEDFNB HL12B HL12BI) (Package . X11) (Package . UA) (Readtable . T) (Fonts CPTFONT CPTFONTB) (\1Mode . Common-Lisp) (\*cold-load . t) (Package . Compiler2) (FONTS cptfont hl10b) (PACKAGE . user) (BASE . 10) (Package . System-Internals) (Package . LT) (Package . Common-lisp-internals) (Package . FCLI) (movitz-mode . t) (Package . COMPILER) (TeX-master . t) (Package . SCHEME-USER) (Fonts CPTFONT HL10B HL12BI) (Package . UCL) (Package . Scheme) (Patch-file . T) (Fonts CPTFONT HL12B HL12BI) (Package . SI) (Patch-file) (Fonts CPTFONT CPTFONTB HL12BI) (Package . Compiler) (Package . c-system) (Package . SYSTEM-APPLICATIONS) (Package . FS) (Fonts MEDFNT HL12B HL12I MEDFNB) (Fonts . CPTFONT\,HL12B\,HL12BI) (Package . TV) (Patch-File . t) (Fonts cptfont) (Package . Net) (Fonts CPTFONT HL12 HL12BI CPTFONTB) (Package . W) (Fonts CPTFONT HL12 TR12I COURIER ADOBE-COURIER14B HL12B CPTFONTBI) (Vsp . 0) (Fonts MEDFNT HL12B HL12BI) (Fonts . hl12\,HL12B\,HL12BI) (Fonts . cptfont\,HL12\,HL12BI\,cptfontb) (Package . User) (Package "CONDITIONS" :USE "LISP" :SHADOW ("BREAK" "ERROR" "CERROR" "SIGNAL" "CHECK-TYPE" "ASSERT" "ETYPECASE" "CTYPECASE" "ECASE" "CCASE" "IGNORE-ERRORS" "MAKE-CONDITION" "WARN" "*BREAK-ON-WARNINGS*")) (Package . X-SCREEN) (Package . ZETALISP-USER) (Package . CLEAT\.DATASTORE\.UTILS) (Package . ZETALISP) (Package color-palette) (Patch-File . T) (Package . CLEAT\.DATASTORE\.INDICES) (Package . CLEAT\.DATASTORE\.XML-IMPORT/EXPORT) (Package . CLEAT\.DATASTORE\.XML-IMPORTER) (Package . CLEAT\.DATASTORE\.XML) (Package . CLEAT\.DATASTORE\.DATA) (Package . DATASTORE) (Package . COLOR) (Syntax . ZETALISP) (Package . SYSTEM-INTERNALS) (Package . POSTSCRIPT) (Package . DBFS) (Package . SPITFIRE-CORE) (Package . cl-user) (Package . SPITFIRE-WINDOWS) (Package . RUNES) (Package . CCL) (Package . ccl) (Base . 8) (Package . ZWEI) (Syntax . Zetalisp) (Package . CURSES) (Package . DREI-BUFFER) (Package . METASIM-CLIM) (Package . CLIM-USER) (Package . CLIM-INTERNALS) (Syntax . Common-lisp) (Package . CL-DOC) (Package . TERMINFO) (Syntax . ANSI-Common-Lisp) (Package . METASIM-UTILS) (Package . CL-HACKS) (eval add-hook (quote write-file-hooks) (quote time-stamp)) (Package . COMMON-LISP-USER) (Syntax . COMMON-LISP) (Package . Destiny) (Package . DESTINY) (Package . METASIM) (Lowercase . Yes) (Package . CL-USER) (Syntax . ANSI-COMMON-LISP) (Lowercase . T) (Package . USER) (Package . C-SYSTEM) (Base . 10) (Syntax . Common-Lisp))))
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(toolbar-captioned-p nil)
 '(url-be-asynchronous t)
 '(url-personal-mail-address "asmodai@gmail.com")
 '(url-privacy-level (quote (os agent)))
 '(user-full-name "Paul Ward")
 '(user-mail-address "asmodai@gmail.com")
 '(user-url-address "http://unixware.kicks-ass.org/" t)
 '(w3-default-homepage "http://localhost/")
 '(widget-mouse-face (quote custom-button-mouse)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background "#0e0412" :foreground "#c4b1cb")))))
