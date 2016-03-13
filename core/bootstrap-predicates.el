;;;==================================================================
;;;{{{ Operating system predicates:

(defsubst windows-32-p ()
  "T if we are running on Windows 386 through Windows 9x/Me."
  (memq system-type '(ms-windows win386)))

(defsubst windows-nt-p ()
  "T if we are running on Windows NT or higher."
  (eq system-type 'windows-nt))

(defsubst windows-p ()
  "T if we are running on some sort of Windows."
  (or (windows-32-p)
      (windows-nt-p)))

(defsubst ms-dos-p ()
  "T if we are running on some version of DOS."
  (memq system-type '(dos ms-dos)))

(defsubst linux-p ()
  "T if we are running on GNU/Linux."
  (or (eq system-type 'linux)
      (eq system-type 'gnu/linux)))

(defsubst next-mach-p ()
    "T if we are running on a NeXT Mach system."
  (eq system-type 'next-mach))

(defsubst mac-os-x-p ()
  "T if we are running on Mac OS X or Darwin."
  (eq system-type 'darwin))

(defsubst vms-p ()
  "T if we are running on VMS."
  (or (eq system-type 'vms)
      (eq system-type 'vax-vms)))

(defsubst unix-p ()
  "T if we are running on a UNIX system of some sort."
  (or (memq system-type '(aix-v3        ; IBM AIX
                          berkeley-unix ; BSD
                          usg-unix-v    ; Unix System V
                          dgux          ; Data General Unix System V
                          gnu           ; GNU Hurd
                          hpux          ; HP-UX
                          irix          ; SGI Irix
                          netbsd        ; NetBSD
                          rtu           ; Some sort of SysV
                          sco           ; SCO SysV 3.2
                          SCO\ sysv5uw7 ; Gee, thanks SCO.
                          unisoft-unix  ; Ancient Unix for the Sun 1
                          xenix))       ; MS/SCO Xenix
      (linux-p)
      (next-mach-p)
      (mac-os-x-p)))

(defsubst cygwin-p ()
  "T if we are running atop the Cygwin emulation layer."
  (eq system-type 'cygwin))

(defsubst emx-p ()
  "T if we are running atop the EMX emulation layer on IBM OS/2."
  (eq system-type 'emx))

;;;}}}
;;;==================================================================

;;;==================================================================
;;;{{{ Windowing system predicates:

(defsubst x-windows-p ()
  "T if we are running inside the X Window System."
  (eq window-system 'x))

(defsubst presentation-manager-p ()
  "T if we are running inside OS/2's Presentation Manager."
  (eq window-system 'pm))

(defsubst nextstep-p ()
  "T if we are running inside NeXTSTEP, GNUstep, or Cocoa."  
  (eq window-system 'ns))

(defsubst terminal-p ()
  "T if we are running in a text-based terminal of some sort."
  (eq window-system nil))

(defsubst unicode-p ()
  "T if we are using something (terminal or not) that can render unicode.

NOTE: If the terminal type is `linux', then we assume that unicode blows chunks
and explicitly return nil."
  (let ((tty (tty-type)))
    (if (and (null tty)
             (not (null window-system)))
        t
      (cond ((string-match "\\`\\(linux\\)\\'" tty)
             nil)
            ((string-match "\\`\\(screen\\)" tty)
             t)
            ((string-match "\\`\\(xterm\\)" tty)
             t)
            ((string-match "\\`\\(putty\\)" tty)
             t)
            (else
             nil)))))
        
(defsubst 256-colour-p ()
  "T if we're running on a terminal or display system capable of
displaying 256 colours or more."
  (>= (display-color-cells) 256))

;;;}}}
;;;==================================================================

;;;==================================================================
;;;{{{ UI toolkit predicates:

(defsubst motif-p ()
  "T if this Emacs was built using the Motif UI toolkit."
  (featurep 'motif))

(defsubst xt-p ()
  "T if this Emacs was built using the X Toolkit."
  (featurep 'x-toolkit))

;;;}}}
;;;==================================================================

;;;==================================================================
;;;{{{ Host predicates:

;;;------------------------------------------------------------------
;;;{{{ Generation macro:

(defsubst %running-on (host)
  "Returns T if the string given in HOST matches the string contained
in `system-name'."
  (eq (string-match (concat "^" host) (system-name)) 0))

;;; TODO: Does this work with Emacs 18?
(defmacro define-host (host)
  "Defines a local host.  HOST is the name of the host to define, for
which we create a predicate named RUNNING-ON-<host>-P."
  (declare (indent 0))
  (let* ((host-str (symbol-name host)) 
         (pred (intern (concat "running-on-" host-str "-p")))
         (doc-str (concat "T if Emacs is running on `" host-str "'.")))
    `(defsubst ,pred ()
       ,doc-str 
       (%running-on ,host-str))))

;;;}}}
;;;------------------------------------------------------------------

;;;------------------------------------------------------------------
;;;{{{ Hosts:

;;; Some hostnames.
(define-host enterprise)                ; GNU/Linux.
(define-host exeter)                    ; GNU/Linux.
(define-host hubble)                    ; FreeBSD.
(define-host yorktown)                  ; Windows.
(define-host challenger)                ; GNU/Linux.
(define-host voyager)                   ; GNU/Linux.
(define-host mbr15_pward)               ; Work laptop, Mac OS X.
(define-host darkstar)                  ; GNU/Linux.

;;;}}}
;;;------------------------------------------------------------------

;;;}}}
;;;==================================================================

(provide 'bootstrap-predicates)

