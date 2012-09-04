(when mac-os-x-p
  (let ((frameworks '("/Library/Frameworks/QtCore.framework/Headers/"
                      "/Library/Frameworks/QtGui.framework/Headers/"
                      "/Library/Frameworks/QtNetwork.framework/Headers/"
                      "/Library/Frameworks/QtSql.framework/Headers/"
                      "/Library/Frameworks/QtXml.framework/Headers/")))
    (loop for path in frameworks
          do (progn
               (semantic-add-system-include path 'c++-mode)
               (add-to-list 'auto-mode-alist (cons path 'c++-mode))))
    (add-to-list 'semantic-lex-c-preprocessor-symbol-file
                 "/Library/Frameworks/QtCore.framework/Headers/qconfig.h")
    (add-to-list 'semantic-lex-c-preprocessor-symbol-file
                 "/Library/Frameworks/QtCore.framework/Headers/qconfig-dist.h")
    (add-to-list 'semantic-lex-c-preprocessor-symbol-file
                 "/Library/Frameworks/QtCore.framework/Headers/qglobal.h")))

(ede-cpp-root-project "XHack"
  :name "XHack"
  :file "~/Documents/Projects/XHack/CMakeLists.txt"
  :include-path '("/include"
                  "/3rd-party")
  :spp-table '(("isUnix" . ""))
  :system-include-path '(("/Library/Frameworks/QtCore.framework/Headers/")
                         ("/Library/Frameworks/QtSql.framework/Headers/")))
