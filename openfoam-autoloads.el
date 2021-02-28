;;; openfoam-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "openfoam" "openfoam.el" (24636 5404 389220
;;;;;;  265000))
;;; Generated autoloads from openfoam.el

(autoload 'openfoam-apply-data-file-template "openfoam" "\
Apply the OpenFOAM data file template to the current buffer.
See ‘openfoam-data-file-template’ for more information." t nil)

(autoload 'openfoam-insert-data-file-header "openfoam" "\
Insert an OpenFOAM data file header into the current buffer." t nil)

(autoload 'openfoam-create-case-directory "openfoam" "\
Create an OpenFOAM case directory.

\(fn DIRECTORY)" t nil)

(autoload 'openfoam-mode "openfoam" "\
Major mode for OpenFOAM data files.

\(fn)" t nil)

(defalias '∇-mode 'openfoam-mode)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "openfoam" '("openfoam-")))

;;;***

(provide 'openfoam-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; openfoam-autoloads.el ends here
