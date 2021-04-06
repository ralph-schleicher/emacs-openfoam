;;; openfoam-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "openfoam" "openfoam.el" (24684 35448 1579
;;;;;;  417000))
;;; Generated autoloads from openfoam.el

(autoload 'openfoam-c++-mode "openfoam" "\
Major mode for editing OpenFOAM C++ code.

\(fn)" t nil)

(autoload 'openfoam-mode "openfoam" "\
Major mode for OpenFOAM data files.

\(fn)" t nil)

(defalias '∇-mode #'openfoam-mode)

(autoload 'openfoam-apply-data-file-template "openfoam" "\
Apply the OpenFOAM data file template to the current buffer.
See ‘openfoam-data-file-template’ for more information." t nil)

(autoload 'openfoam-insert-data-file-header "openfoam" "\
Insert an OpenFOAM data file header into the current buffer.

With prefix argument HERE, insert the data file header at the current
line.  Otherwise, run ‘openfoam-insert-data-file-header-position-hook’
to find a suitable buffer position.  If no hook function is configured,
search for the ‘Code:’ special comment and insert the data file header
after it.  If ‘Code:’ is not found, insert the data file header before
the first dictionary entry.

While looking for a suitable buffer position, the special variable
‘openfoam-insert-data-file-header-limit’ is bound to the buffer position
specified by ‘openfoam-insert-data-file-header-line-limit’.  Whether or
not a hook function obeys this limit is undefined.

\(fn &optional HERE)" t nil)

(autoload 'openfoam-insert-dimension-set "openfoam" "\
Insert a dimension set at point.
Leave point before the opening ‘[’." t nil)

(autoload 'openfoam-create-case-directory "openfoam" "\
Create an OpenFOAM case directory.

Argument DIRECTORY is the directory file name.

\(fn DIRECTORY)" t nil)

(autoload 'openfoam-shell "openfoam" "\
Run a shell in CASE-DIRECTORY and initialize it for PROJECT-DIRECTORY.
With prefix argument, always ask the user to confirm the case directory
and project directory.

If the user option ‘openfoam-shell-save-project-directory’ is non-nil,
save the selected project directory inside the case directory so that
future invocations of ‘openfoam-shell’ can pick up the same project
directory again.

The inferior shell is invoked via the ‘shell’ command with the initial
working directory set to CASE-DIRECTORY.  After normal shell startup,
the OpenFOAM specific startup script ‘PROJECT-DIRECTORY/etc/bashrc’ or
‘PROJECT-DIRECTORY/etc/cshrc’ is read automatically.

The shell buffer has a name of the form ‘*PROJECT CASE-DIRECTORY*’ so
that you can run a separate shell for each case directory.

\(fn CASE-DIRECTORY PROJECT-DIRECTORY)" t nil)

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
