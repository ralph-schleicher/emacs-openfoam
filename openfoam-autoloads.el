;;; openfoam-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "openfoam" "openfoam.el" (24726 57475 129454
;;;;;;  735000))
;;; Generated autoloads from openfoam.el

(autoload 'openfoam-c++-mode "openfoam" "\
Major mode for editing OpenFOAM C++ code.

\(fn)" t nil)

(autoload 'openfoam-minor-mode "openfoam" "\
OpenFOAM minor mode.
If enabled, display the OpenFOAM menu in the menu bar.

If called interactively, enable Openfoam minor mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(autoload 'openfoam-turn-on-minor-mode "openfoam" "\
Turn on OpenFOAM minor mode if applicable for the current buffer." nil nil)

(put 'openfoam-global-minor-mode 'globalized-minor-mode t)

(defvar openfoam-global-minor-mode nil "\
Non-nil if Openfoam-Global minor mode is enabled.
See the `openfoam-global-minor-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `openfoam-global-minor-mode'.")

(custom-autoload 'openfoam-global-minor-mode "openfoam" nil)

(autoload 'openfoam-global-minor-mode "openfoam" "\
Toggle Openfoam minor mode in all buffers.
With prefix ARG, enable Openfoam-Global minor mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Openfoam minor mode is enabled in all buffers where
`openfoam-turn-on-minor-mode' would do it.
See `openfoam-minor-mode' for more information on Openfoam minor mode.

\(fn &optional ARG)" t nil)

(autoload 'openfoam-mode "openfoam" "\
Major mode for OpenFOAM data files.

\(fn)" t nil)

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

(autoload 'openfoam-create-app-directory "openfoam" "\
Create an OpenFOAM application directory.

Argument DIRECTORY is the directory file name.

\(fn DIRECTORY)" t nil)

(autoload 'openfoam-create-case-directory "openfoam" "\
Create an OpenFOAM case directory.

Argument DIRECTORY is the directory file name.

\(fn DIRECTORY)" t nil)

(autoload 'openfoam-shell "openfoam" "\
Run a shell in WORKING-DIRECTORY and initialize it for PROJECT-DIRECTORY.
With prefix argument, always ask the user to confirm the working directory
and project directory.

If the user option ‘openfoam-shell-save-project-directory’ is non-nil,
save the selected project directory inside the working directory so that
future invocations of ‘openfoam-shell’ can pick up the same project
directory again.

The inferior shell is invoked via the ‘shell’ command with the initial
working directory set to WORKING-DIRECTORY.  After normal shell startup,
the OpenFOAM specific startup script ‘PROJECT-DIRECTORY/etc/bashrc’ or
‘PROJECT-DIRECTORY/etc/cshrc’ is read automatically.

The shell buffer has a name of the form ‘*PROJECT WORKING-DIRECTORY*’ so
that you can run a separate shell for each working directory.

The local keymap in OpenFOAM shell buffers is ‘openfoam-shell-map’ which
uses ‘shell-map’ as its parent keymap.  The key bindings are listed below.

Finally, run ‘openfoam-shell-hook’.  If you build OpenFOAM applications
or libraries, e.g. by running ‘wmake’, a good candidate for this hook is
the ‘compilation-shell-minor-mode’ command.

\\{openfoam-shell-map}

\(fn WORKING-DIRECTORY PROJECT-DIRECTORY)" t nil)

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
