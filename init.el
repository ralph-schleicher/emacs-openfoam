;; init.d/openfoam.el --- customize OpenFOAM.

(require 'openfoam-autoloads)

(setq openfoam-data-file-template
      (let ((copyright-notice
	     (if (rs:at-home-p)
		 "// Copyright (C) %Y %n\n"
	       (concat
		"// Copyright (C) %Y %(getenv \"ORGANIZATION\")\n"
		"//\n"
		"// Author: %n <%m>\n"))))
	(concat
	 "// Open ∇             ┆ -*- mode: ∇; coding: utf-8; -*-\n"
	 "//      F ield        ┆\n"
	 "//      O peration    ┆\n"
	 "//      A nd          ┆\n"
	 "//      M anipulation ┆\n"
	 "//\n"
	 copyright-notice
	 "\n"
	 "FoamFile\n"
	 "{\n"
	 "    version 2.0;\n"
	 "    format ascii;\n"
	 "    class dictionary;\n"
	 "    object %f;\n"
	 "}\n"
	 "\n"
	 "/// Code:\n"
	 "\n"
	 "%|\n"
	 "\n"
	 "/// %f ends here\n")))

(add-hook 'openfoam-mode-hook 'rs:openfoam-setup)

(defun rs:openfoam-setup ()
  "Setup OpenFOAM buffers."
  (setq-local comment-auto-fill-only-comments t
	      comment-fill-column 76)
  (turn-on-auto-fill))

;; init.d/openfoam.el ends here
