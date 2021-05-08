;; init.d/openfoam.el --- customize OpenFOAM.

(require 'openfoam-autoloads)

(defalias '∇-mode #'openfoam-mode)

(setq openfoam-project-directory-alist
      '(;; OpenFOAM Foundation releases.
	(8 . "/opt/OpenFOAM/OpenFOAM-8")
	(7 . "/opt/OpenFOAM/OpenFOAM-7")
	(6 . "/opt/OpenFOAM/OpenFOAM-6")
	;; ESI OpenCFD releases.
	(v1912 . "/opt/OpenFOAM/OpenFOAM-v1912"))
      ;; The default is OpenFOAM 8.
      openfoam-default-project-directory 8)

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
	 "/// Code:\n"
	 "\n"
	 "%|\n"
	 "\n"
	 "/// %f ends here\n")))

(add-hook 'openfoam-apply-data-file-template-hook 'openfoam-insert-data-file-header)

(with-eval-after-load 'openfoam
  (openfoam-add-to-file-alist "system/controlDict"
    :body "\
application laplacianFoam;

// Simulation time and time step.
startFrom latestTime;
startTime 0;
stopAt endTime;
endTime 1;
deltaT 0.05;
adjustTimeStep no;
runTimeModifiable yes;

// Data logging.
writeControl timeStep;
writeInterval 1;
writeFormat ascii;
writePrecision 6;
writeCompression off;
timeFormat general;
timePrecision 6;
purgeWrite 0;
")
  (openfoam-add-to-file-alist "system/fvSchemes"
    :body "\
ddtSchemes
{
    default steadyState;
}

gradSchemes
{
    default Gauss linear;
}

divSchemes
{
    default none;
}

laplacianSchemes
{
    default Gauss linear corrected;
}

interpolationSchemes
{
    default linear;
}

snGradSchemes
{
    default corrected;
}
")
  (openfoam-add-to-file-alist "system/fvSolution"
    :body "\
solvers
{
}
")
  (openfoam-add-to-file-alist "Make/files"
    :body "\
myFoam.C

EXE = $(FOAM_USER_APPBIN)/myFoam
")
  (openfoam-add-to-file-alist "Make/options"
    :body "\
EXE_INC = \\
    -I$(LIB_SRC)/finiteVolume/lnInclude

EXE_LIBS = \\
    -lfiniteVolume
"))

(add-hook 'openfoam-mode-hook 'rs:openfoam-setup)

(defun rs:openfoam-setup ()
  "Setup OpenFOAM buffers."
  (setq-local comment-auto-fill-only-comments t
	      comment-fill-column 76)
  (turn-on-auto-fill))

;; init.d/openfoam.el ends here
