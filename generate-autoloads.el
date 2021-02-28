(let ((generated-autoload-file
       (expand-file-name "openfoam-autoloads.el")))
  (update-file-autoloads (expand-file-name "openfoam.el"))
  (set-buffer (get-file-buffer generated-autoload-file))
  (save-buffer 0))
