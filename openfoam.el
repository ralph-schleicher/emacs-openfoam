;;; openfoam.el --- OpenFOAM files and directories.  -*- lexical-binding: t -*-

;; Copyright (C) 2021 Ralph Schleicher

;; Author: Ralph Schleicher <rs@ralph-schleicher.de>
;; Keywords: languages
;; Version: α

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General
;; Public License along with this program.  If not,
;; see <https://www.gnu.org/licenses/>.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; ┆ Open ∇             ┆
;; ┆      F ield        ┆
;; ┆      O peration    ┆
;; ┆      A nd          ┆
;; ┆      M anipulation ┆
;;
;; An Emacs major mode for editing OpenFOAM data files; and user
;; commands for OpenFOAM case directories.

;;; Code:

(require 'cl-lib)
(require 'cc-mode)
(require 'smie)

(defgroup openfoam nil
  "OpenFOAM files and directories."
  :group 'languages
  :link '(emacs-commentary-link "openfoam.el")
  :prefix "openfoam-")

(defun openfoam-string-quote (string)
  "Quote all meta-characters in a string."
  (with-temp-buffer
    (mapc (lambda (char)
	    (when (or (char-equal char ?\\)
		      (char-equal char ?\"))
	      (insert ?\\))
	    (insert char))
	  string)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun openfoam-clamp (number min max)
  "Limit NUMBER to the closed interval [MIN, MAX]."
  (cond ((<= number min)
	 min)
	((>= number max)
	 max)
	(t
	 number)))

(defsubst openfoam-skip-forward ()
  "Move forward across comments and whitespace characters.
Leave point where scanning stops."
  (forward-comment (point-max)))

(defsubst openfoam-skip-backward ()
  "Move backward across comments and whitespace characters.
Leave point where scanning stops."
  (forward-comment (- (point))))

;;;; Data Files

(defcustom openfoam-data-file-template "\
//  =========                 |  -*- OpenFOAM -*-
//  \\\\      /  F ield         |
//   \\\\    /   O peration     |
//    \\\\  /    A nd           |
//     \\\\/     M anipulation  |
//
// Copyright (C) %Y %(or (getenv \"ORGANIZATION\") user-full-name (user-full-name))
//
// Author: %n <%m>

/// Code:

%|

/// %f ends here
"
  "Template for an OpenFOAM data file.
The following substitutions are made:

     %u  user login name
     %n  user full name
     %m  user mail address
     %h  host name, i.e. ‘system-name’
     %d  domain name, i.e. ‘mail-host-address’
     %p  buffer file name
     %f  buffer file name without directory
     %b  buffer file name without directory and file extension
     %Y  current year, i.e. ‘%Y’ time format
     %D  current date, i.e. ‘%Y-%m-%d’ time format
     %T  current time, i.e. ‘%H:%M:%S’ time format
     %L  current date and time, i.e. ‘%Y%m%dT%H%M%S’ time format
     %Z  universal date and time, i.e. ‘%Y%m%dT%H%M%SZ’ time format
     %(  value of Emacs Lisp expression
     %|  existing file contents
     %%  litereal %

For date and time formats, a ‘*’ modifier after the ‘%’ means universal
date and time."
  :type '(choice (const :tag "None" nil)
		 (string :tag "Template"))
  :group 'openfoam)

(defcustom openfoam-apply-data-file-template-hook nil
  "Hook called by ‘openfoam-apply-data-file-template’."
  :type 'hook
  :group 'openfoam)

;;;###autoload
(defun openfoam-apply-data-file-template ()
  "Apply the OpenFOAM data file template to the current buffer.
See ‘openfoam-data-file-template’ for more information."
  (interactive)
  (barf-if-buffer-read-only)
  (when openfoam-data-file-template
    (let* ((now (current-time))
	   (path (buffer-file-name))
	   pos ;position of beginning of body
	   offs ;position of point in body
	   (body (save-restriction
		   (widen)
		   (let* ((start (save-excursion
				   (goto-char (point-min))
				   (skip-chars-forward " \t\n")
				   (point)))
			  (end (save-excursion
				 (goto-char (point-max))
				 (skip-chars-backward " \t\n" start)
				 (point))))
		     (setq offs (- (openfoam-clamp (point) start end) start))
		     (buffer-substring-no-properties start end))))
	   (templ (with-temp-buffer
		    (buffer-disable-undo)
		    ;; In case the user modifies some parameters, e.g.
		    ;; the mail address, depending on the major mode.
		    (openfoam-mode)
		    ;; Turn off syntax highlighting.
		    (when (fboundp 'font-lock-mode)
		      (font-lock-mode 0))
		    ;; Fill in template.
		    (erase-buffer)
		    (insert openfoam-data-file-template)
		    (let (len subst)
		      (goto-char (point-min))
		      (while (search-forward "%" nil t)
			(setq len 1) ;pattern length
			(setq subst (cl-case (char-after (point))
				      (?u (or user-login-name (user-login-name)))
				      (?n (or user-full-name (user-full-name)))
				      (?m user-mail-address)
				      (?h (system-name))
				      (?d mail-host-address)
				      (?p (or path ""))
				      (?f (if path (file-name-nondirectory path) ""))
				      (?b (if path (file-name-base path) ""))
				      (?Y (format-time-string "%Y" now))
				      (?D (format-time-string "%Y-%m-%d" now))
				      (?T (format-time-string "%H:%M:%S" now))
				      (?L (format-time-string "%Y%m%dT%H%M%S" now))
				      (?Z (format-time-string "%Y%m%dT%H%M%SZ" now t))
				      (?* (setq len 2)
					  (cl-case (char-after (1+ (point)))
					    (?Y (format-time-string "%Y" now t))
					    (?D (format-time-string "%Y-%m-%d" now t))
					    (?T (format-time-string "%H:%M:%S" now t))
					    (?L (format-time-string "%Y%m%dT%H%M%S" now t))
					    (?Z (format-time-string "%Y%m%dT%H%M%SZ" now t))
					    (t ;no match
					     (setq len 1)
					     nil)))
				      (?\( (let* ((start (point))
						  (object (read (current-buffer)))
						  (end (point))
						  (value (eval object)))
					     (goto-char start)
					     (setq len (- end start))
					     (when (stringp value)
					       value)))
				      (?| (when (null pos) ;first occurrence
					    (setq pos (1- (point))))
					  body)
				      (?% ?%)))
			(if (null subst)
			    ;; Skip pattern, but don't move beyond
			    ;; end of buffer.
			    (forward-char (min len (- (point-max) (point))))
			  ;; Replace pattern.
			  (delete-char -1)
			  (delete-char len)
			  (insert subst))))
		    ;; Reindent the whole buffer.
		    (indent-region (point-min) (point-max))
		    ;; Return filled in template.
		    (buffer-substring-no-properties (point-min) (point-max)))))
      ;; Replace buffer contents.
      (erase-buffer)
      (insert templ)
      ;; Restore point.
      (goto-char (if (null pos)
		     (point-min)
		   (+ pos offs))))
    ;; Turn on OpenFOAM mode.
    (openfoam-mode)
    ;; Provide a hook for further modifications.
    (run-hooks 'openfoam-apply-data-file-template-hook)))

(defcustom openfoam-insert-data-file-header-position-hook nil
  "Leave point where to insert the OpenFOAM data file header.
Hook called by ‘openfoam-insert-data-file-header’."
  :type 'hook
  :group 'openfoam)

(defcustom openfoam-insert-data-file-header-line-limit 100
  "Number of lines searched at the beginning of a file to find a position
for inserting the OpenFOAM data file header.  A negative value counts
from the end, zero means to search the whole file."
  :type 'integer
  :group 'openfoam)

(defvar openfoam-insert-data-file-header-limit)

;;;###autoload
(defun openfoam-insert-data-file-header (&optional here)
  "Insert an OpenFOAM data file header into the current buffer.

With prefix argument, insert the data file header at the current line.
Otherwise, run ‘openfoam-insert-data-file-header-position-hook’ to find
a suitable buffer position.  If no hook function is configured, search
for the ‘Code:’ special comment and insert the data file header after
it.  If ‘Code:’ is not found, insert the data file header before the
first dictionary entry.

While looking for a suitable buffer position, the special variable
‘openfoam-insert-data-file-header-limit’ is bound to the buffer position
specified by ‘openfoam-insert-data-file-header-line-limit’.  Whether or
not a hook function obeys this limit is undefined."
  (interactive "P")
  (barf-if-buffer-read-only)
  (unless (eq major-mode 'openfoam-mode)
    (openfoam-mode))
  (let* ((buffer-file-name (buffer-file-name))
	 (file-name (and buffer-file-name
			 (file-name-nondirectory buffer-file-name)))
	 (directory (and buffer-file-name
			 (file-name-directory buffer-file-name)))
	 (case-directory (and directory
			      (openfoam-case-directory directory)))
	 (location (and directory case-directory
			(directory-file-name
			 (file-relative-name directory case-directory)))))
    (let ((point (point-marker)))
      (if (not (null here))
	  (beginning-of-line)
	(goto-char (point-min))
	(let ((openfoam-insert-data-file-header-limit
	       (if (= openfoam-insert-data-file-header-line-limit 0)
		   (point-max)
		 (save-excursion
		   (when (< openfoam-insert-data-file-header-line-limit 0)
		     (goto-char (point-max)))
		   (forward-line openfoam-insert-data-file-header-line-limit)
		   (point)))))
	  (cond ((not (null openfoam-insert-data-file-header-position-hook))
		 (run-hooks 'openfoam-insert-data-file-header-position-hook))
		;; Search for the ‘Code:’ special comment.
		((let ((case-fold-search t))
		   (re-search-forward "^//+ *Code:$" openfoam-insert-data-file-header-limit t))
		 (unless (= (forward-line 1) 0)
		   (insert ?\n))
		 ;; Add an extra empty line.
		 (insert ?\n))
		;; Skip across initial comments, i.e. leave point at
		;; the beginning of the line after the last comment.
		((looking-at "/[/*]")
		 (openfoam-skip-forward)
		 (re-search-backward "[^[:blank:]\n]" nil t)
		 ;; The ‘forward-line’ function only returns non-zero
		 ;; if it can't move at all.
		 (end-of-line)
		 (unless (= (forward-line 1) 0)
		   (insert ?\n))
		 ;; Add an extra empty line.
		 (insert ?\n))
		;; Stay at beginning of file.
		(t))))
      (let ((start (point)))
	(insert "FoamFile\n"
		"{\n"
		"version 2.0;\n"
		"format ascii;\n"
		;; TODO: Attempt to infer the class from
		;; the file name or location.
		"class dictionary;\n"
		"object " (or file-name "unknown") ";\n"
		(if location
		    (concat "location \""
			    (openfoam-string-quote location)
			    "\";\n")
		  "")
		"}\n")
	(indent-region start (point)))
      ;; Restore point.
      (goto-char point)))
  ())

(defcustom openfoam-data-file-contents-alist ()
  "Alist of initial file contents for OpenFOAM data files.
List elements are cons cells of the form ‘(FILE-NAME . CONTENTS)’
where FILE-NAME is the relative file name in a case directory and
CONTENTS is the file contents."
  :type '(repeat (cons (string :tag "File name")
		       (string :tag "File contents")))
  :group 'openfoam)

(defun openfoam-add-to-data-file-contents-alist (file-name contents)
  "Add or update an element in ‘openfoam-data-file-contents-alist’."
  (let ((cell (assoc file-name openfoam-data-file-contents-alist 'openfoam-file-name-equal-p)))
    (if (not (null cell))
	(setcdr cell contents)
      (push (cons file-name contents) openfoam-data-file-contents-alist)))
  openfoam-data-file-contents-alist)

(put 'openfoam-add-to-data-file-contents-alist 'lisp-indent-function 1)

;;;; Case Directories

;;;###autoload
(defun openfoam-create-case-directory (directory)
  "Create an OpenFOAM case directory."
  (interactive "F")
  (let ((directory (file-name-as-directory directory)))
    (mkdir directory t)
    (cl-flet ((create-directory (name)
		(let ((dir (expand-file-name name directory)))
		  (unless (file-directory-p dir)
		    (mkdir dir))))
	      (create-data-file (name)
		(let ((file (expand-file-name name directory)))
		  (unless (file-exists-p file)
		    (with-temp-buffer
		      (set-visited-file-name file t)
		      (when-let ((contents (cdr (assoc name openfoam-data-file-contents-alist 'openfoam-file-name-equal-p))))
			(insert contents))
		      (goto-char (point-min))
		      (unless (re-search-forward "-\\*-" (save-excursion (end-of-line) (point)) t)
			(openfoam-apply-data-file-template))
		      (goto-char (point-min))
		      (openfoam-skip-forward)
		      (unless (looking-at "FoamFile\\>")
			(openfoam-insert-data-file-header))
		      (set-buffer-modified-p t)
		      (save-buffer 0))))))
      (create-directory "0")
      (create-directory "constant")
      (create-directory "constant/polyMesh")
      (create-directory "system")
      (create-data-file "system/controlDict")
      (create-data-file "system/fvSchemes")
      (create-data-file "system/fvSolution"))
    directory))

(defun openfoam-file-name-equal-p (file-name-1 file-name-2)
  "Return non-nil if FILE-NAME-1 and FILE-NAME-2 shall be considered equal."
  (if (memq system-type '(windows-nt ms-dos))
      (cl-equalp file-name-1 file-name-2)
    (string= file-name-1 file-name-2)))

(defun openfoam-case-directory (file-name-or-directory)
  "Return the OpenFOAM case directory of FILE-NAME-OR-DIRECTORY, or nil."
  (let ((directory (file-name-directory file-name-or-directory)))
    (while (and directory (not (and (file-directory-p
				     (expand-file-name "constant" directory))
				    (file-directory-p
				     (expand-file-name "system" directory)))))
      (let ((up (file-name-directory (directory-file-name directory))))
	(setq directory (if (openfoam-file-name-equal-p up directory) nil up))))
    directory))

;;;; Major Mode

(c-add-style "OpenFOAM"
	     `((c-basic-offset . 4)
	       (c-tab-always-indent . t)
	       (c-comment-only-line-offset . (0 . 0))
	       (c-indent-comments-syntactically-p . t)
	       (c-block-comments-indent-p nil)
	       (c-cleanup-list . (defun-close-semi list-close-comma scope-operator))
	       (c-backslash-column . 78)
	       ;; See ‘(c-set-stylevar-fallback 'c-offsets-alist ...)’
	       ;; in file ‘cc-vars.el’.
	       (c-offsets-alist
		(c . +)
		(topmost-intro . 0)
		(topmost-intro-cont . 0)
		(member-init-intro . +)
		(member-init-cont . 0)
		(inher-intro . 0)
		(inher-cont . +)
		(substatement . +)
		(substatement-open . 0)
		(case-label . +)
		(label . -)
		(comment-intro . 0)
		(arglist-intro . +)
		(arglist-cont . 0)
		(arglist-cont-nonempty . 0)
		(arglist-close . 0)
		(stream-op . +)
		(cpp-macro . c-lineup-cpp-define)
		)))

(defcustom openfoam-default-style "OpenFOAM"
  "Default indentation style for OpenFOAM data files.
Run the ‘c-set-style’ command to change the indentation style."
  :type 'string
  :group 'openfoam)

(defconst openfoam-cc-mode 'c++-mode
  "Major mode symbol of the underlying CC Mode.")

(defcustom openfoam-mode-hook nil
  "Hook called by ‘openfoam-mode’."
  :type 'hook
  :group 'openfoam)

(defvar openfoam-mode-syntax-table
  (let ((syntax-table (make-syntax-table)))
    ;; String constants.
    (modify-syntax-entry ?\" "\"" syntax-table)
    (modify-syntax-entry ?\\ "\\" syntax-table)
    ;; Comments.  The primary comment style is a C++ line comment and
    ;; the secondary comment style is a C block comment.
    (modify-syntax-entry ?/  ". 124" syntax-table)
    (modify-syntax-entry ?*  ". 23b" syntax-table)
    (modify-syntax-entry ?\n ">"     syntax-table)
    (modify-syntax-entry ?\r ">"     syntax-table)
    ;; Dissimilar pairs.
    (modify-syntax-entry ?\( "()" syntax-table) ;list
    (modify-syntax-entry ?\) ")(" syntax-table)
    (modify-syntax-entry ?\[ "(]" syntax-table) ;dimension set
    (modify-syntax-entry ?\] ")[" syntax-table)
    (modify-syntax-entry ?\{ "(}" syntax-table) ;dictionary
    (modify-syntax-entry ?\} "){" syntax-table)
    ;; All other characters except whitespace, ‘/’ and ‘;’ can be used
    ;; in words (symbols).  However, the OpenFoam convention is to not
    ;; use this feature.  Thus, mark most of them as punctuation.
    (modify-syntax-entry ?!  "." syntax-table)
    (modify-syntax-entry ?#  "'" syntax-table) ;directive
    (modify-syntax-entry ?$  "'" syntax-table) ;macro
    (modify-syntax-entry ?%  "." syntax-table)
    (modify-syntax-entry ?&  "." syntax-table)
    (modify-syntax-entry ?\' "." syntax-table)
    (modify-syntax-entry ?+  "." syntax-table)
    (modify-syntax-entry ?,  "." syntax-table)
    (modify-syntax-entry ?-  "." syntax-table)
    (modify-syntax-entry ?.  "." syntax-table)
    (modify-syntax-entry ?:  "." syntax-table)
    (modify-syntax-entry ?\; "." syntax-table)
    (modify-syntax-entry ?<  "." syntax-table)
    (modify-syntax-entry ?=  "." syntax-table)
    (modify-syntax-entry ?>  "." syntax-table)
    (modify-syntax-entry ??  "." syntax-table)
    (modify-syntax-entry ?@  "." syntax-table)
    (modify-syntax-entry ?^  "." syntax-table)
    (modify-syntax-entry ?_  "." syntax-table)
    (modify-syntax-entry ?`  "." syntax-table)
    (modify-syntax-entry ?|  "." syntax-table)
    (modify-syntax-entry ?~  "." syntax-table)
    syntax-table)
  "Syntax table used in OpenFOAM mode buffers.")

(defvar openfoam-mode-abbrev-table nil
  "Abbreviation table used in OpenFOAM mode buffers.")
(define-abbrev-table 'openfoam-mode-abbrev-table ())

(defvar openfoam-font-lock-keywords c-font-lock-keywords-1
  "Default expressions to highlight in OpenFOAM mode buffers.")

;;;###autoload
(define-derived-mode openfoam-mode prog-mode "OpenFOAM"
  "Major mode for OpenFOAM data files."
  :group 'openfoam
  ;; See ‘antlr-mode’.
  (c-initialize-cc-mode)
  (setq c-buffer-is-cc-mode openfoam-cc-mode)
  (c-init-language-vars-for openfoam-cc-mode)
  (c-basic-common-init openfoam-cc-mode openfoam-default-style)
  ;; C++ comment style.
  (setq-local comment-start "//"
	      comment-start-skip "\\(?://+\\|/\\*+\\)\\s *"
	      comment-end-skip nil
	      comment-end "")
  ;; Syntax highlighting.
  (setq font-lock-defaults '(openfoam-font-lock-keywords))
  ;; Miscellaneous.
  (setq indent-tabs-mode nil)
  ())

;;;###autoload
(defalias '∇-mode 'openfoam-mode)

(provide 'openfoam)

;;; openfoam.el ends here
