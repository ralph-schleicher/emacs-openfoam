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

;;  =========                 |
;;  \\      /  F ield         |
;;   \\    /   O peration     |
;;    \\  /    A nd           |
;;     \\/     M anipulation  |
;;
;; A major mode for editing OpenFOAM data files and utility commands
;; for OpenFOAM case directories.

;;; Code:

(require 'cl-lib)
(require 'cc-mode)

(defgroup openfoam nil
  "OpenFOAM files and directories."
  :group 'languages
  :link '(emacs-commentary-link "openfoam.el")
  :prefix "openfoam-")

;;;; Case Directories

(defun openfoam-create-case-directory (directory)
  "Create an OpenFOAM case directory."
  (interactive "F")
  (let ((directory (file-name-as-directory directory)))
    (mkdir directory t)
    (let ((dir (expand-file-name "0" directory)))
      (unless (file-directory-p dir)
	(mkdir dir)))
    (let ((dir (expand-file-name "constant" directory)))
      (unless (file-directory-p dir)
	(mkdir dir)))
    (let ((dir (expand-file-name "constant/polymesh" directory)))
      (unless (file-directory-p dir)
	(mkdir dir)))
    (let ((dir (expand-file-name "system" directory)))
      (unless (file-directory-p dir)
	(mkdir dir)))
    (let ((file (expand-file-name "system/controlDict" directory)))
      (unless (file-exists-p file)
	(with-temp-buffer
	  (write-file file))))
    (let ((file (expand-file-name "system/fvSchemes" directory)))
      (unless (file-exists-p file)
	(with-temp-buffer
	  (write-file file))))
    (let ((file (expand-file-name "system/fvSolution" directory)))
      (unless (file-exists-p file)
	(with-temp-buffer
	  (write-file file))))
    directory))

(defun openfoam-file-name-equal-p (file-name-1 file-name-2)
  "Return non-nil if FILE-NAME-1 and FILE-NAME-2 shall be considered equal."
  (if (memq system-type '(windows-nt ms-dos))
      (cl-equalp file-name-1 file-name-2)
    (string= file-name-1 file-name-2)))

(defun openfoam-case-directory (file-name-or-directory)
  "Return the OpenFOAM case directory of FILE-NAME-OR-DIRECTORY, or nil."
  (let ((directory (file-name-directory file-name)))
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
		(cpp-macro . +)
		)))

(defcustom openfoam-default-style "OpenFOAM"
  "Default indentation style for OpenFOAM data files.
Run the ‘c-set-style’ command to change the indentation style.")

(defconst openfoam-cc-mode 'c++-mode
  "Major mode symbol of the underlying CC Mode.")

(defvar openfoam-mode-hook nil
  "Hook called by ‘openfoam-mode’.")

(defvar openfoam-mode-syntax-table
  (let ((syntax-table (make-syntax-table)))
    (c-populate-syntax-table syntax-table)
    syntax-table)
  "Syntax table used in OpenFOAM mode buffers.")

(defvar openfoam-mode-abbrev-table nil
  "Abbreviation table used in OpenFOAM mode buffers.")
(define-abbrev-table 'openfoam-mode-abbrev-table ())

(defvar openfoam-font-lock-keywords c-font-lock-keywords-1
  "Default expressions to highlight in OpenFOAM mode buffers.")

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

;;; openfoam.el ends here
