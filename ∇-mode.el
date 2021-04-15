;;; ∇-mode.el --- OpenFOAM files and directories  -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Ralph Schleicher

;; Author: Ralph Schleicher <rs@ralph-schleicher.de>
;; Keywords: languages
;; Version: 1.0
;; Package-Requires: ((openfoam "0"))
;; URL: https://github.com/ralph-schleicher/emacs-openfoam

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

;; ┆ Open ∇             ┆ -*- mode: ∇; coding: utf-8; -*-
;; ┆      F ield        ┆
;; ┆      O peration    ┆
;; ┆      A nd          ┆
;; ┆      M anipulation ┆
;;
;; A bonus package for OpenFOAM data files.

;;; Code:

(require 'openfoam)

;;;###autoload
(defalias '∇-mode #'openfoam-mode)

;;; ∇-mode.el ends here
