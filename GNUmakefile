## GNUmakefile --- OpenFOAM files and directories.

# Copyright (C) 2021 Ralph Schleicher

# This program is free software: you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation, either version 3 of
# the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General
# Public License along with this program.  If not,
# see <https://www.gnu.org/licenses/>.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

## Code:

PACKAGE := openfoam
VERSION := $(shell grep -h ';;;* *Package-Version:' openfoam.el | sed 's/.*: *//')
TARNAME := $(PACKAGE)-$(VERSION)

EMACS = emacs

### Rules

.PHONY: all
all: autoloads

.PHONY: autoloads
autoloads: openfoam-autoloads.el
openfoam-autoloads.el: generate-autoloads.el openfoam.el
	$(EMACS) --batch --load generate-autoloads.el

.PHONY: check
check: all
	$(EMACS) --batch --funcall batch-byte-compile openfoam.el

.PHONY: check-melpa
check-melpa: all
	( set -e ; \
	  here=`/bin/pwd` ; \
	  cd ../melpa ; \
	  git checkout -f openfoam-recipe ; \
	  git clean -f ; \
	  cp -f $$here/openfoam.recipe recipes/openfoam ; \
	  make clean ; \
	  make recipes/openfoam STABLE=t ; \
	  make recipes/openfoam )

.PHONY: clean
clean:
	rm -f *.elc

### Maintenance

.PHONY: tag
tag: all
	@if test 0 != `svn status -q | grep -v "^ " | wc -l` ; then \
	    echo "Working copy is not clean" >&2 ; \
	    exit 1 ; \
	fi
	@if svn info "^/tags/$(TARNAME)" > /dev/null 2>&1 ; then \
	    echo "Tag already exists" >&2 ; \
	    exit 1 ; \
	fi
	svn copy "^/trunk" "^/tags/$(TARNAME)" -m "Version $(VERSION)."

.PHONY: sync
sync: all
	~/sync/github/push.sh emacs-openfoam

## GNUmakefile ends here
