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

PACKAGE = openfoam
VERSION = $(shell cat version.expr)
TARNAME = $(PACKAGE)-$(VERSION)

### Rules

.PHONY: all
all: openfoam-pkg.el autoloads

openfoam-pkg.el: openfoam-pkg.el.in version.expr
	sed -e 's/@PACKAGE@/$(PACKAGE)/g' \
	    -e 's/@VERSION@/$(VERSION)/g' $< > $@~ && mv -f $@~ $@

.PHONY: autoloads
autoloads: openfoam-autoloads.el
openfoam-autoloads.el: generate-autoloads.el openfoam.el
	emacs --batch --load generate-autoloads.el

.PHONY: check
check: all
	emacs --batch --funcall batch-byte-compile openfoam.el

.PHONY: clean
clean:
	rm -f openfoam-pkg.el
	rm -f *.elc

### Maintenance

dist_FILES = openfoam.el openfoam-pkg.el init.el

.PHONY: dist
dist: $(TARNAME).tar
$(TARNAME).tar: check $(dist_FILES)
	rm -fr $(TARNAME)
	mkdir $(TARNAME)
	install -c -m 644 $(dist_FILES) $(TARNAME)
	rm -f $(TARNAME).tar
	tar -cf $(TARNAME).tar --owner=0 --group=0 $(TARNAME)

.PHONY: tag
tag: all
	test 0 = `svn status -q | grep -v "^ " | wc -l` || exit 1
	svn copy "^/trunk" "^/tags/$(TARNAME)" -m "Version $(VERSION)."

.PHONY: sync
sync: all
	~/src/github/github.sh emacs-openfoam

## GNUmakefile ends here
