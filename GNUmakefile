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
VERSION = 0.2

### Rules

.PHONY: all
all: openfoam-pkg.el autoloads

openfoam-pkg.el: openfoam-pkg.el.in
	sed -e 's/@PACKAGE@/$(PACKAGE)/g' \
	    -e 's/@VERSION@/$(VERSION)/g' $< > $@~ && mv -f $@~ $@

.PHONY: clean
clean:
	rm -f openfoam-pkg.el
	rm -f *.elc

.PHONY: check
check: all
	emacs --batch --funcall batch-byte-compile openfoam.el

### Maintenance

dist_FILES = openfoam.el openfoam-pkg.el init.el

.PHONY: dist
dist: $(PACKAGE)-$(VERSION).tar
$(PACKAGE)-$(VERSION).tar: check $(dist_FILES)
	rm -fr $(PACKAGE)-$(VERSION)
	mkdir $(PACKAGE)-$(VERSION)
	install -c -m 644 $(dist_FILES) $(PACKAGE)-$(VERSION)
	rm -f $(PACKAGE)-$(VERSION).tar
	tar -cf $(PACKAGE)-$(VERSION).tar $(PACKAGE)-$(VERSION)

.PHONY: autoloads
autoloads: openfoam-autoloads.el
openfoam-autoloads.el: generate-autoloads.el openfoam.el
	emacs --batch --load generate-autoloads.el

.PHONY: sync
sync: all
	~/src/github/github.sh emacs-openfoam

## GNUmakefile ends here
