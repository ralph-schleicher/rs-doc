## GNUmakefile --- build rules for RS-DOC

# Copyright (C) 2019 Ralph Schleicher

# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions
# are met:
#
#    * Redistributions of source code must retain the above copyright
#      notice, this list of conditions and the following disclaimer.
#
#    * Redistributions in binary form must reproduce the above copyright
#      notice, this list of conditions and the following disclaimer in
#      the documentation and/or other materials provided with the
#      distribution.
#
#    * Neither the name of the copyright holder nor the names of its
#      contributors may be used to endorse or promote products derived
#      from this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
# FOR A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE
# COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
# INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
# BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
# LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
# LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
# ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.

## Code:

PACKAGE := rs-doc
VERSION := $(shell cat VERSION)
TARNAME := $(PACKAGE)-$(VERSION)

.PHONY: all
all: rs-doc.html.in

# There are two options.  Either
#
#      <link rel="stylesheet" type="text/css" href="rs-doc.css.min" />
# or
#      <style type="text/css">
#       ...
#      </style>
#
# When embedding the CSS style sheet, indent the code properly.
#
# Also remove all empty lines in the HTML template.
rs-doc.html.in: rs-doc.html.in.in rs-doc.css.min
	{ \
	  echo '<style type="text/css">' ; \
	  cat rs-doc.css.min | sed -e 's/^/ /' ; \
	  echo '</style>' ; \
	} | \
	sed -e 's/^/  /' > temp.css
	sed -e '/<!-- TMPL_VAR CSS -->/ {' \
	    -e 'r temp.css' \
	    -e 'd' \
	    -e '}' \
	    -e '/^$$/d' \
	rs-doc.html.in.in > $@~
	rm -f temp.css
	mv -f $@~ $@

# Add a final newline character to the compressed style sheet.
# The sed(1) script joins lines of the form
#
#      @media ...{...{...}
#      }
rs-doc.css.min: rs-doc.css.in
	lessc --clean-css='--s1 -b' $< $@~
	echo >> $@~
	sed -i -e ':x /}$$/ { N; s/}\n}/}}/; bx }' $@~
	mv -f $@~ $@

# Ditto without compression.
rs-doc.css: rs-doc.css.in
	lessc $< $@

.PHONY: clean
clean:
	rm -f rs-doc.html.in rs-doc.css.min rs-doc.css temp.css

.PHONY: doc
doc:
	sbcl --non-interactive --load generate-doc.lisp

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
	~/src/github/github.sh rs-doc

## GNUmakefile ends here
