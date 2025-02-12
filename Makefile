########################################################################
## Makefile
##
## pwl04 - python with lisp, version 04-trampolined-fancy
##       https://github.com/minmus-9/pwl04
## Copyright (C) 2025  Mark Hays (github:minmus-9)
## 
## This program is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
## 
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
## 
## You should have received a copy of the GNU General Public License
## along with this program.  If not, see <https://www.gnu.org/licenses/>.

TFLAG:=-p

PYTHON:=python3

F:=lisp.py
LISP:=$(PYTHON) $(F)

all:	sicp bench factorial

run:
	$(LISP) -

bench:
	time $(TFLAG) $(LISP) examples/bench.lisp

factorial:
	$(LISP) examples/factorial.lisp

sicp:
	$(LISP) examples/sicp.lisp

clean:
	rm -f profile
	find . -type d -name __pycache__ -print0 | xargs -0 -n 25 rm -rf || true

## EOF
