#!/usr/bin/env python3
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

"""
scan1.py -- trampolined scanner
"""

## pylint: disable=invalid-name
## XXX pylint: disable=missing-docstring


## {{{ atoms


EL = object()
T = True


class Symbol:
    ## pylint: disable=too-few-public-methods

    __slots__ = ("s",)

    def __init__(self, s):
        self.s = s

    def __str__(self):
        return self.s

    __repr__ = __str__


def is_atom(x):
    return isinstance(x, Symbol) or x is EL or x is T


def eq(x, y):
    return x is y and is_atom(x)


def symcheck(x):
    if isinstance(x, Symbol):
        return x
    raise TypeError(f"expected symbol, got {x!r}")


## }}}
## {{{ scanner #1: trampolined, no lut


class Scanner1:
    T_SYM = "symbol"
    T_INT = "int"
    T_FLOAT = "float"
    T_LPAR = "("
    T_RPAR = ")"
    T_TICK = "'"
    T_BACKTICK = "`"
    T_COMMA = ","
    T_COMMA_AT = ",@"
    T_STRING = "string"
    T_EOF = "eof"

    def __init__(self, callback):
        self.pos = 0
        self.token = []
        self.add = self.token.append
        self.parens = []
        self.cont = self.k_sym
        self.callback = callback

    def feed(self, text):
        if text is None:
            if self.cont not in (self.k_sym, self.k_comment):
                raise SyntaxError("eof in {self.cont!r}")
            if self.parens:
                raise SyntaxError(f"eof expecting {self.parens.pop()!r}")
            self.push(self.T_SYM)
            self.push(self.T_EOF)
            return
        self.pos, n = 0, len(text)
        cont = self.cont
        while self.pos < n:
            p = self.pos
            ch = text[p]
            self.pos = p + 1
            cont = cont(ch) or cont
        self.cont = cont

    def push(self, ttype):
        if self.token:
            t = "".join(self.token)
            self.token.clear()
        elif ttype == self.T_SYM:
            return
        elif ttype == self.T_SYM and t[0] in "0123456789-.+":
            try:
                t = int(t, 0)
                ttype = self.T_INT
            except ValueError:
                try:
                    t = float(t)
                    ttype = self.T_FLOAT
                except:  ## pylint: disable=bare-except
                    pass
        else:
            t = None
        self.callback(ttype, t)

    def k_sym(self, ch):
        ## pylint: disable=too-many-return-statements
        if ch == "(":
            return self.c_lpar(ch)
        if ch == ")":
            return self.c_right(ch)
        if ch in " \n\r\t":
            return self.c_ws(ch)
        if ch == "[":
            return self.c_lbrack(ch)
        if ch == "]":
            return self.c_right(ch)
        if ch == ";":
            return self.c_semi(ch)
        if ch == "'":
            return self.c_tick(ch)
        if ch == ",":
            return self.c_comma(ch)
        if ch == "`":
            return self.c_backtick(ch)
        self.add(ch)
        return self.k_sym

    def c_token(self, ch):
        self.add(ch)
        return self.k_sym

    def k_comment(self, ch):
        return self.k_sym if ch in "\n\r" else self.k_comment

    def k_quote(self, ch):
        if ch == "\\":
            return self.k_backslash
        if ch == '"':
            self.push(self.T_STRING)
            return self.k_sym
        self.add(ch)
        return self.k_quote

    ESC = {
        "\\": "\\",
        "n": "\n",
        "r": "\r",
        "t": "\t",
        '"': '"',
    }

    def k_backslash(self, ch):
        c = self.ESC.get(ch)
        if c is None:
            raise SyntaxError(f"bad escape {ch!r}")
        self.add(c)
        return self.k_quote

    def k_comma(self, ch):
        if ch == "@":
            self.add("@")
            self.push(self.T_COMMA_AT)
        else:
            self.pos -= 1
            self.push(self.T_COMMA)
        return self.k_sym

    def c_semi(self, _):
        self.push(self.T_SYM)
        return self.k_comment

    def c_comma(self, ch):
        if self.token:
            raise SyntaxError(f"{ch!r} not a delimiter")
        self.add(",")
        return self.k_comma

    def c_tick(self, ch):
        if self.token:
            raise SyntaxError(f"{ch!r} not a delimiter")
        self.add(ch)
        self.push(self.T_TICK)
        return self.k_sym

    def c_backtick(self, ch):
        if self.token:
            raise SyntaxError(f"{ch!r} not a delimiter")
        self.add(ch)
        self.push(self.T_BACKTICK)
        return self.k_sym

    def c_lpar(self, _):
        self.parens.append(")")
        self.push(self.T_SYM)
        self.push(self.T_LPAR)
        return self.k_sym

    def c_right(self, ch):
        if not self.parens:
            raise SyntaxError(f"too many {ch!r}")
        exp = self.parens.pop()
        if exp != ch:
            raise SyntaxError(f"{ch!r} while expecting {exp!r}")
        self.push(self.T_SYM)
        self.push(self.T_RPAR)
        return self.k_sym

    def c_lbrack(self, _):
        self.parens.append("]")
        self.push(self.T_SYM)
        self.push(self.T_LPAR)
        return self.k_sym

    def c_ws(self, _):
        self.push(self.T_SYM)
        return self.k_sym


## }}}
## {{{ scanner #2: trampolined with lut


class Scanner2:
    T_SYM = "symbol"
    T_INT = "int"
    T_FLOAT = "float"
    T_LPAR = "("
    T_RPAR = ")"
    T_TICK = "'"
    T_BACKTICK = "`"
    T_COMMA = ","
    T_COMMA_AT = ",@"
    T_STRING = "string"
    T_EOF = "eof"

    def __init__(self, callback):
        self.pos = 0
        self.token = []
        self.add = self.token.append
        self.parens = []
        self.cont = self.k_sym
        self.callback = callback
        self.lut = {
            "(": self.c_lpar,
            ")": self.c_right,
            "[": self.c_lbrack,
            "]": self.c_right,
            " ": self.c_ws,
            "\n": self.c_ws,
            "\r": self.c_ws,
            "\t": self.c_ws,
            ";": self.c_semi,
            "'": self.c_tick,
            ",": self.c_comma,
            "`": self.c_backtick,
        }.setdefault

    def feed(self, text):
        if text is None:
            if self.cont not in (self.k_sym, self.k_comment):
                raise SyntaxError("eof in {self.cont!r}")
            if self.parens:
                raise SyntaxError(f"eof expecting {self.parens.pop()!r}")
            self.push(self.T_SYM)
            self.push(self.T_EOF)
        else:
            self.pos, n = 0, len(text)
            cont = self.cont
            while self.pos < n:
                p = self.pos
                ch = text[p]
                self.pos = p + 1
                cont = cont(ch) or cont
            self.cont = cont

    def push(self, ttype):
        if self.token:
            t = "".join(self.token)
            self.token.clear()
        elif ttype == self.T_SYM:
            return
        elif ttype == self.T_SYM and t[0] in "0123456789-.+":
            try:
                t = int(t, 0)
                ttype = self.T_INT
            except ValueError:
                try:
                    t = float(t)
                    ttype = self.T_FLOAT
                except:  ## pylint: disable=bare-except
                    pass
        else:
            t = None
        self.callback(ttype, t)

    def k_sym(self, ch):
        return self.lut(ch, self.c_token)(ch)

    def c_token(self, ch):
        self.add(ch)
        return self.k_sym

    def k_comment(self, ch):
        return self.k_sym if ch in "\n\r" else self.k_comment

    def k_quote(self, ch):
        if ch == "\\":
            return self.k_backslash
        if ch == '"':
            self.push(self.T_STRING)
            return self.k_sym
        self.add(ch)
        return self.k_quote

    ESC = {
        "\\": "\\",
        "n": "\n",
        "r": "\r",
        "t": "\t",
        '"': '"',
    }

    def k_backslash(self, ch):
        c = self.ESC.get(ch)
        if c is None:
            raise SyntaxError(f"bad escape {ch!r}")
        self.add(c)
        return self.k_quote

    def k_comma(self, ch):
        if ch == "@":
            self.add("@")
            self.push(self.T_COMMA_AT)
        else:
            self.pos -= 1
            self.push(self.T_COMMA)
        return self.k_sym

    def c_semi(self, _):
        self.push(self.T_SYM)
        return self.k_comment

    def c_comma(self, ch):
        if self.token:
            raise SyntaxError(f"{ch!r} not a delimiter")
        self.add(",")
        return self.k_comma

    def c_tick(self, ch):
        if self.token:
            raise SyntaxError(f"{ch!r} not a delimiter")
        self.add(ch)
        self.push(self.T_TICK)
        return self.k_sym

    def c_backtick(self, ch):
        if self.token:
            raise SyntaxError(f"{ch!r} not a delimiter")
        self.add(ch)
        self.push(self.T_BACKTICK)
        return self.k_sym

    def c_lpar(self, _):
        self.parens.append(")")
        self.push(self.T_SYM)
        self.push(self.T_LPAR)
        return self.k_sym

    def c_right(self, ch):
        if not self.parens:
            raise SyntaxError(f"too many {ch!r}")
        exp = self.parens.pop()
        if exp != ch:
            raise SyntaxError(f"{ch!r} while expecting {exp!r}")
        self.push(self.T_SYM)
        self.push(self.T_RPAR)
        return self.k_sym

    def c_lbrack(self, _):
        self.parens.append("]")
        self.push(self.T_SYM)
        self.push(self.T_LPAR)
        return self.k_sym

    def c_ws(self, _):
        self.push(self.T_SYM)
        return self.k_sym


## }}}
## {{{ scanner #3: state-based, no lut


class Scanner3:
    T_SYM = "symbol"
    T_INT = "int"
    T_FLOAT = "float"
    T_LPAR = "("
    T_RPAR = ")"
    T_TICK = "'"
    T_BACKTICK = "`"
    T_COMMA = ","
    T_COMMA_AT = ",@"
    T_STRING = "string"
    T_EOF = "eof"

    S_SYM = "sym"
    S_COMMENT = ";"
    S_STRING = '"'
    S_ESC = "\\"
    S_COMMA = ","

    def __init__(self, callback):
        self.pos = 0
        self.token = []
        self.add = self.token.append
        self.parens = []
        self.callback = callback
        self.stab = {
            self.S_SYM: self.do_sym,
            self.S_COMMENT: self.do_comment,
            self.S_STRING: self.do_string,
            self.S_ESC: self.do_esc,
            self.S_COMMA: self.do_comma,
        }
        self.state = self.S_SYM

    def feed(self, text):
        if text is None:
            if self.state not in (self.S_SYM, self.S_COMMENT):
                raise SyntaxError("eof in {self.state!r}")
            if self.parens:
                raise SyntaxError(f"eof expecting {self.parens.pop()!r}")
            self.push(self.T_SYM)
            self.push(self.T_EOF)
            return
        self.pos, n = 0, len(text)
        while self.pos < n:
            p = self.pos
            ch = text[p]
            self.pos = p + 1
            self.stab[self.state](ch)

    def push(self, ttype):
        if self.token:
            t = "".join(self.token)
            self.token.clear()
        elif ttype == self.T_SYM:
            return
        elif ttype == self.T_SYM and t[0] in "0123456789-.+":
            try:
                t = int(t, 0)
                ttype = self.T_INT
            except ValueError:
                try:
                    t = float(t)
                    ttype = self.T_FLOAT
                except:  ## pylint: disable=bare-except
                    pass
        else:
            t = None
        self.callback(ttype, t)

    def do_sym(self, ch):
        if ch == "(":
            self.c_lpar(ch)
        elif ch == ")":
            self.c_right(ch)
        elif ch in " \n\r\t":
            self.c_ws(ch)
        elif ch == "[":
            self.c_lbrack(ch)
        elif ch == "]":
            self.c_right(ch)
        elif ch == ";":
            self.c_semi(ch)
        elif ch == "'":
            self.c_tick(ch)
        elif ch == ",":
            self.c_comma(ch)
        elif ch == "`":
            self.c_backtick(ch)
        else:
            self.add(ch)

    def do_comment(self, ch):
        if ch in "\n\r":
            self.state = self.S_SYM

    def do_string(self, ch):
        if ch == "\\":
            self.state = self.S_ESC
        elif ch == '"':
            self.push(self.T_STRING)
            self.state = self.S_SYM
        else:
            self.add(ch)

    ESC = {
        "\\": "\\",
        "n": "\n",
        "r": "\r",
        "t": "\t",
        '"': '"',
    }

    def do_esc(self, ch):
        c = self.ESC.get(ch)
        if c is None:
            raise SyntaxError(f"bad escape {ch!r}")
        self.add(c)
        self.state = self.S_STRING

    def do_comma(self, ch):
        if ch == "@":
            self.add("@")
            self.push(self.T_COMMA_AT)
        else:
            self.pos -= 1
            self.push(self.T_COMMA)
        self.state = self.S_SYM

    def c_semi(self, _):
        self.push(self.T_SYM)
        self.state = self.S_COMMENT

    def c_comma(self, ch):
        if self.token:
            raise SyntaxError(f"{ch!r} not a delimiter")
        self.add(",")

    def c_tick(self, ch):
        if self.token:
            raise SyntaxError(f"{ch!r} not a delimiter")
        self.add(ch)
        self.push(self.T_TICK)

    def c_backtick(self, ch):
        if self.token:
            raise SyntaxError(f"{ch!r} not a delimiter")
        self.add(ch)
        self.push(self.T_BACKTICK)

    def c_lpar(self, _):
        self.parens.append(")")
        self.push(self.T_SYM)
        self.push(self.T_LPAR)

    def c_right(self, ch):
        if not self.parens:
            raise SyntaxError(f"too many {ch!r}")
        exp = self.parens.pop()
        if exp != ch:
            raise SyntaxError(f"{ch!r} while expecting {exp!r}")
        self.push(self.T_SYM)
        self.push(self.T_RPAR)

    def c_lbrack(self, _):
        self.parens.append("]")
        self.push(self.T_SYM)
        self.push(self.T_LPAR)

    def c_ws(self, _):
        self.push(self.T_SYM)


## }}}
## {{{ scanner #4: state-based, iterator based with putback buffer


class Scanner4:
    ## pylint: disable=too-many-instance-attributes

    T_SYM = "symbol"
    T_INT = "int"
    T_FLOAT = "float"
    T_LPAR = "("
    T_RPAR = ")"
    T_TICK = "'"
    T_BACKTICK = "`"
    T_COMMA = ","
    T_COMMA_AT = ",@"
    T_STRING = "string"
    T_EOF = "eof"

    S_SYM = "sym"
    S_COMMENT = ";"
    S_STRING = '"'
    S_ESC = "\\"
    S_COMMA = ","

    def __init__(self, callback):
        self.pos = 0
        self.token = []
        self.add = self.token.append
        self.parens = []
        self.callback = callback
        self.stab = {
            self.S_SYM: self.do_sym,
            self.S_COMMENT: self.do_comment,
            self.S_STRING: self.do_string,
            self.S_ESC: self.do_esc,
            self.S_COMMA: self.do_comma,
        }
        self.state = self.S_SYM
        self.pb = None

    def feed(self, text):
        if text is None:
            if self.state not in (self.S_SYM, self.S_COMMENT):
                raise SyntaxError("eof in {self.state!r}")
            if self.parens:
                raise SyntaxError(f"eof expecting {self.parens.pop()!r}")
            self.push(self.T_SYM)
            self.push(self.T_EOF)
            return
        it = iter(text)
        try:
            while True:
                ch = self.pb
                if ch:
                    self.pb = None
                else:
                    ch = next(it)
                self.stab[self.state](ch)
        except StopIteration:
            pass

    def push(self, ttype):
        if self.token:
            t = "".join(self.token)
            self.token.clear()
        elif ttype == self.T_SYM:
            return
        elif ttype == self.T_SYM and t[0] in "0123456789-.+":
            try:
                t = int(t, 0)
                ttype = self.T_INT
            except ValueError:
                try:
                    t = float(t)
                    ttype = self.T_FLOAT
                except:  ## pylint: disable=bare-except
                    pass
        else:
            t = None
        self.callback(ttype, t)

    def do_sym(self, ch):
        if ch == "(":
            self.c_lpar(ch)
        elif ch == ")":
            self.c_right(ch)
        elif ch in " \n\r\t":
            self.c_ws(ch)
        elif ch == "[":
            self.c_lbrack(ch)
        elif ch == "]":
            self.c_right(ch)
        elif ch == ";":
            self.c_semi(ch)
        elif ch == "'":
            self.c_tick(ch)
        elif ch == ",":
            self.c_comma(ch)
        elif ch == "`":
            self.c_backtick(ch)
        else:
            self.add(ch)

    def do_comment(self, ch):
        if ch in "\n\r":
            self.state = self.S_SYM

    def do_string(self, ch):
        if ch == "\\":
            self.state = self.S_ESC
        elif ch == '"':
            self.push(self.T_STRING)
            self.state = self.S_SYM
        else:
            self.add(ch)

    ESC = {
        "\\": "\\",
        "n": "\n",
        "r": "\r",
        "t": "\t",
        '"': '"',
    }

    def do_esc(self, ch):
        c = self.ESC.get(ch)
        if c is None:
            raise SyntaxError(f"bad escape {ch!r}")
        self.add(c)
        self.state = self.S_STRING

    def do_comma(self, ch):
        if ch == "@":
            self.add("@")
            self.push(self.T_COMMA_AT)
        else:
            self.pb = ch
            self.push(self.T_COMMA)
        self.state = self.S_SYM

    def c_semi(self, _):
        self.push(self.T_SYM)
        self.state = self.S_COMMENT

    def c_comma(self, ch):
        if self.token:
            raise SyntaxError(f"{ch!r} not a delimiter")
        self.add(",")

    def c_tick(self, ch):
        if self.token:
            raise SyntaxError(f"{ch!r} not a delimiter")
        self.add(ch)
        self.push(self.T_TICK)

    def c_backtick(self, ch):
        if self.token:
            raise SyntaxError(f"{ch!r} not a delimiter")
        self.add(ch)
        self.push(self.T_BACKTICK)

    def c_lpar(self, _):
        self.parens.append(")")
        self.push(self.T_SYM)
        self.push(self.T_LPAR)

    def c_right(self, ch):
        if not self.parens:
            raise SyntaxError(f"too many {ch!r}")
        exp = self.parens.pop()
        if exp != ch:
            raise SyntaxError(f"{ch!r} while expecting {exp!r}")
        self.push(self.T_SYM)
        self.push(self.T_RPAR)

    def c_lbrack(self, _):
        self.parens.append("]")
        self.push(self.T_SYM)
        self.push(self.T_LPAR)

    def c_ws(self, _):
        self.push(self.T_SYM)


## }}}
## {{{ scanner #5: state-based, no lut


class Scanner5:
    T_SYM = "symbol"
    T_INT = "int"
    T_FLOAT = "float"
    T_STRING = "string"
    T_LPAR = "("
    T_RPAR = ")"
    T_TICK = "'"
    T_BACKTICK = "`"
    T_COMMA = ","
    T_COMMA_AT = ",@"
    T_EOF = "eof"

    S_SYM = "sym"
    S_COMMENT = ";"
    S_STRING = '"'
    S_ESC = "\\"
    S_COMMA = ","

    def __init__(self, callback):
        self.pos = 0
        self.token = []
        self.add = self.token.append
        self.parens = []
        self.callback = callback
        self.stab = {
            self.S_SYM: self.do_sym,
            self.S_COMMENT: self.do_comment,
            self.S_STRING: self.do_string,
            self.S_ESC: self.do_esc,
            self.S_COMMA: self.do_comma,
        }
        self.state = self.S_SYM

    def feed(self, text):
        if text is None:
            if self.state not in (self.S_SYM, self.S_COMMENT):
                raise SyntaxError("eof in {self.state!r}")
            if self.parens:
                raise SyntaxError(f"eof expecting {self.parens.pop()!r}")
            self.push(self.T_SYM)
            self.push(self.T_EOF)
            return
        self.pos, n = 0, len(text)
        while self.pos < n:
            p = self.pos
            ch = text[p]
            self.pos = p + 1
            self.stab[self.state](ch)

    def push(self, ttype):
        if self.token:
            t = "".join(self.token)
            self.token.clear()
        elif ttype == self.T_SYM:
            return
        elif ttype == self.T_SYM and t[0] in "0123456789-.+":
            try:
                t = int(t, 0)
                ttype = self.T_INT
            except ValueError:
                try:
                    t = float(t)
                    ttype = self.T_FLOAT
                except:  ## pylint: disable=bare-except
                    pass
        else:
            t = None
        self.callback(ttype, t)

    def do_sym(self, ch):
        if ch not in "()[] \n\r\t;',`":
            self.add(ch)
            return
        if ch in "([":
            self.parens.append(")" if ch == "(" else "]")
            self.push(self.T_SYM)
            self.push(self.T_LPAR)
        elif ch in ")]":
            if not self.parens:
                raise SyntaxError(f"too many {ch!r}")
            exp = self.parens.pop()
            if exp != ch:
                raise SyntaxError(f"{ch!r} while expecting {exp!r}")
            self.push(self.T_SYM)
            self.push(self.T_RPAR)
        elif ch in " \n\r\t":
            self.push(self.T_SYM)
        elif ch == ";":
            self.push(self.T_SYM)
            self.state = self.S_COMMENT
        else:
            if self.token:
                raise SyntaxError(f"{ch!r} not a delimiter")
            if ch == '"':
                self.state = self.S_STRING
                return
            self.add(ch)
            if ch == "'":
                self.push(self.T_TICK)
            elif ch == ",":
                self.state = self.S_COMMA
            else:
                self.push(self.T_BACKTICK)

    def do_comment(self, ch):
        if ch in "\n\r":
            self.state = self.S_SYM

    def do_string(self, ch):
        if ch == "\\":
            self.state = self.S_ESC
        elif ch == '"':
            self.push(self.T_STRING)
            self.state = self.S_SYM
        else:
            self.add(ch)

    ESC = {
        "\\": "\\",
        "n": "\n",
        "r": "\r",
        "t": "\t",
        '"': '"',
    }

    def do_esc(self, ch):
        c = self.ESC.get(ch)
        if c is None:
            raise SyntaxError(f"bad escape {ch!r}")
        self.add(c)
        self.state = self.S_STRING

    def do_comma(self, ch):
        if ch == "@":
            self.add("@")
            self.push(self.T_COMMA_AT)
        else:
            self.pos -= 1
            self.push(self.T_COMMA)
        self.state = self.S_SYM


## }}}


def main():
    import time  ## pylint: disable=import-outside-toplevel

    K = Scanner1  ## 0.64
    K = Scanner2  ## 0.63
    K = Scanner3  ## 0.64
    K = Scanner4  ## 0.58
    K = Scanner5  ## 0.49
    f = lambda *_: None
    n = 2_000
    s = r"""
    (define neg (lambda (x) (sub 0 x)))
    (define add (lambda (x y) (sub x (neg y))))
    (define bnot (lambda (x) (nand x x)))
    (define bor (lambda (x y) (nand (bnot x) (bnot y))))
    (define band (lambda (x y) (bnot (nand x y))))
    (define bxor (lambda (x y) (band (bor x y) (nand x y))))
    (define s "ab\ncd") ; comment
    (special if (lambda (p c a) (eval (if$ p c a) 1)))
    (define if$ (lambda (p c a) `(cond ((,p ,c) (#t ,a)))))
    (define ! (lambda (n) (if (lt? n 2) 1 (mul n (! (sub n 1))))))
    """
    b = n * len(s)
    t = time.time()
    S = K(f)
    for _ in range(n):
        S.feed(s)
    S.feed(None)
    dt = time.time() - t
    print(b, dt, 1e6 * dt / b, b / dt)


if __name__ == "__main__":
    main()


## EOF
