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

"ctx.py"

## pylint: disable=invalid-name,too-many-lines,unbalanced-tuple-unpacking
## XXX pylint: disable=missing-docstring

import locale
import os
import sys
import traceback


## {{{ exports

__all__ = (
    "Context",
    "EL",
    "Environment",
    "Lambda",
    "LispError",
    "Parser",
    "Queue",
    "SENTINEL",
    "Scanner",
    "Symbol",
    "T",
    "car",
    "cdr",
    "cons",
    "eq",
    "error",
    "ffi",
    "glbl",
    "is_atom",
    "listcheck",
    "set_car",
    "set_cdr",
    "spcl",
    "symcheck",
)

## }}}
## {{{ basics


class LispError(Exception):
    pass


error = LispError


EL = object()
T = True
SENTINEL = object()


class Symbol:
    ## pylint: disable=too-few-public-methods

    __slots__ = ("s",)

    def __init__(self, s):
        self.s = s

    def __str__(self):
        return self.s

    def __repr__(self):
        return "'" + self.s


def symcheck(x):
    if isinstance(x, Symbol):
        return x
    raise SyntaxError(f"expected symbol, got {x!r}")


class SymbolTable:
    ## pylint: disable=too-few-public-methods

    __slots__ = ("t",)

    def __init__(self):
        self.t = {}

    def symbol(self, s):
        assert type(s) is str and s  ## pylint: disable=unidiomatic-typecheck
        if s not in self.t:
            self.t[s] = Symbol(s)
        return self.t[s]


def is_atom(x):
    return isinstance(x, Symbol) or x is EL or x is T


def eq(x, y):
    return x is y


## }}}
## {{{ pairs


def listcheck(x):
    if isinstance(x, list):
        return x
    raise TypeError(f"expected list, got {x!r}")


def car(x):
    return listcheck(x)[0]


def cdr(x):
    return listcheck(x)[1]


def cons(x, y):
    return [x, y]


def set_car(x, y):
    listcheck(x)[0] = y


def set_cdr(x, y):
    listcheck(x)[1] = y


## }}}
## {{{ environment


class Environment:
    __slots__ = ("t", "p")

    def __init__(self, ctx, params, args, parent):
        self.p = parent
        self.t = {}
        self.bind(ctx, params, args)

    def bind(self, ctx, params, args):
        t = self.t
        v = ctx.symbol("&")
        variadic = False
        while params is not EL:
            if not isinstance(params, list):
                raise TypeError(f"expected param list, got {params!r}")
            p, params = params
            if not isinstance(p, Symbol):
                raise TypeError(f"expected param symbol, got {p!r}")
            if p is v:
                variadic = True
            elif variadic:
                if params is not EL:
                    raise SyntaxError("extra junk after '&'")
                t[p] = args
                return
            elif args is EL:
                raise SyntaxError("not enough args")
            elif not isinstance(args, list):
                raise TypeError(f"expected arg list, got {args!r}")
            else:
                t[p], args = args
        if variadic:
            raise SyntaxError("params ends with '&'")
        if args is not EL:
            raise SyntaxError("too many args")

    def get(self, sym):
        if not isinstance(sym, Symbol):
            raise TypeError(f"expected symbol, got {sym!r}")
        e = self
        while e is not SENTINEL:
            try:
                return e.t[sym]
            except KeyError:
                e = e.p
        raise NameError(str(sym))

    def set(self, sym, value):
        if not isinstance(sym, Symbol):
            raise TypeError(f"expected symbol, got {sym!r}")
        self.t[sym] = value

    def setbang(self, sym, value):
        if not isinstance(sym, Symbol):
            raise TypeError(f"expected symbol, got {sym!r}")
        e = self
        while e is not SENTINEL:
            if sym in e.t:
                e.t[sym] = value
                return
            e = e.p
        raise NameError(str(sym))


# }}}
## {{{ decorators


GLOBALS = { "#t": T }


def glbl(name):
    def wrap(func):
        GLOBALS[name] = func
        func.special = False
        func.ffi = False
        return func

    return wrap


def spcl(name):
    def wrap(func):
        GLOBALS[name] = func
        func.special = True
        func.ffi = False
        return func

    return wrap


def ffi(name):
    def wrap(func):
        GLOBALS[name] = func
        func.special = False
        func.ffi = True
        return func

    return wrap


## }}}
## {{{ queue


class Queue:
    __slots__ = ("h", "t")

    def __init__(self):
        self.h = self.t = EL

    def __bool__(self):
        return self.h is not EL

    def enqueue(self, x):
        n = [x, EL]
        if self.h is EL:
            self.h = n
        else:
            self.t[1] = n
        self.t = n

    def dequeue(self):
        n = self.h
        self.h = n[1]
        if self.h is EL:
            self.t = EL
        return n[0]

    def head(self):
        return self.h


## }}}
## {{{ continuation


class Continuation:
    ## pylint: disable=too-few-public-methods

    __slots__ = ("c", "s")

    def __init__(self, state, cont):
        self.c = cont
        self.s = state

    def __call__(self, ctx):
        ctx.restore(self.s)
        return self.c


## }}}
## {{{ lambda


class Lambda:
    ## pylint: disable=too-few-public-methods

    __slots__ = ("p", "b", "e", "special", "ffi")

    def __init__(self, params, body, env):
        self.p = params
        self.b = body
        self.e = env
        self.special = self.ffi = False

    def __call__(self, ctx):
        p = ctx.env if self.special else self.e
        ctx.env = ctx.environment(self.p, ctx.argl, p)
        ctx.exp = self.b
        return ctx.k_leval


## }}}
## {{{ context


class Context:
    ## pylint: disable=too-many-instance-attributes,too-many-public-methods

    def __init__(self):
        ## registers
        self.argl = self.cont = self.env = self.exp = self.val = EL

        ## stack
        self.s = EL

        self.symbol = SymbolTable().symbol

        self.g = self.environment(EL, EL, SENTINEL)
        for k, v in GLOBALS.items():
            self.g.set(self.symbol(k), v)

    ## {{{ top level entries

    def leval(self, x, env=SENTINEL):
        self.cont = self.land
        self.env = self.g if env is SENTINEL else env
        self.exp = x
        return k_leval

    def lisp_value_to_py_value(self, x):
        self.exp = x
        self.cont = self.land
        return self.trampoline(k_lisp_value_to_py_value)


    def stringify(self, x):
        self.cont = self.land
        self.exp = x
        return k_stringify

    ## }}}
    ## {{{ factories

    def continuation(self, cont):
        return Continuation(self.save(), cont)

    def environment(self, params, args, parent):
        return Environment(self, params, args, parent)

    def parser(self, callback):
        return Parser(self, callback)

    ## }}}
    ## {{{ state mgt

    def save(self):
        return self.s, self.argl, self.cont, self.env, self.exp, self.val

    def restore(self, x):
        self.s, self.argl, self.cont, self.env, self.exp, self.val = x

    ## }}}
    ## {{{ registers

    def go(self, val):
        self.val = val
        return self.cont

    ## }}}
    ## {{{ stack

    def clear_stack(self):
        self.s = EL

    def pop(self):
        ret, self.s = self.s
        return ret

    def pop_ce(self):
        self.env, s = self.s
        self.cont, self.s = s

    def push(self, x):
        self.s = [x, self.s]

    def push_ce(self):
        self.s = [self.cont, [self.env, self.s]]

    def top(self):
        return self.s[0]

    ## }}}
    ## {{{ trampoline

    def trampoline(self, func):
        try:
            while True:
                func = func(self) or self.cont
        except self.Land:
            return self.val

    def land(self, _):
        raise self.Land()

    class Land(Exception):
        pass

    ## }}}
    ## {{{ unpack

    def unpack(self, n):
        args = self.argl
        ret = []
        for _ in range(n):
            if args is EL:
                raise SyntaxError("not enough args")
            if not isinstance(args, list):
                raise SyntaxError(f"expected list, got {args!r}")
            x, args = args
            ret.append(x)
        if args is not EL:
            raise SyntaxError("too many args")
        return ret

    ## }}}
    ## {{{ high level parsing routines


    def parse(self, text, callback):
        p = self.parser(callback)
        p.feed(text)
        p.feed(None)


    def execute(self, text):
        results = []

        def callback(expr):
            results.append(self.leval(expr))

        self.parse(text, callback)
        return results


    def load(self, filename, callback=None):
        if os.path.isabs(filename):
            path = filename
        else:
            for d in ["", os.path.dirname(__file__)] + sys.path:
                path = os.path.join(d, filename)
                if os.path.isfile(path):
                    break
            else:
                raise FileNotFoundError(filename)
        with open(path, "r", encoding=locale.getpreferredencoding()) as fp:
            if callback:
                self.parse(fp.read(), callback)
            else:
                self.execute(fp.read())


    ## }}}
    ## {{{ repl and main


    def repl(self, callback):
        try:
            import readline as _  ## pylint: disable=import-outside-toplevel
        except ImportError:
            pass

        ## pylint: disable=unused-variable
        p, rc, stop = self.parser(callback), 0, False

        def feed(x):
            nonlocal p, rc, stop
            try:
                p.feed(x)
            except SystemExit as exc:
                self.clear_stack()
                stop, rc = True, exc.args[0]
            except:  ## pylint: disable=bare-except
                self.clear_stack()
                p = self.parser(callback)
                traceback.print_exception(*sys.exc_info())

        while not stop:
            try:
                line = input("lisp> ") + "\n"
            except (EOFError, KeyboardInterrupt):
                feed(None)
                break
            feed(line)
        print("\nbye")
        return rc


    def main(self, force_repl=False):
        try:
            sys.set_int_max_str_digits(0)
        except AttributeError:
            pass

        def callback(expr):
            try:
                value = self.leval(expr)
            except SystemExit:
                raise
            except:
                print("Offender (pyth):", expr)
                print("Offender (lisp):", self.stringify(expr), "\n")
                raise
            if value is not EL:
                print(self.stringify(value))

        stop = True
        for filename in sys.argv[1:]:
            if filename == "-":
                stop = False
                break
            self.load(filename, callback=callback)
            stop = True
        try:
            if force_repl or not stop:
                raise SystemExit(self.repl(callback))
        finally:
            ## debug code can go here
            pass


## }}}


## }}}
## {{{ scanner #5: state-based, no lut


class Scanner:
    """
    i am shocked that this krusty coding is the fastest. i have fiddled
    with this class a lot, and this is the fastest implementation i know.
    """

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

    S_SYM = 0
    S_COMMENT = 1
    S_STRING = 2
    S_ESC = 3
    S_COMMA = 4

    def __init__(self, callback):
        self.pos = [0]  ## yup, a list
        self.token = []
        self.add = self.token.append
        self.parens = []
        self.callback = callback
        self.stab = (  ## this corresponds to the S_* constants
            self.do_sym,
            self.do_comment,
            self.do_string,
            self.do_esc,
            self.do_comma,
        )
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
        self.pos[0], n = 0, len(text)
        pos, stab = self.pos, self.stab
        p = 0
        while p < n:
            stab[self.state](text[p])
            p = pos[0] = pos[0] + 1

    def push(self, ttype):
        if self.token:
            t = "".join(self.token)
            self.token.clear()
            if ttype == self.T_SYM and t[0] in "0123456789-.+":
                try:
                    t = int(t, 0)
                    ttype = self.T_INT
                except ValueError:
                    try:
                        t = float(t)
                        ttype = self.T_FLOAT
                    except:  ## pylint: disable=bare-except
                        pass
            self.callback(ttype, t)
        elif ttype != self.T_SYM:
            self.callback(ttype, None)

    def do_sym(self, ch):
        ## pylint: disable=too-many-branches
        if ch in "()[] \n\r\t;\"',`":  ## all of this is actually faster.
            if ch in "([":
                self.parens.append(")" if ch == "(" else "]")
                self.push(self.T_SYM)
                self.push(self.T_LPAR)
            elif ch in ")]":
                if not self.parens:
                    raise SyntaxError(f"too many {ch!r}")
                if self.parens.pop() != ch:
                    raise SyntaxError(f"unexpected {ch!r}")
                self.push(self.T_SYM)
                self.push(self.T_RPAR)
            elif ch in " \n\r\t":
                self.push(self.T_SYM)
            elif ch == ";":
                self.push(self.T_SYM)
                self.state = self.S_COMMENT
            else:
                ## less common cases that aren't delimiters: ["] ['] [,] [`]
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
        else:
            self.add(ch)

    def do_comment(self, ch):
        if ch in "\n\r":
            self.state = self.S_SYM

    def do_string(self, ch):
        if ch == '"':
            self.push(self.T_STRING)
            self.state = self.S_SYM
        elif ch == "\\":
            self.state = self.S_ESC
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
            self.pos[0] -= 1
            self.push(self.T_COMMA)
        self.state = self.S_SYM


## }}}
## {{{ parser #2: inlined


class Parser:
    def __init__(self, ctx, callback):
        self.ctx = ctx
        self.callback = callback
        self.stack = []
        self.qstack = []
        self.scanner = Scanner(self.process_token)
        self.feed = self.scanner.feed
        self.qt = {
            "'": ctx.symbol("quote"),
            ",": ctx.symbol("unquote"),
            ",@": ctx.symbol("unquote-splicing"),
            "`": ctx.symbol("quasiquote"),
        }


    def process_token(self, ttype, token):
        s = self.scanner
        if ttype == s.T_SYM:
            self.add(self.ctx.symbol(token))
        elif ttype == s.T_LPAR:
            self.qstack.append(")")
            self.stack.append(Queue())
        elif ttype == s.T_RPAR:
            del self.qstack[-1]
            l = self.quote_wrap(self.stack.pop().head())
            if self.stack:
                self.add(l)
            else:
                self.callback(l)
        elif ttype in (s.T_INT, s.T_FLOAT, s.T_STRING):
            self.add(token)
        elif ttype in (s.T_TICK, s.T_COMMA, s.T_COMMA_AT, s.T_BACKTICK):
            self.qstack.append(self.qt[token])
        else:  ## EOF
            assert not self.stack  ## Scanner checks this
            if self.qstack:
                raise SyntaxError("unclosed quasiquote")

    def add(self, x):
        if not self.stack:
            raise SyntaxError(f"expected '(' got {x!r}")
        self.stack[-1].enqueue(self.quote_wrap(x))

    def quote_wrap(self, x):
        while self.qstack and isinstance(self.qstack[-1], Symbol):
            x = [self.qstack.pop(), [x, EL]]
        return x


## }}}
## {{{ stringify exp -> str


def k_stringify_setup(ctx, rest):
    ctx.exp, rest = rest
    if rest is EL:
        ctx.cont = k_stringify_last
    else:
        ctx.cont = k_stringify_next
        ctx.push(rest)
    return k_stringify


def k_stringify_next(ctx):
    rest = ctx.pop()
    ctx.push(ctx.val)
    return k_stringify_setup(ctx, rest)


def k_stringify_last(ctx):
    parts = [ctx.val]
    while True:
        x = ctx.pop()
        if x is SENTINEL:
            break
        parts.insert(0, x)
    ctx.val = "(" + " ".join(parts) + ")"
    return ctx.pop()


def k_stringify(ctx):
    ## pylint: disable=too-many-return-statements,too-many-locals
    x = ctx.exp
    if x is EL:
        return ctx.go("()")
    if x is T:
        return ctx.go("#t")
    if isinstance(x, (Symbol, int, float, str)):
        return ctx.go(str(x))
    if isinstance(x, Lambda):
        return x.stringify_
    if isinstance(x, Continuation):
        return ctx.go("[continuation]")
    if callable(x):
        return ctx.go("[primitive]")
    if not isinstance(x, list):
        return ctx.go("[opaque]")

    ctx.push(ctx.cont)
    ctx.push(SENTINEL)
    return k_stringify_setup(ctx, x)


## }}}
## {{{ leval exp -> val, all stack ops inlined for speed


def k_leval(ctx):
    ## pylint: disable=too-many-branches
    ## to speed up python code you have to eliminate function
    ## calls. we aggressively do that in leval_*() for a big
    ## speed boost. it *is* a little opaque in spots
    x = ctx.exp
    t = type(x)
    if t is Symbol:
        ## inline env.get
        e = ctx.env
        while e is not SENTINEL:
            try:
                ctx.val = e.d[x]
                return ctx.cont
            except KeyError:
                e = e.p
        raise NameError(str(x)) from None

    if t is list:
        op, args = x
        if isinstance(op, Symbol):
            ## inline env.get
            e = ctx.env
            while e is not SENTINEL:
                try:
                    op = e.d[op]
                    break
                except KeyError:
                    e = e.p
            else:
                raise NameError(str(op)) from None

            if op.special:
                ctx.argl = args
                return op
    elif t is Lambda:
        op = x
        args = EL
    else:
        ctx.val = x
        return ctx.cont

    ## push_ce(); push(args)
    ctx.s = [args, [ctx.env, [ctx.cont, ctx.s]]]

    ## list or sym lookup
    if callable(op):
        ctx.val = op
        return k_leval_proc_done
    if not isinstance(op, list):
        raise SyntaxError(f"expected list or proc, got {op!r}")
    ctx.cont = k_leval_proc_done
    ctx.exp = op
    return k_leval


def k_leval_proc_done(ctx):
    proc = ctx.val
    if not callable(proc):
        raise TypeError(f"expected callable, got {proc!r}")
    ## pop argl and env
    ctx.argl, s = ctx.s
    ctx.env, s = s

    if ctx.argl is EL:
        ## pop r.cont
        ctx.cont, ctx.s = s
        return proc

    if proc.special:
        ## pop r.cont
        ctx.cont, ctx.s = s
        return proc

    ## inline old leval_setup() to avoid function call
    ## push proc, SENTINEL, env
    s = [ctx.env, [SENTINEL, [proc, s]]]
    ctx.exp, args = ctx.argl
    if args is EL:
        ctx.cont = k_leval_last
    else:
        s = [args, s]
        ctx.cont = k_leval_next
    ctx.s = s
    return k_leval


def k_leval_next(ctx):
    ## pop args and r.env
    args, s = ctx.s
    ctx.env, s = s
    ## push val and r.env
    s = [ctx.env, [ctx.val, s]]
    ## inline old leval_setup() to avoid function call
    ctx.exp, args = args
    if args is EL:
        ctx.cont = k_leval_last
    else:
        ## push args
        s = [args, s]
        ctx.cont = k_leval_next
    ctx.s = s
    return k_leval


def k_leval_last(ctx):
    ## pop r.env
    ctx.env, s = ctx.s
    args = [ctx.val, EL]
    while True:
        ## pop x
        x, s = s
        if x is SENTINEL:
            break
        args = [x, args]
    ctx.argl = args
    ## pop proc
    proc, s = s
    ## pop cont
    ctx.cont, ctx.s = s
    if proc.ffi:
        ctx.exp = proc
        return do_ffi
    return proc


## }}}
## {{{ ffi support


def do_ffi(ctx):
    push(r.cont)
    push(r.exp)  ## proc

    if ctx.argl is EL:
        ctx.argl = []
        return ffi_args_done_
    ctx.cont = ffi_args_done_
    ctx.exp = ctx.argl
    return lisp_value_to_py_value_


def k_ffi_args_done(ctx):
    proc = pop()
    ctx.cont = pop()
    ctx.exp = proc(r.val)
    return py_value_to_lisp_value_


def lisp_value_to_py_value_():
    x = ctx.exp
    if x is EL:
        x = None
    elif x is T:
        x = True
    if not isinstance(x, list):
        ctx.val = x
        return ctx.cont
    push(r.cont)
    push([])
    return lv2pv_setup_(x)


def lv2pv_setup_(args):
    ctx.exp, args = args
    push(args)
    ctx.cont = lv2pv_next_
    return lisp_value_to_py_value_


def lv2pv_next_():
    args = pop()
    argl = pop()
    argl.append(r.val)
    if args is EL:
        ctx.val = argl
        return pop()
    push(argl)
    return lv2pv_setup_(args)


def py_value_to_lisp_value(x):
    ctx.cont = land
    ctx.exp = x
    return trampoline(py_value_to_lisp_value_)


def py_value_to_lisp_value_():
    x = ctx.exp
    if x is None or x is False:
        x = EL
    elif x is True:
        x = T
    if not isinstance(x, (list, tuple)):
        return go(x)
    if not x:
        return go(EL)

    push(r.cont)
    push(Queue())
    return pv2lv_setup_(list(x))


def pv2lv_setup_(args):
    ctx.exp = args.pop(0)
    push(args)
    ctx.cont = pv2lv_next_
    return py_value_to_lisp_value_


def pv2lv_next_():
    args = pop()
    argl = pop()
    argl.enqueue(r.val)
    if not args:
        ctx.val = argl.head()
        return pop()
    push(argl)
    return pv2lv_setup_(args)


## }}}
