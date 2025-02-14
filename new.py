#!/usr/bin/env python3

"new.py"

## pylint: disable=invalid-name,unbalanced-tuple-unpacking,too-many-lines
## XXX pylint: disable=missing-docstring

import locale
import os
import sys
import traceback


__all__ = (
    "EL",
    "Environment",
    "Frame",
    "Queue",
    "Stack",
    "Symbol",
    "T",
    "atom",
    "car",
    "cdr",
    "cons",
    "eq",
    "error",
    "ffi",
    "glbl",
    "listcheck",
    "set_car",
    "set_cdr",
    "spcl",
    "split",
    "stack",
    "symbol",
    "symbolcheck",
)


## {{{ trampoline


class LAND_(Exception):
    pass


def trampoline(func, *args):
    try:
        while True:
            func, args = func(*args)
    except LAND_ as exc:
        return exc.args[0]


def bounce(func, *args):
    return func, args


def land(x):
    raise LAND_(x)


## }}}
## {{{ basics and atoms


EL = object()
T = True
SENTINEL = object()


class error(Exception):
    pass


class Symbol:
    ## pylint: disable=too-few-public-methods

    __slots__ = ("s",)

    def __init__(self, s):
        self.s = s

    def __repr__(self):
        return self.s

    __str__ = __repr__


def atom(x):
    return isinstance(x, Symbol) or x is EL or x is T


def eq(x, y):
    return x is y and atom(x)


def symbolcheck(x):
    if isinstance(x, Symbol):
        return x
    raise TypeError(f"expected symbol, got {x!r}")


## }}}
## {{{ global symbol table


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


symbol = SymbolTable().symbol


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


def split(x):
    return listcheck(x)


## }}}
## {{{ unpack


def unpack(args, n):
    ret = []
    for _ in range(n):
        if args is EL:
            raise TypeError(f"not enough args, need {n}")
        ret.append(args[0])
        args = args[1]
    if args is not EL:
        raise TypeError(f"too many args, need {n}")
    return ret


## }}}
## {{{ queue


class Queue:
    __slots__ = ("h", "t")

    def __init__(self):
        self.h = self.t = EL

    def __bool__(self):
        return self.h is not EL

    def dequeue(self):
        node = self.h
        if node is EL:
            raise ValueError("queue is empty")
        self.h = node[1]
        if self.h is EL:
            self.t = EL
        return node[0]

    def enqueue(self, x):
        node = [x, EL]
        if self.h is EL:
            self.h = node
        else:
            self.t[1] = node
        self.t = node

    def head(self):
        return self.h


## }}}
## {{{ stack


class Stack:
    __slots__ = ("s",)

    def __init__(self):
        self.s = EL

    def __bool__(self):
        return self.s is not EL

    def pop(self):
        s = self.s
        if s is EL:
            raise ValueError("stack is empty")
        ret, self.s = s
        return ret

    def push(self, x):
        self.s = [x, self.s]

    def top(self):
        if self.s is EL:
            raise ValueError("stack is empty")
        return self.s[0]

    def get(self):
        return self.s

    def set(self, x):
        self.s = x


## }}}
## {{{ environment


class Environment:
    __slots__ = ("d", "p")

    def __init__(self, params, args, parent):
        self.d = {}
        self.p = parent
        self.bind(params, args)

    def bind(self, params, args):
        d = self.d
        v = symbol("&")
        variadic = False
        while params is not EL:
            p, params = params
            if eq(symbolcheck(p), v):
                variadic = True
            elif variadic:
                p, params = params
                if params is not EL:
                    raise SyntaxError("extra junk after '&'")
                d[p] = args
                return
            elif args is EL:
                raise SyntaxError("not enough args")
            else:
                d[p] = args[0]
                args = args[1]
        if args is not EL:
            raise SyntaxError("too many args")

    def get(self, key, default):
        symbolcheck(key)
        e = self
        while e is not SENTINEL:
            x = e.d.get(key)
            if x is not SENTINEL:
                return x
            e = e.p
        return default

    def set(self, key, value):
        self.d[symbolcheck(key)] = value

    def setbang(self, key, value):
        symbolcheck(key)
        e = self
        while e is not SENTINEL:
            if key in e.d:
                e.d[key] = value
                return
            e = e.p
        raise NameError(str(key))


## }}}
## {{{ global environment and stack


genv = Environment([symbol("#t"), EL], [T, EL], SENTINEL)


stack = Stack()


## }}}
## {{{ primitive definition decorators


def glbl(name):
    def wrap(func):
        genv.set(name, func)
        return func

    return wrap


def spcl(name):
    def wrap(func):
        genv.set(name, func)
        func.special = True
        return func

    return wrap


def ffi(name):
    def wrap(func):
        genv.set(name, func)
        func.ffi = True
        return func

    return wrap


## }}}
## {{{ frame


class Frame:
    ## pylint: disable=too-few-public-methods

    __slots__ = ("x", "c", "e")

    def __init__(self, x, c, e):
        self.x = x
        self.c = c
        self.e = e

    def new(self, x=None, c=None, e=None):
        return Frame(
            self.x if x is None else x,
            self.c if c is None else c,
            self.e if e is None else e,
        )


## }}}
## {{{ scanner


class Scanner:
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
        self.token = Queue()
        self.parens = Stack()
        self.cont = self.k_sym
        self.callback = callback

    def feed(self, text):
        if text is None:
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
        l = self.token
        if l:
            t = ""
            while l:
                t += l.dequeue()
        elif ttype == self.T_SYM:
            return
        else:
            self.callback(ttype, None)
            return
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

    def k_sym(self, ch):
        ## pylint: disable=too-many-return-statements
        if ch == "(":
            return self.c_lpar(ch)
        if ch == ")":
            return self.c_rpar(ch)
        if ch in " \n\r\t":
            return self.c_ws(ch)
        if ch == "[":
            return self.c_lbrack(ch)
        if ch == "]":
            return self.c_rbrack(ch)
        if ch == ";":
            return self.c_semi(ch)
        if ch == "'":
            return self.c_tick(ch)
        if ch == ",":
            return self.c_comma(ch)
        if ch == "`":
            return self.c_backtick(ch)
        self.token.enqueue(ch)
        return self.k_sym

    def k_comment(self, ch):
        return self.k_sym if ch in "\n\r" else self.k_comment

    def k_quote(self, ch):
        if ch == "\\":
            return self.k_backslash
        if ch == '"':
            self.push(self.T_STRING)
            return self.k_sym
        self.token.enqueue(ch)
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
        self.token.enqueue(c)
        return self.k_quote

    def k_comma(self, ch):
        if ch == "@":
            self.token.enqueue("@")
            self.push(self.T_COMMA_AT)
        else:
            self.pos -= 1
            self.push(self.T_COMMA)
        return self.k_sym

    def c_semi(self, _):
        self.push(self.T_SYM)
        return self.k_comment

    def c_quote(self, ch):
        if self.token:
            raise SyntaxError(f"{ch!r} not a delimiter")
        return self.k_quote

    def c_comma(self, ch):
        if self.token:
            raise SyntaxError(f"{ch!r} not a delimiter")
        self.token.enqueue(",")
        return self.k_comma

    def c_tick(self, ch):
        if self.token:
            raise SyntaxError(f"{ch!r} not a delimiter")
        self.token.enqueue(ch)
        self.push(self.T_TICK)
        return self.k_sym

    def c_backtick(self, ch):
        if self.token:
            raise SyntaxError(f"{ch!r} not a delimiter")
        self.token.enqueue(ch)
        self.push(self.T_BACKTICK)
        return self.k_sym

    def c_lpar(self, _):
        self.parens.push(")")
        self.push(self.T_SYM)
        self.push(self.T_LPAR)
        return self.k_sym

    def c_rpar(self, ch):
        if not self.parens:
            raise SyntaxError(f"too many {ch!r}")
        if self.parens.pop() != ch:
            raise SyntaxError(f"{ch!r} inside '['")
        self.push(self.T_SYM)
        self.push(self.T_RPAR)
        return self.k_sym

    def c_lbrack(self, _):
        self.parens.push("]")
        self.push(self.T_SYM)
        self.push(self.T_LPAR)
        return self.k_sym

    def c_rbrack(self, ch):
        if not self.parens:
            raise SyntaxError(f"too many {ch!r}")
        if self.parens.pop() != ch:
            raise SyntaxError(f"{ch!r} inside '('")
        self.push(self.T_SYM)
        self.push(self.T_RPAR)
        return self.k_sym

    def c_ws(self, _):
        self.push(self.T_SYM)
        return self.k_sym


## }}}
## {{{ parser


class Parser:
    def __init__(self, callback):
        self.callback = callback
        self.stack = Stack()
        self.qstack = Stack()
        self.scanner = Scanner(self.process_token)
        self.feed = self.scanner.feed

    def t_sym(self, token):
        self.add(symbol(token))

    def t_lpar(self, _):
        self.qstack.push(")")
        self.stack.push(Queue())

    def t_rpar(self, _):
        assert self.stack  ## Scanner checks this
        assert self.qstack.pop() == ")"
        l = self.quote_wrap(self.stack.pop().head())
        if not self.stack:
            self.callback(l)
        else:
            self.add(l)

    def t_eof(self, _):
        assert not self.stack  ## Scanner checks this
        if self.qstack:
            raise SyntaxError("unclosed quasiquote")

    def process_token(self, ttype, token):
        s = self.scanner
        if ttype == s.T_SYM:
            self.t_sym(token)
        elif ttype == s.T_LPAR:
            self.t_lpar(token)
        elif ttype == s.T_RPAR:
            self.t_rpar(token)
        elif ttype in (s.T_INT, s.T_FLOAT, s.T_STRING):
            self.add(token)
        elif ttype in (s.T_TICK, s.T_COMMA, s.T_COMMA_AT, s.T_BACKTICK):
            self.set_up_quote(token)
        else:
            self.t_eof(token)

    def add(self, x):
        if not self.stack:
            raise SyntaxError(f"expected '(' got {x!r}")
        self.stack.top().enqueue(self.quote_wrap(x))

    def quote_wrap(self, x):
        ret = x
        while self.qstack and isinstance(self.qstack.top(), Symbol):
            s = self.qstack.pop()
            ret = [s, [ret, EL]]
        return ret

    def set_up_quote(self, s):
        if s == "'":
            s = symbol("quote")
        elif s == ",":
            s = symbol("unquote")
        elif s == ",@":
            s = symbol("unquote-splicing")
        else:
            assert s == "`"
            s = symbol("quasiquote")
        self.qstack.push(s)


## }}}
## {{{ high level parsing routines


def parse(text, callback):
    p = Parser(callback)
    p.feed(text)
    p.feed(None)


def execute(text):
    results = []

    def callback(sexpr):
        results.append(leval(sexpr))

    parse(text, callback)
    return results


def load(filename, callback=None):
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
            parse(fp.read(), callback)
        else:
            execute(fp.read())


## }}}
## {{{ repl and main


def repl(callback):
    try:
        import readline as _  ## pylint: disable=import-outside-toplevel
    except ImportError:
        pass

    ## pylint: disable=unused-variable
    p, rc, stop = Parser(callback), 0, False

    def feed(x):
        nonlocal p, rc, stop
        try:
            p.feed(x)
        except SystemExit as exc:
            stop, rc = True, exc.args[0]
        except:  ## pylint: disable=bare-except
            p = Parser(callback)
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


def main(force_repl=False):
    def callback(sexpr):
        try:
            value = leval(sexpr)
        except SystemExit:
            raise
        except:
            print("Offender (pyth):", sexpr)
            print("Offender (lisp):", stringify(sexpr), "\n")
            raise
        if value is not EL:
            print(stringify(value))

    stop = True
    for filename in sys.argv[1:]:
        if filename == "-":
            stop = False
            break
        load(filename, callback=callback)
        stop = True
    if force_repl or not stop:
        raise SystemExit(repl(callback))


## }}}
## {{{ lambda


class Lambda:
    __slots__ = ["p", "b", "e", "special"]

    def __init__(self, params, body, env):
        self.p = params
        self.b = body
        self.e = env
        self.special = False

    def __call__(self, frame):
        args = frame.x
        p = frame.e if self.special else self.e
        e = Environment(self.p, args, p)
        return bounce(leval_, frame.new(x=self.b, e=e))

    ###

    def lambda_body_done_(self, bodystr):
        ## pylint: disable=no-self-use
        frame = stack.pop()
        paramstr = frame.x
        return bounce(frame.c, "(lambda " + paramstr + " " + bodystr + ")")

    def lambda_params_done_(self, paramstr):
        frame = stack.pop()
        body = frame.x
        stack.push(frame.new(x=paramstr))
        return bounce(stringify_, frame.new(x=body, c=self.lambda_body_done_))

    def stringify_(self, frame):
        stack.push(frame.new(x=self.b))
        return bounce(
            stringify_,
            frame.new(x=self.p, c=self.lambda_params_done_),
        )


## }}}
## {{{ continuation


class Continuation:
    ## pylint: disable=too-few-public-methods

    __slots__ = ["c", "s"]

    def __init__(self, continuation):
        self.c = continuation
        self.s = stack.get()

    def __call__(self, frame):
        (x,) = unpack(frame.x, 1)
        stack.set(self.s)
        return bounce(self.c, x)  ## that's it.


## }}}
## {{{ stringify


def stringify(sexpr):
    return trampoline(stringify_, Frame(sexpr, land, SENTINEL))


def stringify_setup(frame, args):
    if isinstance(args, list):
        arg, args = args
    else:
        arg = args
        args = EL
        stack.push(frame.new(x="."))
    stack.push(frame.new(x=args))
    return bounce(stringify_, frame.new(x=arg, c=stringify_cont))


def stringify_cont(value):
    frame = stack.pop()
    args = frame.x

    if args is EL:
        parts = [value]
        while True:
            f = stack.pop()
            if f.x is SENTINEL:
                break
            parts.insert(0, f.x)
        return bounce(frame.c, "(" + " ".join(parts) + ")")

    stack.push(frame.new(x=value))
    return stringify_setup(frame, args)


def stringify_(frame):
    ## pylint: disable=too-many-return-statements,too-many-locals
    x = frame.x
    if x is EL:
        return bounce(frame.c, "()")
    if x is T:
        return bounce(frame.c, "#t")
    if isinstance(x, (Symbol, int, float, str)):
        return bounce(frame.c, str(x))
    if isinstance(x, Lambda):
        return bounce(x.stringify_, frame)
    if isinstance(x, Continuation):
        return bounce(frame.c, "[continuation]")
    if callable(x):
        return bounce(frame.c, "[primitive]")
    if not isinstance(x, list):
        return bounce(frame.c, "[opaque]")

    stack.push(frame.new(x=SENTINEL))

    return stringify_setup(frame, x)


## }}}
## {{{ eval


def leval(sexpr, env=SENTINEL):
    e = genv if env is SENTINEL else env
    return trampoline(leval_, Frame(sexpr, land, e))


def eval_setup(frame, args):
    if isinstance(args, list):
        arg, args = args
    else:
        arg = args
        args = EL
    stack.push(frame.new(x=args))
    return bounce(leval_, frame.new(x=arg, c=eval_next_arg))


def eval_next_arg(value):
    frame = stack.pop()
    args = frame.x

    if args is EL:
        ret = [value, EL]
        while True:
            f = stack.pop()
            if f.x is SENTINEL:
                proc = f.c  ## NB abuse of .c field
                break
            ret = [f.x, ret]
        ## at this point, need to see if proc is ffi
        if getattr(proc, "ffi", False):
            ## should construct args as a pylist not pair but then Frame would
            ## need a new field to hold proc all the way through. this is about
            ## a global 5% performance hit. i care more about ffi capability
            ## than performance. plus, this thing is slow enough already.
            return bounce(do_ffi, frame.new(x=[ret, proc]))
        return bounce(proc, frame.new(x=ret))

    stack.push(frame.new(x=value))
    return eval_setup(frame, args)


def eval_proc_done(proc):
    frame = stack.pop()
    args = frame.x

    if not callable(proc):  ## python func Lambda Continuation
        raise TypeError(f"expected callable, got {proc!r}")

    ## specials don't have their args evaluated
    if getattr(proc, "special", False):
        return bounce(proc, frame)

    ## shortcut the no-args case
    if args is EL:
        return bounce(proc, frame)

    ## evaluate args...

    stack.push(frame.new(c=proc, x=SENTINEL))  ## NB abuse .c field

    return eval_setup(frame, args)


def leval_(frame):
    ## pylint: disable=too-many-locals

    x = frame.x
    if isinstance(x, Symbol):
        obj = frame.e.get(x, SENTINEL)
        if obj is SENTINEL:
            raise NameError(x)
        return bounce(frame.c, obj)
    if isinstance(x, list):
        sym, args = x
    elif isinstance(x, Lambda):
        sym = x
        args = EL
    else:
        return bounce(frame.c, x)
    if isinstance(sym, Symbol):
        op = frame.e.get(sym, SENTINEL)
        if op is SENTINEL:
            raise NameError(sym)
        if getattr(op, "special", False):
            return bounce(op, frame.new(x=args))
        sym = op
    if callable(sym):
        ## primitive Lambda Continuation
        stack.push(frame.new(x=args))
        return bounce(eval_proc_done, sym)
    if not isinstance(sym, list):
        raise TypeError(f"expected proc or list, got {sym!r}")

    stack.push(frame.new(x=args))
    return bounce(leval_, frame.new(x=sym, c=eval_proc_done))


## }}}
## {{{ ffi


def do_ffi(frame):
    af = frame.x
    args, func = af
    stack.push(frame.new(x=func))

    if args is EL:
        return bounce(ffi_args_done, [])

    return bounce(lisp_value_to_py_value_, frame.new(x=args, c=ffi_args_done))


def lisp_value_to_py_value(x):
    return trampoline(lisp_value_to_py_value_, Frame(x, land, SENTINEL))


def lv2pv_setup(frame, args):
    arg, args = args
    stack.push(frame.new(x=args))
    return bounce(lisp_value_to_py_value_, frame.new(x=arg, c=lv2pv_next_arg))


def lv2pv_next_arg(value):
    frame = stack.pop()
    args = frame.x

    if args is EL:
        ret = [value]
        while True:
            f = stack.pop()
            if f.x is SENTINEL:
                break
            ret.insert(0, f.x)
        return bounce(frame.c, ret)

    stack.push(frame.new(x=value))
    return lv2pv_setup(frame, args)


def lisp_value_to_py_value_(frame):
    x = frame.x
    if x is EL:
        x = None
    elif x is T:
        x = True
    if not isinstance(x, list):
        return bounce(frame.c, x)

    stack.push(frame.new(x=SENTINEL))
    return lv2pv_setup(frame, x)


def py_value_to_lisp_value(x):
    return trampoline(py_value_to_lisp_value_, Frame(x, land, SENTINEL))


def pv2lv_setup(frame, args):
    arg = args.pop(0)
    stack.push(frame.new(x=args))
    return bounce(py_value_to_lisp_value_, frame.new(x=arg, c=pv2lv_next_arg))


def pv2lv_next_arg(value):
    frame = stack.pop()
    args = frame.x

    if not args:
        ret = [value, EL]
        while True:
            f = stack.pop()
            if f.x is SENTINEL:
                break
            ret = [f.x, ret]
        return bounce(frame.c, ret)

    stack.push(frame.new(x=value))
    return pv2lv_setup(frame, args)


def py_value_to_lisp_value_(frame):
    x = frame.x
    if x is None or x is False:
        x = EL
    elif x is True:
        x = T
    if not isinstance(x, (list, tuple)):
        return bounce(frame.c, x)
    if not x:
        return bounce(frame.c, EL)

    stack.push(frame.new(x=SENTINEL))
    return pv2lv_setup(frame, list(x))


def ffi_args_done(args):
    frame = stack.pop()
    func = frame.x

    ret = func(args)

    return bounce(py_value_to_lisp_value_, frame.new(x=ret))


## }}}


if __name__ == "__main__":
    main()


## EOF
