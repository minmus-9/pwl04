#!/usr/bin/env python3

"r.py"

## pylint: disable=invalid-name,unbalanced-tuple-unpacking,too-many-lines
## XXX pylint: disable=missing-docstring


import locale
import os
import sys
import traceback


## {{{ basics


class LispError(Exception):
    pass


error = LispError
SENTINEL = object()
EL = object()
T = object()


class Symbol:
    ## pylint: disable=too-few-public-methods

    __slots__ = ("s",)

    def __init__(self, s):
        self.s = s

    def __repr__(self):
        return self.s


class SymbolTable:
    ## pylint: disable=too-few-public-methods

    __slots__ = ("t",)

    def __init__(self):
        self.t = {}

    def symbol(self, s):
        if s not in self.t:
            self.t[s] = Symbol(s)
        return self.t[s]


symbol = SymbolTable().symbol


def is_atom(x):
    return isinstance(x, Symbol) or x is EL or x is T


def eq(x, y):
    return x is y


def symcheck(x):
    if isinstance(x, Symbol):
        return x
    raise TypeError(f"expected symbol, got {x!r}")


## }}}
## {{{ regs


class Registers:
    ## pylint: disable=too-few-public-methods

    __slots__ = ("argl", "cont", "env", "exp", "val")

    def __init__(self):
        self.argl = self.cont = self.env = self.exp = self.val = EL

    def push_ce(self):
        push(self.cont)
        push(self.env)

    def pop_ce(self):
        self.env = pop()
        self.cont = pop()

    def go(self, val):
        self.val = val
        return self.cont

    def get(self):
        return dict((k, getattr(self, k)) for k in self.__slots__)

    def set(self, x):
        for k, v in x.items():
            setattr(self, k, v)


r = Registers()
push_ce = r.push_ce
pop_ce = r.pop_ce
go = r.go

regs_get = r.get
regs_set = r.set


## }}}
## {{{ XXX trampoline


def trampoline(func):
    ## XXX
    last = None
    try:
        while True:
            last = func
            func = func()
    except Land_:
        return r.val
    except:
        print("L", last)
        print("F", func)
        raise


class Land_(Exception):
    pass


def land():
    raise Land_()


## }}}
## {{{ stack and global stack


class Stack:
    __slots__ = ("s",)

    def __init__(self):
        self.s = EL

    def __bool__(self):
        return self.s is not EL

    def clear(self):
        self.s = EL

    def push(self, x):
        self.s = [x, self.s]

    def pop(self):
        ret, self.s = self.s
        return ret

    def top(self):
        return self.s[0]

    def get(self):
        return self.s

    def set(self, x):
        self.s = x


stack = Stack()
push = stack.push
pop = stack.pop
top = stack.top

stack_get = stack.get
stack_set = stack.set


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
## {{{ env and global env


class Environment:
    __slots__ = ("d", "p")

    def __init__(self, params, args, parent):
        self.d = {}
        self.p = parent
        self.bind(self.d, params, args)

    def bind(self, d, params, args):
        ## pylint: disable=no-self-use
        v = symbol("&")
        variadic = False
        while params is not EL:
            p, params = params
            if symcheck(p) is v:
                variadic = True
            elif variadic:
                if params is not EL:
                    raise SyntaxError("extra junk after '&'")
                d[p] = args
                return
            elif args is EL:
                raise SyntaxError("not enough args")
            else:
                d[p] = args[0]
                args = args[1]
        if variadic:
            raise SyntaxError("params ends with '&'")
        if args is not EL:
            raise SyntaxError("too many args")

    def get(self, sym):
        symcheck(sym)
        e = self
        while e is not SENTINEL:
            x = e.d.get(sym, SENTINEL)
            if x is not SENTINEL:
                return x
            e = e.p
        raise NameError(str(sym))

    def set(self, sym, value):
        self.d[symcheck(sym)] = value

    def setbang(self, sym, value):
        symcheck(sym)
        e = self
        while e is not SENTINEL:
            if sym in e.d:
                e.d[sym] = value
                return EL
            e = e.p
        raise NameError(str(sym))

    def up(self):
        return self.p


genv = Environment(EL, EL, SENTINEL)
genv.set(symbol("#t"), T)


## }}}
## {{{ op decorators


def glbl(name):
    def wrap(func):
        genv.set(symbol(name), func)
        return func

    return wrap


def spcl(name):
    def wrap(func):
        genv.set(symbol(name), func)
        func.special = True
        return func

    return wrap


def ffi(name):
    def wrap(func):
        genv.set(symbol(name), func)
        func.ffi = True
        return func

    return wrap


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
    QT = {
        "'": symbol("quote"),
        ",": symbol("unquote"),
        ",@": symbol("unquote-splicing"),
        "`": symbol("quasiquote"),
    }

    def __init__(self, callback):
        self.callback = callback
        self.stack = []
        self.qstack = []
        self.scanner = Scanner(self.process_token)
        self.feed = self.scanner.feed

    def process_token(self, ttype, token):
        s = self.scanner
        if ttype == s.T_SYM:
            self.add(symbol(token))
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
            self.qstack.append(self.QT[token])
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
            stack.clear()
            stop, rc = True, exc.args[0]
        except:  ## pylint: disable=bare-except
            stack.clear()
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
    try:
        sys.set_int_max_str_digits(0)
    except AttributeError:
        pass

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
    try:
        if force_repl or not stop:
            raise SystemExit(repl(callback))
    finally:
        assert not stack


## }}}
## {{{ unpack argl -> []


def unpack(n):
    args = r.argl
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
## {{{ continuation


class Continuation:
    ## pylint: disable=too-few-public-methods

    __slots__ = ("c", "r", "s")

    def __init__(self, continuation):
        self.c = continuation
        self.r = regs_get()
        self.s = stack_get()

    def __call__(self):
        (x,) = unpack(1)
        regs_set(self.r)
        stack_set(self.s)
        r.val = x
        return self.c  ## that's it


## }}}
## {{{ lambda (argl) -> val


class Lambda:
    __slots__ = ("p", "b", "e", "special")

    def __init__(self, params, body, env):
        self.p = params
        self.b = body
        self.e = env
        self.special = False

    def __call__(self):
        p = r.env if self.special else self.e
        r.env = Environment(self.p, r.argl, p)
        r.exp = self.b
        return leval_

    ###

    def lambda_body_done(self):
        ## pylint: disable=no-self-use
        bodystr = r.val
        paramstr = stack.pop()
        r.val = "(lambda " + paramstr + " " + bodystr + ")"
        return stack.pop()

    def lambda_params_done(self):
        r.cont = self.lambda_body_done
        r.exp = stack.pop()
        push(r.val)
        return stringify_

    def stringify_(self):
        push(r.cont)
        push(self.b)
        r.cont = self.lambda_params_done
        r.exp = self.p
        return stringify_


## }}}
## {{{ stringify exp -> str


def stringify(expr):
    r.exp = expr
    r.cont = land
    return trampoline(stringify_)


def stringify_setup_(rest):
    r.exp, rest = rest
    if rest is EL:
        r.cont = stringify_last_
    else:
        r.cont = stringify_next_
        stack.push(rest)
    return stringify_


def stringify_next_():
    rest = stack.pop()
    stack.push(r.val)
    return stringify_setup_(rest)


def stringify_last_():
    parts = [r.val]
    while True:
        x = stack.pop()
        if x is SENTINEL:
            break
        parts.insert(0, x)
    r.val = "(" + " ".join(parts) + ")"
    return stack.pop()


def stringify_():
    ## pylint: disable=too-many-return-statements,too-many-locals
    x = r.exp
    if x is EL:
        return go("()")
    if x is T:
        return go("#t")
    if isinstance(x, (Symbol, int, float, str)):
        return go(str(x))
    if isinstance(x, Lambda):
        return x.stringify_
    if isinstance(x, Continuation):
        return go("[continuation]")
    if callable(x):
        return go("[primitive]")
    if not isinstance(x, list):
        return go("[opaque]")

    stack.push(r.cont)
    stack.push(SENTINEL)
    return stringify_setup_(x)


## }}}
## {{{ leval exp -> val


def leval(exp, env=SENTINEL):
    r.cont = land
    r.env = genv if env is SENTINEL else env
    r.exp = exp
    return trampoline(leval_)


def leval_():
    x = r.exp
    if isinstance(x, Symbol):
        return go(r.env.get(x))
    if isinstance(x, list):
        op, args = x
        if isinstance(op, Symbol):
            op = r.env.get(op)
            if getattr(op, "special", False):
                r.argl = args
                return op
    elif isinstance(x, Lambda):
        op = x
        args = EL
    else:
        return go(x)

    push_ce()
    push(args)

    ## list or sym lookup
    if callable(op):
        r.val = op
        return leval_proc_done_
    if not isinstance(op, list):
        raise SyntaxError(f"expected list or proc, got {op!r}")
    r.cont = leval_proc_done_
    r.exp = op
    return leval_


def leval_proc_done_():
    proc = r.val
    if not callable(proc):
        raise TypeError(f"expected callable, got {proc!r}")
    r.argl = pop()
    r.env = pop()

    if getattr(proc, "special", False):
        r.cont = pop()
        return proc

    if r.argl is EL:
        r.cont = pop()
        return proc

    push(proc)
    push(SENTINEL)
    push(r.env)

    return leval_setup_(r.argl)


def leval_setup_(args):
    r.exp, args = args
    if args is EL:
        r.cont = leval_last_
    else:
        push(args)
        r.cont = leval_next_
    return leval_


def leval_next_():
    args = pop()
    r.env = pop()
    push(r.val)
    push(r.env)
    return leval_setup_(args)


def leval_last_():
    r.env = pop()
    args = [r.val, EL]
    while True:
        x = pop()
        if x is SENTINEL:
            break
        args = [x, args]
    r.argl = args
    proc = pop()
    r.cont = pop()
    if getattr(proc, "ffi", False):
        r.exp = proc
        return do_ffi
    return proc


## }}}
## {{{ ffi


def do_ffi():
    push(r.cont)
    push(r.exp)  ## proc

    if r.argl is EL:
        r.argl = []
        return ffi_args_done_
    r.cont = ffi_args_done_
    r.exp = r.argl
    return lisp_value_to_py_value_


def ffi_args_done_():
    proc = pop()
    r.cont = pop()
    r.exp = proc(r.val)
    return py_value_to_lisp_value_


def lisp_value_to_py_value(x):
    r.exp = x
    r.cont = land
    return trampoline(lisp_value_to_py_value_)


def lisp_value_to_py_value_():
    x = r.exp
    if x is EL:
        x = None
    elif x is T:
        x = True
    if not isinstance(x, list):
        r.val = x
        return r.cont
    push(r.cont)
    push([])
    return lv2pv_setup_(x)


def lv2pv_setup_(args):
    r.exp, args = args
    push(args)
    r.cont = lv2pv_next_
    return lisp_value_to_py_value_


def lv2pv_next_():
    args = pop()
    argl = pop()
    argl.append(r.val)
    if args is EL:
        r.val = argl
        return pop()
    push(argl)
    return lv2pv_setup_


def py_value_to_lisp_value(x):
    r.cont = land
    r.exp = x
    return trampoline(py_value_to_lisp_value_)


def py_value_to_lisp_value_():
    x = r.exp
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
    r.exp = args.pop(0)
    push(args)
    r.cont = pv2lv_next_
    return py_value_to_lisp_value_


def pv2lv_next_():
    args = pop()
    argl = pop()
    argl.enqueue(r.val)
    if not args:
        r.val = argl.head()
        return pop()
    push(argl)
    return pv2lv_setup_(args)


## }}}
## {{{ special forms


@spcl("cond")
def op_cond():
    (args,) = unpack(1)
    if args is EL:
        return go(EL)
    push(r.cont)
    return op_cond_setup_(args)


def op_cond_setup_(args):
    pc, args = args
    r.argl = pc
    r.exp, c = unpack(2)
    push(c)
    push(r.env)
    if args is EL:
        r.cont = op_cond_last_
    else:
        push(args)
        r.cont = op_cond_next_
    return leval_


def op_cond_next_():
    args = pop()
    r.env = pop()
    r.exp = pop()
    if r.val is not EL:
        r.cont = pop()
        return leval_
    return op_cond_setup_(args)


def op_cond_last_():
    r.env = pop()
    r.exp = pop()
    r.cont = pop()
    if r.val is EL:
        return go(EL)
    return leval_


@spcl("define")
def op_define():
    sym, value = unpack(2)
    push(symcheck(sym))
    push_ce()
    r.cont = op_define_
    r.exp = value
    return leval_


def op_define_():
    pop_ce()
    r.env.set(pop(), r.val)
    return go(EL)


@spcl("if")
def op_if():
    r.exp, c, a = unpack(3)
    push_ce()
    push([c, a])
    r.cont = op_if_
    return leval_


def op_if_():
    c, a = pop()
    pop_ce()
    r.exp = a if r.val is EL else c
    return leval_


@spcl("lambda")
def op_lambda():
    params, body = unpack(2)
    return go(Lambda(params, body, r.env))


@spcl("quote")
def op_quote():
    (r.val,) = unpack(1)
    return r.cont


@spcl("set!")
def op_setbang():
    sym, value = unpack(2)
    push(symcheck(sym))
    push_ce()
    r.cont = op_setbang_
    r.exp = value
    return leval_


def op_setbang_():
    pop_ce()
    sym = pop()
    r.env.setbang(sym, r.val)
    return r.cont


@spcl("special")
def op_special():
    sym, value = unpack(2)
    push(symcheck(sym))
    push_ce()
    r.cont = op_special_
    r.exp = value
    return leval_


def op_special_():
    pop_ce()
    sym = pop()
    if not isinstance(r.val, Lambda):
        raise TypeError(f"expected lambda, got {r.val!r}")
    r.val.special = True
    r.env.set(sym, r.val)
    return r.cont


@spcl("trap")
def op_trap():
    (x,) = unpack(1)
    ok = T
    push_ce()
    try:
        res = leval(x, r.env)
    except:  ## pylint: disable=bare-except
        ok = EL
        t, v = sys.exc_info()[:2]
        res = f"{t.__name__}: {str(v)}"
    pop_ce()
    return go([ok, [res, EL]])


## }}}
## {{{ primitives


def unary(f):
    (x,) = unpack(1)
    return go(f(x))


def binary(f):
    x, y = unpack(2)
    return go(f(x, y))


@glbl("apply")
def op_apply():
    proc, args = unpack(2)
    if not callable(proc):
        raise TypeError(f"expected callable, got {proc!r}")
    r.argl = args
    return proc


@glbl("atom?")
def op_atom():
    def f(x):
        return T if is_atom(x) else EL

    return unary(f)


@glbl("call/cc")
@glbl("call-with-current-contination")
def op_callcc():
    (x,) = unpack(1)
    if not callable(x):
        raise TypeError(f"expected callable, got {x!r}")
    r.argl = [Continuation(r.cont), EL]
    return x


def listcheck(x):
    if isinstance(x, list):
        return x
    raise TypeError(f"expected list, got {x!r}")


@glbl("car")
def op_car():
    def f(x):
        return listcheck(x)[0]

    return unary(f)


@glbl("cdr")
def op_cdr():
    def f(x):
        if x is EL:
            return x
        return listcheck(x)[1]

    return unary(f)


@glbl("cons")
def op_cons():
    r.val = unpack(2)
    return r.cont


@glbl("div")
def op_div():
    def f(x, y):
        if isinstance(x, int) and isinstance(y, int):
            return x // y
        return x / y

    return binary(f)


@glbl("do")
def op_do():
    x = r.argl
    r.val = EL
    while x is not EL:
        r.val, x = x
    return r.cont


@glbl("eq?")
def op_eq():
    def f(x, y):
        return T if eq(x, y) else EL

    return binary(f)


@glbl("equal?")
def op_equal():
    def f(x, y):
        if not (isinstance(x, (int, float)) and isinstance(y, (int, float))):
            raise TypeError(f"expected numbers, got {x!r} {y!r}")
        return T if x == y else EL

    return binary(f)


@glbl("error")
def op_error():
    (x,) = unpack(1)
    raise LispError(x)


@glbl("eval")
def op_eval():

    args = r.argl
    if args is EL:
        raise TypeError("need at least one arg")
    x, args = args
    if args is EL:
        n_up = 0
    else:
        n_up, args = args
        if args is not EL:
            raise TypeError("too many args")

    if isinstance(x, str):
        l = []
        p = Parser(l.append)
        p.feed(x)
        p.feed(None)
        x = l[-1] if l else EL
    e = r.env
    for _ in range(n_up):
        if e is SENTINEL:
            raise ValueError(f"cannot go up {n_up} levels")
        e = e.up()
    r.exp = x
    r.env = e
    return leval_


@glbl("exit")
def op_exit():
    (x,) = unpack(1)
    if isinstance(x, int):
        raise SystemExit(x)
    r.exp = x
    r.cont = op_exit_
    return stringify_


def op_exit_():
    raise SystemExit(r.val)


@glbl("last")
def op_last():
    (x,) = unpack(1)
    ret = EL
    while x is not EL:
        ret, x = x
    r.val = ret
    return r.cont


@glbl("lt?")
def op_lt():
    def f(x, y):
        if not (isinstance(x, (int, float)) and isinstance(y, (int, float))):
            raise TypeError(f"expected numbers, got {x!r} and {y!r}")
        return T if x < y else EL

    return binary(f)


@glbl("mul")
def op_mul():
    def f(x, y):
        if not (isinstance(x, (int, float)) and isinstance(y, (int, float))):
            raise TypeError(f"expected numbers, got {x!r} and {y!r}")
        return x * y

    return binary(f)


@glbl("nand")
def op_nand():
    def f(x, y):
        if not (isinstance(x, int) and isinstance(y, int)):
            raise TypeError(f"expected integers, got {x!r} and {y!r}")
        return ~(x & y)

    return binary(f)


@glbl("null?")
def op_null():
    (x,) = unpack(1)
    r.val = T if x is EL else EL
    return r.cont


@glbl("print")
def op_print():
    args = r.argl

    if args is EL:
        print()
        r.val = EL
        return r.cont

    arg, args = args

    push(r.cont)
    push(args)
    r.exp = arg
    r.cont = op_print_
    return stringify_


def op_print_():
    args = stack.pop()

    if args is EL:
        print(r.val)
        r.val = EL
        return pop()

    print(r.val, end=" ")

    arg, args = args

    push(args)
    r.exp = arg
    r.cont = op_print_
    return stringify_


@glbl("set-car!")
def op_setcarbang():
    def f(x, y):
        listcheck(x)[0] = y

    return binary(f)


@glbl("set-cdr!")
def op_setcdrbang():
    def f(x, y):
        listcheck(x)[1] = y

    return binary(f)


@glbl("sub")
def op_sub():
    def f(x, y):
        if not (isinstance(x, (int, float)) and isinstance(y, (int, float))):
            raise TypeError(f"expected numbers, got {x!r} and {y!r}")
        return x - y

    return binary(f)


@glbl("type")
def op_type():
    def f(x):
        ## pylint: disable=too-many-return-statements
        if x is EL:
            return symbol("()")
        if x is T:
            return symbol("#t")
        if isinstance(x, list):
            return symbol("pair")
        if isinstance(x, Symbol):
            return symbol("symbol")
        if isinstance(x, int):
            return symbol("integer")
        if isinstance(x, float):
            return symbol("float")
        if isinstance(x, str):
            return symbol("string")
        if isinstance(x, Lambda):
            return symbol("lambda")
        if isinstance(x, Continuation):
            return symbol("continuation")
        if callable(x):
            return symbol("primitive")
        return symbol("opaque")

    return unary(f)


@glbl("while")
def op_while():
    (x,) = unpack(1)
    if not callable(x):
        raise TypeError(f"expected callable, got {x!r}")

    push(r.cont)
    push(x)
    push(r.env)
    r.exp = x
    r.cont = op_while_
    return leval_


def op_while_cont():
    r.env = pop()
    x = top()

    if r.val is EL:
        stack.pop()  ## x
        return stack.pop()
    stack.push(r.env)
    r.exp = x
    r.cont = op_while_
    return leval_


## }}}
## {{{ ffi


def module_ffi(args, module):
    if not args:
        raise TypeError("at least one arg required")
    sym = symcheck(args.pop(0))
    func = getattr(module, str(sym), SENTINEL)
    if func is SENTINEL:
        raise ValueError(f"function {sym!r} does not exist")
    return func(*args)


@ffi("math")
def op_ffi_math(args):
    import math  ## pylint: disable=import-outside-toplevel

    return module_ffi(args, math)


@ffi("random")
def op_ffi_random(args):
    import random  ## pylint: disable=import-outside-toplevel

    return module_ffi(args, random)


@ffi("range")
def op_ffi_range(args):
    return list(range(*args))


@ffi("shuffle")
def op_ffi_shuffle(args):
    import random  ## pylint: disable=import-outside-toplevel

    (l,) = args
    random.shuffle(l)
    return l


@ffi("time")
def op_ffi_time(args):
    import time  ## pylint: disable=import-outside-toplevel

    def f(args):
        return [tuple(arg) if isinstance(arg, list) else arg for arg in args]

    return module_ffi(f(args), time)


## }}}


if __name__ == "__main__":
    main()


## EOF
