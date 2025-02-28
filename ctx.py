#!/usr/bin/env python3

"ctx.py"

## pylint: disable=invalid-name
## XXX pylint: disable=missing-docstring

import locale
import os
import sys
import traceback


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
    def __init__(self):
        ## registers
        self.argl = self.cont = self.env = self.exp = self.val = EL

        ## stack
        self.s = EL

        self.symbol = SymbolTable().symbol

        self.g = self.environment(EL, EL, SENTINEL)
        for k, v in GLOBALS.items():
            self.g.set(self.symbol(k), v)

    ## {{{ factories

    def continuation(self, cont):
        return Continuation(self.save(), cont)

    def environment(self, params, args, parent):
        return Environment(self, params, args, parent)

    ## }}}
    ## {{{ state mgt

    def save(self):
        return self.s, self.argl, self.cont, self.env, self.exp, self.val

    def restore(self, x):
        self.s, self.argl, self.cont, self.env, self.exp, self.val = x

    ## }}}
    ## {{{ stack

    def pop(self):
        ret, self.s = self.s
        return ret

    def pop_ce(self):
        self.r.env, s = self.s
        self.r.cont, self.s = s

    def push(self, x):
        self.s = [x, self.s]

    def push_ce(self):
        self.s = [self.r.cont, [self.r.env, self.s]]

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


## }}}
## {{{ special forms


@spcl("define")
def op_define(ctx):
    sym, value = ctx.unpack(2)
    if not isinstance(sym, Symbol):
        raise SyntaxError(f"expected symbol, got {sym!r}")
    ctx.push_ce()
    ctx.push(sym)
    ctx.cont = k_op_define
    ctx.exp = value
    return ctx.k_leval


def k_op_define(ctx):
    sym = ctx.pop()
    ctx.pop_ce()
    ctx.env.set(sym, ctx.val)
    ctx.val = EL
    ## returning None causes trampoline to use ctx.cont


@spcl("lambda")
def op_lambda(ctx):
    params, body = ctx.unpack(2)
    ctx.val = Lambda(params, body, ctx.env)
    ## returning None causes trampoline to use ctx.cont


## }}}
