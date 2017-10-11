#!/usr/bin/env node

defineBinaryOperator ('|', (a, b) => b (a))
defineBinaryOperator ('>>', curry ((a, b) => compose (b, a)))
defineBinaryOperator ('<<', curry ((a, b) => compose (a, b)))

import ramda, {
    isEmpty, tap, has, hasIn, flip, fromPairs, toPairs, assoc, assocPath, head,
    tail, reduceRight, chain, identity, reduce, map, filter, reject, join,
    split, prop, curry, zip, contains,
    forEach as each, forEachObjIndexed as eachObj, complement,
    isNil, addIndex, take, equals, mapAccum, compose, append, concat,
    repeat as rRepeat, times as rTimes,
} from 'ramda'

import fishLib, {
    log, info, warn, error, green, yellow, magenta, brightRed, cyan, brightBlue,
    sprintf, forceColors, getopt,
} from 'fish-lib'

import {
    ok, ifOk, dot, dot1, dot2, ifTrue, cond, whenOk, appendFrom, pass1, sprintf1, sprintfN, times,
    noop, condEquals, concatTo, guard, otherwise, doe, ifPredicate, applyN, appendTo,
    laat, laatDat, laatStar, gt, eq, applyScalar, assocMut,
    laatStarDat, blush,
    ifEmpty, range, rangeBy,
} from 'stick'

import express from 'express'
import proxy from 'http-proxy-middleware'

import { usageF, } from './lib'

const config = {
    defaultPort: 9191,
    usage() { return this.defaultPort | sprintf1 ('[-p port=%s]') },
}

const usage = usageF (config.usage ())

const opt = getopt ({
    h: 'b',
    p: 's',
})

if (opt.h) {
    info (usage ())
    process.exit (0)
}

if (opt.argv.remain.length) error (usage ())

const port = opt.p | ifOk (Number) (config.defaultPort | blush)

const use = dot1 ('use')
const on = dot2 ('on')
const listen = dot1 ('listen')

const wsProxy = proxy ('ws://127.0.0.1:9160', {
    changeOrigin: true,
    logLevel: 'debug',

    onError: (err, req, res) => {
        console.log ('error, err', err)
    },
    onProxyReq: (proxyReq, req, res) => {
        console.log ('proxyReq', proxyReq)
        console.log ('proxyReq.headers', proxyReq.headers)
        console.log ('req.headers', req.headers)
        console.log ('res.headers', res.headers)
    },
    onProxyRes: (proxyRes, req, res) => {
        console.log ('proxyRes.headers', proxyRes.headers)
    },
})

const { upgrade } = wsProxy

port
| cyan
| sprintf1 ("Listening on port %s")
| info

const app = express ()
| use (wsProxy)
| listen (port)
| on ('upgrade') (upgrade)
