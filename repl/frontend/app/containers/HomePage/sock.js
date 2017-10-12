defineBinaryOperator ('|', (a, b) => b (a))
defineBinaryOperator ('>>', (a, b) => (...args) => b (a (...args)))

import {
    isEmpty, tap, has, hasIn, flip, fromPairs, toPairs, assoc, assocPath, head,
    tail, reduceRight, chain, identity, reduce, map, filter, reject, join,
    split, prop as rProp, path as rPath, defaultTo as rDefaultTo, curry,
    forEach as each, forEachObjIndexed as eachObj, complement, times as rTimes,
    range as rRange, isNil, addIndex, take, equals, mapAccum,
    repeat as rRepeat,
    append as rAppend,
    concat as rConcat,
    zip,
} from 'ramda'

import {
  invoke, pass1, sprintfN, dot1, ifTrue, tapDot2,
} from 'stick'

const env = process.env.NODE_ENV || 'development'

const iwarn = (...args) => console.warn (...args)

const config = {
  urls: {
    local: 'ws://localhost:9191',
    production: 'ws://lazysig.mister-fish.net/ws',
  },
  url() { return this.urls [env] || iwarn ("bad env", env) },
}

const send = dot1 ('send')

const on = curry ((ev, f, ws) => ws
  | tapDot2 ('addEventListener') (ev, f)
)

export const init = ({ onInit, onRecv, onClose, }) => {
  const client = {
    ready: false,
  }

  const ws = new WebSocket (config.url ())
  | on ('open', () => {
    client.ready = true
    onInit ()
  })
  | on ('message', ({ data, }) => {
    if (!data) return console.warn ('no data')
    onRecv (data)
  })
  | on ('close', () => {
    client.ready = false
    onClose ()
  })

  const sendMsg = (data) => client.ready | ifTrue
    (_ => ws | send (data))
    (_ => console.warn ('socket not ready, ignoring msg'))

  return {
    send: sendMsg,
  }
}

export default {
  init,
}
