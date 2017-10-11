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
  invoke, pass1,
} from 'stick'

const config = {
    url: 'ws://localhost:9160',
}

const on = curry ((ev, f, ws) => {
  ws.addEventListener (ev, f)
  return ws
})

export const init = ({ onInit, onRecv, onClose, }) => {
  const client = {
    ready: false,
  }

  const ws = new WebSocket (config.url)
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

  const send = (data) => {
	if (!client.ready) return console.warn ('socket not ready, ignoring msg')
	return ws.send (data)
  }

  return {
    send,
  }
}

export default {
  init,
}
