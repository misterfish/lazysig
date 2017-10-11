#!/usr/bin/env node
'use strict';

var _ramda = require('ramda');

var _ramda2 = _interopRequireDefault(_ramda);

var _fishLib = require('fish-lib');

var _fishLib2 = _interopRequireDefault(_fishLib);

var _stick = require('stick');

var _express = require('express');

var _express2 = _interopRequireDefault(_express);

var _httpProxyMiddleware = require('http-proxy-middleware');

var _httpProxyMiddleware2 = _interopRequireDefault(_httpProxyMiddleware);

var _lib = require('./lib');

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

var _op = function _op(a, b) {
    return b(a);
};

var _op2 = (0, _ramda.curry)(function (a, b) {
    return (0, _ramda.compose)(b, a);
});

var _op3 = (0, _ramda.curry)(function (a, b) {
    return (0, _ramda.compose)(a, b);
});

var config = {
    defaultPort: 9191,
    usage: function usage() {
        return _op(this.defaultPort, (0, _stick.sprintf1)('[-p port=%s]'));
    }
};

var usage = (0, _lib.usageF)(config.usage());

var opt = (0, _fishLib.getopt)({
    h: 'b',
    p: 's'
});

if (opt.h) {
    (0, _fishLib.info)(usage());
    process.exit(0);
}

if (opt.argv.remain.length) (0, _fishLib.error)(usage());

var port = _op(opt.p, (0, _stick.ifOk)(Number)(_op(config.defaultPort, _stick.blush)));

var use = (0, _stick.dot1)('use');
var on = (0, _stick.dot2)('on');
var listen = (0, _stick.dot1)('listen');

var wsProxy = (0, _httpProxyMiddleware2.default)('ws://127.0.0.1:9160', {
    changeOrigin: true,
    logLevel: 'debug',

    onError: function onError(err, req, res) {
        console.log('error, err', err);
    },
    onProxyReq: function onProxyReq(proxyReq, req, res) {
        console.log('proxyReq', proxyReq);
        console.log('proxyReq.headers', proxyReq.headers);
        console.log('req.headers', req.headers);
        console.log('res.headers', res.headers);
    },
    onProxyRes: function onProxyRes(proxyRes, req, res) {
        console.log('proxyRes.headers', proxyRes.headers);
    }
});

var upgrade = wsProxy.upgrade;


_op(_op(_op(port, _fishLib.cyan), (0, _stick.sprintf1)("Listening on port %s")), _fishLib.info);

var app = _op(_op(_op((0, _express2.default)(), use(wsProxy)), listen(port)), on('upgrade')(upgrade));