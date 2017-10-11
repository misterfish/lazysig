#!/usr/bin/env node
'use strict';

Object.defineProperty(exports, "__esModule", {
    value: true
});
exports.usageF = undefined;

var _toArray2 = require('babel-runtime/helpers/toArray');

var _toArray3 = _interopRequireDefault(_toArray2);

var _ramda = require('ramda');

var _ramda2 = _interopRequireDefault(_ramda);

var _fishLib = require('fish-lib');

var _fishLib2 = _interopRequireDefault(_fishLib);

var _stick = require('stick');

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

var usageF = exports.usageF = function usageF(msg) {
    return function () {
        var _process$argv = (0, _toArray3.default)(process.argv),
            _ = _process$argv[0],
            scriptName = _process$argv[1],
            args = _process$argv.slice(2);

        var str = (0, _ramda.join)(' ', (0, _stick.compactOk)([scriptName, msg]));
        return (0, _fishLib.sprintf)("Usage: %s", str);
    };
};