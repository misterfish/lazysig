var cp, fishLib, ref$, brightRed, warn, ierror, expectToEqual, config, token, badParse, badGenerate, badParseStr, badGenerateStr, isString, doTest, _expectParseError, expectParseError, chomp, runSig, toString$ = {}.toString, this$ = this, out$ = typeof exports != 'undefined' && exports || this;
cp = require('child_process');
ref$ = fishLib = require('fish-lib'), brightRed = ref$.brightRed, warn = ref$.warn, ierror = ref$.ierror;
expectToEqual = require('./common').expectToEqual;
config = {
  runCmd: __dirname + '/../lazysig'
};
token = function(){
  return {};
};
badParse = token();
badGenerate = token();
badParseStr = 'Error: bad parse';
badGenerateStr = 'Error: bad generate';
isString = compose$((function(it){
  return toString$.call(it).slice(8, -1);
}), (function(it){
  return it === 'String';
}));
doTest = function(arg$){
  var inp, out, input, output;
  inp = arg$.inp, out = arg$.out, input = arg$.input, output = arg$.output;
  if (isString(output)) {
    return expectToEqual(out(output))(
    runSig(inp(input)));
  } else {
    return expectParseError(output)(
    runSig(inp(input)));
  }
};
_expectParseError = function(x){
  switch (false) {
  case x !== badParse:
    return badParseStr;
  case x !== badGenerate:
    return badGenerateStr;
  default:
    return ierror();
  }
};
expectParseError = compose$(_expectParseError, expectToEqual);
chomp = function(it){
  return it.replace(/\n$/, '');
};
runSig = function(sig){
  var runCmd, ref$, stdout, stderr, status, err;
  runCmd = config.runCmd;
  ref$ = cp.spawnSync(runCmd, [], {
    input: sig
  }), stdout = ref$.stdout, stderr = ref$.stderr, status = ref$.status, err = ref$.error;
  if (err) {
    warn('Error with cmd', brightRed(runCmd), err);
    return 'ERROR';
  }
  if (status) {
    if (/bad\s+parse/.exec(stderr)) {
      return badParseStr;
    } else if (/bad\s+generate/.exec(stderr)) {
      return badGenerateStr;
    }
    warn('Error:', brightRed(stderr));
    return;
  }
  return chomp(stdout.toString());
};
describe('dummy', function(){
  return test('dummy', function(){});
});
out$.doTest = doTest;
out$.expectParseError = expectParseError;
out$.badParse = badParse;
out$.badGenerate = badGenerate;
function compose$() {
  var functions = arguments;
  return function() {
    var i, result;
    result = functions[0].apply(this, arguments);
    for (i = 1; i < functions.length; ++i) {
      result = functions[i](result);
    }
    return result;
  };
}