var cp, fishLib, ref$, brightRed, warn, expectToEqual, config, expectParseError, chomp, runSig, this$ = this, out$ = typeof exports != 'undefined' && exports || this;
cp = require('child_process');
ref$ = fishLib = require('fish-lib'), brightRed = ref$.brightRed, warn = ref$.warn;
expectToEqual = require('./common').expectToEqual;
config = {
  runCmd: __dirname + '/../hsig'
};
expectParseError = expectToEqual('bad parse');
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
      return 'bad parse';
    }
    warn('Error:', brightRed(stderr));
    return 'ERROR';
  }
  return chomp(stdout.toString());
};
describe('dummy', function(){
  return test('dummy', function(){});
});
out$.runSig = runSig;
out$.expectParseError = expectParseError;