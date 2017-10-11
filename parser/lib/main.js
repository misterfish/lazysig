var ref$, assoc, assocPath, head, tail, reduceRight, chain, identity, reduce, map, filter, join, split, rProp, rPath, rDefaultTo, curry, each, complement, isNil, rRepeat, rTimes, reverse, tap, flip, zip, fishLib, sprintf, log, list, test, xtest, expectToEqual, expectToBe, runSig, expectParseError, tests, parseTestsTerms, parseTestsConstraints, parseTestsName, zipAll, slice$ = [].slice;
ref$ = require('ramda'), assoc = ref$.assoc, assocPath = ref$.assocPath, head = ref$.head, tail = ref$.tail, reduceRight = ref$.reduceRight, chain = ref$.chain, identity = ref$.identity, reduce = ref$.reduce, map = ref$.map, filter = ref$.filter, join = ref$.join, split = ref$.split, rProp = ref$.prop, rPath = ref$.path, rDefaultTo = ref$.defaultTo, curry = ref$.curry, each = ref$.forEach, complement = ref$.complement, isNil = ref$.isNil, rRepeat = ref$.repeat, rTimes = ref$.times, reverse = ref$.reverse, tap = ref$.tap, flip = ref$.flip, zip = ref$.zip;
ref$ = fishLib = require('fish-lib'), sprintf = ref$.sprintf, log = ref$.log;
ref$ = require('./common'), list = ref$.list, test = ref$.test, xtest = ref$.xtest, expectToEqual = ref$.expectToEqual, expectToBe = ref$.expectToBe;
ref$ = require('./common-hsig'), runSig = ref$.runSig, expectParseError = ref$.expectParseError;
tests = require('./data').tests;
parseTestsTerms = tests.parseTestsTerms, parseTestsConstraints = tests.parseTestsConstraints, parseTestsName = tests.parseTestsName;
describe('terms', function(){
  return each(function(arg$){
    var input, output;
    input = arg$[0], output = arg$[1];
    return test(input, function(){
      var that;
      if ((that = output) != null) {
        return expectToEqual(that)(
        runSig(input));
      } else {
        return expectParseError(
        runSig(input));
      }
    });
  })(
  parseTestsTerms);
});
describe('constraints', function(){
  return each(function(arg$){
    var input, output;
    input = arg$[0], output = arg$[1];
    return test(input, function(){
      var inp, out, that;
      inp = function(input){
        return sprintf(";%s;i", input);
      };
      out = function(output){
        return sprintf("%s => Int", output);
      };
      if ((that = output) != null) {
        return expectToEqual(out(that))(
        runSig(inp(input)));
      } else {
        return expectParseError(
        runSig(inp(input)));
      }
    });
  })(
  parseTestsConstraints);
});
describe('name', function(){
  return each(function(arg$){
    var input, output;
    input = arg$[0], output = arg$[1];
    return test(input, function(){
      var inp, out, that;
      inp = function(input){
        return sprintf("%s;;i", input);
      };
      out = function(output){
        return sprintf("%s :: Int", output);
      };
      if ((that = output) != null) {
        return expectToEqual(out(that))(
        runSig(inp(input)));
      } else {
        return expectParseError(
        runSig(inp(input)));
      }
    });
  })(
  parseTestsName);
});
describe('full', function(){
  return each(function(arg$){
    var input, output;
    input = arg$[0], output = arg$[1];
    return test(input, function(){
      var that;
      if ((that = output) != null) {
        return expectToEqual(that)(
        runSig(input));
      } else {
        return expectParseError(
        runSig(input));
      }
    });
  })(
  parseTestsFull);
});
zipAll = function(){
  var xss, res$, i$, to$, ret, l, i;
  res$ = [];
  for (i$ = 0, to$ = arguments.length; i$ < to$; ++i$) {
    res$.push(arguments[i$]);
  }
  xss = res$;
  ret = [];
  l = xss[0].length;
  for (i$ = 0, to$ = l - 1; i$ <= to$; ++i$) {
    i = i$;
    ret.push(map(fn$)(
    xss));
  }
  return ret;
  function fn$(xs){
    return xs[i];
  }
};
if (true) {
  describe('extended-combinations', function(){
    var combo, a, i, j, k;
    combo = a = (function(){
      var i$, ref$, len$, j$, ref1$, len1$, k$, ref2$, len2$, results$ = [];
      for (i$ = 0, len$ = (ref$ = parseTestsName).length; i$ < len$; ++i$) {
        i = ref$[i$];
        for (j$ = 0, len1$ = (ref1$ = parseTestsConstraints).length; j$ < len1$; ++j$) {
          j = ref1$[j$];
          for (k$ = 0, len2$ = (ref2$ = parseTestsTerms).length; k$ < len2$; ++k$) {
            k = ref2$[k$];
            results$.push([i, j, k]);
          }
        }
      }
      return results$;
    }());
    return map(function(arg$){
      var input, output;
      input = arg$.input, output = arg$.output;
      return test(input, function(){
        if (output != null) {
          return expectToEqual(output)(
          runSig(input));
        } else {
          return expectParseError(
          runSig(input));
        }
      });
    })(
    map(function(arg$){
      var input, output;
      input = arg$[0], output = arg$[1];
      return {
        input: join(';')(
        input),
        output: in$(null, output)
          ? null
          : sprintf.apply(null, ["%s :: %s => %s"].concat(slice$.call(output)))
      };
    })(
    map(function(arg$){
      var names, constraints, terms;
      names = arg$[0], constraints = arg$[1], terms = arg$[2];
      return zipAll(names, constraints, terms);
    })(
    combo)));
  });
}
function in$(x, xs){
  var i = -1, l = xs.length >>> 0;
  while (++i < l) if (x === xs[i]) return true;
  return false;
}