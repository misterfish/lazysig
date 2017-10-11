var ref$, assoc, assocPath, head, tail, reduceRight, chain, identity, reduce, map, filter, join, split, rProp, rPath, rDefaultTo, curry, each, complement, isNil, rRepeat, rTimes, reverse, tap, flip, zip, fishLib, sprintf, log, list, test, xtest, expectToEqual, expectToBe, zipAll, doTest, expectParseError, badParse, badGenerate, tests, parseTestsTerms, parseTestsConstraints, parseTestsName, slice$ = [].slice;
ref$ = require('ramda'), assoc = ref$.assoc, assocPath = ref$.assocPath, head = ref$.head, tail = ref$.tail, reduceRight = ref$.reduceRight, chain = ref$.chain, identity = ref$.identity, reduce = ref$.reduce, map = ref$.map, filter = ref$.filter, join = ref$.join, split = ref$.split, rProp = ref$.prop, rPath = ref$.path, rDefaultTo = ref$.defaultTo, curry = ref$.curry, each = ref$.forEach, complement = ref$.complement, isNil = ref$.isNil, rRepeat = ref$.repeat, rTimes = ref$.times, reverse = ref$.reverse, tap = ref$.tap, flip = ref$.flip, zip = ref$.zip;
ref$ = fishLib = require('fish-lib'), sprintf = ref$.sprintf, log = ref$.log;
ref$ = require('./common'), list = ref$.list, test = ref$.test, xtest = ref$.xtest, expectToEqual = ref$.expectToEqual, expectToBe = ref$.expectToBe, zipAll = ref$.zipAll;
ref$ = require('./common-hsig'), doTest = ref$.doTest, expectParseError = ref$.expectParseError, badParse = ref$.badParse, badGenerate = ref$.badGenerate;
tests = require('./data').tests;
parseTestsTerms = tests.parseTestsTerms, parseTestsConstraints = tests.parseTestsConstraints, parseTestsName = tests.parseTestsName;
describe('signature-combinations', function(){
  var i, j, k;
  return map(function(arg$){
    var input, output;
    input = arg$.input, output = arg$.output;
    return test("signature-combinations: " + input, function(){
      var inp, out;
      inp = identity;
      out = identity;
      return doTest({
        inp: inp,
        out: out,
        input: input,
        output: output
      });
    });
  })(
  map(function(arg$){
    var input, output;
    input = arg$[0], output = arg$[1];
    return {
      input: join(';')(
      input),
      output: in$(badParse, output)
        ? badParse
        : in$(badGenerate, output)
          ? badGenerate
          : sprintf.apply(null, ["%s :: %s => %s"].concat(slice$.call(output)))
    };
  })(
  map(function(arg$){
    var names, constraints, terms;
    names = arg$[0], constraints = arg$[1], terms = arg$[2];
    return zipAll(names, constraints, terms);
  })(
  (function(){
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
  }()))));
});
function in$(x, xs){
  var i = -1, l = xs.length >>> 0;
  while (++i < l) if (x === xs[i]) return true;
  return false;
}