var ref$, assoc, assocPath, head, tail, reduceRight, chain, identity, reduce, map, filter, join, split, rProp, rPath, rDefaultTo, curry, each, complement, isNil, rRepeat, rTimes, reverse, tap, flip, zip, fishLib, sprintf, log, list, test, xtest, expectToEqual, expectToBe, zipAll, doTest, expectParseError, tests, parseTestsTerms, parseTestsConstraints, parseTestsName, parseTestBody, parseTestsFull;
ref$ = require('ramda'), assoc = ref$.assoc, assocPath = ref$.assocPath, head = ref$.head, tail = ref$.tail, reduceRight = ref$.reduceRight, chain = ref$.chain, identity = ref$.identity, reduce = ref$.reduce, map = ref$.map, filter = ref$.filter, join = ref$.join, split = ref$.split, rProp = ref$.prop, rPath = ref$.path, rDefaultTo = ref$.defaultTo, curry = ref$.curry, each = ref$.forEach, complement = ref$.complement, isNil = ref$.isNil, rRepeat = ref$.repeat, rTimes = ref$.times, reverse = ref$.reverse, tap = ref$.tap, flip = ref$.flip, zip = ref$.zip;
ref$ = fishLib = require('fish-lib'), sprintf = ref$.sprintf, log = ref$.log;
ref$ = require('./common'), list = ref$.list, test = ref$.test, xtest = ref$.xtest, expectToEqual = ref$.expectToEqual, expectToBe = ref$.expectToBe, zipAll = ref$.zipAll;
ref$ = require('./common-hsig'), doTest = ref$.doTest, expectParseError = ref$.expectParseError;
tests = require('./data').tests;
parseTestsTerms = tests.parseTestsTerms, parseTestsConstraints = tests.parseTestsConstraints, parseTestsName = tests.parseTestsName, parseTestBody = tests.parseTestBody, parseTestsFull = tests.parseTestsFull;
describe('terms', function(){
  return each(function(arg$){
    var input, output;
    input = arg$[0], output = arg$[1];
    return test("terms: " + input, function(){
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
  parseTestsTerms);
});
describe('constraints', function(){
  return each(function(arg$){
    var input, output;
    input = arg$[0], output = arg$[1];
    return test("constraints: " + input, function(){
      var inp, out;
      inp = function(input){
        return sprintf(";%s;i", input);
      };
      out = function(output){
        return sprintf(":: %s => Int", output);
      };
      return doTest({
        inp: inp,
        out: out,
        input: input,
        output: output
      });
    });
  })(
  parseTestsConstraints);
});
describe('name', function(){
  return each(function(arg$){
    var input, output;
    input = arg$[0], output = arg$[1];
    return test("name: " + input, function(){
      var inp, out;
      inp = function(input){
        return sprintf("%s;;i", input);
      };
      out = function(output){
        return sprintf("%s :: Int", output);
      };
      return doTest({
        inp: inp,
        out: out,
        input: input,
        output: output
      });
    });
  })(
  parseTestsName);
});
describe('body', function(){
  return each(function(arg$){
    var input, output;
    input = arg$[0], output = arg$[1];
    return test("body: " + input, function(){
      var inp, out;
      inp = function(input){
        return sprintf("=%s", input);
      };
      out = identity;
      return doTest({
        inp: inp,
        out: out,
        input: input,
        output: output
      });
    });
  })(
  parseTestBody);
});
describe('full', function(){
  return each(function(arg$){
    var input, output;
    input = arg$[0], output = arg$[1];
    return test("full: " + input, function(){
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
  parseTestsFull);
});