var map, list, test, xtest, toEqual, toBe, expectToEqual, expectToBe, zipAll, out$ = typeof exports != 'undefined' && exports || this;
map = require('ramda').map;
list = function(){
  var i$, x$, len$, results$ = [];
  for (i$ = 0, len$ = (arguments).length; i$ < len$; ++i$) {
    x$ = (arguments)[i$];
    results$.push(x$);
  }
  return results$;
};
test = curry$(function(desc, theTest){
  return global.it(desc, theTest);
});
xtest = curry$(function(desc, theTest){
  return global.xit(desc, theTest);
});
toEqual = curry$(function(v, o){
  return o.toEqual(v);
});
toBe = curry$(function(v, o){
  return o.toBe(v);
});
expectToEqual = curry$(function(expected, received){
  return toEqual(expected)(
  expect(
  received));
});
expectToBe = curry$(function(expected, received){
  return toBe(expected)(
  expect(
  received));
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
describe('dummy', function(){
  return test('dummy', function(){});
});
out$.list = list;
out$.test = test;
out$.xtest = xtest;
out$.zipAll = zipAll;
out$.expectToEqual = expectToEqual;
out$.expectToBe = expectToBe;
function curry$(f, bound){
  var context,
  _curry = function(args) {
    return f.length > 1 ? function(){
      var params = args ? args.concat() : [];
      context = bound ? context || this : this;
      return params.push.apply(params, arguments) <
          f.length && arguments.length ?
        _curry.call(context, params) : f.apply(context, params);
    } : f;
  };
  return _curry();
}