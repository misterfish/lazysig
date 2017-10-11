var list, test, xtest, toEqual, toBe, expectToEqual, expectToBe, out$ = typeof exports != 'undefined' && exports || this;
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
describe('dummy', function(){
  return test('dummy', function(){});
});
out$.list = list;
out$.test = test;
out$.xtest = xtest;
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