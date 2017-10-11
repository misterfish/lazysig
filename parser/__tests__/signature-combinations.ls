# --- recursive grouping of ps allowed as last element.
# --- (non-last not syntactically possible)
# --- only single letters possible in anon terms -- for more, use literals.

{
    assoc, assocPath, head, tail, reduceRight, chain, identity, reduce, map, filter, join, split, prop: rProp, path: rPath, defaultTo: rDefaultTo, curry, forEach: each, complement, isNil,
    repeat: rRepeat,
    times: r-times,
    reverse,
    tap,
    flip,
    zip,
} = require 'ramda'

{
    sprintf, log,
} = fish-lib = require 'fish-lib'

{
    list,
    test, xtest,
    expect-to-equal, expect-to-be,
    zip-all,
} = require './common'

{
    do-test,
    expect-parse-error,
    bad-parse, bad-generate
} = require './common-hsig'

{
    tests,
} = require './data'

{
    parse-tests-terms, parse-tests-constraints, parse-tests-name,
} = tests

# --- all combinations of names, constraints, and terms.
# --- (easily grows to 1000s)
# --- automatic combinations with body are not useful.
describe 'signature-combinations' ->
    [[i,j,k] for i in parse-tests-name for j in parse-tests-constraints for k in parse-tests-terms]
    |> map ([names, constraints, terms]) ->
        zip-all names, constraints, terms
    |> map ([input, output]) ->
        input: input |> join ';'
        output:
            if bad-parse in output then bad-parse
            else if bad-generate in output then bad-generate
            else sprintf "%s :: %s => %s" ...output
    |> map ({ input, output }) ->
        test "signature-combinations: #input" ->
            inp = identity
            out = identity
            do-test inp: inp, out: out, input: input, output: output

