# --- recursive grouping of ps allowed as last element.
# --- (non-last not syntactically possible)
# --- only single letters possible in anon terms -- for more, use literals.

{
    assoc, assocPath, head, tail, reduceRight, chain, identity, reduce, map, filter, join, split, prop: rProp, path: rPath, defaultTo: rDefaultTo, curry, forEach: each, complement, isNil,
    repeat: r-repeat,
    times: r-times,
    reverse, tap, flip, zip,
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
} = require './common-hsig'

{
    tests,
} = require './data'

{
    parse-tests-terms, parse-tests-constraints, parse-tests-name, parse-test-body,
    parse-tests-full,
} = tests

describe 'terms' ->
    parse-tests-terms |> each ([input, output]) ->
        test "terms: #input" ->
            inp = identity
            out = identity
            do-test inp: inp, out: out, input: input, output: output

describe 'constraints' ->
    parse-tests-constraints |> each ([input, output]) ->
        test "constraints: #input" ->
            inp = (input) -> sprintf ";%s;i" input
            out = (output) -> sprintf ":: %s => Int" output
            do-test inp: inp, out: out, input: input, output: output

describe 'name' ->
    parse-tests-name |> each ([input, output]) ->
        test "name: #input" ->
            inp = (input) -> sprintf "%s;;i" input
            out = (output) -> sprintf "%s :: Int" output
            do-test inp: inp, out: out, input: input, output: output

describe 'body' ->
    parse-test-body |> each ([input, output]) ->
        test "body: #input" ->
            inp = (input) -> sprintf "=%s" input
            out = identity
            do-test inp: inp, out: out, input: input, output: output

describe 'full' ->
    parse-tests-full |> each ([input, output]) ->
        test "full: #input" ->
            inp = identity
            out = identity
            do-test inp: inp, out: out, input: input, output: output
