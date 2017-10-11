{
    map,
} = require 'ramda'

list = -> [.. for &]

test = (desc, the-test) --> global.it desc, the-test
xtest = (desc, the-test) --> global.xit desc, the-test

# --- i.e., 'dot1', but we're not allowed to use that in tests.
to-equal = (v, o) --> o.to-equal v
to-be = (v, o) --> o.to-be v

expect-to-equal = (expected, received) -->
    received |> expect |> to-equal expected
expect-to-be = (expected, received) -->
    received |> expect |> to-be expected

zip-all = (...xss) ->
    ret = []
    l = xss.0.length
    for i from 0 to l - 1
        ret.push do
            xss |> map (xs) -> xs[i]
    ret

describe 'dummy' ->
    test 'dummy' ->

export
    list,
    test, xtest,
    zip-all,
    expect-to-equal, expect-to-be,
