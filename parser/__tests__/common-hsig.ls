cp = require 'child_process'

{
    bright-red, warn, ierror,
} = fish-lib = require 'fish-lib'

{
    expect-to-equal,
} = require './common'

config =
    run-cmd: __dirname + '/../lazysig'

# --- arbitrary unique object.
token = -> {}

bad-parse = token()
bad-generate = token()

bad-parse-str = 'Error: bad parse'
bad-generate-str = 'Error: bad generate'

is-string = (typeof!) >> (== 'String')

do-test = ({ inp, out, input, output, }) ->
    if is-string output then run-sig inp input |> expect-to-equal out output
    else run-sig inp input |> expect-parse-error output

_expect-parse-error = (x) ->
    | x == bad-parse => bad-parse-str
    | x == bad-generate => bad-generate-str
    | _ => ierror()

expect-parse-error = _expect-parse-error >> expect-to-equal

chomp = (.replace // \n $ // '')

# --- don't die on errors (messes up test suite).
# just warn and return.
run-sig = (sig) ->
    { run-cmd, } = config
    { stdout, stderr, status, error: err, } = cp.spawn-sync do
        run-cmd
        []
        input: sig
    # --- error calling command: bad.
    if err
        warn 'Error with cmd' (bright-red run-cmd), err
        return 'ERROR'
    # --- program error.
    if status
        # ------ bad parse, ok.
        if stderr == // bad \s+ parse // then return bad-parse-str
        # --- we throw away the reason.
        else if stderr == // bad \s+ generate // then return bad-generate-str

        # ------- unexpected error: bad.
        warn 'Error:' bright-red stderr
        return
    chomp stdout.to-string()

describe 'dummy' -> test 'dummy' ->

export
    do-test
    expect-parse-error
    bad-parse
    bad-generate
