/*
 * HomePage
 *
 * This is the first thing users see of our App, at the '/' route
 */

defineBinaryOperator ('|', (a, b) => b (a))
defineBinaryOperator ('>>', curry ((a, b) => compose (b, a)))
defineBinaryOperator ('<<', curry ((a, b) => compose (a, b)))

import {
  map, curry, compose, join, split, addIndex, identity, prop,
  path, T, F, multiply,
} from 'ramda'

const mapX = map | addIndex

import {
  sprintfN, xReplace, ifNo, whenOk, ifOk, ifYes, blush, whenYes,
  compact, match, cond, guard, otherwise, xMatch, appendTo,
  concatFrom, concatTo,
  ifTrue,
  ifPredicate, gt, noop, whenPredicate, whenTrue,
  assocMut,
  mergeFromMut,
} from 'stick'

import { init as sockInit, } from './sock'

import React from 'react';
import PropTypes from 'prop-types';

import LoadingIndicator from 'components/LoadingIndicator';

import styled from 'styled-components';

const config = {
  socketWait: 500,
  demoSrc: require('../../movies/demo.webm'),
}

const spinner = () => <LoadingIndicator/>

const desc = getDesc ()

const Section = styled.section`
  height: 100%;
`

const Article = styled.article`
  height: 100%;
`

const BlahDiv = styled.div`
`

const All = ({ ready, children }) => <AllS
  ready={ready}
  >
  <div style={{
    height: '100%',
    display: ready ? '' : 'none',
  }}>
    {children}
  </div>
  <div style={{
    height: '100%',
    display: ready ? 'none' : '',
  }}>
    {spinner ()}
  </div>
</AllS>

const AllS = styled.div`
  display: flex;
  flex-direction: column;
  height: 100%;
`

const Item = ({ children, onClick, }) => <div
  onClick={_ => onClick (children)}
>
 <ItemS>
  <ItemInnerS>
    {children}
  </ItemInnerS>
 </ItemS>
</div>

const TooltipS = styled.div`
  background: yellow;
  position: absolute;
  padding: 7px;
  top: 0px;
  right: ${prop ('idx') >> Number >> ifPredicate (-1 | gt) (multiply (10) >> String >> concatFrom ('%')) ('-12%' | blush)};
  opacity: ${prop ('idx') >> Number >> ifPredicate (-1 | gt) (1 | blush) (0 | blush)};
  transition: right 300ms, opacity 300ms;
`

const ItemS = styled.div`
  font-family: 'Roboto Mono', monospace;
  padding: 10px;
  background: black;
  color: darkkhaki;
`

const ItemInnerS = styled.div`
  display: inline-block;
  border-bottom: 1px solid black;
  &:hover {
    cursor: pointer;
    // darksalmon
    border-bottom: 1px solid crimson;
  }
`

const P = _ => <PS/>

const PS = styled.div`
  height: 8px;
`

const H4 = ({ children }) =>
  <H4S>
    <H4Inner>
      { children }
    </H4Inner>
  </H4S>

const H3 = ({ children }) =>
  <H3S>
    <H3Inner>
      { children }
    </H3Inner>
  </H3S>

const H2 = ({ children }) =>
  <H2S>
    <H2Inner>
      { children }
    </H2Inner>
  </H2S>

const H4S = styled.div`
  font-size: 1em;
  margin-bottom: 5px;
`

const H2S = styled.div`
  font-size: 1.5em;
  margin-bottom: 5px;
  display: flex;
  text-decoration: underline;
  justify-content: center;
`

const H4Inner = styled.span`
`

const H2Inner = styled.span`
`

const H3Inner = styled.span`
  border-bottom: 1px dotted black;
`

const H3S = styled.div`
  font-size: 1.2em;
  margin-bottom: 5px;
  &:before {
    content: ''
  }
`

const Comment = styled.div`
  font-style: italic;
  padding-left: 10px;
`

const DescS = styled.div`
  margin-top: 20px;
  height: 60%;
  overflow-y: scroll;
  padding: 10px;
  border-top: 2px solid black;
`

const DescInnerS = styled.div`
  width: 75%;
  margin-left: auto;
  margin-right: auto;
`

const ParsedS = styled.div`
  font-size: 1.3em;
  font-family: 'Roboto Mono', monospace;
  border: ${prop ('hasText') >> ifTrue
    ('2px solid black' | blush) ('0px' | blush)};
  padding: 30px;
  margin-top: 49px;
  min-height: 120px;
  cursor: not-allowed;
`

const ReplS = styled.div`
  font-size: 1.1em;
  margin-top: 20px;
  padding: 0px;
  font-family: 'Roboto Mono', monospace;
`

const ReplWrapper = styled.div`
  height: 31%;
  min-height: 270px;
`

const SourceS = styled.div`
  text-align: center;
  a {
    font-size: 14px;
    margin-left: 10px;
    margin-right: 10px;
    color: maroon;
    text-decoration: none;
    &:hover {
      border-bottom: 1px solid maroon;
    }
  }
`

const SourceA = ({ a, b, c }) => <a
  href={ "https://github.com/misterfish/" + a }
>
  misterfish / {a}
</a>

const Source = () => <SourceS>
  <H2>Source</H2>
  <SourceA a="lazysig"/>
  <SourceA a="lazysig.vim"/>
</SourceS>

const convertLinks = xReplace (/ \[\[ (.+?) \| (.+?) \]\] /g)
((_, text, href) => [text, href] | sprintfN ('<a href=\'%s\'>%s</a>'))

const Desc = ({
  contents = [], onScroll, showTooltip = false,
  onClickItem, onClickDemo, demoExpanded,
}) => {
  let tooltipIdx = -2 // --- skip first item.
  const tooltips = [
    'or this', 'or this', 'or this', 'etc.',
  ]
  const numTooltips = tooltips.length;
  const tooltipText = tooltips | map (concatTo ('🡐 '))

  return <DescS
    onScroll={onScroll}
  >
    <Source/>

    <Demo
      expanded={demoExpanded}
      onClick={onClickDemo}
    />

    <DescInnerS>
      {
        contents | mapX (([component, textArg], idx) => {
          const props = {
            key: idx,
            ... (component === Item ? { onClick: onClickItem } : {}),
          }

          const text = textArg | convertLinks

          if (component == Item) ++tooltipIdx

          return <div key={idx} style={{position: 'relative'}}>
            { React.createElement(component, props, text) }
            { /*(component === Item && tooltipIdx >= 0 && tooltipIdx < numTooltips) | whenTrue (
              _ => <TooltipS idx={showTooltip ? (numTooltips - tooltipIdx) : -1}>
                {tooltipText [tooltipIdx]}
              </TooltipS>
            )*/}
          </div>
        })
      }
    </DescInnerS>
  </DescS>
}

const InputS = styled.input`
  border: 1px solid blue;
  opacity: ${prop ('isDefaultVal') >> ifTrue (0.5 | blush) (1 | blush)};
  padding: 30px;
  width: 100%;
`

const DemoS = styled.div`
  height: ${prop ('expanded') >> ifTrue ('580px' | blush) ('200px' | blush)};
  width: ${prop ('expanded') >> ifTrue ('580px' | blush) ('200px' | blush)};
  transition: height 600ms, width 600ms;
  margin: auto;
`

const DemoInnerS = styled.video`
  width: 100%;
  height: 100%;
  cursor: pointer;
`

const Demo = ({ expanded, onClick, }) =>
  <DemoS expanded={expanded}>
    <DemoInnerS
      onClick={onClick}
      src={config.demoSrc}
      autoPlay="1"
      loop="1"
      controls="1"
    ></DemoInnerS>
  </DemoS>

class Input extends React.Component {
  constructor (props) {
    super (props)
    const { defaultValue, linkText, onChange, } = this.props
    this.state = {
      value: defaultValue,
      isDefaultVal: true,
    }
    this.onFocus = isDefaultVal => e => isDefaultVal
      | whenYes (_ => e.target.value = '')

    this.onChange = (value) => {
        // --- causes receive props / render.
        onChange ()
        this.setState ({ value, isDefaultVal: false, })
        socketClient.send (value)
      }
  }

  componentWillReceiveProps ({ linkText }) {
    linkText | whenOk (x => {
      this.setState ({ value: x })
      this.onChange (x)
    })
  }

  render () {
    const { isDefaultVal, } = this.state
    const { onFocus } = this

    return <InputS
      type='text'
      onFocus={onFocus (isDefaultVal)}
      isDefaultVal={isDefaultVal}
      value={this.state.value}
      onChange={({ target: { value }}) => this.onChange (value)}
    />
  }
}

const Repl = (props) => {
    const {
        init = 'type here',
        onChange,
        linkText,
    } = props

    return <ReplS>
      <Input
        defaultValue={init}
        onChange={onChange}
        linkText={linkText}
      />
    </ReplS>
}

const Parsed = ({ contents = '' }) =>
  <ParsedS
    hasText={contents.length | Boolean}
  >
    { contents
      | split ('\n')
      | mapX ((p, idx) => <div key={idx}>{p}</div>)
    }
  </ParsedS>

const formatDesc = split ('\n') >> map ((line) =>
    line.trim () | cond ([
      xMatch (/ ^ h2\. \s+ (.+) /) | guard ((x, m) => [H2, m[1]]),
      xMatch (/ ^ h3\. \s+ (.+) /) | guard ((x, m) => [H3, m[1]]),
      xMatch (/ ^ h4\. \s+ (.+) /) | guard ((x, m) => [H4, m[1]]),
      xMatch (/ ^ # /) | guard ((x) => ([Comment, x])),
      xMatch (/ ^ $ /) | guard (appendTo ([P])),
//       xMatch (/ ^ item\. \s+ (.+) /) | guard ((x, m) => [Item1, m[1]]),
      otherwise | guard (appendTo ([Item])),
    ])
)

const socketClient = {
  onInit: void 8,
  onRecv: void 8,
  onClose: void 8,
  send: () => console.warn ('socket not ready'),

  initClient ({ onInit, onRecv, onClose, }) {
    this | mergeFromMut ({ onInit, onRecv, onClose, })
  },

  initConnection () {
    const { onInit, onRecv, onClose, } = this
    const { send, } = sockInit ({ onInit, onRecv, onClose, })
    this | mergeFromMut ({ send, })
  },
}

export class HomePage extends React.PureComponent {
  constructor () {
    super ()
    this.state = {
      socketReady: undefined,
      parsed: undefined,
      isDefaultVal: true,
      showTooltip: false,
      linkText: undefined,
      demoExpanded: false,
    }
    this.desc = getDesc () | formatDesc

    const onInit = () => {
      this.setState ({ socketReady: true, })
    }
    const onRecv = (parsed) => {
      this.setState ({ parsed, })
    }
    const onClose = () => {
      this.setState ({ socketReady: false, })
      setTimeout (() => socketClient.initConnection (), config.socketWait)
    }
    socketClient.initClient ({ onInit, onRecv, onClose, })
    socketClient.initConnection ()
  }

  render() {
    return (
      <Article>

        <Section>
          <All
            ready={this.state.socketReady}
          >
            <ReplWrapper>
              <Repl
                linkText={this.state.linkText}
                onChange={() => this.setState ({
                  isDefaultVal: false,
                  linkText: void 8,
                })}
              />
              <Parsed contents={this.state.parsed}/>
            </ReplWrapper>

            <Desc
              contents={this.desc}
              onClickItem={text => {
                this.setState ({ linkText: text, })
              }}
              onClickDemo={() => this.setState ({
                demoExpanded: !this.state.demoExpanded,
              })}
              demoExpanded={this.state.demoExpanded}
              onScroll={
                path (['target', 'scrollTop']) >>
                ifPredicate (200 | gt) (T) (F) >>
                (s => this.setState ({ showTooltip: s }))
              }
              showTooltip={this.state.showTooltip}
          />
          </All>
        </Section>

      </Article>
    );
  }
}

HomePage.propTypes = {
  loading: PropTypes.bool,
};

export function mapDispatchToProps(dispatch) {
  return {
    onChangeUsername: (evt) => dispatch(changeUsername(evt.target.value)),
    onSubmitForm: (evt) => {
      if (evt !== undefined && evt.preventDefault) evt.preventDefault();
      dispatch(loadRepos());
    },
  };
}

//     h4. You could integrate it with your editor so that types can be inserted or looked up on Hoogle for example. Below is an example of how it works with vim.

function getDesc () {
    return `

    h2. What is this?

    h4. lazysig is a mini-language for quickly generating Haskell-style type signatures and/or function declarations.

    h4. You can probably best get a feel for the grammar by typing out some of the examples in the REPL above.

    h4. Try typing this:
    add;i i i=a-

    h2. Examples

    h3. Terms.

    # --- known types.

    i i i
    i b j f c d s
    ls lf l.a i
    lli lllj lllld
    pii pfcd
    lpii llpii

    # --- the known types are:
    # --- i = Int, j = Integer, b = Bool, f = Float, c = Char,
    # --- d = Double, s = String, l for a list, p for a tuple.

    # --- tuples can have an optional P at the end (useful later)

    piiP

    # --- they can nest arbitrarily but only as the last element each time.

    piipijplsipii
    piipiipiiiijjpij

    # --- param types, user-defined.

    .a .b .z .O
    i .a pi.b

    # --- functions.

    (i s)
    i (i s)
    .a (i .b)

    # --- compound types, single letter.

    tm.a
    tmij
    tf.a.b
    tnpii

    # --- compound types, known aliases (I = IO, M = Maybe, E = Either).
    color;s tIs
    test;s tEspss
    charToInt;c tMi

    # --- closing T optional.

    tm.aT

    # --- but necessary when ambiguous.

    ptmiTi

    # --- arbitrary text.

    /Duck Int/

    # --- compound types, arbitrary name.

    t/Duck/ij

    # --- more.

    l(i j)
    pitmi
    ptmiTi
    pitmiipiiTi
    pitmiipiii
    tppii

    h3. Name + terms.

    # --- basic form

    add;i i i
    head;l.a .a b

    # --- use empty name to get the :: prefix

    ;i i i
    ;l.a .a b

    h3. Name + constraints + terms.

    # --- basic form.

    lt;oa;.a .a b

    # --- known constraint, single-letter param.
    # --- class name is auto-capitalised.

    duck;fa eb ic md;.a .b .c .d
    goose;ne ox ry sz;.e .x .y .z

    # --- known multi-letter constraint (must be uppercase), single-letter param

    sheep;RFa FRb;.a .b

    # --- the known constraints are:
    # --- a = Applicative, e = Eq, f = Functor, FR = Fractional, i = Integral,
    # --- m = Monad, n = Num, o = Ord, RF = RealFrac, r = Real, s = Show.

    # --- arbitrary text, auto-capitalised.

    octopus;/functor a/;.a
    octopus;/random a/;.a
    octopus;/Functor c/;.c
    octopus;/Random z/;.z
    octopus;/random a/ fb;.a .b

    h3. Constraints + terms.

    ;oa;.a .a b
    ;/random a/ fb;.a .b

    h3. Only body.

    =add;a b
    =add;a _ t

    h3. Signature & body.

    add;i i i=add;a b

    # --- name in body can be omitted when given in signature

    add;i i i=a b

    # --- generated params in body.

    add;i i i i=a-c
    add;i i i i=a0-a2

    # --- ranges can be generated when the number of args can be deduced:

    add;i i i i=a-
    add;i i i i=-d

    add;i i i i=c0-
    add;i i i i=c5-
    add;i i i i=-z10

    # --- all underscores
    add;i i i i=_-

    # --- mask all but one
    add;i i i i=0l
    add;i i i i=1m
    add;i i i i=2n

    h3. Full expression.

    # --- Finally, the full pattern:
    # --- name;constraints;terms=name;parameters
    # --- Various things are optional, and only certain combinations make sense (see above).
    # --- Some more examples:

    add;na;.a .a .a
    add;na   ea oa;.a .a .a
    p4;nx ey oz;i .x .y .z pi.x.y.z

    fmap;ff;(.a .b) tf.a tf.b

    fmap;ff;(.a .b) tf.a tf.b
    functorThing;ff;tf.t tf.a tf.b

    functorThing;ff;t/fun/.t t/fun/.a t/fun/i

    functorThing;ff;/fun t/ /fun a/ /fun b/
    functorThing;ff;/fun ttt/ /fun aaa/ /fun bbb/

    (i i i) i
    add;;i i i
    add;;f f f
    ;;.a

    # --- standard library.

    toUpper;c c
    unwords;ls s
    delete;ea;.a l.a l.a
    foldl;lt;(.b .a .b) .b tt.a .b
    tail;l.a l.a
    unfoldr;(.b tMp.a.b) .b l.a
    zip;l.a l.b lp.a.b
    intercalate;l.a ll.a l.a
    liftM2;mm;(/a1/ /a2/ .r) /m a1/ /m a2/ /m r/
    fail;mm;s tm.a
    split;/randomGen g/;.g p.g.g
    uniform;/MonadRandom m/ lt;tt.a tm.a
    replicateM;am;i tm.a tml.a
`

}

export default HomePage;
