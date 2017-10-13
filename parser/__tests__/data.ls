{
    list,
} = require './common'

{
    bad-parse, bad-generate,
} = require './common-hsig'

tests =
    # ------ non-empty.
    parse-tests-name: list do
        ['add' 'add']
        ["sortList'" "sortList'"]

    parse-tests-constraints: list do
        ['ma RFb' '(Monad a, RealFrac b)']
        ['ma    RFb' '(Monad a, RealFrac b)']
        ['FRa' 'Fractional a']
        ['RFa' 'RealFrac a']

        ['fa' 'Functor a']
        ['na nb' '(Num a, Num b)']
        ['ea' 'Eq a']
        ['fb' 'Functor b']
        ['ic' 'Integral c']
        ['md' 'Monad d']
        ['ne' 'Num e']
        ['oa' 'Ord a']
        ['ra' 'Real a']
        ['sa' 'Show a']
        ['sz' 'Show z']
        ['qa' bad-parse]

        ['/functor a/' 'Functor a']
        ['/random a/' 'Random a']
        ['/Functor a/' 'Functor a']
        ['/Random z/' 'Random z']
        ['/random a/ fb' '(Random a, Functor b)']

        ['FRa RFb ec fd ie' '(Fractional a, RealFrac b, Eq c, Functor d, Integral e)']
        ['ma nb oc rd se' '(Monad a, Num b, Ord c, Real d, Show e)']

    parse-tests-terms: list do
        ['.a' 'a']
        ['.b' 'b']
        ['.c' 'c']
        ['.d' 'd']
        ['.z' 'z']

        ['i i i' 'Int -> Int -> Int']
        ['b i b' 'Bool -> Int -> Bool']
        ['j d c' 'Integer -> Double -> Char']
        ['i i j' 'Int -> Int -> Integer']
        ['d i j' 'Double -> Int -> Integer']
        ['d f' 'Double -> Float']
        ['s s' 'String -> String']

        ['pii' '(Int, Int)']
        ['piij' '(Int, Int, Integer)']
        ['pijf' '(Int, Integer, Float)']
        ['i piijj.a.b', 'Int -> (Int, Int, Integer, Integer, a, b)']
        ['p.x.y.zf', '(x, y, z, Float)']

        ['tm.a' 'm a']
        ['tm.aT' 'm a']
        ['tm.a.b' 'm a b']
        ['tm.a.bT' 'm a b']
        ['tmi.aj.b' 'm Int a Integer b']
        ['tmi.aj.bT' 'm Int a Integer b']
        ['tf.t' 'f t']
        ['tmpssPpss' 'm (String, String) (String, String)']

        ['tI.a' 'IO a']
        ['tM.a' 'Maybe a']
        ['tE.a' 'Either a']
        ['tIs' 'IO String']
        ['tMpbb' 'Maybe (Bool, Bool)']
        ['tEsls' 'Either String [String]']
        ['tEslls' 'Either String [[String]]']

        ['li' '[Int]']
        ['lj' '[Integer]']
        ['l.a' '[a]']
        ['ltmi' '[m Int]']
        ['ltm.a' '[m a]']
        ['llli' '[[[Int]]]']
        ['lpis' '[(Int, String)]']
        ['llpis' '[[(Int, String)]]']
        ['llpi(i j)' '[[(Int, (Int -> Integer))]]']

        ['pi(i j)' '(Int, (Int -> Integer))']
        ['l(i j)' '[(Int -> Integer)]']

        ['pitmi' '(Int, m Int)']
        ['ptmiTi' '(m Int, Int)']

        ['pitmiipiiTi' '(Int, m Int Int (Int, Int), Int)']
        ['pitmiipiii' '(Int, m Int Int (Int, Int, Int))']
        ['tppii' 'p (Int, Int)']

        ['t/Either/ij' 'Either Int Integer']
        ['t/Either/ij j' 'Either Int Integer -> Integer']
        ['t/Either/ijT' 'Either Int Integer']
        ['t/Maybe/i' 'Maybe Int']
        ['/Maybe Int/' 'Maybe Int']

        ['piipiipiiiijjpij' '(Int, Int, (Int, Int, (Int, Int, Int, Int, Integer, Integer, (Int, Integer))))']

        ['...asdf' bad-parse]

    parse-test-body: list do
        ['add;_ _ _ _' 'add _ _ _ _ = ']
        ['add;o q z' 'add o q z = ']
        ['add;a-' bad-generate]
        ['add;-z' bad-generate]
        ['add;a0-' bad-generate]
        ['add;-a10' bad-generate]
    # /------

    # ------ can include empty.
    parse-tests-full: list do
        ['add;na;.a .a .a' 'add :: Num a => a -> a -> a']
        ['add;na   ea oa;.a .a .a' 'add :: (Num a, Eq a, Ord a) => a -> a -> a']
        ['p4;nx ey oz;i .x .y .z pi.x.y.z' 'p4 :: (Num x, Eq y, Ord z) => Int -> x -> y -> z -> (Int, x, y, z)']

        ['fmap;ff;(.a .b) tf.a tf.b', 'fmap :: Functor f => (a -> b) -> f a -> f b']

        ['fmap;ff;(.a .b) tf.a tf.b', 'fmap :: Functor f => (a -> b) -> f a -> f b']
        ['functorThing;ff;tf.t tf.a tf.b', 'functorThing :: Functor f => f t -> f a -> f b']

        ['functorThing;ff;t/fun/.t t/fun/.a t/fun/i', 'functorThing :: Functor f => fun t -> fun a -> fun Int']

        ['functorThing;ff;/fun t/ /fun a/ /fun b/', 'functorThing :: Functor f => fun t -> fun a -> fun b']
        ['functorThing;ff;/fun ttt/ /fun aaa/ /fun bbb/', 'functorThing :: Functor f => fun ttt -> fun aaa -> fun bbb']

        ['(i i i) i', '(Int -> Int -> Int) -> Int']
        ['add;;i i i' 'add :: Int -> Int -> Int']
        ['add;;f f f' 'add :: Float -> Float -> Float']
        [';;.a' ':: a']

        ['toUpper;c c' 'toUpper :: Char -> Char']
        ['unwords;ls s' 'unwords :: [String] -> String']

        ['delete;ea;.a l.a l.a' 'delete :: Eq a => a -> [a] -> [a]']

        ['foldl;lt;(.b .a .b) .b tt.a .b' 'foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b']

        ['tail;l.a l.a' 'tail :: [a] -> [a]']
        ['unfoldr;(.b t/Maybe/p.a.b) .b l.a' 'unfoldr :: (b -> Maybe (a, b)) -> b -> [a]']
        ['zip;l.a l.b lp.a.b' 'zip :: [a] -> [b] -> [(a, b)]']
        ['intercalate;l.a ll.a l.a' 'intercalate :: [a] -> [[a]] -> [a]']

        ['liftM2;mm;(/a1/ /a2/ .r) /m a1/ /m a2/ /m r/'  'liftM2 :: Monad m => (a1 -> a2 -> r) -> m a1 -> m a2 -> m r']

        ['fail;mm;s tm.a' 'fail :: Monad m => String -> m a']

        ['split;/randomGen g/;.g p.g.g' 'split :: RandomGen g => g -> (g, g)']
        ['uniform;/MonadRandom m/ lt;tt.a tm.a' 'uniform :: (MonadRandom m, Foldable t) => t a -> m a']
        ['replicateM;am;i tm.a tml.a' 'replicateM :: Applicative m => Int -> m a -> m [a]']

        ['add;i i i=add;' 'add :: Int -> Int -> Int\nadd = ']
        ['add;i i i=odd;' bad-generate]
        ['add;i i i=odd;_' bad-generate]

        ['add;i i i i=_ _ _ _ _' 'add :: Int -> Int -> Int -> Int\nadd _ _ _ _ _ = ']
        ['add;i i i i=add;_ _ _ _ _' 'add :: Int -> Int -> Int -> Int\nadd _ _ _ _ _ = ']
        ['add;i i i i=_' 'add :: Int -> Int -> Int -> Int\nadd _ = ']
        ['add;i i i i=_-' 'add :: Int -> Int -> Int -> Int\nadd _ _ _ = ']
        ['add;i i i i=0p' 'add :: Int -> Int -> Int -> Int\nadd p _ _ = ']
        ['add;i i i i=1l' 'add :: Int -> Int -> Int -> Int\nadd _ l _ = ']
        ['add;i i i i=2c' 'add :: Int -> Int -> Int -> Int\nadd _ _ c = ']
        ['add;i i i i=2C' bad-parse]
        ['add;i i i i=3x' bad-generate]

        ['constant;s=' 'constant :: String\nconstant = ']

        ['add;i i i i=y-' bad-generate]
        ['add;i i i i=d-' 'add :: Int -> Int -> Int -> Int\nadd d e f = ']

        ['add;i i i i=-c' 'add :: Int -> Int -> Int -> Int\nadd a b c = ']
        ['add;i i i i=-b' bad-generate]
        ['add;i i i i=-a' bad-generate]

        ['add;i i i i=x-' 'add :: Int -> Int -> Int -> Int\nadd x y z = ']
        ['add;i i i i=y-' bad-generate]
        ['add;i i i i=z-' bad-generate]

        ['add;i i i i=d-' 'add :: Int -> Int -> Int -> Int\nadd d e f = ']

        ['add;i i i i=a5-' 'add :: Int -> Int -> Int -> Int\nadd a5 a6 a7 = ']
        ['add;i i i i=-a5' 'add :: Int -> Int -> Int -> Int\nadd a3 a4 a5 = ']
        ['add;i i i i=-a5' 'add :: Int -> Int -> Int -> Int\nadd a3 a4 a5 = ']
        ['add;i i i i=-a2' 'add :: Int -> Int -> Int -> Int\nadd a0 a1 a2 = ']
        ['add;i i i i=-a1' bad-generate]
        ['add;i i i i=-a0' bad-generate]

        ['add;i i i i=A-' bad-parse]
        ['add;i i i i=-A' bad-parse]
        ['add;i i i i=-C' bad-parse]

# todo
# i i=add;a0-a2

# param counting thing, only if you have any params. maybe need -1.

        [';;...a' bad-parse]
        ['add;Na;' bad-parse]

export
    tests
