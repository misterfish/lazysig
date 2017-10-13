module Lazysig
    ( parseInput
    , Generate(generate)
    ) where

import Data.Char
    ( toUpper
    , digitToInt
    )

import Data.List
    ( intercalate
    )

import Control.Monad
    ( liftM4
    , liftM
    , liftM3
    , liftM2
    , guard
    )

import Debug.Trace
    ( trace
    , traceM
    )

import Text.ParserCombinators.Parsec
    ( char
    , digit
    , satisfy
    , try
    , string
    , optional
    , many
    , many1
    , oneOf
    , sepBy
    , sepBy1
    , GenParser
    , ParseError
    , eof
    , (<|>)
    , noneOf
    , parse
    )

import Data
    ( S(S1, S2, S3)
    , Sig(Sig1, Sig2, Sig3)
    , Body(Body)
    , BodyArgs( BodyArgs, BodyArgsRangeLR, BodyArgsRangeL, BodyArgsRangeR
              , BodyArgsNumRangeLR, BodyArgsNumRangeL, BodyArgsNumRangeR
              , BodyArgsOnly, BodyArgsUnderscoreRange, BodyEmpty
              )
    , Name(Name)
    , Constraints(Constraints)
    , Constraint(Constraint, ConstraintLiteral)
    , Class(Class, ClassAbbrev)
    , ConstraintTypeParam(ConstraintTypeParam)
    , Terms(Terms)
    , Term(Term, TermLiteral)
    , Type(TypeInt, TypeInteger, TypeChar, TypeString, TypeFloat
          , TypeDouble, TypeBool, TypeList, TypeTuple, TypeUser
          , TypeCompound, TypeFunction
          )
    , Generate(generate)
    )

import Util
    ( anyChar
    , toLowerString
    , toUpperString
    , upperFirst
    , trim
    , digitsToInt
    , slurp
    , oneOfIgnoreCase
    , stringIgnoreCase
    , sp
    , sps
    , varId, varIdCh, varId1Ch
    , alphaCh, alphaLowerCh, alphaUpperCh,
    )

start :: GenParser Char st S
start = try s1 <|> try s2 <|> try s3 where
    s1 = S1 <$> sig <*> body' <* eof
    s2 = S2 <$> sig <* eof
    s3 = S3 <$> body' <* eof
    body' = char '=' *> body

body :: GenParser Char st Body
body = try b1 <|> try b2 where
    b1 = do
        n <- many1 nameCh
        _ <- char ';'
        a <- bodyArgs
        return $ b' n a
    b2 = do
        a <- bodyArgs
        return $ b' "" a
    b' n a = Body (Name n) a

bodyArgs :: GenParser Char st BodyArgs
bodyArgs = try bunderscores <|> try bnumonly <|> try bnumrange <|>
           try brange <|> try bparams <|>
           empty' where
    bparams = BodyArgs <$> args where
        args = sepBy1 arg (char ' ')
        arg = varId
    brange = try brange_lr <|> try brange_l <|> try brange_r where
        brange_lr = do
            l <- alphaLowerCh
            _ <- char '-'
            r <- alphaLowerCh
            return $ BodyArgsRangeLR l r
        brange_l = do
            l <- alphaLowerCh
            _ <- char '-'
            return $ BodyArgsRangeL l
        brange_r = do
            _ <- char '-'
            r <- alphaLowerCh
            return $ BodyArgsRangeR r
    bnumrange = try bnumrange_lr <|> try bnumrange_l <|> try bnumrange_r where
        bnumrange_lr = do
            c <- alphaLowerCh
            nl <- many1 digit
            _ <- char '-'
            _ <- char c
            nr <- many1 digit
            return $ BodyArgsNumRangeLR c (digitsToInt nl) (digitsToInt nr)
        bnumrange_l = do
            c <- alphaLowerCh
            nl <- many1 digit
            _ <- char '-'
            return $ BodyArgsNumRangeL c $ digitsToInt nl
        bnumrange_r = do
            _ <- char '-'
            c <- alphaLowerCh
            nr <- many1 digit
            return $ BodyArgsNumRangeR c $ digitsToInt nr
    bnumonly = try b' where
        b' = do
            n <- many1 digit
            s <- many1 alphaLowerCh
            return $ BodyArgsOnly (digitsToInt n) s
    bunderscores = char '_' >> char '-' >> return BodyArgsUnderscoreRange

    empty' = return BodyEmpty

nameCh :: GenParser Char st Char
nameCh = noneOf ";= "

-- | empty name ok -> allows ;x;y
name :: GenParser Char st Name
name = Name <$> many nameCh

sigSep :: GenParser Char st Char
sigSep = char ';'

sig :: GenParser Char st Sig
sig =  (try $ Sig3 <$> name <*> sep constraints <*> sep terms)
   <|> (try $ Sig2 <$> name <*> sep terms)
   <|> (try $ Sig1 <$> terms) where
       sep x = sigSep *> x

constraint :: GenParser Char st Constraint
constraint =  try constraint2 <|> try constraint1

classAbbrev :: GenParser Char st String
classAbbrev = try classAbbrev' where
    classAbbrev' =  str' "FR" <|> str' "RF"
                <|> c2sa single
    single = oneOf "aefilmnors"
    str' = try . string

c2sa :: GenParser Char st Char -> GenParser Char st String
c2sa = (<$>) (:[])

constraintTypeChar :: GenParser Char st Char
constraintTypeChar = noneOf ",; ="

constraintLiteralChar :: GenParser Char st Char
constraintLiteralChar = noneOf "/,="

constraintTypeParam :: GenParser Char st Char
constraintTypeParam = constraintTypeChar

constraint1 :: GenParser Char st Constraint
constraint1 = Constraint <$> c' <*> t' where
    c' = ClassAbbrev <$> classAbbrev
    t' = ConstraintTypeParam <$> c2sa constraintTypeParam

constraint2 :: GenParser Char st Constraint
constraint2 = ConstraintLiteral <$> l' where
    before = char '/' *> sps
    after = sps <* char '/'
    l' = before *> many1 constraintLiteralChar <* after

constraintsSep :: GenParser Char st String
constraintsSep = many1 $ char ' '

termsSep :: GenParser Char st Char
termsSep = char ' '

constraints :: GenParser Char st Constraints
constraints = do
    result <- try $ sepBy constraint constraintsSep
    return $ Constraints result

terms :: GenParser Char st Terms
terms = do
       result <- sepBy1 term termsSep
       return $ Terms result

-- | two or more.
terms2 :: GenParser Char st Terms
terms2 = do
    res1 <- term
    _ <- termsSep
    res2 <- sepBy1 term termsSep
    return . Terms $ (res1:res2)

termLiteralCh :: GenParser Char st Char
termLiteralCh = noneOf "/="

term :: GenParser Char st Term
term = try t' <|> try tl' where
    t' = liftM Term type'
    tl' = liftM TermLiteral tl''
    tl'' = char '/' *> many1 termLiteralCh <* char '/'

type' :: GenParser Char st Type
type' =  try typeTuple <|> try typeSimple <|> try typeList
     <|> try typeCompound <|> try typeFunction

typeCompoundSingleCharCh :: GenParser Char st Char
typeCompoundSingleCharCh = noneOf "/T,; ="

typeCompoundSingleChar :: GenParser Char st String
typeCompoundSingleChar = (:[]) <$> typeCompoundSingleCharCh

typeCompoundLiteralCh :: GenParser Char st Char
typeCompoundLiteralCh = noneOf "/="

typeCompoundLiteral :: GenParser Char st String
typeCompoundLiteral = char '/' *> many1 typeCompoundLiteralCh <* char '/'

typeCompound :: GenParser Char st Type
typeCompound = do
    _ <- char 't'
    t <- typeCompoundSingleChar <|> typeCompoundLiteral
    ts <- many type'
    _ <- optional $ char 'T'
    return $ TypeCompound t ts

typeSimple :: GenParser Char st Type
typeSimple =  (char 'c' >> return TypeChar)
          <|> (char 'b' >> return TypeBool)
          <|> (char 'f' >> return TypeFloat)
          <|> (char 'i' >> return TypeInt)
          <|> (char 'j' >> return TypeInteger)
          <|> (char 'd' >> return TypeDouble)
          <|> (char 's' >> return TypeString)
          <|> TypeUser <$> (:[]) <$> param' where
              param' = try $ do
                  _ <- char '.'
                  anyChar

typeList :: GenParser Char st Type
typeList = char 'l' >> liftM TypeList type'

typeTuple :: GenParser Char st Type
typeTuple = try t' where
    t' = do
        _ <- char 'p'
        ts <- (:) <$> type' <*> many1 type'
        _ <- optional $ char 'P'
        return . TypeTuple $ ts

typeFunction :: GenParser Char st Type
typeFunction = char '(' *> terms' <* char ')' where
    terms' = liftM TypeFunction terms2

parseInput :: String -> Either ParseError S
parseInput = parse start "(unknown)" . trim

