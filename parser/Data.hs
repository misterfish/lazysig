module Data
    ( S(S1, S2, S3)
    , Sig(Sig1, Sig2, Sig3)
    , Body(Body)
    , BodyArgs( BodyArgs
              , BodyArgsRangeLR, BodyArgsRangeL, BodyArgsRangeR
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
    , Type( TypeInt, TypeInteger, TypeChar, TypeString, TypeFloat, TypeBool
          , TypeDouble, TypeList
          , TypePair
          , TypeAnon
          , TypeCompound, TypeFunction
          )
    , Generate(generate)
    ) where

import Debug.Trace
    ( trace
    , traceM
    )

import Control.Monad
    ( (<=<)
    )

import Data.Char
    ( ord
    )
import Data.List
    ( intercalate
    , unfoldr
    )

import Text.Printf
    ( printf
    )

import Util
    ( upperFirst
    , charRange, charRangeFrom, charRangeTo
    , numCharRange, numCharRangeFrom, numCharRangeTo
    )

data S = S1 Sig Body
       | S2 Sig
       | S3 Body

data Sig = Sig1 Terms
         | Sig2 Name Terms
         | Sig3 Name Constraints Terms

data Body = Body Name BodyArgs
data BodyArgs = BodyArgs [String]
              | BodyArgsRangeLR Char Char
              | BodyArgsRangeL Char
              | BodyArgsRangeR Char
              | BodyArgsNumRangeLR Char Int Int
              | BodyArgsNumRangeL Char Int
              | BodyArgsNumRangeR Char Int
              | BodyArgsOnly Int String
              | BodyArgsUnderscoreRange
              | BodyEmpty

data Name = Name String
data Constraints = Constraints [Constraint]
data Constraint = Constraint Class ConstraintTypeParam
                | ConstraintLiteral String

data Class = Class String
           | ClassAbbrev String

data ConstraintTypeParam = ConstraintTypeParam String

data Terms = Terms [Term]

data Term = Term Type
          | TermLiteral String

data Type = TypeInt
          | TypeInteger
          | TypeChar
          | TypeBool
          | TypeString
          | TypeFloat
          | TypeDouble
          | TypeList Type
          | TypePair [Type]
          | TypeCompound String [Type]
          | TypeAnon String
          | TypeFunction Terms

class Generate a where
    generate :: a -> Either String String

instance Generate S where
    generate (S1 sig' body') = do
        gsig' <- generate sig'
        gbody'' <- body'' body'
        gbody' <- generateBody (max 0 $ numTermsFromSig sig' - 1) gbody''
        return $ intercalate "\n" [gsig', gbody'] where

        body'' (Body (Name "") args) = body''' $ nameFromSig sig' where
            body''' (Name "") = Left $ "no name"
            body''' n = Right $ Body n args
        body'' (Body n@(Name nameFromBody) args) = body''' $ nameFromSig sig' where
            body''' (Name "") = Right $ Body n args
            body''' (Name nameFromSig')
              | nameFromBody == nameFromSig' = Right $ Body n args
              | otherwise = Left $ "multiple names"

        nameFromSig (Sig1 _) = Name ""
        nameFromSig (Sig2 name _) = name
        nameFromSig (Sig3 name _ _) = name

        numTermsFromSig (Sig1 terms) = numTerms' terms
        numTermsFromSig (Sig2 _ terms) = numTerms' terms
        numTermsFromSig (Sig3 _ _ terms) = numTerms' terms
        numTerms' (Terms ts) = length ts

    generate (S2 sig') = generate sig'
    generate (S3 body') = generateBody 0 body'

instance Generate Sig where
    generate (Sig1 terms') = generate terms'
    generate (Sig2 name' terms') = do
        gterms' <- generate terms'
        gname' <- generateSigName name'
        return $ gname' ++ gterms'
    generate (Sig3 name' constraints' terms') = do
        gsig' <- generateSigCT constraints' terms'
        gname' <- generateSigName name'
        return $ gname' ++ gsig'

instance Generate Terms where
    generate (Terms terms') = t' terms' where
        t' = return . intercalate " -> " <=< sequence . map generate

instance Generate Term where
    generate (Term type') = generate type'
    generate (TermLiteral t) = return t

instance Generate Name where
    generate (Name s) = return s

instance Generate Constraints where
    generate (Constraints con) = do
        gcon' <- sequence $ map generate con
        let t = intercalate ", " gcon'
        return $ surround t (length con) where
        surround _ 0 = ""
        surround t' 1 = t'
        surround t' _ = "(" ++ t' ++ ")"

instance Generate Constraint where
    generate (Constraint class' param') = do
        gclass' <- generate class'
        gparam' <- generate param'
        return $ gclass' ++ " " ++ gparam'
    generate (ConstraintLiteral literal') = return $ upperFirst literal'

instance Generate Class where
    generate (Class s) = return $ upperFirst s
    generate (ClassAbbrev "FR") = return "Fractional"
    generate (ClassAbbrev "RF") = return "RealFrac"
    generate (ClassAbbrev "a") = return "Applicative"
    generate (ClassAbbrev "e") = return "Eq"
    generate (ClassAbbrev "f") = return "Functor"
    generate (ClassAbbrev "i") = return "Integral"
    generate (ClassAbbrev "l") = return "Foldable"
    generate (ClassAbbrev "m") = return "Monad"
    generate (ClassAbbrev "n") = return "Num"
    generate (ClassAbbrev "o") = return "Ord"
    generate (ClassAbbrev "r") = return "Real"
    generate (ClassAbbrev "s") = return "Show"
    generate (ClassAbbrev _) = Left "bad class abbrevation"

instance Generate ConstraintTypeParam where
    generate (ConstraintTypeParam s) = return s

instance Generate Type where
    generate TypeInt = return "Int"
    generate TypeInteger = return "Integer"
    generate TypeChar = return "Char"
    generate TypeBool = return "Bool"
    generate TypeString = return "String"
    generate TypeFloat = return "Float"
    generate TypeDouble = return "Double"
    generate (TypeList t) = do
        gt' <- generate t
        return $ "[" ++ gt' ++ "]"
    generate (TypePair ts) = t' ts where
        t' = return . printf "(%s)" . intercalate ", " <=< sequence . map generate
    generate (TypeCompound t ts) = t' ts where
        t' = return . intercalate " " <=< sequence . (:) (return t) . map generate
    generate (TypeAnon s) = return s
    generate (TypeFunction ts) = do
        gts' <- generate ts
        return $ "(" ++ gts' ++ ")"

generateBody :: Int -> Body -> Either String String
generateBody numTerms (Body name args) = do
    p1 <- printf'1
    p2 <- printf'2
    return $ printf "%s%s= " p1 p2 where
        printf'1 = do
            gname' <- generate name
            gargs' <- bodyArgs
            return $ printf'' gname' gargs'

        printf'2 = do
            gname' <- generate name
            gargs' <- bodyArgs
            return $ printf''2 gname' gargs'
        printf''2 _ "" = ""
        printf''2 _ _ = " "

        printf'' "" "" = intercalate " " []
        printf'' "" bodyArgs' = intercalate " " [bodyArgs']
        printf'' n bodyArgs' = intercalate " " [n, bodyArgs']

        bodyArgs = showBodyArgs numTerms args

showBodyArgs :: Int -> BodyArgs -> Either String String
showBodyArgs _ (BodyArgs as) = Right $ intercalate " " as
showBodyArgs numParams (BodyArgsRangeLR l r)
  | ord r - ord l > numParams - 1 = Left "too many args in body"
  | otherwise = maybe (Left "bad char range") (Right . intercal'') $ charRange l r
showBodyArgs numParams (BodyArgsNumRangeLR c nl nr)
  | nr - nl > numParams - 1 = Left "too many args in body"
  | otherwise = maybe (Left "bad char range") (Right . intercal') $ numCharRange c nl nr where
showBodyArgs 0 _ = Left "unable to calculate range" -- open-ended range
showBodyArgs numParams (BodyArgsRangeL l)
  | ord 'z' - ord l < numParams - 1 = Left . printf "range char too high (%c)" $ l
  | otherwise = Right $ intercal'' $ charRangeFrom l numParams
showBodyArgs numParams (BodyArgsRangeR r)
  | ord r - ord 'a' < numParams - 1 = Left . printf "range char too low (%c)" $ r
  | otherwise = Right $ intercal'' $ charRangeTo r numParams
showBodyArgs numParams (BodyArgsNumRangeL c nl)
  = Right $ intercal' $ numCharRangeFrom c nl numParams
showBodyArgs numParams (BodyArgsNumRangeR c nr)
  | nr < numParams - 1 = Left . printf "range digit too low (%d)" $ nr
  | otherwise = Right $ intercalate " " $ numCharRangeTo c nr numParams
showBodyArgs numParams (BodyArgsOnly n str)
  | n > numParams - 1 = Left "too many args in body"
  | otherwise = Right $ intercalate " " $ t' where
    t' = unfoldr f 0
    f m
      | m == numParams = Nothing
      | m == n = Just (str, m + 1)
      | otherwise = Just ("_", m + 1)
showBodyArgs numParams (BodyArgsUnderscoreRange) = Right $ intercalate " " . take numParams $ repeat "_"
showBodyArgs _ (BodyEmpty) = Right $ ""

intercal'' :: [Char] -> String
intercal'' = intercalate " " . map (:[])

intercal' :: [[Char]] -> [Char]
intercal' = intercalate " "

generateSigCT :: Constraints -> Terms -> Either String String
generateSigCT constraints' terms' = do
    gterms' <- generate terms'
    gconstraints' <- c' constraints'
    return $ gconstraints' ++ gterms' where
        c' :: Constraints -> Either String String
        c' (Constraints []) = return ""
        c' constraints'' = do
            gcontraints' <- generate constraints''
            return $ gcontraints' ++ " => "

generateSigName :: Name -> Either String String
generateSigName (Name "") = return ":: "
generateSigName name'' = do
    gname' <- generate name''
    return $ gname' ++ " :: "
