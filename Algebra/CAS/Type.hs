{-#LANGUAGE StandaloneDeriving#-}
{-#LANGUAGE TemplateHaskell#-}
{-#LANGUAGE QuasiQuotes#-}
{-#LANGUAGE CPP#-}

module Algebra.CAS.Type where

import Language.Haskell.TH
import Data.String

data Const =
   Zero
 | One
 | CI Integer
 | CF Integer Integer
 | CR Double
 deriving (Show,Eq)

instance Ord Const where
  compare Zero Zero = EQ
  compare Zero One = LT
  compare Zero (CI a) = compare 0 a
  compare Zero (CF a b) = compare 0 (a*b)
  compare Zero (CR a) = compare 0 a
  compare One One = EQ
  compare One (CI a) = compare 1 a
  compare One (CF a b) = compare 1 (a*b)
  compare One (CR a) = compare 1 a
  compare (CI a) (CI b) = compare a b
  compare (CI a) (CF b c) = compare (fromIntegral a) (fromIntegral b / fromIntegral c)
  compare (CI a) (CR b) = compare (fromIntegral a)  b
  compare (CF a b) (CF c d) = compare (fromIntegral a / fromIntegral b)  (fromIntegral c / fromIntegral d)
  compare (CF a b) (CR c) = compare (fromIntegral a / fromIntegral b) c
  compare (CR a) (CR b) = compare a b
  compare a b =
    case compare b a of
    LT -> GT
    GT -> LT
    EQ -> EQ

neg :: Const
neg = CI (-1)

constSimplify :: Const -> Const
constSimplify (CI 0) = Zero
constSimplify (CI 1) = One
constSimplify (CF 0 _) = Zero
constSimplify (CF a b) | a == b = One
                       | otherwise = let g = gcd a b
                                     in CF (a`div`g) (b`div`g)
constSimplify (CR 0) = Zero
constSimplify (CR 1) = One
constSimplify a = a


instance Num Const where
  fromInteger 0 = Zero
  fromInteger 1 = One
  fromInteger a = CI (fromIntegral a)
  (+) a'' b'' = 
    case (a'',b'') of
    (Zero,Zero) -> Zero
    (Zero,b) -> b
    (One,One) -> CI 2
    (One,CI b') -> constSimplify $ CI (1+b')
    (One,CF a' b') -> constSimplify $ CF (a'+b') b'
    (One,CR b') -> constSimplify $ CR (1+b')
    (CI a',CI b') -> constSimplify $ CI (a'+b')
    (CI a',CF b' c') -> constSimplify $ CF (b'+c'*a') c'
    (CI a',CR b') -> constSimplify $ CR ((fromIntegral a')+b')
    (CF a' b',CF c' d') -> constSimplify $ CF (a'*d'+b'*c') (b'*d')
    (CF a' b',CR c') -> constSimplify $ CR ((fromIntegral a')/(fromIntegral b')+c')
    (CR a',CR b') -> constSimplify $ CR (a'+b')
    (a,b) -> (+) b a
  (-) a b | a == b = Zero
          | otherwise = a + (neg * b)
  (*) a'' b'' =
    case (a'',b'') of
    (Zero,Zero) -> Zero
    (Zero,_) -> Zero
    (One,One) -> One
    (One,a') -> a'
    (CI a',CI b') -> constSimplify $ CI (a'*b')
    (CI a',CF b' c') -> constSimplify $ CF (b'*a') c'
    (CI a',CR b') -> constSimplify $ CR ((fromIntegral a')*b')
    (CF a' b',CF c' d') -> constSimplify $ CF (a'*c') (b'*d')
    (CF a' b',CR c') -> constSimplify $ CR (fromIntegral a' * c' / fromIntegral b')
    (CR a',CR b') -> constSimplify $ CR (a'*b')
    (_,Zero) -> Zero
    (a',One) -> a'
    (a,b) -> (*) b a
  abs Zero = Zero
  abs One = One
  abs (CI a) = CI (abs a)
  abs (CF a b) = CF (abs a) (abs b)
  abs (CR a) = CR (abs a)
  signum Zero = CI 0
  signum One  = CI (-1)
  signum (CI a) = CI (signum a)
  signum (CF a b) = CI $ signum a * signum b
  signum (CR a) = CI $ round $ signum a

instance Fractional Const where
  fromRational 0 = Zero
  fromRational 1 = One
  fromRational a = CR (fromRational a)
  recip a = (/) One a
  (/) a' b' =
    case (a',b') of
    (Zero,_) -> Zero
    (_,Zero) -> error "DivideByZero"
    (One,One) -> One
    (One,CI a) -> CF 1 a
    (One,CF a b) -> CF b a
    (One,CR a) -> CR (1/a)
    (CI a,CI b) -> CF a b
    (CI a,CF b c) -> constSimplify $ CF (a*c) b
    (CI a,CR b) -> constSimplify $ CR (fromIntegral a /b)
    (CF a b,CF c d) -> constSimplify $ CF (a*d) (b*c)
    (CF a b,CR c) -> constSimplify $ CR (fromIntegral a /fromIntegral b * c)
    (CR a,CR b) -> constSimplify $ CR (a/b)
    (a,One) -> a
    (CF b c,CI a) -> constSimplify $ CF b (a*c)
    (CR b,CI a) -> constSimplify $ CR (b/fromIntegral a)
    (CR c,CF a b) -> constSimplify $ CR (fromIntegral b /fromIntegral a * c)

data SpecialFunction =
   Sin Value
 | Cos Value
 | Tan Value
 | Sinh Value
 | Cosh Value
 | Tanh Value
 | Asin Value
 | Acos Value
 | Atan Value
 | Asinh Value
 | Acosh Value
 | Atanh Value
 | Exp Value
 | Log Value
 | Abs Value
 | Sig Value
 | LogBase Value Value
 | Sqrt Value
 | Diff Value Value
 | Integrate Value Value
 deriving (Show,Eq,Ord)

-- | Mathematical expression
data Value =
   C Const
 | Pi
 | V Name
 | S SpecialFunction
 | Value :^: Value
 | Value :*: Value
 | Value :+: Value
 | Value :/: Value
 deriving (Show,Eq)

instance Ord Value where
  compare (C a) (C b) = compare a b
  compare (C _) _ = LT
  compare Pi _ = LT
  compare (V a) (V b) = compare a b
  compare (V _) b@(S _) | isConst b = GT
                        | otherwise = LT
  compare a@(V _) (c@(V _):^:d) | a == c = compare 1 d
                                | otherwise = compare a c
  compare (V a) b | isConst b = GT
                  | otherwise = LT
  compare (S a) (S b) = compare a b
  compare (a :*: b) (c :*: d) | b == d = compare a c
                              | otherwise = compare b d
  compare (a :*: b) c | b == c = GT
                      | otherwise = compare b c
  compare a (b :*: c) | a == c = LT
                      | otherwise = compare a c
  compare (a :^: b) (c :^: d) | a == c = compare b d
                              | otherwise = compare a c
  compare (a :^: b) c | a == c = compare b 1
                      | otherwise = compare a c
  compare a (b :^: c) | a == b = compare 1 c
                      | otherwise = compare a b
  compare (a :+: b) (c :+: d) | b == d = compare a c
                              | otherwise = compare b d
  compare (a :+: b) c | b == c = GT
                      | otherwise = compare b c
  compare a (b :+: c) | a == c = LT
                      | otherwise = compare b c
  compare (a :/: b) (c :/: d) = compare (a*d) (c*b)
  compare (a :/: b) c = compare b (c*b)
  compare a (b :/: c) = compare (a*c) b
  compare a b =
    case (isConst a,isConst b) of
    (True,True) -> EQ
    (True,False) -> LT
    (False,True) -> GT
    (False,False) -> 
      case compare b a of
      LT -> GT
      GT -> LT
      EQ -> EQ


-- #if MIN_VERSION_template_haskell(2,10,0)
-- #else
-- deriving instance Ord Exp
-- #endif

tryPlus :: Value -> Value -> Maybe Value
tryPlus (C Zero) (C Zero) = Just $ (C Zero)
tryPlus (C Zero) a = Just $ a
tryPlus a (C Zero) = Just $ a
tryPlus (C a) (C b) = Just $ C (a+b)
tryPlus a@(V _) b@(V _) | a == b = Just $ (C (CI 2)) :*: a
                        | otherwise = Nothing
tryPlus a@(V _:^: _) b@(V _:^: _) | a == b = Just $ (C (CI 2)) :*: a
                                  | otherwise = Nothing
tryPlus (a:+:b) c =
  case tryPlus b c of
  Nothing ->
    case tryPlus a c of
    Nothing -> Nothing
    Just v -> Just $ v + b
  Just v -> Just $ a + v
tryPlus (a:*:b) (c:*:d) =
  if b == d then
    case tryPlus a c of
    Nothing -> Nothing
    Just v -> Just $ v * b
  else
    Nothing
tryPlus (a:*:b) d =
  if b == d then
    case tryPlus a (C One) of
    Nothing -> Nothing
    Just v -> Just $ v * b
  else
    Nothing
tryPlus a (c:*:d) =
  if a == d then
    case tryPlus (C One) c of
    Nothing -> Nothing
    Just v -> Just $ v * a
  else
    Nothing
tryPlus _ _ = Nothing

insertPlus :: Value -> Value -> Value
insertPlus a'@(a:+:b) v | v <= b = insertPlus a v :+: b
                       | otherwise = a':+:v
insertPlus a v | a <= v = a :+: v
               | otherwise = v :+: a


tryMul :: Value -> Value -> Maybe Value
tryMul (C Zero) _ = Just $ C Zero
tryMul _ (C Zero) = Just $ C Zero
tryMul (C One) a = Just $ a
tryMul a (C One) = Just $ a
tryMul (C a) (C b) = Just $ C (a*b)
tryMul a@(V _) b@(V _) | a == b = Just $ a :^: (C (CI 2))
                       | otherwise = Nothing
tryMul (a@(V _):^: b@_) (c@(V _):^:d@_) | a == c = Just $ a :^: (b+d)
                                        | otherwise = Nothing
tryMul a@(V _) (c@(V _):^:d@_) | a == c = Just $ a :^: (1+d)
                               | otherwise = Nothing
tryMul (a@(V _):^: b@_) c@(V _) | a == c = Just $ a :^: (b+1)
                                | otherwise = Nothing
tryMul (a:*:b) c = 
  case tryMul b c of
  Nothing ->
    case tryMul a c of
    Nothing -> Nothing
    Just v -> Just $ v * b
  Just v -> Just $ a * v
tryMul _ _ = Nothing

insertMul :: Value -> Value -> Value
insertMul a'@(a:*:b) v | v <= b = insertMul a v :*: b
                       | otherwise = a':*:v
insertMul a v | a <= v = a :*: v
              | otherwise = v :*: a



instance Num Value where
  fromInteger 0 = C Zero
  fromInteger 1 = C One
  fromInteger a = C $ CI (fromIntegral a)
  (+) a (b:+:c) =
    case tryPlus a c of
    Just v -> v + b
    Nothing -> insertPlus a c + b
  (+) a b =
    case tryPlus a b of
    Just v -> v
    Nothing -> insertPlus a b
  (-) a b | a == b = C Zero
          | otherwise = a + (b * (C neg))
  (*) a (b:*:c) =
    case tryMul a c of
    Just v -> v * b
    Nothing -> insertMul a c * b
  (*) a b =
    case tryMul a b of
    Just v -> v
    Nothing -> insertMul a b
  abs a = S (Abs a)
  signum a = S (Sig a)

instance Fractional Value where
  fromRational 0 = C Zero
  fromRational 1 = C One
  fromRational a = C $ CR (fromRational a)
  recip a = (/) (C One) a
  (/) a'' b'' =
    case (a'',b'') of
    (C a',C b') -> C (a'/b')
    (C Zero,_) -> C Zero
    (_,C Zero) -> error "divide by zero"
    (C One,b) -> C One :/: b
    (a,C One) -> a
    (a,b) | a == b -> C One
          | otherwise -> a :/: b

instance Floating Value where
  pi = Pi
  exp (C Zero) = C One
  exp a = S $ Exp a
  sqrt a = S $ Sqrt a
  log (C One) = C Zero
  log a = S $ Log a
  (**) _ (C Zero) = C One
  (**) a (C One) = a
  (**) a b = (:^:) a b
  logBase a b = S $ LogBase a b 
  sin = S . Sin
  tan = S . Tan
  cos = S . Cos
  asin = S . Asin
  atan = S . Atan
  acos = S . Acos
  sinh = S . Sinh
  tanh = S . Tanh
  cosh = S . Cosh
  asinh = S . Asinh
  atanh = S . Atanh
  acosh = S . Acosh

instance IsString Value where
  fromString = val

-- | Lift String to variable of Value
val ::  String ->  Value
val v = V (mkName v)


instance Enum Value where
  succ a = a+1
  pred a = a-1
  toEnum v = fromIntegral v
  fromEnum (C Zero) = 0
  fromEnum (C One) = 1
  fromEnum (C (CI a)) = fromIntegral a
  fromEnum a = error $ "can not do fromEnum:" ++ show a

instance Real Value where
  toRational (C (CI v)) = toRational v
  toRational (C (CR v)) = toRational v
  toRational Pi = toRational pi
  toRational (C Zero) = toRational 0
  toRational (C One) = toRational 1
  toRational _ = toRational 0

instance Integral Value where
  quot a b = fst $ quotRem a b
  rem a b = snd $ quotRem a b
  quotRem a b =
    case (mva,mvb) of
    (Just va,Just vb) | va == vb -> quotRem' a b
                      | otherwise -> error "quotRem does not support multi variable"
    (Nothing,Nothing) -> (a/b,0)
    otherwise -> quotRem' a b
      
    where
      (da,mva,ca)=degree a
      (db,mvb,cb)=degree b
      divnum = ca/cb
      vv = case mva of
        Just va -> (va**(fromIntegral (da-db)))
        Nothing -> 1
      rem' = expand $ a - (b*vv*divnum)
      (div'',rem'') = quotRem rem' b
      quotRem' a b =
        if da < db
        then (0,a)
        else (expand (div'' + divnum * vv),rem'')


  div = quot
  mod = rem
  toInteger (C Zero) = 0
  toInteger (C One) = 1
  toInteger (C (CI a)) = toInteger a
  toInteger a = error $ "can not do toInteger:" ++ show a



degree :: Value -> (Integer,Maybe Value,Value)
degree (a:*:(b:^:(C (CI c)))) = (c,Just b,a)
degree (b:^:(C (CI c))) = (c,Just b,C One)
degree b@(V _) = (1,Just b,C One)
degree (_:*:b) = degree b
degree (_:+:b) = degree b
degree a  | isConst a = (0,Nothing,a)
          | otherwise = error $ "abort degree operation:" ++ show a


converge ::  (Value ->  Value) -> Value -> Value
converge func val =
  let v' = func val
  in if v' ==  val
     then v'
     else converge func v'

expand :: Value -> Value
expand ((a:+:b):*:(c:+:d)) = expand (a*c) + expand (b*c) +expand (a*d) +expand (b*d)
expand ((a:+:b):*:c) = expand (a*c) + expand (b*c)
expand (a:*:(b:+:c)) = expand (a*b) + expand (a*c)
expand (a:+:b) = expand a + expand b
expand a = a


gcdV :: Value -> Value -> Value
gcdV a b | a == C Zero = b
         | b == C Zero = a
         | otherwise = 
             let (da,_,_) = degree a
                 (db,_,_) = degree b
             in if da >= db then
                  gcdV (a `rem` b) b
                else
                  gcdV a (b `rem` a)

partialFractionExpansion :: Value -> Maybe Value
partialFractionExpansion (_:/:_) = Nothing
partialFractionExpansion _ = Nothing
--partialFractionExpansion =
{-
toListValue :: Value -> ListValue
toListValue (a:*:b) = Mul $ toList a ++ toList b
  where
    toList :: Value -> [ListValue]
    toList a = 
      case toListValue a of
      Mul a' -> a'
      Mul a' -> error ""
toListValue (a:+:b) = Sum $ toList a ++ toList b
  where
    toList :: Value -> [ListValue]
    toList a = 
      case toListValue a of
      Sum a' -> a'
      Mul a' -> error ""
-}

{-
solver :: Value -> Value -> [Value]
solver a b =
  case da of
  1 -> (a-b*ca)/(-ca)
  where
    (da,mva,ca) = degree a
-}
--solver a b = error $ "can not solve: " ++ show a ++ " : " ++ show b


-- | When formula does not include variable,
-- isConst returns True.
-- >>> import Algebra.CAS.Core(prettyPrint)
-- >>> let x = "x" :: Value
-- >>> isConst x
-- False
-- >>> isConst $ sin(x)*3
-- False
-- >>> isConst $ 3.0 * sin(3.0)
-- True
isConst :: Value ->  Bool
isConst (C _) = True
isConst (V _) = False
isConst (S (Sin v)) = isConst v
isConst (S (Cos v)) = isConst v
isConst (S (Tan v)) = isConst v
isConst (S (Sinh v)) = isConst v
isConst (S (Cosh v)) = isConst v
isConst (S (Tanh v)) = isConst v
isConst (S (Asin v)) = isConst v
isConst (S (Acos v)) = isConst v
isConst (S (Atan v)) = isConst v
isConst (S (Asinh v)) = isConst v
isConst (S (Acosh v)) = isConst v
isConst (S (Atanh v)) = isConst v
isConst (S (Exp v)) = isConst v
isConst (S (Log v)) = isConst v
isConst (S (Abs v)) = isConst v
isConst (S (Sig v)) = isConst v
isConst (S (LogBase v0 v1)) = isConst v0 &&  isConst v1
isConst Pi = True
isConst (S (Sqrt v)) = isConst v
isConst (S (Diff v0 v1)) = isConst v0 &&  isConst v1
isConst (S (Integrate v0 v1)) = isConst v0 &&  isConst v1
isConst (v0 :^: v1) = isConst v0 &&  isConst v1
isConst (v0 :*: v1) = isConst v0 &&  isConst v1
isConst (v0 :+: v1) = isConst v0 &&  isConst v1
isConst (v0 :/: v1) = isConst v0 &&  isConst v1

