{-#LANGUAGE StandaloneDeriving#-}
{-#LANGUAGE TemplateHaskell#-}
{-#LANGUAGE QuasiQuotes#-}
{-#LANGUAGE OverloadedStrings#-}
{-#LANGUAGE CPP#-}

module Algebra.CAS.Type where

import Language.Haskell.TH
import Data.String
import Data.Maybe

import qualified Language.Haskell.TH.Ppr as P
import qualified Data.Text as T
--import Algebra.CAS.Type

exp2val :: Exp -> Value

exp2val (InfixE (Just a) (VarE op) (Just b))
  | op == '(+) = exp2val a + exp2val b
  | op == '(-) = exp2val a - exp2val b
  | op == '(*) = exp2val a * exp2val b
  | op == '(/) = exp2val a / exp2val b
  | op == '(**) = exp2val a ** exp2val b
  | otherwise = error "exp2val // can not parse"

exp2val (AppE (VarE fun) a)
  | fun ==  'log = S $ Log $ exp2val a
  | fun ==  'sqrt = S $ Sqrt $ exp2val a
  | fun ==  'exp = S $ Exp $ exp2val a
  | fun ==  'sin = S $ Sin $ exp2val a
  | fun ==  'cos = S $ Cos $ exp2val a
  | fun ==  'tan = S $ Tan $ exp2val a
  | fun ==  'asin = S $ Asin $ exp2val a
  | fun ==  'acos = S $ Acos $ exp2val a
  | fun ==  'atan = S $ Atan $ exp2val a
  | fun ==  'sinh = S $ Sinh $ exp2val a
  | fun ==  'cosh = S $ Cosh $ exp2val a
  | fun ==  'tanh = S $ Tanh $ exp2val a
  | fun ==  'asinh = S $ Asinh $ exp2val a
  | fun ==  'acosh = S $ Acosh $ exp2val a
  | fun ==  'atanh = S $ Atanh $ exp2val a
  | fun ==  'negate = C (CI (-1)) * (exp2val a)
  | otherwise = error "can not parse"
exp2val (LitE (IntegerL a)) = C (CI a)
exp2val (LitE (RationalL a)) = C (CR (fromRational a))
exp2val (VarE a) | a == 'pi = Pi
                 | otherwise = V a

exp2val a@_ = error $ "exp2val // can not parse:" ++ show a

val2exp :: Value -> Exp
val2exp (a :+: b) = (InfixE (Just (val2exp a)) (VarE '(+)) (Just (val2exp b)))
val2exp (a :*: b) = (InfixE (Just (val2exp a)) (VarE '(*)) (Just (val2exp b)))
val2exp (a :/: b) = (InfixE (Just (val2exp a)) (VarE '(/)) (Just (val2exp b)))
val2exp (a :^: b) = (InfixE (Just (val2exp a)) (VarE '(**)) (Just (val2exp b)))

val2exp (S (Log a)) = (AppE (VarE 'log) (val2exp a))
val2exp (S (Sqrt a)) = (AppE (VarE 'sqrt) (val2exp a))
val2exp (S (Exp a)) = (AppE (VarE 'exp) (val2exp a))
val2exp (S (Sin a)) = (AppE (VarE 'sin) (val2exp a)) 
val2exp (S (Cos a)) = (AppE (VarE 'cos) (val2exp a)) 
val2exp (S (Tan a)) = (AppE (VarE 'tan) (val2exp a))
val2exp (S (Asin a)) = (AppE (VarE 'asin) (val2exp a))
val2exp (S (Acos a)) = (AppE (VarE 'acos) (val2exp a))
val2exp (S (Atan a)) = (AppE (VarE 'atan) (val2exp a))
val2exp (S (Sinh a)) = (AppE (VarE 'sinh) (val2exp a))
val2exp (S (Cosh a)) = (AppE (VarE 'cosh) (val2exp a))
val2exp (S (Tanh a)) = (AppE (VarE 'tanh) (val2exp a))
val2exp (S (Asinh a)) = (AppE (VarE 'asinh) (val2exp a))
val2exp (S (Acosh a)) = (AppE (VarE 'acosh) (val2exp a))
val2exp (S (Atanh a)) = (AppE (VarE 'atanh) (val2exp a))

val2exp (C (CI a)) = LitE (IntegerL a)
val2exp (C (CR a)) = LitE (RationalL (toRational a))
val2exp (C (CF a b)) = LitE (RationalL ((toRational a)/(toRational b)))
val2exp (C One) = LitE (IntegerL 1)
val2exp (C Zero) = LitE (IntegerL 0)
val2exp Pi = VarE 'pi
val2exp (V a) = VarE $ a

val2exp a@_ = error $ "val2exp // can not parse:" ++ show a

lift  ::  Value -> Exp
lift  = val2exp
lift1 ::  (Value -> Value) -> Exp -> Exp
lift1 a b = val2exp $ a (exp2val b)
lift2 ::  (Value -> Value -> Value) -> Exp -> Exp -> Exp
lift2 a b c = val2exp $ a (exp2val b) (exp2val c)
lift3 ::  (Value -> Value -> Value -> Value) -> Exp -> Exp -> Exp -> Exp
lift3 a b c d = val2exp $ a (exp2val b) (exp2val c) (exp2val d)

prettyPrint ::  Value ->  String
prettyPrint var = T.unpack $
                  T.replace "GHC.Num." "" $
                  T.replace "GHC.Float." "" $
                  T.replace "GHC.Real." "" $
                  T.pack $ show $ P.ppr $ val2exp var


data Equation = Value :=: Value deriving (Show,Eq,Ord)

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
  compare (CI a) (CF b c) =
    let v = fromIntegral b / fromIntegral c :: Double
    in compare (fromIntegral a) v
  compare (CI a) (CR b) = compare (fromIntegral a)  b
  compare (CF a b) (CF c d) =
    let v0 = fromIntegral a / fromIntegral b :: Double
        v1 = fromIntegral c / fromIntegral d :: Double
    in compare v0 v1
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
constSimplify (CF a 1) = (CI a)
constSimplify (CF a (-1)) = (CI (-a))
constSimplify (CF a b) | a == b = One
                       | otherwise =
                         case gcd a b of
                         1 -> CF a b
                         g -> constSimplify $ CF (a`div`g) (b`div`g)
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
    (One,CI (-1)) -> CI (-1)
    (One,CI a) -> CF 1 a
    (One,CF a b) -> CF b a
    (One,CR a) -> CR (1/a)
    (CI a,CI (-1)) -> CI (-a)
    (CI a,CI b) -> constSimplify $ CF a b
    (CI a,CF b c) -> constSimplify $ CF (a*c) b
    (CI a,CR b) -> constSimplify $ CR (fromIntegral a /b)
    (CF a b,CF c d) -> constSimplify $ CF (a*d) (b*c)
    (CF a b,CR c) -> constSimplify $ CR (fromIntegral a /fromIntegral b * c)
    (CR a,CR b) -> constSimplify $ CR (a/b)
    (a,One) -> a
    (a,CI (-1)) -> -a
    (CF b c,CI a) -> constSimplify $ CF b (a*c)
    (CR b,CI a) -> constSimplify $ CR (b/fromIntegral a)
    (CR c,CF a b) -> constSimplify $ CR (fromIntegral b /fromIntegral a * c)

instance Enum Const where
  succ a = a+1
  pred a = a-1
  toEnum v = fromIntegral v
  fromEnum Zero = 0
  fromEnum One = 1
  fromEnum (CI a) = fromEnum a
  fromEnum (CR a) = fromEnum a
  fromEnum (CF a b) = fromEnum ((fromIntegral a::Double)  / fromIntegral b)

instance Real Const where
  toRational Zero = toRational (0::Int)
  toRational One = toRational (1::Int)
  toRational (CI v) = toRational v
  toRational (CR v) = toRational v
  toRational (CF a b) = toRational ((fromIntegral a :: Double) / fromIntegral b)

instance Floating Const where
  pi = CR pi
  exp Zero = 1
  exp a = CR $ exp $ fromRational $ toRational a
  sqrt a = CR $ sqrt $ fromRational $ toRational a
  log One = Zero
  log a = CR $ log $ fromRational $ toRational a
  (**) _ Zero = 1
  (**) a One = a
  (**) a (CI b) = a^b
  (**) a b = CR $ (fromRational $ toRational a :: Double) ** (fromRational $ toRational b :: Double)
  logBase a b = CR $ logBase (fromRational $ toRational $ a) (fromRational $ toRational $ b)
  sin a = CR $ sin $ fromRational $ toRational a
  tan a = CR $ tan $ fromRational $ toRational a
  cos a = CR $ cos $ fromRational $ toRational a
  asin a = CR $ asin $ fromRational $ toRational a
  atan a = CR $ atan $ fromRational $ toRational a
  acos a = CR $ acos $ fromRational $ toRational a
  sinh a = CR $ sinh $ fromRational $ toRational a
  tanh a = CR $ tanh $ fromRational $ toRational a
  cosh a = CR $ cosh $ fromRational $ toRational a
  asinh a = CR $ asinh $ fromRational $ toRational a
  atanh a = CR $ atanh $ fromRational $ toRational a
  acosh a = CR $ acosh $ fromRational $ toRational a


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
   C Const -- ^ Constant value
 | Pi      -- ^ Pi
 | CV Name -- ^ Constant variable which is used to deal variable(V Name) as constant value
 | V Name  -- ^ Variable
 | S SpecialFunction  -- ^ Special Functions (sin, cos, exp and etc..)
 | Value :^: Value
 | Value :*: Value
 | Value :+: Value
 | Value :/: Value
 deriving (Eq)

instance Show Value where
  show a = prettyPrint a


instance Ord Value where
  compare (C a) (C b) = compare a b
  compare (C _) Pi = LT
  compare (C _) (CV _) = LT
  compare (C _) _ = LT
  compare Pi (CV _) = LT
  compare Pi _ = LT
  compare (CV _) _ = LT
  compare (V a) (V b) = compare a b
  compare (V _) b@(S _) | isConst b = GT
                        | otherwise = LT
  compare a@(V _) (c@(V _):^:d) | a == c = compare 1 d
                                | otherwise = compare a c
  compare (V _) b | isConst b = GT
                  | otherwise = LT
  compare (S a) (S b) = compare a b
  compare (a :*: b) (c :*: d) | b == d = compare a c
                              | otherwise = compare b d
  compare (_ :*: b) c | b == c = GT
                      | otherwise = compare b c
  compare a (_ :*: c) | a == c = LT
                      | otherwise = compare a c
  compare (a :^: b) (c :^: d) | a == c = compare b d
                              | otherwise = compare a c
  compare (a :^: b) c | a == c = compare b 1
                      | otherwise = compare a c
  compare a (b :^: c) | a == b = compare 1 c
                      | otherwise = compare a b
  compare (a :+: b) (c :+: d) | b == d = compare a c
                              | otherwise = compare b d
  compare (_ :+: b) c | b == c = GT
                      | otherwise = compare b c
  compare a (b :+: c) | a == c = LT
                      | otherwise = compare b c
  compare (a :/: b) (c :/: d) = compare (a*d) (c*b)
  compare (_ :/: b) c = compare b (c*b)
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
    (a,C c) -> C (1/c) * a
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
  (**) (C (CI a)) (C (CI b)) = C (CI (a^b))
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
  toRational Pi = toRational (pi::Double)
  toRational (C Zero) = toRational (0::Int)
  toRational (C One) = toRational (1::Int)
  toRational _ = toRational (0::Int)


instance Integral Value where
  quot a b = fst $ quotRem a b
  rem a b = snd $ quotRem a b
  quotRem a b =
    case quotRemV a b of
    (S (Abs a),S (Abs b)) -> (a,b)
    (S (Abs a),b) -> (a,b)
    (a,S (Abs b)) -> (a,b)
    (a,b) -> (a,b)
    where
      quotRemV (S (Abs a)) (S (Abs b)) = quotRemV a b
      quotRemV (S (Abs a)) b = quotRemV a b
      quotRemV a (S (Abs b)) = quotRemV a b
      quotRemV a b =
        case (mva,mvb) of
        (Just va,Just vb) | va == vb -> quotRem'
                          | otherwise -> error "quotRem does not support multi variable"
        (Nothing,Nothing) -> (a/b,0)
        (_,_) -> quotRem'
          
        where
          (da,mva,ca)=degree a
          (db,mvb,cb)=degree b
          divnum = ca/cb
          vv = case mva of
            Just va -> (va**(fromIntegral (da-db)))
            Nothing -> 1
          rem' = expand $ a - (b*vv*divnum)
          (div'',rem'') = quotRem rem' b
          quotRem'  =
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
converge func v =
  let v' = func v
  in if v' ==  v
     then v'
     else converge func v'

expand :: Value -> Value
expand ((a:+:b):*:(c:+:d)) = expand (a*c) + expand (b*c) +expand (a*d) +expand (b*d)
expand ((a:+:b):*:c) = expand (a*c) + expand (b*c)
expand (a:*:(b:+:c)) = expand (a*b) + expand (a*c)
expand (a:+:b) = expand a + expand b
expand (a:*:b) = expand a * expand b
expand (a:/:1) = a
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

lcmV :: Value -> Value -> Value
lcmV a b =
  case lcm a b of
  (S (Abs v)) -> expand v
  v -> expand v

--pfe :: Value -> Maybe Value
--pfe (a:/:(b:*:c) = Nothing
--pfe _ = Nothing
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

--degreeVariable :: Value -> Maybe (Integer,Value)
--degreeVariable (b:^:(C (CI c))) = Just (c,b)
--degreeVariable b@(V _) = Just (1,b)
--degreeVariable _ Nothing

headAdd :: Value -> Value
headAdd (_ :+: ab) = ab
headAdd ab = ab
tailAdd :: Value -> Value
tailAdd (a :+: _) = a
tailAdd _ = 0
mapAdd :: (Value -> Value) -> Value -> Value
mapAdd func formula =
  case t of
  0 -> func h
  v -> (mapAdd func t) + (func h)
  where
    h = headAdd formula
    t = tailAdd formula

headMul :: Value -> Value
headMul (_ :*: ab) = ab
headMul ab = ab
tailMul :: Value -> Value
tailMul (a :*: _) = a
tailMul _ = 1

headDiv :: Value -> Value
headDiv (_ :/: ab) = ab
headDiv ab = ab
tailDiv :: Value -> Maybe Value
tailDiv (a :/: _) = Just a
tailDiv _ = Nothing


subst :: Value -> Value -> Value -> Value
subst org mod' formula = mapValue func formula
  where
    func v = if v == org then mod' else v

mapValue :: (Value -> Value) -> Value -> Value
mapValue conv a@(C _) = conv a
mapValue conv a@(CV _) = conv a
mapValue conv a@Pi = conv a
mapValue conv a@(V _) = conv a
mapValue conv (S (Sin v)) = S $ Sin $ mapValue conv v
mapValue conv (S (Cos v)) = S $ Cos $ mapValue conv v
mapValue conv (S (Tan v)) = S $ Tan $ mapValue conv v
mapValue conv (S (Sinh v)) = S $ Sinh $ mapValue conv v
mapValue conv (S (Cosh v)) = S $ Cosh $ mapValue conv v
mapValue conv (S (Tanh v)) = S $ Tanh $ mapValue conv v
mapValue conv (S (Asin v)) = S $ Asin $ mapValue conv v
mapValue conv (S (Acos v)) = S $ Acos $ mapValue conv v
mapValue conv (S (Atan v)) = S $ Atan $ mapValue conv v
mapValue conv (S (Asinh v)) = S $ Asinh $ mapValue conv v
mapValue conv (S (Acosh v)) = S $ Acosh $ mapValue conv v
mapValue conv (S (Atanh v)) = S $ Atanh $ mapValue conv v
mapValue conv (S (Exp v)) = S $ Exp $ mapValue conv v
mapValue conv (S (Log v)) = S $ Log $ mapValue conv v
mapValue conv (S (Abs v)) = S $ Abs $ mapValue conv v
mapValue conv (S (Sig v)) = S $ Sig $ mapValue conv v
mapValue conv (S (LogBase v1 v2)) = S $ LogBase (mapValue conv v1) (mapValue conv v2)
mapValue conv (S (Sqrt v)) = S $ Sqrt $ mapValue conv v
mapValue conv (S (Diff v1 v2)) = S $ Diff (mapValue conv v1) (mapValue conv v2)
mapValue conv (S (Integrate v1 v2)) = S $ Integrate (mapValue conv v1) (mapValue conv v2)
mapValue conv (a:^:b) = mapValue conv a ** mapValue conv b
mapValue conv (a:*:b) = mapValue conv a * mapValue conv b
mapValue conv (a:+:b) = mapValue conv a + mapValue conv b
mapValue conv (a:/:b) = mapValue conv a / mapValue conv b

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
isConst (CV _) = True
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

isVariable :: Value -> Bool
isVariable = not.isConst

variables :: Value ->  [Value]
variables (C _) = []
variables (CV _) = []
variables a@(V _) = [a]
variables (S (Sin v)) = variables v
variables (S (Cos v)) = variables v
variables (S (Tan v)) = variables v
variables (S (Sinh v)) = variables v
variables (S (Cosh v)) = variables v
variables (S (Tanh v)) = variables v
variables (S (Asin v)) = variables v
variables (S (Acos v)) = variables v
variables (S (Atan v)) = variables v
variables (S (Asinh v)) = variables v
variables (S (Acosh v)) = variables v
variables (S (Atanh v)) = variables v
variables (S (Exp v)) = variables v
variables (S (Log v)) = variables v
variables (S (Abs v)) = variables v
variables (S (Sig v)) = variables v
variables (S (LogBase v0 v1)) = variables v0 ++  variables v1
variables Pi = []
variables (S (Sqrt v)) = variables v
variables (S (Diff v0 v1)) = variables v0 ++  variables v1
variables (S (Integrate v0 v1)) = variables v0 ++  variables v1
variables (v0 :^: v1) = variables v0 ++  variables v1
variables (v0 :*: v1) = variables v0 ++  variables v1
variables (v0 :+: v1) = variables v0 ++  variables v1
variables (v0 :/: v1) = variables v0 ++  variables v1

denom :: Value -> Value
denom (_:/:b) = b
denom _ = 1

numer :: Value -> Value
numer (a:/:_) = a
numer a = a

headV :: Value -> (Value,Value)
headV v = var (firstTerm,1)
  where
    firstTerm = headAdd v
    var (c,v) =
      case (isConst c) of
      True -> (c,v)
      False -> var (tailMul c,headMul c*v)


splitExp :: Value -> (Value,Value)
splitExp (a:^:b) = (a,b)
splitExp a = (a,1)

lcmGB :: Value -> Value -> Value
lcmGB a b = lcmV ca cb * lcmGB' va vb
  where
    (ca,va) = headV a
    (cb,vb) = headV b

lcmGB' :: Value -> Value -> Value
lcmGB' 1 1 = 1
lcmGB' a 1 = a
lcmGB' 1 a = a
lcmGB' a b = 
  if hva == hvb
  then lcmGB' ta tb * (hva ** max hpa hpb)
  else if hva < hvb
       then lcmGB' a tb * (hvb ** hpb)
       else lcmGB' ta b * (hva ** hpa)
  where
    (hva,hpa) = splitExp $ headMul a
    (hvb,hpb) = splitExp $ headMul b
    ta = tailMul a
    tb = tailMul b

divAllGB :: Value -> Value -> Value
divAllGB a b = expand $ t + divGB h b
  where
    h = headAdd a
    t = case (tailAdd a) of
      0 -> 0
      v -> divAllGB v b

divGB :: Value -> Value -> Value
divGB a b = (ca / cb) * divGB' va vb
  where
    (ca,va) = headV a
    (cb,vb) = headV b

divGB' :: Value -> Value -> Value
divGB' 1 1 = 1
divGB' a 1 = a
divGB' 1 a = 1 / a
divGB' a b =
  if hva == hvb
  then divGB' ta tb * (hva ** (hpa- hpb))
  else if hva < hvb
       then divGB' a tb / (hvb ** hpb)
       else divGB' ta b * (hva ** hpa)
  where
    (hva,hpa) = splitExp $ headMul a
    (hvb,hpb) = splitExp $ headMul b
    ta = tailMul a
    tb = tailMul b

sGB f g = expand $ divAllGB (expand $ expand $ (ca*cb)*(lcmGB va vb)*f) (headAdd f)
                 - divAllGB (expand $ expand $ (ca*cb)*(lcmGB va vb)*g) (headAdd g)
  where
    (ca,va) = headV f
    (cb,vb) = headV g

divs :: Value -> Value -> Bool
divs f g = va == lcm'
  where
    (ca,va) = headV f
    (cb,vb) = headV g
    lcm' = lcmGB va vb

reduction :: Value -> Value -> (Value,Value)
reduction f g =
  if va == lcm'
  then
    let (a,b) = reduction (expand (f - c*g)) g
    in (c+a,b)
  else
    case mt of
    0 -> (0,h)
    t -> let (a,b) = reduction t g
         in (a,b+h)
  where
    (ca,va) = headV f
    (cb,vb) = headV g
    lcm' = lcmGB va vb
    h = headAdd f
    mt = tailAdd f
    c = (divGB lcm' vb)*ca/cb

reductions :: Value -> [Value] -> Value
reductions f [] = f
reductions f (g:gs) =
  let (a,b) = reduction f g
  in case b of
     0 -> 0
     c -> expand $ reductions (expand c) gs

allPair [] = []
allPair (x:xs) = map (\x' -> (x,x')) xs ++ allPair xs
  
grobnerG :: [Value] -> [Value]
grobnerG formulas = filter ((/=) 0) $ map (uncurry sGB) $ allPair formulas


grobnerBasis :: [Value] -> [Value]
grobnerBasis formulas = map lc1 $ grobnerBasis' formulas $ allPair formulas
{-  where
    fs = 
    fs' = map headAdd fs
    fs'' =  map (\f -> lcmGB f ) fs'
    (ca,va) = headV f
    (cb,vb) = headV g
    lcm' f g = lcmGB va vb
-}

lc1 :: Value -> Value
lc1 formula = expand $ expand $ formula / ca
  where
    (ca,va) = headV formula

grobnerBasis' :: [Value] -> [(Value,Value)] -> [Value]
grobnerBasis' formulas [] = formulas
grobnerBasis' formulas aa@((a,b):other) =
  case reductions (sGB a b) formulas of
  0 -> grobnerBasis' formulas other
  c -> grobnerBasis (formulas++[c])

grobnerBasisIO :: [Value] -> IO [Value]
grobnerBasisIO formulas = grobnerBasisIO' formulas $ allPair formulas

grobnerBasisIO' :: [Value] -> [(Value,Value)] -> IO [Value]
grobnerBasisIO' formulas [] = return formulas
grobnerBasisIO' formulas aa@((a,b):other) = do
  print "formulas"
  print formulas
  print "div"
  print aa
  print "a"
  print a
  print "b"
  print b
  print "sGB"
  print (sGB a b)
  print "r"
  print (reductions (sGB a b) formulas)
  case reductions (sGB a b) formulas of
    0 -> grobnerBasisIO' formulas other
    c -> grobnerBasisIO (formulas++[c])

(x,y,z)=(val "x",val "y",val "z")

fs = [((1 + ((-3) * (x ** 2))) + (x ** 3)) + ((-1) * y),
      ((-1) + ((-1) * (x ** 2))) + (y ** 2)
     ]
