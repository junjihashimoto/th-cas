{-#LANGUAGE OverloadedStrings#-}

module Algebra.CAS.Base where

import Data.String
import Data.List(nub)

-- | Mathematical constant expression
data Const =
   Zero  -- ^ Zero
 | One   -- ^ One
 | CI Integer -- ^ Integer
 | CF Integer Integer  -- ^ Faction = CF numer denom
 | CR Double -- ^ Real Number
 deriving (Eq,Show,Read)

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
  sqrt (CI a) | a2 == a = CI a1
              | otherwise = CR $ sqrt $ fromRational $ toRational a
    where
      a0 = sqrt (fromIntegral a) :: Double
      a1 = round a0 :: Integer
      a2 = a1 * a1
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

toInt :: Const -> Maybe Integer
toInt Zero = Just 0
toInt One = Just 1
toInt (CI a) = Just a
toInt _ = Nothing

mapTuple :: (a -> b) -> (a,a) ->  (b,b)
mapTuple f (a,b) = (f a, f b)

instance Integral Const where
  quot a b = fst $ quotRem a b
  rem a b = snd $ quotRem a b
  quotRem a b =
    case (toInt a,toInt b) of
    (Just a',Just b') -> mapTuple (constSimplify.CI) $ quotRem a' b'
    _ -> if a == b then (1,0) else (0,a)
  div = quot
  mod = rem
  toInteger Zero = 0
  toInteger One = 1
  toInteger (CI a) = toInteger a
  toInteger a = error $ "can not do toInteger:" ++ show a


data SpecialFunction =
   Sin Formula
 | Cos Formula
 | Tan Formula
 | Sinh Formula
 | Cosh Formula
 | Tanh Formula
 | Asin Formula
 | Acos Formula
 | Atan Formula
 | Asinh Formula
 | Acosh Formula
 | Atanh Formula
 | Exp Formula
 | Log Formula
 | Abs Formula
 | Sig Formula
 | LogBase Formula Formula
 | Sqrt Formula
 | Diff Formula Formula
 | Integrate Formula Formula
 deriving (Show,Read,Eq,Ord)

-- | Mathematical expression
data Formula =
   C Const -- ^ Constant value
 | Pi      -- ^ Pi
 | I       -- ^ Imaginary Number
 | CV String -- ^ Constant variable which is used to deal variable(V Name) as constant value
 | V String  -- ^ Variable
 | S SpecialFunction  -- ^ Special Functions (sin, cos, exp and etc..)
 | Formula :^: Formula
 | Formula :*: Formula
 | Formula :+: Formula
 | Formula :/: Formula
 deriving (Eq,Read)

(=:) :: Formula -> Formula -> Formula
(=:) a b = a - b
infix 0 =:

instance Ord Formula where
  compare (C a) (C b) = compare a b
  compare (C _) Pi = LT
  compare (C _) I = LT
  compare (C _) (CV _) = LT
  compare (C _) _ = LT
  compare Pi (C _) = GT
  compare Pi Pi = EQ
  compare Pi I = LT
  compare Pi (CV _) = LT
  compare Pi _ = LT
  compare I (C _) = GT
  compare I Pi = GT
  compare I I = EQ
  compare I (CV _) = LT
  compare I _ = LT
  compare (CV _) (C _) = GT
  compare (CV _) Pi = GT
  compare (CV _) I = GT
  compare (CV a) (CV b) = compare a b
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

tryPlus :: Formula -> Formula -> Maybe Formula
tryPlus (C Zero) (C Zero) = Just (C Zero)
tryPlus (C Zero) a = Just a
tryPlus a (C Zero) = Just a
tryPlus (C a) (C b) = Just $ C (a+b)
tryPlus I I = Just $ C (CI 2) :*: I
tryPlus a@(V _) b@(V _) | a == b = Just $ C (CI 2) :*: a
                        | otherwise = Nothing
tryPlus a@(V _:^: _) b@(V _:^: _) | a == b = Just $ C (CI 2) :*: a
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

insertPlus :: Formula -> Formula -> Formula
insertPlus a'@(a:+:b) v | v <= b = insertPlus a v :+: b
                       | otherwise = a':+:v
insertPlus a v | a <= v = a :+: v
               | otherwise = v :+: a

-- | try simplification for multiply
-- 
-- >>> let [x,y] = map V ["x","y"]
-- >>> tryMul (x**(-1)) x
-- Just 1
tryMul :: Formula -> Formula -> Maybe Formula
tryMul I I = Just $ C neg
tryMul (C Zero) _ = Just $ C Zero
tryMul _ (C Zero) = Just $ C Zero
tryMul (C One) a = Just a
tryMul a (C One) = Just a
tryMul (C a) (C b) = Just $ C (a*b)
tryMul a@(V _) b@(V _) | a == b = Just $ a :^: C (CI 2)
                       | otherwise = Nothing
tryMul (a@(V _):^: b) (c@(V _):^:d) | a == c = Just $ if b+d==0 then 1 else a :^: (b+d)
                                        | otherwise = Nothing
tryMul a@(V _) (c@(V _):^:d) | a == c = Just $ if 1+d == 0 then 1 else  a :^: (1+d)
                               | otherwise = Nothing
tryMul (a@(V _):^: b) c@(V _) | a == c = Just $ if b+1 == 0 then 1 else a :^: (b+1)
                                | otherwise = Nothing
tryMul (a@(V _):/: b) c | b == c = Just a
                        | otherwise = Nothing
tryMul (a:*:b) c = 
  case tryMul b c of
  Nothing ->
    case tryMul a c of
    Nothing -> Nothing
    Just v -> Just $ v * b
  Just v -> Just $ a * v
tryMul _ _ = Nothing

insertMul :: Formula -> Formula -> Formula
insertMul a'@(a:*:b) v | v <= b = insertMul a v :*: b
                       | otherwise = a':*:v
insertMul a v | a <= v = a :*: v
              | otherwise = v :*: a


constDiv :: Formula -> Formula -> Formula
constDiv a'' b'' = 
  case (a'',b'') of
  (C a',C b') -> C (a'/b')
  (C Zero,_) -> C Zero
  (_,C Zero) -> error "divide by zero"
  (C One,b) -> C One :/: b
  (a,C One) -> a
  (a,C c) -> C (1/c) * a
  (a,b) | a == b -> C One
        | otherwise -> a :/: b

splitExp :: Formula -> (Formula,Formula)
splitExp (a:^:b) = (a,b)
splitExp a = (a,1)

divGB :: Formula -> Formula -> Formula
divGB a b = conv $ (ca `constDiv` cb) * divGB' va vb
  where
    (ca,va) = head' a
    (cb,vb) = head' b
    head' :: Formula -> (Formula,Formula)
    head' v' = var (firstTerm,1)
      where
        firstTerm = v'
        var (c,v) =
          if isConst c
          then (c,v)
          else var (tailMul c,headMul c*v)
    conv (a':*:(C One :/: c)) = a':/:c
    conv a' = a'

divGB' :: Formula -> Formula -> Formula
divGB' 1 1 = 1
divGB' a 1 = a
divGB' 1 a = 1 `constDiv` a
divGB' a b =
  if hva == hvb
  then divGB' ta tb * (hva ** (hpa- hpb))
  else if hva < hvb
       then divGB' a tb `constDiv` (hvb ** hpb)
       else divGB' ta b * (hva ** hpa)
  where
    (hva,hpa) = splitExp $ headMul a
    (hvb,hpb) = splitExp $ headMul b
    ta = tailMul a
    tb = tailMul b

divAll :: Formula -> Formula -> Formula
divAll a b = expand $ t + (h/b)
  where
    h = headAdd a
    t = case (tailAdd a) of
      0 -> 0
      v -> divAll v b

instance Num Formula where
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

instance Fractional Formula where
  fromRational 0 = C Zero
  fromRational 1 = C One
  fromRational a = C $ CR (fromRational a)
  recip a = (/) (C One) a
  (/) a b = divGB a b

instance Floating Formula where
  pi = Pi
  exp (C Zero) = C One
  exp a = S $ Exp a
  sqrt (C Zero) = 0
  sqrt (C One) = 1
  sqrt a'@(C (CI a)) | a < 0 = I * sqrt (-a')
                     | a2 == a = C $ CI a1
                     | otherwise = S $ Sqrt a'
    where
      a0 = sqrt (fromIntegral a) :: Double
      a1 = round a0 :: Integer
      a2 = a1 * a1
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

instance IsString Formula where
  fromString = val

-- | Lift String to variable of Formula
val ::  String ->  Formula
val v = V v

-- | Lift String to constant of Formula
cval :: String ->  Formula
cval v = CV v

instance Enum Formula where
  succ a = a+1
  pred a = a-1
  toEnum v = fromIntegral v
  fromEnum (C Zero) = 0
  fromEnum (C One) = 1
  fromEnum (C (CI a)) = fromIntegral a
  fromEnum a = error $ "can not do fromEnum:" ++ show a

instance Real Formula where
  toRational (C (CI v)) = toRational v
  toRational (C (CR v)) = toRational v
  toRational Pi = toRational (pi::Double)
  toRational (C Zero) = toRational (0::Int)
  toRational (C One) = toRational (1::Int)
  toRational _ = toRational (0::Int)


lcmMonomial :: Formula -> Formula -> Formula
lcmMonomial a b = lcmV ca cb * lcmMonomial' va vb
  where
    (ca,va) = headV a
    (cb,vb) = headV b
    lcmV :: Formula -> Formula -> Formula
    lcmV (C a') (C b') = C (lcm a' b')
    lcmV a' b' = a' * b'

lcmMonomial' :: Formula -> Formula -> Formula
lcmMonomial' 1 1 = 1
lcmMonomial' a 1 = a
lcmMonomial' 1 a = a
lcmMonomial' a b = 
  if hva == hvb
  then lcmMonomial' ta tb * (hva ** max hpa hpb)
  else if hva < hvb
       then lcmMonomial' a tb * (hvb ** hpb)
       else lcmMonomial' ta b * (hva ** hpa)
  where
    (hva,hpa) = splitExp $ headMul a
    (hvb,hpb) = splitExp $ headMul b
    ta = tailMul a
    tb = tailMul b

reduction :: Formula -> Formula -> (Formula,Formula)
reduction f g =
  if va == vl
  then
    if va == 1
    then (ca/cb,0)
    else
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
    lcm' = lcmMonomial va vb
    (cl,vl) = headV lcm'
    h = headAdd f
    mt = tailAdd f
    c = (lcm' / vb)*ca/cb

reductions :: Formula -> [Formula] -> Formula
reductions f [] = f
reductions f (g:gs) =
  let (_,b) = reduction f g
  in case b of
     0 -> 0
     c -> expand $ reductions (expand c) gs

instance Integral Formula where
  quot a b = fst $ quotRem a b
  rem a b = snd $ quotRem a b
  quotRem = reduction
  div = quot
  mod = rem
  toInteger (C Zero) = 0
  toInteger (C One) = 1
  toInteger (C (CI a)) = toInteger a
  toInteger a = error $ "can not do toInteger:" ++ show a

degree :: Formula -> Formula
degree (C _) = 0
degree (CV _) = 0
degree (V _) = 1
degree (S (Sin v)) = degree v
degree (S (Cos v)) = degree v
degree (S (Tan v)) = degree v
degree (S (Sinh v)) = degree v
degree (S (Cosh v)) = degree v
degree (S (Tanh v)) = degree v
degree (S (Asin v)) = degree v
degree (S (Acos v)) = degree v
degree (S (Atan v)) = degree v
degree (S (Asinh v)) = degree v
degree (S (Acosh v)) = degree v
degree (S (Atanh v)) = degree v
degree (S (Exp v)) = degree v
degree (S (Log v)) = degree v
degree (S (Abs v)) = degree v
degree (S (Sig v)) = degree v
degree (S (LogBase v0 v1)) = max (degree v0) (degree v1)
degree Pi = 0
degree I = 0
degree (S (Sqrt v)) = degree v
degree (S (Diff v0 v1)) = max (degree v0) (degree v1)
degree (S (Integrate v0 v1)) =  max (degree v0) (degree v1)
degree (v0 :^: v1) = v1 * degree v0
degree (v0 :*: v1) = max (degree v0) (degree v1)
degree (v0 :+: v1) = max (degree v0) (degree v1)
degree (v0 :/: v1) = max (degree v0) (degree v1)


converge ::  (Formula ->  Formula) -> Formula -> Formula
converge func v =
  let v' = func v
  in if v' ==  v
     then v'
     else converge func v'

-- | try to reduce a variable.
-- >>> let [a,b,c] = map CV ["a","b","c"]
-- >>> let [x,y,z] = map V ["x","y","z"]
-- >>> let [f0,f1] = [(2*x+4*y+4),(x-2*y+1)]
-- >>> f0*f1
-- (1 + x + (-2)*y)*(4 + 2*x + 4*y)
-- >>> expand $ f0*f1
-- 4 + 6*x + 2*(x^2) + (-4)*y + (-8)*(y^2)
-- >>> expand $ (a*x-2*b*y+c)
-- c + a*x + (-2)*b*y
-- >>> expand $ - ((-2*b)/(4*b))
-- (-1)*(((-2)*b)/(4*b))
-- >>> expand $ - ((-2*b)/(4*b))*(2*a*x+4*b*y+4*c)
-- (-4)*(((-2)*b)/(4*b))*c + (-2)*a*(((-2)*b)/(4*b))*x + (-4)*(((-2)*b)/(4*b))*b*y
-- >>> expand $ (a*x-2*b*y+c) - ((-2*b)/(4*b))*(2*a*x+4*b*y+4*c)
-- c + (-4)*c*(((-2)*b)/(4*b)) + a*x + (-2)*(((-2)*b)/(4*b))*a*x + (-4)*b*(((-2)*b)/(4*b))*y + (-2)*b*y
expand :: Formula -> Formula
expand f = expand' 100 f


expand' :: Int -- ^ a number of repetition of expanding formula
        -> Formula -- ^ original formula
        -> Formula -- ^ expanded formula
expand' d f | d <= 0 = f
            | otherwise =
  case f of
    ((a:+:b):*:c) -> let (a',b',c') = (expand' (d-1) a,expand' (d-1) b,expand' (d-1) c) in expand' (d-1) (a'*c') + expand' (d-1) (b'*c')
    (a:*:(b:+:c)) -> let (a',b',c') = (expand' (d-1) a,expand' (d-1) b,expand' (d-1) c) in expand' (d-1) (a'*b') + expand' (d-1) (a'*c')
    (a:+:b) -> let (a',b') = (expand' (d-1) a,expand' (d-1) b) in  a'+b'
    c@(a:*:b) -> let (a',b') = (expand' (d-1) a,expand' (d-1) b)
                     c' = a'*b'
                 in if c==c' then c' else expand' (d-1) c'
    (a:/:1) -> expand' (d-1) a
    a -> a

expandIO :: Formula -> IO Formula
expandIO f = do
  print f
  case f of
    ((a:+:b):*:c) -> do
      print "((a:+:b):*:c)"
      print a
      print b
      print c
      a' <- expandIO a
      b' <- expandIO b
      c' <- expandIO c
      ac <- expandIO (a'*c')
      bc <- expandIO (b'*c')
      return $ ac + bc
    (a:*:(b:+:c)) -> do
      print "(a:*:(b:+:c))"
      print a
      print b
      print c
      a' <- expandIO a
      b' <- expandIO b
      c' <- expandIO c
      ab <- expandIO (a'*b')
      ac <- expandIO (a'*c')
      return $ ab + ac
    (a:+:b) -> do
      print "(a:+:b)"
      print a
      print b
      a' <- expandIO a
      b' <- expandIO b
      return $ a'+b'
    c@(a:*:b) -> do
      print "(a:*:b)"
      print a
      print b
      a' <- expandIO a
      b' <- expandIO b
      let c' = a'*b'
      if c==c' then return c' else expandIO c'
    (a:/:1) -> return $ expand a
    a -> return a


gcdPolynomial :: Formula -> Formula -> Formula
gcdPolynomial a b | a == 0 = b
                  | b == 0 = a
                  | otherwise =
                      let (a',b') = if a>=b then (a,b) else (b,a)
                          r = a' `rem` b'
                      in case r of
                         0 -> b'
                         _ -> if r == a' then 1 else gcdPolynomial r b'

lcmPolynomial :: Formula -> Formula -> Formula
lcmPolynomial a b =
  let g = gcdPolynomial a b
      d0 = a `div` g
      d1 = b `div` g
  in expand $ d0*d1*g

headAdd :: Formula -> Formula
headAdd (_ :+: ab) = ab
headAdd ab = ab
tailAdd :: Formula -> Formula
tailAdd (a :+: _) = a
tailAdd _ = 0
mapAdd :: (Formula -> Formula) -> Formula -> Formula
mapAdd func formula =
  case t of
  0 -> func h
  _ -> mapAdd func t + func h
  where
    h = headAdd formula
    t = tailAdd formula

splitAdd :: Formula -> [Formula]
splitAdd formula =
  case t of
  0 -> [h]
  _ -> h : splitAdd t
  where
    h = headAdd formula
    t = tailAdd formula

headMul :: Formula -> Formula
headMul (_ :*: ab) = ab
headMul ab = ab
tailMul :: Formula -> Formula
tailMul (a :*: _) = a
tailMul _ = 1

headDiv :: Formula -> Formula
headDiv (_ :/: ab) = ab

headDiv ab = ab
tailDiv :: Formula -> Maybe Formula
tailDiv (a :/: _) = Just a
tailDiv _ = Nothing


-- | substitute expression
-- 
-- >>> let [x,y,z] = map V ["x","y","z"]
-- >>> subst [(x,3),(y,5)] $ x+y
-- 8
-- >>> subst [(tan(x),z)] (tan(x)**2+1)
-- 1 + z^2
subst :: [(Formula,Formula)] -- ^ List of tuple(orignal term, new term)
      -> Formula  -- ^ original formula
      -> Formula  -- ^ replaced formula
subst [] formula = formula
subst ((org,mod'):other) formula = subst other $ subst' org mod' formula

subst' :: Formula -> Formula -> Formula -> Formula
subst' org new formula =
  if org == formula
  then new
  else case formula of
         a@(C _) -> a
         a@(CV _) -> a
         a@Pi -> a
         a@I -> a
         a@(V _) -> a
         (S (Sin v)) -> S $ Sin $ subst' org new v
         (S (Cos v)) -> S $ Cos $ subst' org new v
         (S (Tan v)) -> S $ Tan $ subst' org new v
         (S (Sinh v)) -> S $ Sinh $ subst' org new v
         (S (Cosh v)) -> S $ Cosh $ subst' org new v
         (S (Tanh v)) -> S $ Tanh $ subst' org new v
         (S (Asin v)) -> S $ Asin $ subst' org new v
         (S (Acos v)) -> S $ Acos $ subst' org new v
         (S (Atan v)) -> S $ Atan $ subst' org new v
         (S (Asinh v)) -> S $ Asinh $ subst' org new v
         (S (Acosh v)) -> S $ Acosh $ subst' org new v
         (S (Atanh v)) -> S $ Atanh $ subst' org new v
         (S (Exp v)) -> S $ Exp $ subst' org new v
         (S (Log v)) -> S $ Log $ subst' org new v
         (S (Abs v)) -> S $ Abs $ subst' org new v
         (S (Sig v)) -> S $ Sig $ subst' org new v
         (S (LogBase v1 v2)) -> S $ LogBase (subst' org new v1) (subst' org new v2)
         (S (Sqrt v)) -> S $ Sqrt $ subst' org new v
         (S (Diff v1 v2)) -> S $ Diff (subst' org new v1) (subst' org new v2)
         (S (Integrate v1 v2)) -> S $ Integrate (subst' org new v1) (subst' org new v2)
         (a:^:b) -> subst' org new a ** subst' org new b
         (a:*:b) -> subst' org new a * subst' org new b
         (a:+:b) -> subst' org new a + subst' org new b
         (a:/:b) -> subst' org new a / subst' org new b


mapFormula :: (Formula -> Formula) -> Formula -> Formula
mapFormula conv a@(C _) = conv a
mapFormula conv a@(CV _) = conv a
mapFormula conv a@Pi = conv a
mapFormula conv a@I = conv a
mapFormula conv a@(V _) = conv a
mapFormula conv (S (Sin v)) = S $ Sin $ mapFormula conv v
mapFormula conv (S (Cos v)) = S $ Cos $ mapFormula conv v
mapFormula conv (S (Tan v)) = S $ Tan $ mapFormula conv v
mapFormula conv (S (Sinh v)) = S $ Sinh $ mapFormula conv v
mapFormula conv (S (Cosh v)) = S $ Cosh $ mapFormula conv v
mapFormula conv (S (Tanh v)) = S $ Tanh $ mapFormula conv v
mapFormula conv (S (Asin v)) = S $ Asin $ mapFormula conv v
mapFormula conv (S (Acos v)) = S $ Acos $ mapFormula conv v
mapFormula conv (S (Atan v)) = S $ Atan $ mapFormula conv v
mapFormula conv (S (Asinh v)) = S $ Asinh $ mapFormula conv v
mapFormula conv (S (Acosh v)) = S $ Acosh $ mapFormula conv v
mapFormula conv (S (Atanh v)) = S $ Atanh $ mapFormula conv v
mapFormula conv (S (Exp v)) = S $ Exp $ mapFormula conv v
mapFormula conv (S (Log v)) = S $ Log $ mapFormula conv v
mapFormula conv (S (Abs v)) = S $ Abs $ mapFormula conv v
mapFormula conv (S (Sig v)) = S $ Sig $ mapFormula conv v
mapFormula conv (S (LogBase v1 v2)) = S $ LogBase (mapFormula conv v1) (mapFormula conv v2)
mapFormula conv (S (Sqrt v)) = S $ Sqrt $ mapFormula conv v
mapFormula conv (S (Diff v1 v2)) = S $ Diff (mapFormula conv v1) (mapFormula conv v2)
mapFormula conv (S (Integrate v1 v2)) = S $ Integrate (mapFormula conv v1) (mapFormula conv v2)
mapFormula conv (a:^:b) = mapFormula conv a ** mapFormula conv b
mapFormula conv (a:*:b) = mapFormula conv a * mapFormula conv b
mapFormula conv (a:+:b) = mapFormula conv a + mapFormula conv b
mapFormula conv (a:/:b) = mapFormula conv a / mapFormula conv b


-- | When formula does not include variable,
-- isConst returns True.
-- 
-- >>> let x = "x" :: Formula
-- >>> isConst x
-- False
-- >>> isConst $ sin(x)*3
-- False
-- >>> isConst $ 3.0 * sin(3.0)
-- True
isConst :: Formula ->  Bool
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
isConst I = True
isConst (S (Sqrt v)) = isConst v
isConst (S (Diff v0 v1)) = isConst v0 &&  isConst v1
isConst (S (Integrate v0 v1)) = isConst v0 &&  isConst v1
isConst (v0 :^: v1) = isConst v0 &&  isConst v1
isConst (v0 :*: v1) = isConst v0 &&  isConst v1
isConst (v0 :+: v1) = isConst v0 &&  isConst v1
isConst (v0 :/: v1) = isConst v0 &&  isConst v1

hasVariable :: Formula -> Formula -> Bool
hasVariable f v = elem v $ variables f

isVariable :: Formula -> Bool
isVariable = not.isConst

variables :: Formula ->  [Formula]
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
variables I = []
variables (S (Sqrt v)) = variables v
variables (S (Diff v0 v1)) = variables v0 ++  variables v1
variables (S (Integrate v0 v1)) = variables v0 ++  variables v1
variables (v0 :^: v1) = variables v0 ++  variables v1
variables (v0 :*: v1) = variables v0 ++  variables v1
variables (v0 :+: v1) = variables v0 ++  variables v1
variables (v0 :/: v1) = variables v0 ++  variables v1

denom :: Formula -> Formula
denom (_ :*: (_:/:b)) = b
denom (_:/:b) = b
denom _ = 1

numer :: Formula -> Formula
numer (a :*: (b:/:_)) = a * b
numer (a:/:_) = a
numer a = a

headV :: Formula -- ^ formula
      -> (Formula,Formula) -- ^ (coefficient of first term,variables of first term)
headV v' = var (firstTerm,1)
  where
    firstTerm = headAdd v'
    var (c,v) =
      if isConst c
      then (c,v)
      else var (tailMul c,headMul c*v)

-- | Pretty print for Formula type.
-- Formula's show function is the same as this.
ppr :: Formula -> String
ppr (C Zero) = "0"
ppr (C One) = "1"
ppr (C (CI a)) = show a
ppr (C (CF a b)) = show a ++"/"++show b
ppr (C (CR a)) = show a
ppr Pi = "π"
ppr I = "i"
ppr (CV v) = v
ppr (V v) = v
ppr (S (Exp v)) = "e(" ++ ppr v ++")"
ppr (S (Log v)) = "log(" ++ ppr v ++")"
ppr (S (Sqrt v)) = "√(" ++ ppr v ++")"
ppr (S (Diff f x)) = "diff(" ++ ppr f ++","++ppr x++")"
ppr (S (Integrate f x)) ="integrate(" ++ ppr f ++","++ppr x++")"
ppr (S (LogBase a b)) = "log_" ++ ppr a++ "(" ++ppr b ++")"
ppr (S (Sig v)) = "sig(" ++ ppr v ++")"
ppr (S (Abs v)) = "|" ++ ppr v ++"|"
ppr (S (Sin v)) = "sin(" ++ ppr v ++")"
ppr (S (Cos v)) = "cos(" ++ ppr v ++")"
ppr (S (Tan v)) = "tan(" ++ ppr v ++")"
ppr (S (Sinh v)) = "sinh(" ++ ppr v ++")"
ppr (S (Cosh v)) = "cosh(" ++ ppr v ++")"
ppr (S (Tanh v)) = "tanh(" ++ ppr v ++")"
ppr (S (Asin v)) = "asin(" ++ ppr v ++")"
ppr (S (Acos v)) = "acos(" ++ ppr v ++")"
ppr (S (Atan v)) = "atan(" ++ ppr v ++")"
ppr (S (Asinh v)) = "asinh(" ++ ppr v ++")"
ppr (S (Acosh v)) = "acosh(" ++ ppr v ++")"
ppr (S (Atanh v)) = "atanh(" ++ ppr v ++")"
ppr (a:^:b) = ppr a ++"^"++ ppr b
ppr (a'@(_:*:_):*:c) = ppr a'++"*" ++ ppr' c
ppr (a:*:b) = ppr' a ++"*"++ ppr' b
ppr (a:+:b) = ppr a ++" + "++ ppr b
ppr (a:/:b) = "(" ++ ppr a ++")/("++ ppr b ++")"

ppr' :: Formula -> String
ppr' c@(C (CI _)) = if c >= 0 then ppr c else "(" ++ ppr c ++ ")"
ppr' c@(C (CR _)) = if c >= 0 then ppr c else "(" ++ ppr c ++ ")"
ppr' c@I = ppr c
ppr' c@Pi = ppr c
ppr' c@(V _) = ppr c
ppr' c@(CV _) = ppr c
ppr' c = "(" ++ ppr c ++ ")"

instance Show Formula where
  show = ppr

-- | This print shows bare structure of Formula type.
-- This string can be read by Formula's read function.
showFormula :: Formula -> String
showFormula (C a) = "C (" ++ show a ++")"
showFormula Pi = "Pi"
showFormula I = "I"
showFormula (CV v) = "CV \"" ++ v ++"\""
showFormula (V v) = "V \"" ++ v ++"\""
showFormula (S a) = "S (" ++ show a ++")"
showFormula (a:^:b) = "(" ++ showFormula a ++" :^: "++ showFormula b ++")"
showFormula (a:*:b) = "(" ++ showFormula a ++" :*: "++ showFormula b ++")"
showFormula (a:+:b) = "(" ++ showFormula a ++" :+: "++ showFormula b ++")"
showFormula (a:/:b) = "(" ++ showFormula a ++" :/: "++ showFormula b ++")"



genCoeff :: String -> Int -> [Formula]
genCoeff prefix a = genCoeff' prefix a
  where
    len = fromIntegral (round (logBase 10 (fromIntegral a))) :: Int
    nstr n =
      let str = show n
          l = length str
      in take (len-l) (repeat '0') ++ str
    genCoeff' prefix' a' | a' <=0 = []
                         | otherwise = CV (prefix' ++ nstr (pred a')) : genCoeff' prefix' (pred a')


genVars :: String -> Int -> [Formula]
genVars prefix a = gen' prefix a
  where
    len = fromIntegral (round (logBase 10 (fromIntegral a))) :: Int
    nstr n =
      let str = show n
          l = length str
      in take (len-l) (repeat '0') ++ str
    gen' prefix' a' | a' <=0 = []
                    | otherwise = V (prefix' ++ nstr (pred a')) : gen' prefix' (pred a')


-- | Find indeterminates of an expression
-- 
-- >>> let [x,y,z] = map V ["x","y","z"]
-- >>> indets (x*y+z/x)
-- [x,y,z]
-- >>> indets (3*x^2-x*y-y^2)
-- [x,y]
-- >>> indets (sin(x)*cos(x)**2)
-- [sin(x),x,cos(x)]
indets :: Formula ->  [Formula]
indets = nub.indets'

indets' :: Formula ->  [Formula]
indets' (C _) = []
indets' (CV _) = []
indets' a@(V _) = [a]
indets' a@(S (Sin v)) = a:indets' v
indets' a@(S (Cos v)) = a:indets' v
indets' a@(S (Tan v)) = a:indets' v
indets' a@(S (Sinh v)) = a:indets' v
indets' a@(S (Cosh v)) = a:indets' v
indets' a@(S (Tanh v)) = a:indets' v
indets' a@(S (Asin v)) = a:indets' v
indets' a@(S (Acos v)) = a:indets' v
indets' a@(S (Atan v)) = a:indets' v
indets' a@(S (Asinh v)) = a:indets' v
indets' a@(S (Acosh v)) = a:indets' v
indets' a@(S (Atanh v)) = a:indets' v
indets' a@(S (Exp v)) = a:indets' v
indets' a@(S (Log v)) = a:indets' v
indets' a@(S (Abs v)) = a:indets' v
indets' a@(S (Sig v)) = a:indets' v
indets' a@(S (LogBase v0 v1)) = a:indets' v0 ++  a:indets' v1
indets' Pi = []
indets' I = []
indets' a@(S (Sqrt v)) = a:indets' v
indets' a@(S (Diff v0 v1)) = (a:indets' v0) ++  indets' v1
indets' a@(S (Integrate v0 v1)) = (a:indets' v0) ++  indets' v1
indets' (v0 :^: v1) = indets' v0 ++  indets' v1
indets' (v0 :*: v1) = indets' v0 ++  indets' v1
indets' (v0 :+: v1) = indets' v0 ++  indets' v1
indets' (v0 :/: v1) = indets' v0 ++  indets' v1

maskVariables :: Formula -- ^ Original formula which is not masked
              -> Formula -- ^ Not masked variable
              -> (Formula, (Formula -> Formula)) -- ^ (masked formula, reverse function)
maskVariables f x =
  let vs = filter ((/=) x) $ variables f
      toCV (V v) = (CV v)
      toCV a = a
      toV (CV v) = (V v)
      toV a = a
      v2cv = zip vs (map toCV vs)
      cv2v = zip (map toCV vs) (map toV vs)
      f' = subst v2cv f
  in (f',subst cv2v)


-- | Greatest common divisor of the coefficients of formula with respect to variable of second function-args
--
-- >>> let [x,y] = map V ["x","y"]
-- >>> content (-4*x*y+6*y^2) x
-- ((-4)*y,x + (3/-2)*y)
content :: Formula -- ^ formula
        -> Formula -- ^ variable
        -> (Formula,Formula) -- ^ (gcd-result,formula/gcd-result)
content f x =
  let (f',rev) = maskVariables (expand f) x
      (c,_) = headV f'
      gcdr = gcdPolynomial f (rev c)
  in (gcdr,f `quot` gcdr)
