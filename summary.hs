{-# OPTIONS_GHC -Wincomplete-patterns -Wmissing-methods -Wno-noncanonical-monad-instances #-}
{-# LANGUAGE DeriveFoldable, DeriveFunctor, InstanceSigs, ScopedTypeVariables, DeriveTraversable #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}

module Summary where 

import Prelude
import Control.Monad
import Control.Applicative
import Data.Char
import Data.Functor

{- Algebraic data types -}



xor :: Bool -> Bool -> Bool
xor a b = a /= b

{-How to use case notation in a simple usecase-}

xor' :: Bool -> Bool -> Bool
xor' a b = case a of
    True -> not b  --if a is True then not b
    False -> b

{-The usecase of 'where'-}
function :: Int-> Int -> Bool
function a b = f a b where
    f :: Int -> Int -> Bool
    f a b = a == b

{-Beadja az a,b -t az f függvénynek ami annyit a functionon belül definiálunk-}

{-Az id függvény-}

id':: a -> a
--id' = id --xd
id' a = a

-- data Either a b = Left a | Right b
eitherShowCase :: Either a b -> Bool
eitherShowCase a = case a of
    Left a -> False
    Right b -> True

--data Maybe a = Just a | Nothing
eitherShowCase' :: Either a b -> Maybe a
eitherShowCase' a = case a of
    Left a -> Just a
    Right b -> Nothing

{--------------------------------Lists-----------------------------------------}


{-Function that applies all functions on a variable listed in a list-}
applyMany :: [a->b] -> a -> [b]
applyMany [] a = []
applyMany (x:xs) a = x a : applyMany xs a 

applyMany' :: [a->b] -> a -> [b]
applyMany' l a = map ($a) l --($) :: (a -> b) -> a -> b
--applyMany' l a = foldr (\f b -> (f a ) : b) [] l



{---------------------Classes--------------------------------}

class Eq' a where
    eql :: a -> a -> Bool 

class Eq' a => Ord' a where
    lte :: a -> a -> Bool

class Show' a where
    show' :: a -> String

data Color = Green | Yellow | Blue | Red
data List a = Cons a (List a) | Nil
    deriving(Show)
infixr 5 `Cons` ---fogalmam sincs mit jelent xddd

{--Instance declaration--}


instance Eq' Color where
    eql Green Green = True
    eql Yellow Yellow = True
    eql Blue Blue = True
    eql Red Red = True
    eql _ _ = False

instance Ord' Color where
    lte _ Red = True
    lte Blue _ = True
    {--...--}
    lte _ _ = False

instance Show' Color where
    show' Red = "Red"
    show' Blue = "Blue"
    show' Green = "Green"
    show' Yellow = "Yellow"

instance Eq' a => Eq' (List a) where -- This only work for Colour lists in this instance as Eq' is only defined for Colors
    eql Nil Nil = True
    eql (Cons a as) (Cons b bs) = eql a b && eql as bs
    eql _ _ = False
-- *Summary> show' Green 
-- >"Green"

instance Ord' a => Ord' (List a) where -- the longer list is bigger than the shorter one
    lte Nil Nil = False
    lte Nil (Cons a as) = True
    lte (Cons a as) Nil = False
    lte (Cons a as) (Cons b bs) = lte a b && lte as bs
    
data NonEmpyt a = Const a (NonEmpyt a) | Last a

instance Eq' a => Eq' (NonEmpyt a) where
    eql (Last a) (Last b) =  eql a b
    eql (Summary.Const a as) (Summary.Const b bs) = eql a b && eql as bs
    eql _ _ = False

--ord ua mint előbb csak ha egyenlő hosszúak akkor a head dönt

{-------------------------Foldable instances --------------------------------}


--data List a = Cons a (List a) | Nil
instance Foldable List where
    foldr _ d Nil = d
    foldr f d (Cons a as) = f a (foldr f d as)

--data NonEmpyt a = Const a (NonEmpyt a) | Last a
data NonEmpty a = a :| [a] -- (a :| as) -ként lehet az elemeire referálni
    deriving (Eq, Show)

instance Foldable NonEmpty where
    foldr f d (a :| as) = f a $ foldr f d as

--a fafélék megtalálhatóak a második gyakorlati anyagban

data Tree a = Leaf a | Branch (Tree a) (Tree a) deriving (Eq, Show, Foldable)

{-------------------------A foldable felhasználása---------------------------------------}
sumFold::(Foldable f, Num a ) => f a -> a
sumFold a = foldr (+) 0 a

-- *Summary> sumFold [1,2,3,4] 
-- >10
-- *Summary> sumFold $ 5 :| [3,4]
-- > 12

-- Itt azt határozzuk meg a típusban, hogy az a valami olyan datában van ami foldable
-- pont ezért a sumFold 3 error de a sumFold [3] 3

elemFold :: (Foldable f, Eq a) => a -> f a -> Bool
elemFold a l = foldr (\x p-> p  || a == x) False l -- ????????????????


{------Semi grup instance: was only mentioned at our practice----------}
-- a <> ( b <> c ) == (a <> b) <> c     The type has an associative operation
-- where [1,2,3] <> [3,4,5] = [1,2,3,3,4,5]

instance Semigroup (List a) where
    Nil <> a = a
    (Cons x xs) <> ys = Cons x (xs <> ys)

-- Monoid instance, associative binary operation and an identity element

instance Monoid (List a) where
    mempty = Nil -- this is the identity element


--Using these we define the foldable instance

data List_ a = Cons_ a (List_ a) | Nil_ deriving (Eq, Show)

instance Semigroup (List_ a) where
    Nil_ <> a = Nil_
    (Cons_ x xs) <> ys = Cons_ x $ xs <> ys

instance Monoid (List_ a) where
    mempty = Nil_

instance Foldable List_ where ----semmire sem jó de fasza hogy van
    foldMap f Nil_ = mempty
    foldMap f (Cons_ a as) = f a <> foldMap f as


{---A functor típusosztály    -------------------------   fmap -}

instance Functor List where
    fmap _ Nil  = Nil
    fmap f (Cons a as) = Cons (f a) (fmap f as)


instance Functor Tree where
    fmap f (Leaf a) = Leaf $ f a
    fmap f (Branch left right) = Branch (fmap f left) (fmap f right)
    --- fmap operator :<$>
    --- (+3) <$> [1..10] == [4..13]

----------- Put in operator
putIn :: (Functor f) => b -> f a -> f b
putIn b fa = (const b) <$> fa -- == b <$ fa
--3 <$ [1..10] == [3,3,3,3,3,3,3,3,3,3]

---------------functor for a recursive data type

data MyFavouriteType a = Three a a a | Recurse (MyFavouriteType a) | RecurseTwice (MyFavouriteType a) (MyFavouriteType a)
    deriving (Show, Eq, Ord)

instance Functor MyFavouriteType where
    fmap f (Three a b c) = Three (f a) (f b) (f c) 
    fmap f (Recurse a) = Recurse (f <$> a)
    fmap f (RecurseTwice a b) = RecurseTwice (f <$> a) (f <$> b) 

{-------------------------Mellékhatás modellezése---------------------------------------}

{- 
data Maybe a = Just a | Nothing  Itt a nothing a hibaeset

data Either a = Left a| Right b  Itt a left a hibaüzenetet is tárolhatja
-}

mapMaybe :: (a -> Maybe b) -> [a] -> Maybe [b]
mapMaybe f [] = Just []
mapMaybe f (x:xs) = case mapMaybe f xs of
    Nothing -> Nothing
    Just ys -> case f x of
        Nothing -> Nothing
        Just y -> Just (y:ys)

{-
mapMaybe (\x -> if x ==3 then Just True else Nothing) [3,3,3]
mapMaybe ... [3,3]
mapMaybe ... [3]
mapMaybe ... []
Just []
Just 3:[]
Just 3:3:[]
Just 3:3:3:[]
-}


mapEither :: (a -> Either e b) -> [a] -> Either e [b]
mapEither f [] = Right []
mapEither f (x:xs) = case mapEither f xs of
    Left e -> Left e
    Right ys -> case f x of
        Left e' -> Left e'
        Right y-> Right (y:ys)

{- 
*Prelude> mapEither (\x -> if x == 3 then Right True else Left "Error") [1..10]
> Left "Error" 
-}

filterMaybe :: (a -> Maybe Bool) -> [a] -> Maybe [a]
filterMaybe f [] = Just []
filterMaybe f (x:xs) = case filterMaybe f xs of
    Nothing -> Nothing
    Just ys -> case f x of
        Nothing -> Nothing
        Just y -> if y then Just (x:ys) else Just ys

{-
*Summary> filterMaybe (\x -> if x < 10 then Just True else Just False) [3..12]
Just [3,4,5,6,7,8,9]
-}

{--------------------------- Bindolás/ bevezetés ---------------------------------------------}

bindMaybe :: (a -> Maybe b) -> Maybe a -> Maybe b
bindMaybe f  Nothing = Nothing
bindMaybe f (Just x) = f x

{-
A justból is lehet nothing. A maybe belső eleme kihat a külő struktúrára ez a mellékhatás

*Summary> bindMaybe (\x -> if x < 4 then Just 4 else Nothing) (Just 3)
Just 4

*Summary> bindMaybe (\x -> if x < 4 then Just 4 else Nothing) (Just 5)
Nothing
-}

bindEither :: (a -> Either e b) -> Either e a -> Either e b
bindEither f (Left e) = Left e
bindEither f (Right a) = f a

returnMaybe :: a -> Maybe a
returnMaybe a = Just a 

{-
*Summary> bindMaybe (\x -> if x < 4 then Just 4 else Nothing) (returnMaybe 5)
>Nothing
-}

returnEither :: a -> Either e a
returnEither a = Right a

{-
<$> - Functor
-- (<$>) :: Functor f => (a -> b) -> f a -> f b
> a = (*2)
> b = Just 4
> a <$> b
Just 8


<*> - Applicative
-- (<*>) :: Applicative f => f (a -> b) -> f a -> f b
> a = Just (*2)
> b = Just 4
> a <*> b
Just 8

minden monad applicative és minden applicative functor

class Applicative m => Monad m where
    return :: a -> m a
    (>>=) :: m a -> (a -> m b) -> m b

ap :: Monad m => m (a -> b) -> m a -> m b

instance Applicative Maybe where
    (<*>) = ap
    pure = return 

-}

data Maybe' a = Just' a | Nothing' deriving (Eq, Show, Functor)

instance Monad Maybe' where
    return :: a -> Maybe' a
    return a = Just' a
    (>>=) :: Maybe' a -> (a -> Maybe' b) -> Maybe' b
    (>>=) a f = case a of
        Nothing' -> Nothing'
        Just' b -> f b

instance Applicative Maybe' where
    (<*>) :: Maybe' (a -> b) -> Maybe' a -> Maybe' b
    (<*>) f a = ap f a
    pure :: a -> Maybe' a
    pure a = return a


{-----------------------Általános függvények monadikus variánsa-----------------------------------------}

replicate' :: Int -> Int -> [Int]
replicate' i m 
    |i <= 0 = []
    |otherwise = m : replicate' (i-1) m

replicateM'  :: Monad m => Int -> m a -> m [a]
replicateM' i m
    |i <= 0 = pure [] --- return Maybe [] || return Either [] vagy bármilyen monád []
    |otherwise = (>>=) m (\a -> (>>=) (replicateM' (i-1) m) (\as -> pure (a:as))) -- (>>=) :: m a -> (a -> m b) -> m b


{-
replicateM' 3 (Just 1) ::
Az eredeti recursive call stack ->
(>>=) m (\a -> (>>=) (replicateM' (i-1) m) (\as -> pure (a:as)))

(>>=) m (\a -> (>>=) ((>>=) m (\a -> (>>=) (replicateM' (i-1) m) (\as -> pure (a:as)))) (\as -> pure (a:as)))

(>>=) (Just 1) (\1 -> (>>=) ((>>=) (Just 1) (\1 -> (>>=) ((>>=) (Just 1) (\1 -> (>>=) (Just []) (\as -> Just (1:as)))) (\as -> Just (1:as)))) (\as -> Just (1:as)))
Első visszalépés
(>>=) (Just 1) (\1 -> (>>=) ((>>=) (Just 1) (\1 -> (>>=) ((>>=) (Just 1) (\1 -> (>>=) (Just []) (\as -> Just (1:[])))) (\as -> Just (1:as)))) (\as -> Just (1:as)))
Második
(>>=) (Just 1) (\1 -> (>>=) ((>>=) (Just 1) (\1 -> (>>=) ((>>=) (Just 1) (\1 -> (>>=) (Just []) (\as -> Just (1:[])))) (\as -> Just (1:1:[])))) (\as -> Just (1:as)))
Harmadik
(>>=) (Just 1) (\1 -> (>>=) ((>>=) (Just 1) (\1 -> (>>=) ((>>=) (Just 1) (\1 -> (>>=) (Just []) (\as -> Just (1:as)))) (\as -> Just (1:as)))) (\as -> Just (1:1:1:[])))

lényegében a >>= Just [] (\as -> pure (a:as)) nél az as az [] és ezért kell a dupla binding

-}

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (a:as)
    | f a = a : filter' f as
    | otherwise = filter' f as

filterM' :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM' p [] = pure []
filterM' p (a:as) = (>>=) (p a) (\b -> (>>=) (filterM' p as) (\ys -> if b then pure (a:ys) else pure ys)) 

--- filterM' (\a -> if a > 5 then Just True else (if a < 0 then Just False else Nothing)) [-1, -2, 6, 7]
--- Just [6,7]


{-------------------IO osztály---------------------}


-- I/O haskellben: IO monád
-- standard konzol műveletek
-- getLine  :: IO String
-- print    :: Show a => a -> IO ()
-- putStr   :: String -> IO ()
-- putStrLn :: String -> IO ()
-- readLn   :: Read a => IO a

---ezeket lehet binddal kombinálni
-- pl (readLn :: IO Int) >>= print
-- ez azért működik mert a readLn az IO a amit ha bindolunk akkor lesz belőle egy print a => IO ()

-- do-notáció: Imperatív monád írási stílus
{-
do
    b <- a
≡
a >>= \b -> ...
do
    a
    b
≡
a >> b
do
    b <- a
    c <- a
    return $ b + c
≡
a >>= \b -> a >>= \c -> return $ b + c
-}

readAndPrint :: IO ()
readAndPrint = do
    b <- readLn :: IO Int
    print b

readAndPrint' :: IO ()
readAndPrint' = (readLn :: IO Int) >>= print

mapM' :: (Monad m) => (a -> m b) -> [a] -> m [b]
mapM' f [] = return []
mapM' f (x:xs) = do
    y <- f x
    ys <- mapM' f xs
    return (y:ys)

replicateM'' :: (Monad m) => Int -> m a -> m [a]
replicateM'' i m
    |i <= 0 =  return []
    |otherwise = do
        b <- m
        bs <- replicateM'' (i-1) m
        return (b:bs)

filterM'' :: (Monad m) => (a -> m Bool) -> [a] -> m [a]
filterM'' f [] = return []
filterM'' f (x:xs) = do
    b <- f x
    bs <- filterM'' f xs
    if b then return (x:bs) else return bs

sumM :: (Monad m, Num a ) => [m a] -> m a
sumM [] = return 0
sumM (x:xs) = do
    y <- x
    ys <- sumM xs
    return (y+ys)

{-------------------------Applicative Class---------------------------------------}

{-
:i Applicative
class Functor f => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b
    liftA2 :: (a -> b -> c) -> f a -> f b -> f c
    (*>) :: f a -> f b -> f b
    (<*) :: f a -> f b -> f a
    {-# MINIMAL pure, ((<*>) | liftA2) #-}
-}

--data List a = Cons a | Nil
newtype One a = One a deriving (Eq, Show, Functor)
--data Maybe a    = Just a | Nothing      deriving (Show, Functor)
--data Either e a = Left e | Right a      deriving (Show, Functor)

instance Applicative One where
    pure :: a -> One a
    pure a = One a
    (<*>) :: One(a -> b) -> One a -> One b
    (<*>) (One f) (One a) = One (f a)
    liftA2 :: (a -> b -> c) -> One a -> One b -> One c
    liftA2 f (One a) (One b) = One (f a b)

{- instance Applicative Maybe where
    pure :: a -> Maybe a
    pure a = Just a
    (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
    (Just f) <*> (Just a) = Just (f a)
    _ <*> _ = Nothing
    liftA2 :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
    liftA2 f (Just a) (Just b) = Just (f a b)
    liftA2 _ _ _ = Nothing

instance Applicative (Either e) where -- hibakomponens típusa fix
    pure :: a -> Either e a
    pure a = Right a
    (<*>) :: Either e (a -> b) -> Either e a -> Either e b
    (Left e) <*> _ = Left e
    _ <*> (Left e) = Left e
    (Right f) <*> (Right a) = Right (f a)
    liftA2 :: (a -> b -> c) -> Either e a -> Either e b -> Either e c
    liftA2 f (Left e) (Left e') = Left e
    liftA2 f (Left e) (Right b) = Left e
    liftA2 f (Right a) (Left e) = Left e
    liftA2 f (Right a) (Right b) = Right (f a b) -}


{-Concatenation of two List types-}
(+++) :: List a -> List a -> List a
(+++) xs Nil = xs
(+++) Nil xs = xs
(+++) (Cons x xs) ys = Cons x (xs +++ ys)
infixr 5 +++ 

instance Applicative List where
    pure :: a -> List a
    pure a = Cons a Nil
    (<*>) :: List (a->b) -> List a -> List b
    (<*>) Nil _ = Nil
    (<*>) _ Nil = Nil
    (<*>) (Cons x xs) as = fmap x as +++ (xs <*> as)
    liftA2 :: (a -> b -> c) -> List a -> List b -> List c
    liftA2 f Nil _ = Nil
    liftA2 f _ Nil = Nil
    liftA2 f (Cons x xs) ys = fmap (f x) ys +++ liftA2 f xs ys


{----------------------- Monád class -------------------------------------}

-- Applikatív folytatása a Monád
{-
:i Monad
class Applicative m => Monad m where
    (>>=) :: m a -> (a -> m b) -> m b
    (>>) :: m a -> m b -> m b
    return :: a -> m a
    {-# MINIMAL (>>=) #-}
-}

instance Monad One where
    (>>=) :: One a -> (a -> One b) -> One b
    (>>=) (One a) f = f a
    (>>) :: One a -> One b -> One b
    (>>) (One a) (One b) = One b
    return :: a -> One a
    return a = One a

{-
instance Monad Maybe where
    (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
    (Just x) >>= f = f x
    Nothing  >>= _ = Nothing

instance Monad (Either e) where
    (>>=) :: Either e a -> (a -> Either e b) -> Either e b
    (Right a) >>= f = f a
    (Left e) >>= _ = Left e 
-}

instance Monad List where
    (>>=) :: List a -> (a -> List b) -> List b
    (>>=) Nil _ = Nil
    (>>=) (Cons a as) f = f a +++ (>>=) as f
    (>>) :: List a -> List b -> List b
    (>>) a b = b
    return :: a -> List a
    return a = Cons a Nil

-- Megtalálható a Control.Monad könyvtárban
(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
(>=>) f g a = f a >>= g

-- Join művelet 
join :: Monad m => m (m a) -> m a
join mma = mma >>= id

--Summary.join (Just (Just 4))

data NonEmpty' a = Last' a | NCons' a (NonEmpty' a) deriving (Eq,Show, Functor)

concNE :: NonEmpty' a -> NonEmpty' a -> NonEmpty' a
concNE (Last' a) xs  = NCons' a xs
concNE (NCons' a as) xs = NCons' a (concNE as xs)

instance Applicative NonEmpty' where
    pure :: a -> NonEmpty' a
    pure a = Last' a
    (<*>) :: NonEmpty' (a->b) -> NonEmpty' a -> NonEmpty' b
    (<*>) (Last' f) as = fmap f as
    (<*>) (NCons' f fs) as = concNE (fmap f as) (fs <*> as) 

instance Monad NonEmpty' where
    -- >>= = bind
    -- <*> = ap
    -- <$> = fmap
    (>>=) :: NonEmpty' a -> (a -> NonEmpty' b) -> NonEmpty' b
    (>>=) (Last' a) f = f a
    (>>=) (NCons' a as) f = concNE (f a) (as >>= f)
    
    
{------------------Rendes IO Műveletek-----------------------}


io1 :: IO()
io1 = putStr "Hello World\n" >> putStr "Hello World again"

io1' ::IO()
io1' = putStr "Hello World\n" <* putStr "Hello World, again!\n"

io1'' ::IO()
io1'' = do
    putStr "Hello World\n"
    putStr "Hello World again\n"

io2 ::IO()
io2 = do
    s <- getLine
    putStr $ "Hello " ++ s ++ " \n"

readAndPrintInteger :: IO ()
readAndPrintInteger = do
    (i :: Integer) <- readLn
    print i

getAndPrint :: IO ()
getAndPrint = do
    i <- getLine
    putStr i

-- print i "alma"
-- putStr i alma

addTwoNumbers :: IO ()
addTwoNumbers = do
    j <- readLn :: IO Integer
    k <- readLn :: IO Integer
    print (j+k)

-- (readLn :: IO Integer) >>= \i -> (readLn :: IO Integer) >>= \j -> print (i + j)
-- egyéb kreatív megoldás: liftA2 (+) readLn readLn >>= print

sumIO :: Int -> IO Int --5 -> 5 db számot olvas be
sumIO n = fmap sum ( replicateM n readLn)

sumIO' ::IO ()
sumIO' = do
    putStr "How many numbers would you like to add?\n"
    x <- readLn :: IO Int
    xs <- replicateM x (readLn :: IO Integer)
    print $ sum xs

sumIO'' :: Int -> IO Int
sumIO'' n
    | n == 1 = do
        i <- readLn :: IO Int
        return i
    | otherwise = do
        i <- readLn :: IO Int
        i' <- sumIO'' (n - 1)
        return (i + i')

{---------------------------New type állapotok tárolására | A State -------------------------------------}

newtype State s a = State(s -> (s,a))
--s állapot 
--a mellékhatás

runState :: State s a -> (s -> (s,a))
runState (State f) s = f s

-- Ezt a runState és data konstruktort össze lehet vonni egy úgynevezett "rekord típusban"
-- data State s a = State { runState :: s -> (s,a) }

get :: State s s
get = State (\ s -> (s, s))

-- Olyan effektus amiben csak az állapotot írjuk felül (nincs mellékhatás)
put :: s -> State s ()
-- put s = State $ \s' -> (s, ())
put s = State $ const (s, ())

instance Functor (State s) where
    fmap :: (a -> b) -> State s a -> State s b
    -- fmap :: (a -> b) -> (s -> (s,a)) -> (s -> (s,b))
    fmap f (State sf) = State $ \s -> case sf s of
        (s', a) -> (s', f a)

instance Applicative (State s) where
    pure :: a -> State s a
    -- pure :: a -> (s -> (s,a))
    pure a = State $ \s -> (s, a)
    (<*>) :: State s (a -> b) -> State s a -> State s b
    -- (<*>) :: (s -> (s, a -> b)) -> (s -> (s, a)) -> (s -> (s, b))
    (State sf) <*> (State sa) = State $ \s -> case sf s of
        (s', f) -> case sa s' of
            (s'', a) -> (s'', f a)
    liftA2 :: (a -> b -> c) -> State s a -> State s b -> State s c
    -- liftA2 :: (a -> b -> c) -> (s -> (s, a)) -> (s -> (s, b)) -> (s -> (s, c))
    liftA2 f (State sa) (State sb) = State $ \s -> case sa s of
        (s', a) -> case sb s' of
            (s'', b) -> (s'', f a b)

instance Monad (State s) where
    (>>=) :: State s a -> (a -> State s b) -> State s b
    -- (>>=) :: (s -> (s,a)) -> (a -> (s -> (s, b))) -> (s -> (s,b))
    (State sa) >>= f = State $ \s -> case sa s of
        (s', a) -> case f a of
            (State sb) -> sb s' -- runState (f a) s' 

modify :: (s -> s) -> State s ()
modify f = get >>= \a -> put (f a)

stateExample1 :: State Int Bool
stateExample1 = do
    st <- get -- eredeti állapot
    if st > 20 then put 20 else pure ()
    -- Egyágú if - csak akkor fut le az állapotváltozás ha a feltétel igaz
    -- when (st > 20) $ put 20
    pure (mod st 9 == 0)

--runState stateExample1 10
--(10,False)

stateExample2 :: State Int Bool
stateExample2 = do
    st <- get
    put (st*st)
    return (if st == 10 then True else False)

stateExample3 :: State Integer ()
stateExample3 = replicateM_ 17 (modify (^2))

stateExample4 :: Int -> State Int [Int] -- 2 -> (s^8, [s^2, s^4])
stateExample4 k
    | k <= 0 = pure []
    | otherwise = do
        i <- get
        put (i^2)
        is <- stateExample4 (k - 1)
        pure (i:is)

{-
runState (stateExample4 3) 4
(65536,[4,16,256]) 
-}

statePractice1:: a -> State a a
statePractice1 a = do
    st <- get
    put a 
    return st 

statePractice2 :: State Int Int
statePractice2 = do
    st <- get
    put (st*4)
    if abs(st - st*4) > 10 then return 3 else return 15


{-
newtype State s a = State { runState :: s -> (s,a) } deriving Functor

instance Applicative (State s) where
    (<*>) = ap
    pure = State . flip (,)

instance Monad (State s) where
    (State a) >>= f = State $ \s -> let (s', a') = a s in runState (f a') s'


get :: State s s
get = State $ \s -> (s,s)

put :: s -> State s ()
put = State . const . flip (,) ()

modify :: (s -> s) -> State s ()
modify f = get >>= put . f
-}

productIO:: Int -> IO Int
productIO n = do
    xs <- replicateM n  (readLn ::IO Int)
    return (product xs)

--data Tree a = Leaf a | Brach (Tree a) (Tree a)
infixr 5 `Branch`

-- Címkézzük meg a fát jobbról balra! bejárásban! (1 pont, egyéb bejárás esetén 0.5 pont)
-- Használjunk state-t!

labelBackwards :: Tree a -> Tree (Int, a)
labelBackwards tr = snd(runState (go tr) 0) where
    go :: Tree a -> State Int (Tree (Int, a))
    go (Leaf a) = do
        i <- get
        put (i + 1)
        return (Leaf (i + 1, a))
    go (Branch l r) = do
        r' <- go r
        l' <- go l
        return (Branch l' r')

{------------Traversable---------------}

mapList:: Applicative m => (a -> m b) -> List a -> m (List b)
mapList f Nil = pure Nil
mapList f (Cons a as) = Cons <$> f a <*> mapList f as --(<*>) :: f (a -> b) -> f a -> f b

{-
*Summary> mapList (\a -> if a < 5 then Just True else Just False) (Cons 4 Nil)
Just (Cons True Nil)

*Summary> mapList (\a -> if a < 5 then Just True else Just False) (Cons 4 (Cons 10 Nil))
Just (Cons True (Cons False Nil))
-}


-- Ez egy létező típusosztály
{-
:i Traversable
type Traversable :: (* -> *) -> Constraint
class (Functor t, Foldable t) => Traversable t where
  traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
  sequenceA :: Applicative f => t (f a) -> f (t a)
  mapM :: Monad m => (a -> m b) -> t a -> m (t b)
  sequence :: Monad m => t (m a) -> m (t a)
  {-# MINIMAL traverse | sequenceA #-}
-}

instance Traversable List where
    traverse :: Applicative f => (a -> f b) -> List a -> f (List b)
    traverse = mapList
    sequenceA :: Applicative f => List (f a) -> f (List a)
    sequenceA Nil = pure Nil
    sequenceA (Cons a as) = Cons <$> a <*> sequenceA as

data MyData a = Tuple a a deriving (Eq,Show,Ord,Foldable, Functor, Traversable)

-- A traverse-nek egy fő haszna van, a mapM és traverse függvények
-- vegyünk egy traversable-t és írjuk ki az összes elemét stdout-ra

printAll :: (Traversable t, Show a) => t a -> IO ()
printAll t = void $ traverse print t

partialSum :: (Traversable t, Num a) => t a -> State a (t a)
partialSum t = traverse (\a -> do
    i <- get
    put(i+a)
    pure i
    )t

{-----------------------Parser-----------------------------}

newtype Parser a = Parser {runParser :: String -> Maybe (String, a)}
--azért maybe mert nem biztos hogy fogunk tudni parsonlni

instance Functor Parser where
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap f (Parser p) = Parser $ \st -> case p st of
        Nothing -> Nothing
        Just (st', a) -> Just (st', f a)

instance Applicative Parser where
    pure :: a -> Parser a
    pure a = Parser $ \st -> Just (st, a)
    liftA2 :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
    liftA2 f (Parser p1) (Parser p2) = Parser $ \st -> case p1 st of
        Nothing -> Nothing
        Just (st', a) -> case p2 st' of
            Nothing -> Nothing
            Just (st'', b) -> Just (st'', f a b)

instance Monad Parser where 
    (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    (Parser p1) >>= f = Parser $ \st -> case p1 st of
        Nothing -> Nothing
        Just (st', a) -> runParser (f a) st'

{-----------Alternative typeclass-----------------}

-- Új típusosztály a hibák kezelésére: Alternative
{-
:i
class Applicative f => Alternative f where
  empty :: f a
  (<|>) :: f a -> f a -> f a
  some :: f a -> f [a]
  many :: f a -> f [a]
  {-# MINIMAL empty, (<|>) #-}
-}
-- Az empty egy garantáltan hibás eset
-- A (<|>) lekezeli a hibás esetet (parser esetén csak akkor futtatja le a jobb oldali parsert ha a bal oldali hibás)

instance Alternative Parser where
    empty :: Parser a
    empty = Parser $ \st -> Nothing
    (<|>) :: Parser a -> Parser a -> Parser a
    (<|>) (Parser p1) (Parser p2) = Parser $ \st -> case p1 st of
        Nothing -> p2 st
        Just (st', a) -> Just (st', a)

-- many: 0 vagy többször értékeli ki az alternatívot
-- some: 1 vagy többször, ha nincs 1 se akkor sikertelen

many' :: Alternative f => f a -> f [a]
many' f = some' f <|> pure []

some' :: Alternative f => f a -> f [a]
some' f = liftA2 (:) f (many' f) ----liftA2 :: (a -> b -> c) -> f a -> f b -> f c

{- 
*Summary> liftA2 (:) (Just 3) (Just [])
Just [3]
-}

--fix characther parsolása

char :: Char -> Parser()
char c = Parser $ \s -> case s of
    [] -> Nothing
    (x:xs) -> if c == x then Just (xs, ()) else Nothing

{-
*Summary> runParser (char 'c') "cica"
Just ("ica",())
-}

-- Definiálunk egy parser ami akármilyen karaktert parseol
anyChar :: Parser Char
anyChar = Parser $ \s -> case s of
    [] -> Nothing
    (x:xs) -> Just (xs,x)

{-
*Summary> runParser (anyChar) "cica"
Just ("ica",'c')
-}

-- Definiáljuk egy olyan parsert ami egy függvény alapján parseol egy karaktert
satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \s -> case s of
    [] -> Nothing
    (x:xs) -> if f x then Just (xs,x) else Nothing

{-
*Summary> runParser (satisfy (=='c')) "cica"
Just ("ica",'c')
-}

char' :: Char -> Parser ()
char' c = void $ satisfy (\c' -> c' == c) -- satisfy (== c)

anyChar' :: Parser Char
anyChar' = satisfy (\x -> True)

eof :: Parser ()
eof = Parser $ \s -> case s of
    [] -> Just ([],())
    _ -> Nothing
{-
*Summary> runParser eof ""
Just ("",())
-}

ascii :: Parser Char
ascii = satisfy Data.Char.isAscii

digit :: Parser Int
digit = (\c -> read [c]) <$> satisfy isDigit

--a lambda miatt rakja el mellékhatásba
--így:   Just(..., szám)
--amúgy: Just(...,())

string :: [Char] -> Parser ()
string [] = return ()
string (x:xs) = char x >> string xs

{-
*Summary> runParser (string "almafa") "almafasz"
Just ("sz",())
-}

string' :: Int -> Parser String
string' c = replicateM c anyChar

                                                            {-F-A-S-Z-}

bananaThenInt:: Parser Int
bananaThenInt = do
    string "banana"
    i <- digit
    return i
    
-- Egy kapott listából parseoljuk a szavakat
-- note: mapM_, traverse, void
parseWords :: [String] -> Parser ()
parseWords strs = void $ traverse (\s -> string s) strs

-- Definiáljuk az aOrZ parsert ami vagy a-t vagy z-t parseol
aOrZ :: Parser Char
aOrZ = ('A' <$ char 'A') <|> ('Z' <$ char 'Z')


-- Regex féle parserek
{-
    Regex gyorstalpaló:                               Haskell megfelelő:
    c        - Parseol egy c karaktert                char 'c'
    ℓ+       - Parseol 1 vagy több ℓ kifejezést       some ℓ
    ℓ*       - Parseol 0 vagy több ℓ kifejezést       many ℓ
    (ℓ₁|ℓ₂)  - Parseol ℓ₁-t vagy ℓ₂-t                 ℓ₁ <|> ℓ₂
    ℓ?       - Parseol 0 vagy 1 ℓ kifejezést          optional ℓ
    .        - Akármilyen karakter                    anyChar
    ℓ{n}     - Parseol n darab ℓ kifejezést           replicateM n ℓ
    $        - Nincs mit parseolni                    eof
    \d       - Parseol egy számjegyet                 satisfy isDigit
    [c₁-c₂]  - c₁ és c₂ között parseol egy karaktert  <Még nem írtuk meg>
-}

optional' :: Parser a -> Parser (Maybe a)
optional' p = (Just <$> p) <|> pure Nothing

charRange :: Char -> Char -> Parser Char
charRange c1 c2 = oneOf [c1..c2]
    where
        oneOf [] = empty
        oneOf (x:xs) = (x <$ char x) <|> oneOf xs


between :: Parser left -> Parser a -> Parser right -> Parser a
between l a r = l *> a <* r

-- *Summary> runParser (between anyChar (string "almafa") anyChar ) "calmafaaaaa"
-- Just ("aaa",())

sepBy :: Parser a -> Parser delim -> Parser [a]
sepBy p pdelim = sepBy1 p pdelim <|> return []

-- parsers any a separated by delim
{-
*Summary> runParser (sepBy anyChar (char ',')   ) "a,b,c,d"
Just ("","abcd")
-}

sepBy1 :: Parser a -> Parser delim -> Parser [a]
sepBy1  pa pdelim = do
    a <- pa
    delim <- optional $ pdelim
    case delim of
        Just _ -> (\as -> a:as) <$> sepBy1 pa pdelim
        Nothing -> return [a]

--the difference is that sepBy1 returns nothing if there is no occurance of the given parserers

digit' :: Parser Int
digit' = (\a -> ord 'a'- ord '0' ) <$> satisfy isDigit

-- ord '1' = 49 I guess ascii code
-- (toEnum (ord '1')) :: Char = '1'

natural :: Parser Int
natural = foldl1 (\acc a -> acc*10 + a) <$> some digit

integer :: Parser Int
integer = do
    l <- optional $ char '-'
    case l of
        Just _ -> negate <$> natural 
        _ -> natural 

float :: Parser Double
float = do
    s <- (\s -> if null s then 1 else -1) <$> optional (char '-') -- ha tud az elején '-' - parsolni akkor -1 az s else 1
    i <- natural 
    char '.'
    r <- foldr1 (\a acc -> a + acc / 10) <$> some (fromIntegral <$> digit) -- a pont után a cucc tizedéd kell hozzáadni
    return $ s * (r / 10  + fromIntegral i) -- itt szoroz az s.el

ws :: Parser ()
ws = void $ many $ satisfy isSpace

{-
*Summary> runParser ws "       alma"
Just ("alma",())
-}

tok :: Parser a -> Parser a
tok p = p <* ws --(<*) :: f a -> f b -> f a

{-
*Summary> runParser (tok (string "alma")) "alma   "
Just ("",())
-}

topLevel :: Parser a -> Parser a
topLevel p = ws *> p <* eof

{-
*Summary> runParser (ws <|> (string "alma")) "   alma"
Just ("alma",())

*Summary> runParser (ws *> (string "alma")) "   alma"
Just ("",())

*Summary> runParser (ws *> (string "alma") *> topLevel (string "fa")) "   alma fa"
Just ("",())
-}

parseAddition :: Parser Int
parseAddition = do
    ws
    i <- tok natural
    tok (char '+')
    j <- tok natural
    return (i+j)

    -- ugyan az mint ha végig lenne ws -elve mivel a tok utána vágja le a spaceket

{----------------Kifejezésnyelv------------------}    


{-
Kifejezésnyelv parseolása ún. Recursive Precedence Parsing algoritmussal
Ötlet: Precedencia alapján csökkenő sorrendbe áltítjuk a műveleteinket:
    Mul | (*) | Infixl 7
    Add | (+) | Infixl 6
Először +-t próbálunk parseolni, majd *-t majd literált és zárójelet - minden erősségi szintnek 1 db parsere van
Zárójelek után a parseolási sorrend resetelődik
Mi mit parseoljon?
atom: int literál, zárójelezett kifejezés (zárójeles kifejezés meghívja a legerősebb parsert (itt az összeadás))
mul : szorzást (meghívja az atomot)
add : összeadást (meghívja a szorzást)
-}

-- fontos megjezyés: számokat csak a pAtom-ba akarunk parseolni!

-- Jobbra asszocialó kifejezést parseoljon.
-- Sep által elválasztott kifejezéseket gyűjtsön össze, majd azokra a megfelelő sorrendbe alkalmazza a függvényt
rightAssoc :: (a -> a -> a) -> Parser a -> Parser sep -> Parser a
rightAssoc f a sep = foldr1 f <$> sepBy1 a sep

-- Ugyanaz mint a rightAssoc csak balra
leftAssoc :: (a -> a -> a) -> Parser a -> Parser sep -> Parser a
leftAssoc f a sep = foldl1 f <$> sepBy1 a sep

-- leftAssoc' :: Parser a -> Parser (a -> a -> a) -> Parser a
-- leftAssoc' integer ((+) <$ char '+' <|> (*) <$ char '*')

-- Olyan parser amit nem lehet láncolni (pl == mert 1 == 2 == 3 == 4 se jobbra se balra nem asszociál tehát nincs értelmezve)
nonAssoc :: (a -> a -> a) -> Parser a -> Parser sep -> Parser a
nonAssoc f a sep = do
    a' <- a
    sep' <- optional $ sep
    case sep' of
        Just _ -> (f a') <$> a
        Nothing -> pure a'


-- A fentebbi parserek nem tudnak több ugyanolyan kötési erősségü kifejezést parseolni, mert nem differenciálnak az elválasztók között
-- A chain függvények ezt megoldják

chainr1 :: Parser a->  Parser (a -> a -> a) ->  Parser a
chainr1 v op = do
    val <- v
    (do
        opr <- op
        res <- chainr1 v op
        pure (opr val res)
        ) <|> pure val

chainl1 :: Parser a ->  Parser (a -> a -> a) -> Parser a
chainl1 v op = v >>= parseLeft
    where
        parseLeft val = (do
            opr <- op
            val2 <- v
            parseLeft (opr val val2)) <|> pure val


data Exp = Lit Int | Plus Exp Exp | Mul Exp Exp deriving (Show)

evalExp :: Exp -> Int
evalExp (Lit n) = n
evalExp (Plus e1 e2) = evalExp e1 + evalExp e2
evalExp (Mul e1 e2) = evalExp e1 * evalExp e2

pAdd :: Parser Exp
pAdd = rightAssoc Plus pMul (char '+')

pMul :: Parser Exp
pMul = rightAssoc Mul pAtom (char '*')

pAtom :: Parser Exp 
pAtom = (Lit <$> integer) <|> between (char '(') pAdd (char ')')

pExp :: Parser Exp
pExp = topLevel pAdd

evaluationWithParser :: Maybe (a,Exp) -> Int
evaluationWithParser Nothing = 0
evaluationWithParser (Just (a,b)) = evalExp b

{-
*Summary> evaluationWithParser (runParser pExp "3*(4+3)")
21
-}

--Változók a nyelvben

-- Vezessünk be változókat a nyelvbe!
data Exp' = Lit' Integer | Plus' Exp' Exp' | Mul' Exp' Exp' | Var String deriving Show

evalExp' :: Exp' -> [(String, Exp')] -> Maybe Integer
evalExp' (Lit' n) _ = Just n
evalExp' (Plus' e1 e2) lt = liftA2 (+) (evalExp' e1 lt) (evalExp' e2 lt)
evalExp' (Mul'  e1 e2) lt = liftA2 (*) (evalExp' e1 lt) (evalExp' e2 lt)
evalExp' (Var     str) lt = case lookup str lt of
    Just exp -> evalExp' exp lt
    _        -> Nothing

{-
>>> lookup 2 [(1, "first"), (2, "second"), (3, "third")]
Just "second"
-}

pAdd' :: Parser Exp'
pAdd' = rightAssoc Plus' pMul' (char '+')

-- szorzás kifejezés parseolása
pMul' :: Parser Exp'
pMul' = rightAssoc Mul' pAtom' (char '*')

-- legkisebb szintű kifejezés parseolása (ez esetben literálok, változók és zárójelezett kifejezés)
pAtom' :: Parser Exp'
pAtom' = ((Lit' . fromIntegral) <$> integer) 
    <|>  (Var <$> some (satisfy isLetter))
    <|>  between (char '(') pAdd' (char ')')

-- teljes kifejezés parseolása
pExp' :: Parser Exp'
pExp' = topLevel pAdd'  

{-
*Summary> runParser pExp' "a+42*b"
Just ("",Plus' (Var "a") (Mul' (Lit' 42) (Var "b")))
(0.04 secs, 133,848 bytes)
*Summary> evalExp' (Plus' (Var "a") (Mul' (Lit' 42) (Var "b"))) [("a",Lit' 4), ("b",Lit' 3)]
Just 130
-}

-- Több kifejezés ugyanazon a szinten:

data Exp2 = Add2 Exp2 Exp2 | Sub Exp2 Exp2 | Mul2 Exp2 Exp2 | Lit2 Int | Var2 String | Div2 Exp2 Exp2 | Mod2 Exp2 Exp2 | Pow2 Exp2 Exp2 deriving Show
-- Osztás, 7 , balra
-- Modulus, 7, balra
-- Hatványozás, 8, jobbra

p2Atom :: Parser Exp2
p2Atom = (Lit2 <$> integer) <|> (Var2 <$> some (satisfy isLetter)) <|> between (tok $ char '(') p2AddAndSub (tok $ char ')')

p2Pow :: Parser Exp2
p2Pow = chainr1 p2Atom (Pow2 <$ char '^')

p2Mul :: Parser Exp2
p2Mul = chainl1 p2Pow (Mul2 <$ char '*' <|> Div2 <$ char '/' <|> Mod2 <$ char '%')

p2AddAndSub :: Parser Exp2
p2AddAndSub = chainl1 p2Mul (Add2 <$ char '+' <|> Sub <$ char '-')

p2Exp :: Parser Exp2
p2Exp = topLevel p2AddAndSub

{-
*Summary> runParser p2Exp "12+4^4*4%65-23/2-almafa"
Just ("",Sub (Sub (Add2 (Lit2 12) (Mod2 (Mul2 (Pow2 (Lit2 4) (Lit2 4)) (Lit2 4)) (Lit2 65))) (Div2 (Lit2 23) (Lit2 2))) (Var2 "almafa"))
-}