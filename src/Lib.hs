{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Lib
    ( someFunc
    ) where

-- Playing around with recursion schemes from the two blog posts here:
-- http://jozefg.bitbucket.org/posts/2014-05-19-like-recursion-but-cooler.html
-- http://jozefg.bitbucket.org/posts/2014-06-14-like-recursion-but-cooler-2.html
--
-- And the paper here:
-- http://eprints.eemcs.utwente.nl/7281/01/db-utwente-40501F46.pdf

import Control.Monad
import Data.Functor.Foldable
import Prelude hiding (Foldable)

data MyList a = MyCons a (MyList a) | MyNil
    deriving Show

data BList a b = BCons a b | BNil
    deriving (Functor, Show)

type instance Base (MyList a) = BList a

instance Foldable (MyList a) where

    -- project :: t -> (Base t) t
    -- project :: MyList a -> (Base (MyList a)) (MyList a)
    project :: MyList a -> BList a (MyList a)
    project (MyCons a b) = BCons a b
    project MyNil        = BNil

    -- cata :: (Base t b -> b) -> t -> b
    -- cata :: (Base (MyList a) b -> b) -> MyList a -> b
    -- cata :: (BList a b -> b) -> MyList a -> b

    -- para :: (Base t (t, b) -> b) -> t -> b
    -- para :: (Base (MyList a) (MyList a, b) -> b) -> t -> b
    -- para :: (BList a (MyList a, b) -> b) -> t -> b

instance Unfoldable (MyList a) where
    -- embed :: Base t t -> t
    -- embed :: Base (MyList a) (MyList a) -> MyList a
    embed :: BList a (MyList a) -> MyList a
    embed BNil = MyNil
    embed (BCons a rest) = MyCons a rest

    -- ana :: (b -> Base t b) -> b -> t
    -- ana :: (b -> Base (MyList a) b) -> b -> MyList a
    -- ana :: (b -> BList a b) -> b -> MyList a

-- In the webpage, these two are only defined as partial functions, with
-- only the BCons cases defined...
out :: Fix (BList a) -> MyList a
out (Fix (BCons a rest)) = MyCons a (out rest)
out (Fix BNil)           = MyNil
into :: MyList a -> Fix (BList a)
into (MyCons a rest) = Fix (BCons a $ into rest)
into MyNil           = Fix BNil

cataTest :: BList Int Int -> Int
cataTest (BCons i rest) = i + rest
cataTest BNil = 0

paraTest :: BList Int (MyList Int, String) -> String
paraTest (BCons i (myList, rest)) = show i ++ rest ++ "\n\t" ++ show myList ++ "\n"
paraTest BNil = "BNil"

anaTest :: Int -> BList String Int
anaTest 10 = BNil
anaTest n = BCons (show n) (n+1)

betweenBad :: (Ord a, Enum a) => a -> a -> MyList a
betweenBad a b | succ a >= b = MyNil
               | otherwise   = succ a `MyCons` betweenBad (succ a) b

between :: Int -> Int -> MyList Int
between low high = ana builder low
  where
    builder :: Int -> BList Int Int
    builder i | succ i >= high = BNil
              | otherwise      = join BCons $ succ i

myMap :: forall a b . (a -> b) -> MyList a -> MyList b
myMap f = cata mapper
  where
    mapper :: BList a (MyList b) -> MyList b
    mapper BNil = MyNil
    mapper (BCons a rest) = f a `MyCons` rest


mySum :: Num a => MyList a -> a
mySum (MyCons a rest) = a + mySum rest
mySum MyNil = 0

sumTails :: MyList Int -> MyList Int
sumTails = para summer
  where
    summer :: BList Int (MyList Int, MyList Int) -> MyList Int
    summer (BCons a (list, rest)) = MyCons (a + mySum list) rest
    summer BNil                   = MyNil


data Op = Plus | Sub | Mult | Div
    deriving Show

data Foo = Num Int           -- Numeric literals
         | String String     -- String literals
         | Binop Op Foo Foo  -- Primitive operation
         | Fun String Foo    -- Lambda/Abstraction over terms
         | App Foo Foo       -- Application
         | Var String        -- Variables
    deriving Show

compute :: Op -> Int -> Int -> Int
compute Plus = (+)
compute Sub  = (-)
compute Mult = (*)
compute Div  = div


data FooB a = NumB Int
            | StringB String
            | BinopB Op a a
            | FunB String a
            | AppB a a
            | VarB String
    deriving (Functor, Show)

type instance Base Foo = FooB

instance Foldable Foo where
    -- project :: t -> (Base t) t
    -- project :: Foo -> (Base Foo) Foo
    project :: Foo -> FooB Foo
    project (Num a)        = NumB a
    project (String a)     = StringB a
    project (Binop op a b) = BinopB op a b
    project (Fun v a)      = FunB v a
    project (App a b)      = AppB a b
    project (Var a)        = VarB a

-- reverse of project
-- rProject :: Base Foo Foo -> Foo
rProject :: FooB Foo -> Foo
rProject (NumB a)        = Num a
rProject (StringB a)     = String a
rProject (BinopB op a b) = Binop op a b
rProject (FunB v a)      = Fun v a
rProject (AppB a b)      = App a b
rProject (VarB a)        = Var a

-- This is reduce without using recursion-schemes.
reduceBad :: Foo -> Foo
reduceBad (Binop op (Num a) (Num b)) = Num $ compute op a b -- The reduction
reduceBad a                          = a

-- This is reduce with recursion-schemes.
-- reduce :: Base Foo Foo -> Foo
reduce :: FooB Foo -> Foo
reduce (BinopB op (Num a) (Num b)) = Num $ compute op a b -- The reduction
reduce a                           = rProject a

constFold :: Foo -> Foo
constFold = cata reduce


test :: Foo
test = Binop Plus (Num 1) (Binop Mult (Num 2) (Num 3))

optimized :: Foo
optimized = constFold test

type Foo' = Fix FooB'

data FooB' a = NumB' Int
            | StringB' String
            | BinopB' Op a a
            | FunB' String a
            | AppB' a a
            | VarB' String
    deriving (Functor, Show)

-- This is reduce without using recursion-schemes.
-- reduceBad :: Foo -> Foo
-- reduceBad (Binop op (Num a) (Num b)) = Num $ compute op a b -- The reduction
-- reduceBad a                          = a

-- This is reduce with recursion-schemes.
-- reduce' :: Base Foo Foo -> Foo
-- reduce' :: FooB' (Fix FooB') -> Fix FooB'
reduce' :: FooB' Foo' -> Foo'
reduce' (BinopB' op (Fix (NumB' a)) (Fix (NumB' b))) = Fix $ NumB' $ compute op a b
reduce' a = embed a

constFold' :: Foo' -> Foo'
constFold' = cata reduce'

test' :: Foo'
test' = Fix (BinopB' Plus
                     (Fix (NumB' 1))
                     (Fix (BinopB' Mult
                                   (Fix (NumB' 2))
                                   (Fix (NumB' 3))
                          )
                     )
            )

optimized' :: Foo'
optimized' = constFold' test'

-- freeVar :: Base Foo' [String] -> [String]
freeVar :: FooB' [String] -> [String]
freeVar (NumB' _)         = []
freeVar (StringB' _)      = []
freeVar (VarB' s)         = [s]
freeVar (BinopB' _ v1 v2) = v1 ++ v2
freeVar (AppB' v1 v2)     = v1 ++ v2
freeVar (FunB' v vs)      = delete v vs

delete :: (Eq a) => a -> [a] -> [a]
delete _ []                 = []
delete x (y:ys) | x == y    = ys
                | otherwise = y : delete x ys

-- As we’d hope, this traversal is much easier to write than the first one.
-- You can imagine that the boilerplate of writing FooB and project is
-- amortized over each traversal, making it much easier to write subsequent
-- traversals once we’ve gone through the trouble of actually laying down
-- the foundation.
freeIn :: Foo' -> [String]
freeIn = cata freeVar

test'' :: [String]
test'' = freeIn $ Fix (AppB' (Fix (FunB' "hello"
                                         (Fix (VarB' "hello"))))
                             (Fix (VarB' "bye"))
                      )


someFunc :: IO ()
someFunc = putStrLn "someFunc"

