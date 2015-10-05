{-# LANGUAGE DeriveFunctor #-}
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

myMap :: forall a b . (a -> b) -> MyList a -> MyList b
myMap f = cata mapper
  where
    mapper :: BList a (MyList b) -> MyList b
    mapper BNil = MyNil
    mapper (BCons a rest) = f a `MyCons` rest



someFunc :: IO ()
someFunc = putStrLn "someFunc"

