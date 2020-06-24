{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BangPatterns #-}
module Streams where

import Data.List(intercalate)
import Control.Applicative()

-- ������� 1 -----------------------------------------

-- ��� Stream a ������������ ����������� ������ (������) �������� ���� a
-- (� ������� �� [a], ������� ����� ���� ��� ���������, ��� � ������������
data Stream a = a :> Stream a

-- ��������� Show ��� Stream a �������� ������ 10 ��������� ������
instance Show a => Show (Stream a) where
    show s = "[" ++ intercalate ", " (map show $ take 10 $ streamToList s)
             ++ ", ..."

-- ���������� �������, ������������ ����� � (�����������) ������
streamToList :: Stream a -> [a]
streamToList (x :> xs) = x : streamToList xs

-- ������� 2 -----------------------------------------

-- ���������� ��������� ������� ������� ��� ������ � ��������

-- �����, ��������� �� ���������� ���������
sRepeat :: a -> Stream a
sRepeat x = x :> y
    where
        y = x :> y

-- sRepeat 1 == [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, ...

-- �����, ��������� �� ������������ ����� �������� ������
-- (���������: ��� � ���������� ����� ����������� ���, ��� ���������� ����� 
-- ����� ����������� (��������� ��� �� ����), � �� ���������� ��������)
sCycle :: [a] -> Stream a
sCycle xs
    | null xs = error "sCycle: empty list"
    | otherwise = ys
        where
            ys = foldr (:>) ys xs

-- sCycle [1, 2, 3] == [1, 2, 3, 1, 2, 3, 1, 2, 3, 1, ...

-- �����, �������� ��������� ��������� � ��������, �������� ��������� ��������
-- �� ��������
sIterate :: (a -> a) -> a -> Stream a
sIterate f a = a :> sIterate f (f a)

-- sIterate (/ 2) 1.0 == [1.0, 0.5, 0.25, 0.125, 0.0625, ...

-- �������, ������������ n ������ ��������� ������
sTake :: Int -> Stream a -> [a]
sTake n (a :> as)
    | n == 0 = []
    | n > 0 = a : sTake (n - 1) as
    | otherwise = error "Negative number of elements"

-- sTake 3 $ sRepeat 1 == [1, 1, 1]

-- �������, ������������ ����� �� ������������ ��������� ���� �������
-- (��� ���������� ������� ����� ������� ��� ������� ������� ��
-- ������� ���������, �� ���� �� ������������ ��� � ��������)
sInterleave :: Stream a -> Stream a -> Stream a
sInterleave (x :> xs) ys = x :> sInterleave ys xs

-- sInterleave (sRepeat 1) (sRepeat 2) == [1, 2, 1, 2, 1, 2, ...

-- ������� 3 -----------------------------------------

-- ��������� ���������� �������, ����������

-- ����� ����������� ����� (������� � 1)
nats :: Stream Integer
nats = sIterate (+ 1) 1

-- nats == [1, 2, 3, 4, 5, 6, 7, ...

-- �����, n-��� ������� �������� (������� � 1) -- ������������ ������� 2,
-- ������� n ������. ���������: � ������� sInterleave ��� ����� ������� ��� 
-- �������� �� ���������, ���� � ���������� ������ �� ������� ���������
-- (���������, ������ ��� �����).
ruler :: Stream Integer
ruler = 
    let helper n = sInterleave (sRepeat n) (helper (n+1))
    in helper 0

-- ruler == [0, 1, 0, 2, 0, 1, 0, 3, ...

-- ������� 4 -----------------------------------------

-- ���������� �������� ������������ ��������� ������-��������� �����.
-- ��� ������ �������� x ��������� ���������� �� �������
-- x' = (1103515245 * x + 12345) `mod` 2^31
-- (��� ��������� ������������, ��������, � GCC).
-- ��������� � ���������: https://ru.wikipedia.org/wiki/%D0%9B%D0%B8%D0%BD%D0%B5%D0%B9%D0%BD%D1%8B%D0%B9_%D0%BA%D0%BE%D0%BD%D0%B3%D1%80%D1%83%D1%8D%D0%BD%D1%82%D0%BD%D1%8B%D0%B9_%D0%BC%D0%B5%D1%82%D0%BE%D0%B4
rand :: Int -> Stream Int
rand = 
    let f x = (1103515245 * x + 12345) `mod` 2^(31::Integer)
    in sIterate f


-- ������� 5 -----------------------------------------

minMaxSlow, minMax, minMaxBang :: [Int] -> Maybe (Int, Int)
{- Total time: ??? Total Memory in use: ??? -}
minMaxSlow [] = Nothing
minMaxSlow xs = Just (minimum xs, maximum xs)

-- ������� minMax ������ �������� ����������� � ������������ �������� ������,
-- ��� ��, ��� minMaxSlow. �������� � minMaxSlow � ���, ��� ��� �������� �� ������ ��� ����
-- � ������� ��������� ��������� ��� � ������ �������. ���������� minMax ���, ����� 
-- ������� ������ ���� ������ �� ������.

{- Total time: ??? Total Memory in use: ??? -}
minMax [] = Nothing
minMax (x:xs) =
    let helper pair [] = Just pair
        helper (min', max') (y:ys) = helper (min min' y, max max' y) ys
    in helper (x, x) xs


-- �������������� �������: ���������� �� �� ����� ������� (��� ��������� minMaxBang) �
-- �������������� ����� ��������� (seq �/��� !)

{- Total time: ??? Total Memory in use: ??? -}
minMaxBang [] = Nothing
minMaxBang (x:xs) = 
    let minMax' pair [] = Just pair
        minMax' (min', max') (y:ys) = 
            let !min'' = min min' y
                !max'' = max max' y
            in minMax' (min'', max'') ys
    in minMax' (x, x) xs

-- ������������� ��������� � ����������� `ghc Streams.hs -O -rtsopts -main-is Streams`
-- � ��������� `Streams.exe +RTS -s` (./Streams � Linux/OSX).
-- ������������ ������ ����� � https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/runtime-control.html.
-- �������� ����� ���������� � ����� ������ ��� ������ ���������.
-- ����� ���������� �� ������ ��� ���������� ����������� (-O)

main :: IO ()
main = print $ minMaxSlow $ sTake 1000000 $ rand 7666532
-- main = print $ minMax $ sTake 1000000 $ rand 7666532
-- main = print $ minMaxBang $ sTake 1000000 $ rand 7666532


-- ������� 6 (������ ����� ��� ������ � FunctorsMonads) ----------------------------------

-- ���������� ���������� ������� ��� �������
instance Functor Stream where
    fmap f (x:>xs) = f x :> fmap f xs

instance Applicative Stream where
    pure = sRepeat
    (f:>fs) <*> (x:>xs) = f x :> (fs <*> xs)

instance Monad Stream where
    return = pure
    s >>= f = join' (fmap f s)
        where
            head' (x :> _) = x
            tail' (_ :> ys) = ys
            join' (xs :> xss) = head' xs :> (join' (fmap tail' xss))
