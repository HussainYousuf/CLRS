module Lib where

import Debug.Trace


main = print("asdsad")




-- picks :: [a] -> [(a, [a])]
-- picks []= []
-- picks (x : xs) = (x, xs) : [(y, x : ys) | (y, ys) <- picks xs]

-- -- foldr f e · map g = foldr (f · g) e

-- -- h (f x y) = g x (h y)

-- -- h (foldr f e xs) = foldr g (h e) xs
-- -- function change and initial value function factor out

-- -- foldr f e · concat = ????

-- -- concat = foldr (++) []
-- -- (++) = flip (foldr (:)) 

-- -- foldr f e (xs ++ ys) = ????

-- -- foldr f e (foldr (:) ys xs) = foldr f e (xs ++ ys)
-- -- h (foldr f e xs) = foldr g (h e) xs

-- -- e = ys
-- -- xs = xs
-- -- h = foldr f e
-- -- g = f = (:)

-- -- foldr f e · (foldr (++) []) = foldr f e · concat 

-- -- foldr f e · (foldr (flip (foldr (:))) []) = foldr f e · concat 

-- -- foldr (flip (foldr (:))) [] = (foldr (foldr (:)) []) . reverse
-- --                             =  h     (foldr  f )

-- -- foldr f e · concat = foldr f e · (foldr (foldr (:)) []) . reverse

-- takeWhile' p = foldr (\x xs -> if p x then x:xs else []) []

-- dropWhileEnd' p = foldr (\x xs -> if p x && null xs then xs else x:xs) []