module Chp5 where
  
import Debug.Trace

eval_polynomials :: Int -> [Int] -> Int
eval_polynomials x as = foldr (\(i,a) acc -> acc + a*x^i) 0 $ zip [0..length as] as

one_to_one_mappings :: Eq a => [(a, a)] -> [(a, a)]
one_to_one_mappings [(a,b)] = [(a,b)]
one_to_one_mappings xxs@(x@(a,_):xs) = if length (filter (==a) (map snd xxs)) == 0 
    then one_to_one_mappings xs
    else x : one_to_one_mappings xs

max_sub_seq :: (Ord a, Num a) => [a] -> [a]
max_sub_seq [x] = if x > 0 then [x] else []
max_sub_seq xs = reverse $ go xs [] []
    where
        go [] _ max_sub = max_sub
        go (x:xs) cur_sub max_sub
            | cur_sum > 0 = if cur_sum > sum max_sub 
                            then go xs cur_sub' cur_sub' 
                            else go xs cur_sub' max_sub
            | cur_sum <= 0 =     go xs []       max_sub
            where
                cur_sub' = x : cur_sub
                cur_sum = sum cur_sub'

inserts :: Num a => a -> [a] -> [[a]]
inserts y [] = [[y]]
inserts y (x:xs) = (y:x:xs) : map (x:) (inserts y xs)

perms [] = [[]]
perms (x:xs) = [l | ys <- perms xs, l <- inserts x ys]

