
flatten :: [[a]] -> [a]

flatten [] = []
flatten ([]:vs) = flatten vs
flatten ((x:xs):vs) = x:flatten (xs:vs)
