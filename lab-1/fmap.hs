
fmapX :: (a -> b) -> [a] -> [b]

fmapX _ [] = []  

fmapX f (x:xs) = (f x) : (fmapX f xs)



