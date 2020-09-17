
sinHelper :: Int -> Int -> Double -> Double -> Double -> Double
sinHelper n k x stepx current 
    | n + 1 == k = current
    | otherwise      =  sinHelper n (k + 1) x ((stepx * (-1.0) * x * x) / (fromIntegral ((2 * (k + 1)) * (2 * (k + 1) + 1)) :: Double)) (stepx + current)

sinX :: Double -> Int -> Double
sinX x n = sinHelper n 1 x ((x * x * x) / (-6.0)) x
