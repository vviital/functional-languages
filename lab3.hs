module Lab3 where

isPowerOfTwo n | or [n == 1, n == (-1)] = True
isPowerOfTwo n | n `mod` 2 == 1 = False
isPowerOfTwo n = isPowerOfTwo(n `div` 2)

isntPowerOfTwo n = not(isPowerOfTwo n)

getBySelector [] sel = []
getBySelector (x:xs) sel | sel x == True = x : getBySelector xs sel
getBySelector (x:xs) sel | sel x /= True = getBySelector xs sel

sepatate x = (getBySelector x isPowerOfTwo) ++ (getBySelector x isntPowerOfTwo) 

callSeparate = sepatate array
    where array = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
