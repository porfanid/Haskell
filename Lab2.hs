-----------------------------------------------------------------------------------------

-- ASKHSH 1
sumInt::[Int]->Int
sumInt (s:t)=s + (sumInt t)
sumInt[] = 0


listMultiplication:: [Int]->[Int]->Int
listMultiplication a b = (sumInt a)*(sumInt b)


xsum'::[Int]->[Int]->Int
xsum' a (s:t) = (listMultiplication a (s:t)) + (xsum' ([s]++a) t)
xsum' a [] = 0


xsum :: [Int]->Int
xsum a = xsum' [] a





-----------------------------------------------------------------------------------------
     
-- ASKHSH 2

getNum:: (Int,Int)->Int->Int
getNum (x,y) z
    |z==0 =x
    |z==1 =y
    |otherwise = error ("Element not found")
trace':: [(Int,Int)]->Int
trace' (s:t) = getNum s 0


trace :: [(Int,Int)]->[(Int,Int)]
trace s = [(-2019,-2019)]





-----------------------------------------------------------------------------------------
     
-- ASKHSH 3



partition :: String->[[String]]
partition x = [["-2019"]]





-----------------------------------------------------------------------------------------
     
-- ASKHSH 4

coefficient:: Integer->Integer->Integer
coefficient i n = (toInteger (fromIntegral ((n-i+1)`div`(2^(i-1)))))

hof :: [Integer->Integer]-> Int ->(Integer->Integer)
hof (s:t) i = (s . (coefficient 1)) . (hof t (i+1))
hof [] k= (+0)

bits :: Integer -> Int
bits 0 = 0
bits n = fromInteger n `mod` 2 + bits(fromInteger n `div` 2)


-----------------------------------------------------------------------------------------
     
-- ASKHSH 5                                   

combine :: [u]->[v]->(u->v->w)->(u->v->w)->(Int->Bool)->[w]

combine s t f g h = []




main = do
    putStrLn("---------Excersise 1------------")
    print(xsum [4,5,8])
    print( xsum [10,3,12,7])
    print(xsum [10,7,4,8,12])
    print(xsum [1..10])
    print(xsum [1..100])
    putStrLn("---------Excersise 2------------")
    print(trace' [(3,8)])
    putStrLn("---------Excersise 4------------")
    print(map (hof [(+1),(+2)] 0) [1..10])