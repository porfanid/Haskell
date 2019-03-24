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

    
trace':: (Int,Int)->(Int,Int)->[(Int,Int)]
trace' (ax,ay) (bx,by) = y++[(bx,by)]
    where
        ax'=
            if(bx<ax) then
                bx+1
            else
                if(bx>ax) then
                    bx-1
                else
                    bx
        ay'=
                if(by<ay) then
                    by+1
                else
                    if(by>ay) then
                        by-1
                    else
                        by
        y = 
            if((ax==bx&&ay==by))  then
                []
            else
                (trace'(ax,ay) (ax',ay'))


trace :: [(Int,Int)]->[(Int,Int)]
trace a = 
    if(length a>1)then
        trace' (a!!0) (a!!1) ++ (trace (drop 2 a))
    else
        []





-----------------------------------------------------------------------------------------
     
-- ASKHSH 3



partition :: String->[[String]]
partition x = [["-2019"]]





-----------------------------------------------------------------------------------------
     
-- ASKHSH 4

coefficient:: Integer->Integer->Integer
coefficient i n = (toInteger (fromIntegral (x)))
    where
        x=(n-i+1)`div`(2^(i-1))

multiplication:: (Integer->Integer)->(Integer->Integer)->(Integer->Integer)
multiplication a b= a --  b)

hof' :: [Integer->Integer]-> Integer ->(Integer->Integer)
hof' (s:t) i = (multiplication s (coefficient i)) . (hof' t (i+1))
hof' [] k= (+0)

hof :: [Integer->Integer] ->(Integer->Integer)
hof a =hof' a 1

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
    print(trace [(2 , 2), (3 , 5)])
    putStrLn("---------Excersise 4------------")
    print(map (hof [(+1),(+2)]) [1..10])