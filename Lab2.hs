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

    
trace':: (Int,Int)->(Int,Int)->[(Int,Int)]
trace' (ax,ay) (bx,by) = [(bx,by)]++y
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


trace'' :: [(Int,Int)]->[(Int,Int)]
trace'' a = 
    if(length a>1)then
        (trace'' (drop 1 a))++trace' (a!!0) (a!!1)
    else
        []

trace :: [(Int,Int)]->[(Int,Int)]
trace matrix = ((trace'' a))
        where
            a=[(0,0)]++matrix++[(0,0)]



-----------------------------------------------------------------------------------------
     
-- ASKHSH 3



partition :: String->[[String]]
partition x = [["-2019"]]





-----------------------------------------------------------------------------------------
     
-- ASKHSH 4


coefficient:: Integer->Integer->Integer
coefficient i n = (n-i+1)

division::Integer->Integer->Integer
division x y= (y `div` x)

function:: (Integer->Integer)->(Integer->Integer) -> Integer->(Integer->Integer)
function a b i= (b . a) . (division ((2)^(i-1)))

hof' :: [Integer->Integer]-> Integer ->(Integer->Integer)
hof' (s:t) i = (function s (coefficient i) i) . (hof' t (i+1))
hof' [] i= (+0)


hof :: [Integer->Integer] ->(Integer->Integer)
hof a =hof' a 1

-----------------------------------------------------------------------------------------
     
-- ASKHSH 5                                   

combine :: Int->[u]->[v]->(u->v->w)->(u->v->w)->(Int->Bool)->[w]

combine originalSize (s:t) (a:b) f g h =
    if h(originalSize-(length (s:t))) then
        [f s a]++(combine originalSize t b f g h)
    else
        [g s a]++(combine originalSize t b f g h)
combine originalSize [] [] f g h=[]



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
    print(map (hof [(+1)]) [1..10])
    print(map (hof [(+1),(+2)]) [1..10])
    --print( map (hof [(2^),(2^),(2^),(2^),(2^)]) [5..12])
    print (combine (length [5,4,3,2]) [5,4,3,2] [7,8,9,10] (*) (^) odd)