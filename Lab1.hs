------------------------------------------------------------------------------------------------------------
-- ASKHSH 1



-- Function to calculate the distance between 2 points

distance (x1 , y1) (x2 , y2) = sqrt (x'*x' + y'*y')
    where
      x' = x1 - x2
      y' = y1 - y2





-- Function to calculate the area between 3 points in space as required by the excersise

area :: (Double,Double)->(Double,Double)->(Double,Double)->Double

area (xx,xy) (yx,yy) (zx,zy)
     =sqrt(t*(t-a)*(t-b)*(t-c))
        where
            a=distance(xx,xy) (yx,yy)
            b=distance(xx,xy) (zx,zy)
            c=distance(zx,zy) (yx,yy)
            t=(a+b+c)/2




----------------------------------------------------------------------------------
-- ASKHSH 2


-----------------------------------------------------------------------------------
-- ASKHSH 3






-- renamed the function as product', as there was a problem caused by a function already imported from Prelude.
-- #NOT_WORKING

product' :: Integer->Integer
product' n
    | n<0 = 0
    | n==0 =1
    | n>0 && (gcd n (n-1))==0  = n*x
    | n>0 && (gcd n (n-1)) /=0 = x
                where
                    x=product' (n-1)

        

-------------------------------------------------------------------------------------------------------------------
--ASKHSH 4


-- getting a list with all the digits of a number in reverse order
-- E.G. digs'' 15=[5,1]
--      digs'' 132=[2,3,1]
-- required only if we are going to work with lists. Otherwise it is not important

digits' :: Integral x => x -> [x]
digits' 0 = []
digits' x = x `mod` 10 : digits' (x `div` 10)






-- getting the digits of a number in the correct order. Doing the exact same thing as the above function, but in the correct order
-- E.G. digs' 15=[1,5]
--      digs' 132=[1,3,2]

digits :: Integral x => x -> [x]
digits x = reverse (digits' x)


-- function to 
numberofdigs :: [a]->[a]->Int
numberofdigs a b = x
    where 
        lengtha=length (a)
        lengthb=length (b)
        x= max lengtha lengthb


greater :: [a]->[a]->Int
greater a b
        | (length a)>(length b) = 1
        | (length a)<(length b) = (-1 )
        | (length a)==(length b) = 0

concatenate::Integral x => [x] -> x
concatenate x 
    |(length x)<=0 =0
    |otherwise = (last x)*(10^((length x)-1)) + concatenate (init x)


-- calculating the one digit of the new number
join'' :: Integer -> Integer -> Integer
join'' a b = (a*c) `mod` 10
                    where
                        c=9-b


join' :: Int -> Int -> Int
join' a b = x
        where
            digitsfirst = digits a
            digitssecond=digits b
            x=digitssecond!!1

join :: Int -> Int -> Int
join a b = x
    where
        tablea=digits a
        tableb=digits b
        max=numberofdigs tablea tableb
        x=tablea!!(max-1)



-- Defining main to check the results
main = do
    --print ( area (-1.01,-0.02) (0.99,-0.02) (-0.01,1.71))
    --print ( numberofdigs 120 15)
    --print (product' (-1))
    print(join 113 12)
    --print(concatenate(digs'' 15))