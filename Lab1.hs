------------------------------------------------------------------------------------------------------------
-- ASKHSH 1



-- Function to calculate the distance between 2 points

distance (x1 , y1) (x2 , y2) = sqrt (x'*x' + y'*y')
    where
      x' = x1 - x2
      y' = y1 - y2





-- Function to calculate the area between 3 points in space

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
digs'' :: Integral x => x -> [x]
digs'' 0 = []
digs'' x = x `mod` 10 : digs'' (x `div` 10)






-- getting the digits of a number in the correct order
digs' :: Integral x => x -> [x]
digs' x = reverse (digs'' x)



numberofdigs :: [a]->[a]->Int
numberofdigs a b = x
    where 
        lengtha=length (a)
        lengthb=length (b)
        x= max lengtha lengthb


greater :: [a]->[a]->Int
greater a b
        | (length a)>((length b) =1
        | (length a)<((length b) =-1 
        | (length a)==((length b) = 0

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
            digits=digs' a
            digitssecond=digs' b
            x=digitssecond!!1

join :: Int -> Int -> Int
join a b = x
    where
        tablea=digs' a
        tableb=digs' b
        max=numberofdigs tablea tableb
        x=tablea!!(max-1)



-- Defining main to check the results
main = do
    --print ( area (-1.01,-0.02) (0.99,-0.02) (-0.01,1.71))
    --print ( numberofdigs 120 15)
    --print (product' (-1))
    print(join 113 12)
    --print(concatenate(digs'' 15))