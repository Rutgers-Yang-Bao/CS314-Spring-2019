module Project1 where

div7or9 :: Integer -> Bool
div7or9 n = if mod n 7 == 0 || mod n 9 == 0 then True else False

echo :: [Char] -> [Char]
echo x = if length x /= 0 then head x : head x : echo(tail x) else []

echons :: [Char] -> [Char]
echons x = 
	if length x /= 0 
		then if head x /= ' ' 
			then head x : head x : echons(tail x) 
			else head x : echons(tail x) 
		else []

countEvens :: [Integer] -> Integer
countEvens x = 
	if length x /= 0
		then if mod (head x) 2 == 0
			then 1 + countEvens (tail x)
			else 0 + countEvens (tail x)
		else 0 
centroid :: [(Double,Double)] -> (Double,Double)
centroid x = 
	if length x /= 0
	{-
		then add x
	-}

		then ( fst (add x) / fromIntegral (length x), snd (add(x)) / fromIntegral (length x))

		else (0,0)	

add :: [(Double,Double)] -> (Double,Double)
add a =
	if a /= []
		then (fst(head a) + fst(add (tail a)), snd(head a) + snd(add (tail a)))
		else (0,0)


hailstone :: Integer -> Integer
hailstone x = snd (func (x,1)) 

func :: (Integer,Integer) -> (Integer,Integer)
func a = 
	if fst a /= 1
		then if mod (fst a) 2 == 0
			then func(div (fst a) 2, snd a + 1)
			else func(3*(fst a) + 1, snd a + 1)
		else (fst a, snd a)


