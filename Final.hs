data II = I String deriving (Show,Eq)
data UU = U String deriving (Show,Eq)
data Fractional a => Rating a = R a | NoRating deriving (Show,Eq)

dis :: Eq a => [a] -> [a]
dis [] = []
dis (x:xs) | ((elem x xs)==True)=dis xs
		   |otherwise = (x:(dis xs))

			
fromRatingsToItems :: Eq a => [(b,a,c)] -> [a]
fromRatingsToItems [] = []
fromRatingsToItems a = reverse(dis (fromRatingsToItemsH a [])) 
fromRatingsToItemsH [] a = a
fromRatingsToItemsH ((a,b,c):xs) d= fromRatingsToItemsH xs (b:d)


fromRatingsToUsers :: Eq a => [(a,b,c)] -> [a]
fromRatingsToUsers [] = []
fromRatingsToUsers a = dis (reverse (fromRatingsToUsersH a [])) 
fromRatingsToUsersH [] a = a
fromRatingsToUsersH ((a,b,c):xs) d= fromRatingsToUsersH xs (a:d)


hasRating ::(Eq a, Eq b) => a -> b -> [(a,b,c)] -> Bool
hasRating _ _ [] = False
hasRating x y ((a,b,c):xs) | (x==a&&y==b)= True
							| otherwise= hasRating x y xs 
		

getRating :: (Eq a, Eq b) => a -> b -> [(a,b,c)] -> c
getRating _ _ [] = error "No given rating"
getRating x y ((a,b,c):xs) | (x==a&&y==b)= c
							| otherwise= getRating x y xs 

formMatrixUser :: (Eq a, Eq b, Fractional c) => b -> [a] -> [(b,a,c)] -> [Rating c]				
formMatrixUser _ _ [] = []
formMatrixUser a b c = reverse (formMatrixUserH a b c [])
formMatrixUserH a [] c d = d
formMatrixUserH a (x:xs) c d | ((hasRating a x c)==True)=formMatrixUserH a xs c ((R (getRating a x c)):d) 
							| otherwise = formMatrixUserH a xs c (NoRating:d)

formMatrix :: (Eq a, Eq b, Fractional c) => [b] -> [a] -> [(b,a,c)] -> [[Rating c]]
formMatrix [] _ _ =[]
formMatrix (x:xs) a b = (formMatrixUser x a b):(formMatrix xs a b)

numberRatingsGivenItem :: (Fractional a, Num b) => Int -> [[Rating a]] -> b
numberRatingsGivenItem c []=0
numberRatingsGivenItem c (x:xs)=if(numberRatingsGivenItemH c 0 x == NoRating) then 0+  numberRatingsGivenItem c xs
								else 1 + numberRatingsGivenItem c xs

numberRatingsGivenItemH c a (x:xs) = if(c==a) then x 
									else numberRatingsGivenItemH c (a+1) xs

differeneRatings :: Fractional a => Rating a -> Rating a -> a						
differeneRatings NoRating _ =0.0
differeneRatings _ NoRating =0.0
differeneRatings (R x) (R y)= x-y

matrixPairs :: Num a => a -> [(a,a)]
matrixPairs m = matrixPairsH1 m 0 0
matrixPairsH1 m n p = if(m/=n) then matrixPairsH2 m n 0 ++  matrixPairsH1 m (n+1) p else []
matrixPairsH2 m n p = if(m/=p) then (n,p): matrixPairsH2 m n (p+1) else []


dMatrix :: Fractional a => [[Rating a]] -> [a]
dMatrix  []=[]
dMatrix (x:xs)= sumH (dMatrixH x x) (dMatrix xs)
sumH x []=x
sumH (x1:xs1) (x2:xs2)=(x1+x2):(sumH xs1 xs2)

dMatrixH [] _ =[]
dMatrixH (x:xs) l= (diff x l)++(dMatrixH xs l)
diff _ []=[]
diff x (x2:xs) = (differeneRatings x x2): diff x xs 

freqMatrix :: (Num a, Fractional b) => [[Rating b]] -> [a]
freqMatrix []=[]
freqMatrix (x:xs) = sumH (freqMatrixH x x) (freqMatrix xs)

freqMatrixH [] _ =[]
freqMatrixH (x:xs) l = (freqMatrixHH x l)++freqMatrixH xs l
freqMatrixHH _ [] =[]
freqMatrixHH x (x1:xs)= if (x/=NoRating && x1/=NoRating) then 1:freqMatrixHH x xs
                        else 0:freqMatrixHH x xs

diffFreqMatrix :: Fractional a => [[Rating a]] -> [a]
diffFreqMatrix  []=[]
diffFreqMatrix x=  diffFreqMatrixH (dMatrix x) (freqMatrix x)

diffFreqMatrixH [] []=[]
diffFreqMatrixH (x:xs) (x1:xs1)= (x/x1):(diffFreqMatrixH xs xs1)

predictH1 [] c z=[]
predictH1 (x:xs) c z |((z)>0)= predictH1 xs c (z-1)
					| ((z)==0&&(c>0)) = x:predictH1 xs (c-1) z
					|otherwise= predictH1 xs c z

predictH2 y c v= predictH1 (diffFreqMatrix (formMatrix (  (fromRatingsToUsers y)) (predictH7 y) y)) c (c*v) 
predictH3 [] _= []
predictH3 (x:xs) c | (c>0)= predictH3 xs (c-1)
					| otherwise= x

turn [] =[]
turn (x:xs) | (x==NoRating)= (0.0:turn xs)
			| otherwise= (y:turn xs) where (R y)=(x)

predictH4 [] [] =[]
predictH4 (x:xs) (y:ys) = (x+y):predictH4 xs ys 

predictH5 y c v u= predictH4 (predictH2 y c v) 	(turn (predictH3 (formMatrix (  (fromRatingsToUsers y)) (predictH7 y) y) u))		
predictH6 :: Eq a =>[a]->Double
predictH6 [] =0	
predictH6 (x:xs) = 1+predictH6 xs
predict y u i = (sum ((predictH5 y (predictH6 (fromRatingsToItems y)) i u)))/((predictH6 (fromRatingsToItems y))-1)	


helper (x:xs) y z | (elem NoRating (formMatrixUser x y z))= helper xs y z
					| otherwise= x
helper2 _ [] = []
helper2 x ((f,y,z):xs) | (x==f) = y:(helper2 x xs) 
						|otherwise = helper2 x xs 
predictH7 y = helper2  (helper (  (fromRatingsToUsers y)) (reverse (fromRatingsToItems y)) y) y