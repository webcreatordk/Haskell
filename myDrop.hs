import Data.Char

double num=num*2

myDrop n xs= if n<=0 || null xs
		then xs
		else myDrop (n-1) (tail xs)

mySecond xs= if null (tail xs)
			 then error "list too short"
			 else head (tail xs)

safeSecond xs= if null (tail xs)
			   then Nothing
			   else Just (head (tail xs))

lend amount balance = let
						reverse=100
						newB=balance-amount
					  in if balance<reverse
					  	  then Nothing
					  	  else Just newB

lend2 amount balance = if amount<reverse*0.5
					    then Just newB
					    else Nothing
			where 
				reverse=100
				newB=balance-amount

pluralise word counts=map plural counts
		where 
			plural 0="no "++word++"s"
			plural 1="one "++word
			plural n=show n++" "++word++"s"

-- pattern match function examples
addVectors (x1,y1) (x2,y2)=(x1+x2,y1+y2)

head' []=error "input empty!"
head' (x:_)=x

length' []=0
length' (_:xs) =1+length' xs

sum' []=0
sum' (x:xs)=x+sum' (xs)

safeHead []=Nothing
safeHead (x:_)=Just x;

--initials ::String->String->String
initials firstname lastname=[f]++". "++[l]++"."
	where
		(f:_)=firstname
		(l:_)=lastname


capital ""="Empty string!"
capital all@(x:xs)="The first letter of "++all++" is "++[x]

--Guards Examples
--Guards are indicated by pipes that follow a function's name and its parameters.

bmiTell bmi
	|bmi<=18.5 = "You are underweight!"
	|bmi<=25.0 = "You are supposedly normal!"
	|bmi<=30.0 = "You are fat!"
	|otherwise = "unNormal!!!!!"

bmiTell2 weight height
	|weight/height^2 <=18.5 = "You are underweight!"
	|weight/height^2 <=25.0 = "You are supposedly normal!"
	|weight/height^2 <=30.0 = "You are fat!"
	|otherwise = "unNormal!!!!!"

bmiTell3 weight height
	|bmi<=18.5 = "You are underweight!"
	|bmi<=25.0 = "You are supposedly normal!"
	|bmi<=30.0 = "You are fat!"
	|otherwise = "unNormal!!!!!"
	where bmi=weight/height^2

maxNum a b
	| a>b =Just a 
	| a==b =Just "eq"
	| otherwise =Just b

--Exercises Real World Haskell P69

calElements []=0
calElements (x:xs)=1+ calElements xs
 
calMean xs=(sum' xs) / (calElements xs)

reverse' []=[]
reverse' [x]=[x]
reverse' (x:xs)= reverse' xs ++ [x]


palindrome xs= xs++ (reverse' xs)

isPalindrome xs
		|xs=="" ="It is empty list"
		|xs==(reverse' xs) && (mod (length xs) 2)==0 ="It is Palindrome List"
		|otherwise ="Not Palindrome List"

test a = let c=a-10
		 in if a<10
		 	then Nothing
		 	else Just c
--loop

asInt xs=loop 0 xs

loop acc []=acc
loop acc(x:xs)=let acc'=acc*10+digitToInt x
				in loop acc' xs

elementSquare (x:xs)=x*x:elementSquare xs
elementSquare []=[]

upperCase (x:xs)=(toUpper x):(upperCase xs)
upperCase [] = []

--lambda example








