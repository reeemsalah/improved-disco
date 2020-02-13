import System.Random 
import System.IO.Unsafe
import DataFile 



--listToSet
listToSet :: (Eq a)=>[a]->[a]->[a]
listToSet [] acc = acc
listToSet (x:xs) acc = if elem x acc then listToSet xs acc else x: listToSet xs acc




--createEmptyFreqList
createEmptyFreqList :: [a] -> [(a, [b])]
createEmptyFreqList [] = []
createEmptyFreqList (x:xs) = (x,[]) : createEmptyFreqList xs


--get frequency of an item helper of getfreq2 
getfreq :: [(String,Int)]-> Int
getfreq [] = 0
getfreq ((a,b):xs) = b + getfreq xs 


--get freq2
getfreq2 :: String -> [(String,[(String,Int)])]-> Int
getfreq2 itemA ((itemB,item_list):xs) = if itemA == itemB then getfreq item_list
									    else getfreq2 itemA xs
									

-- getfreqAll , a is the list of purchases of some user								
getfreqAll :: [String] -> [(String,[(String,Int)])] -> [(String,Int)]
getfreqAll [] a = []
getfreqAll (x:xs) a = if getfreq2 x a == 0 then getfreqAll xs a
					  else (x,getfreq2 x a ) : getfreqAll xs a
					
					
--freqListItems
freqListItems :: String -> [(String, Int)]
freqListItems user = freqListItemsHelper user (getAllUsersStats purchasesHistory)


--freqListItemsHelper0
freqListItemsHelper :: String ->[(String, [(String, [(String, Int)])])] -> [(String,Int)] 
freqListItemsHelper user ((userx,item_list):xs) = if user==userx then getfreqAll items item_list
											      else freqListItemsHelper user xs 
											
											
-- get cart of a user from purchase history
getCart :: [(String,[(String,[(String,Int)])])]	-> String -> 	[(String,[(String,Int)])]
getCart ((userX,cart):xs) userY	= if userX == userY then cart 
								  else getCart xs userY

								
--get items of a cart 
getItemsOfCart :: [(String,[(String,Int)])] -> String -> (String,[(String,Int)])
getItemsOfCart ((itemX,x):xs) itemY = if itemX == itemY then (itemX,x)
									  else getItemsOfCart xs itemY


--getItemsOfCart
getItemsOfCart2 :: [(String,[(String,Int)])] -> [String] -> [(String,[(String,Int)])]
getItemsOfCart2 pH [] = [] 
getItemsOfCart2 pH (x:xs) = getItemsOfCart pH x : getItemsOfCart2 pH xs 


--get freq of item with another item
getH :: [(String,Int)]-> String -> Int
getH [] x = 0
getH ((item1,x):xs) item2 = if item1 == item2 then x else getH xs item2 


getH2 :: [(String,[(String,Int)])]->String->Int
getH2 [] it = 0
getH2 ((it,x):xs) itemx =  getH x itemx + getH2 xs itemx 


getH3 :: [(String,[(String,Int)])]->String->(String,Int)
getH3 x item = (item,getH2 x item)


getAllitems ::  [(String,[(String,Int)])]->[String]->[(String,Int)]
getAllitems x [] = []
getAllitems x (h:t) = if getH2 x h == 0 then  getAllitems x t else getH3 x h : getAllitems x t


freqListCart :: String ->[String] -> [(String, Int)]
freqListCart user itemsX = getAllitems (getItemsOfCart2 (getCart (getAllUsersStats purchasesHistory) user ) itemsX ) items 


--adds cart and previous purchase
addX :: [(String,Int)]->[(String,Int)]->[(String,Int)]
addX [] x = x 
addX x [] = [] 
addX ((itemx,x):xs) ((itemy,y):ys) = if itemx==itemy then (itemx,x+y):addX xs ys 
									else if itemx < itemy then (itemx,x): addX xs ((itemy,y):ys) 
										 else (itemy,y):addX ((itemx,x):xs) ys

freqListCartAndItems :: String -> [String] -> [(String, Int)]
freqListCartAndItems user itemS = addX (freqListCart user itemS) (freqListItems user)


--get freq itemx with itemy
intersectH :: [[String]]->String->String->Int
intersectH [] itemX itemY = 0
intersectH (x:xs) itemX itemY = if elem itemX x && elem itemY x then 1 + intersectH xs itemX itemY
							    else intersectH xs itemX itemY


--intersection between an item and each item in purchase history
intersect_with_all :: [[String]]->String -> [String]-> [(String,Int)]
intersect_with_all pH item [] = []
intersect_with_all pH item (x:xs) = if x == item then intersect_with_all pH item xs 
								    else if intersectH pH item x == 0 then  intersect_with_all pH item xs
								         else (x,intersectH pH item x):intersect_with_all pH item xs 
								

--intersection between each item and each item in purchase history		
intersect_whole :: [[String]]->[String]->[String]->[(String,[(String,Int)])]
intersect_whole pH [] itemsX = []
intersect_whole pH (x:xs) itemsX = (x,intersect_with_all pH x itemsX) : intersect_whole pH xs itemsX	


-- get one user stats
getUserStats :: [(String,[[String]])]-> String->(String,[(String,[(String,Int)])])
getUserStats ((userA,item_list):xs) userB = if userB==userA then (userB ,intersect_whole item_list items items)
										    else getUserStats xs userB
										
										
--get all users stats helper
getUserStatsH :: [(String,[[String]])]-> [String]->[(String,[(String,[(String,Int)])])]
getUserStatsH pH [] = [] 
getUserStatsH pH (userB:t) = getUserStats pH userB : getUserStatsH pH t


-- get all users stats
getAllUsersStats :: [(String, [[String]])] -> [(String, [(String, [(String, Int)])])]
getAllUsersStats pH = getUserStatsH pH users


-- get purchases of a user
getPurchases :: [(String,[[String]])] -> String -> [[String]]
getPurchases ((userx,x):xs) usery = if userx==usery then x 
                                    else getPurchases xs usery 


--recommend empty cart
recommendEmptyCart :: String -> String
recommendEmptyCart user = if length (getPurchases purchasesHistory user)==0 then []
                          else unzip1 ( freqListItems user ) !! randomZeroToX(  length ( unzip1( freqListItems user))-1 )
		
-- Flatten Function that flattens a list
flatten l = foldr (++) [] l


--ListToSet function
listToSet1 list = helperListToSet [] list
helperListToSet set [] = set
helperListToSet acc (x:xs) | elem x acc = helperListToSet acc xs
						   | otherwise =  helperListToSet (acc++[x]) xs
						   


--recommenBasedOnItemsInCart
recommendBasedOnItemsInCart :: String -> [String] -> String
recommendBasedOnItemsInCart user currentCart | length (getPurchases purchasesHistory user) ==0 =[]
											 | boughtBefore user (currentCart !! 0) = unzip1 (freqListCartAndItems user currentCart) !! randomZeroToX(  length ( unzip1 (freqListCartAndItems user currentCart))-1 )
											 | otherwise = flatten (getPurchases purchasesHistory user) !! randomZeroToX(  length (flatten (getPurchases purchasesHistory user))-1 )
														


--unzip1
unzip1 :: [(String,Int)] -> [String]
unzip1 []=[]
unzip1 ((item,n):xs) = repeat1 item n ++ ( unzip1 xs )

														
--freqListUsers
freqListUsers :: String -> [(String,Int)]
freqListUsers user =freqListHelper items ( purchasesIntersection (getPurchasesOf ( getAllUsersStats purchasesHistory) user) (getPurchasesExcept (getAllUsersStats purchasesHistory) user))


--getPurchasesExcept => gets the purchases of all users except the one as an input
getPurchasesExcept :: [(String ,[ (String,[(String,Int)])])] -> String ->[(String ,[ (String,[(String,Int)])])]
getPurchasesExcept [] user = []
getPurchasesExcept ((userX,l):xs) user | user == userX = xs
										| otherwise =(userX,l):(getPurchasesExcept xs user)
										
										
--getPurchasesOf 
getPurchasesOf :: [(String ,[ (String,[(String,Int)])])]  -> String -> [ (String,[(String,Int)])]
getPurchasesOf [] user =[]
getPurchasesOf ((userX,l):xs) user| userX==user=l
								  | otherwise = getPurchasesOf xs user

--freqListUsersHelper
freqListHelper :: [ String]-> [[(String,[(String,Int)])]] -> [(String ,Int)]
freqListHelper [] l=[]
freqListHelper (itemX:xs) l | (getItemFreq1 itemX l)==0 =freqListHelper xs l
                            | otherwise= (itemX,(getItemFreq1 itemX l )):freqListHelper xs l


--getItemFreq1=> in a list of lists` 
getItemFreq1 :: String -> [[(String,[(String,Int)])]] -> Int
getItemFreq1 item [] =0
getItemFreq1 item (x:xs) = ( getItemFreq2 item x )+ (getItemFreq1 item xs)


--getItemFreq2 => in a list
getItemFreq2 :: String ->[(String,[(String,Int)])] -> Int
getItemFreq2 item [] =0 
getItemFreq2 itemX ((item,l):xs) = getItemFreq3 itemX l + getItemFreq2 itemX xs


--getItemFreq3 => from frequencies with other item
getItemFreq3 :: String -> [(String , Int)]->Int
getItemFreq3 item [] =0
getItemFreq3 itemX  ((item,f):xs) | itemX == item =f
							      | otherwise= getItemFreq3 itemX xs
								  
								  
--a function checks whether a user bought an item before
boughtBefore :: String -> String -> Bool
boughtBefore user item = elem item ( flatten ( getPurchases  purchasesHistory user ) )


--purchasesIntersection
purchasesIntersection :: [(String,[(String,Int)])]-> [(String,[(String,[(String,Int)])])]-> [[(String,[(String,Int)])]]
purchasesIntersection user [] = []
purchasesIntersection user ((userX,purchasesX):ux) = (userIntersection user purchasesX ): purchasesIntersection user ux


-- intersection between 2 users using purchases History
userIntersection ::  [(String,[(String,Int)])]-> [(String,[(String,Int)])] -> [(String,[(String,Int)])]
userIntersection [] l = []
userIntersection l [] = []
userIntersection ((i1,l1):x1) ((i2,l2):x2) = if (l1/=[] && l2/=[]) then (if (interSect l1 l2 )== False then (i1,(l2++l1)):userIntersection x1 x2
																	   else (i1 ,((getIntersection l1 l2)++(removeIntersection l1 l2))):userIntersection x1 x2 )
											else userIntersection x1 x2
																	   
																	   
--get the intersection of the list of frequencies 
getIntersection :: [(String,Int)] -> [(String,Int)] -> [(String,Int)]
getIntersection [] l =[]
getIntersection l [] =[]
getIntersection ((i1,x):xs) ((i2,y):ys)| i1==i2 = (i1,(x+y)):getIntersection xs ys			
									   | i1<i2 = getIntersection xs ((i2,y):ys)
									   | otherwise = getIntersection ((i1,x):xs) ys



-- remove the intersection of the list of frequencies
removeIntersection :: [(String,Int)] -> [(String,Int)] -> [(String,Int)]
removeIntersection [] l =l
removeIntersection l [] =l
removeIntersection ((i1,x):xs) ((i2,y):ys)| i1==i2 = removeIntersection xs ys			
									   | i1<i2 = (i1,x):removeIntersection xs ((i2,y):ys)
									   | otherwise = (i2,y):removeIntersection ((i1,x):xs) ys
									


--intersect between frequency of 2 items
interSect :: [(String,Int)] -> [(String,Int)] -> Bool
interSect [] l = False
interSect ((i1,x):xs) l = if found i1 l then True
						  else interSect xs l


--found element in a list
found i1 [] = False
found i1 ((i2,x):xs) = if i1==i2 then True
					   else found i1 xs



--getUsersPurchasesFromHistory
getUsersPurchasesFromHistory :: [(String,[(String,Int)])] -> [String]
getUsersPurchasesFromHistory [] =[] 
getUsersPurchasesFromHistory ((x,t): xs) | t==[] = getUsersPurchasesFromHistory xs
										  | otherwise = [x]++ getUsersPurchasesFromHistory xs



repeat1 :: String -> Int -> [String]
repeat1 s n | n==0 =[]
		    | otherwise = [s] ++ ( repeat1 s (n-1) )



--recommendBasedOnUsers 
recommendBasedOnUsers :: String -> String
recommendBasedOnUsers user |length ( unzip1 (freqListUsers user) ) ==0 = ""
                           |otherwise =( unzip1 (freqListUsers user) ) !! randomZeroToX(  length ( unzip1 (freqListUsers user))-1 )


--RECOMMENDDDDD
recommend :: String -> [String] -> String
recommend user cart = if cart==[] then 
									if recommendEmptyCart user ==[] then items !!  randomZeroToX(  length ( items)-1 )
									 else recommendEmptyCart user
					  else   
								if recommendBasedOnItemsInCart user cart ==[] then items !!  randomZeroToX(  length ( items)-1 )
								else (( recommendBasedOnItemsInCart user cart ): (recommendBasedOnUsers user):[])!!randomZeroToX 1
		   

					