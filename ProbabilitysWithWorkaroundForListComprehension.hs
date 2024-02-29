-- Work in Progress 
-- tried to make Workaround for List Comprehensions
probabilitylist :: [(Int, Double)]

probabilitylist = [(1,0.001),(2,0.001),(3,0.01),(4,0.001),(5,0.01),(6,0.001),(7,0.01),(8,0.01),(9,0.0033),(10,0.0067),(11,0.001),(12,0.0133),(13,0.0033),(14,0.01),(15,0.0067),(16,0.0067),(17,0.0033),(18,0.0033),(19,0.0033),(20,0.0033),(21,0.001),(22,0.0067),(23,0.0033),(24,0.0033),(25,0.0033),(26,0.0033),(27,0.0067),(28,0.0033),(29,0.0067),(30,0.0033),(31,0.0067),(32,0.0067),(33,0.0067),(34,0.0033),(35,0.0067),(36,0.0033),(37,0.0067),(38,0.0067),(39,0.001),(40,0.0033),(41,0.001),(42,0.01),(43,0.001),(44,0.01),(45,0.01),(46,0.0033),(47,0.0067),(48,0.01),(49,0.0033),(50,0.0033),(51,0.001),(52,0.0067),(53,0.0033),(54,0.001),(55,0.01),(56,0.001),(57,0.0067),(58,0.0133),(59,0.0067),(60,0.0033),(61,0.001),(62,0.001),(63,0.0033),(64,0.0033),(65,0.01),(66,0.0067),(67,0.0067),(68,0.001),(69,0.001),(70,0.0033),(71,0.01),(72,0.001),(73,0.001),(74,0.0233),(75,0.0167),(76,0.22),(77,0.1167),(78,0.0933),(79,0.07),(80,0.0467),(81,0.0367),(82,0.01),(83,0.01),(84,0.0067),(85,0.001),(86,0.001),(87,0.0033),(88,0.001),(89,0.0033),(90,0.001)]



probabilitylistdroppedp p = drop p probabilitylist


-- old part for reference
createnewlistforp p = [((fst x),(snd x)/(sumallsndfromtuplelist 0 (probabilitylistdroppedp p))) | x <-(probabilitylistdroppedp p)]



combinations_calc azw p =  [((snd x)*(snd y)*(snd z)) | x <-(drop p probabilitylist), y <- probabilitylist, z <- probabilitylist, ((fst x)+(fst y)+(fst z))<=azw+p]


combination_prob azw p = [((snd x),(snd y),(snd z)) | x <- (drop p probabilitylist), y <- probabilitylist, z <- probabilitylist, ((fst x)+(fst y)+(fst z))<=azw+p]



combinations_count azw p = [((fst x),(fst y),(fst z)) | x <- (drop p probabilitylist), y <- probabilitylist, z <- probabilitylist, ((fst x)+(fst y)+(fst z))<=azw+p]


-- calculates probability for getting n specified things when trying azw times and already having tried p times if the generall Probabilitys are like in the probability list above
wsksumvar n azw p

   | n == 1 = sumlist 0 (combinations_calc_1 azw p)

   | n == 2 = sumlist 0 (combinations_calc_2 azw p)

   | n == 3 = sumlist 0 (combinations_calc_3 azw p)

   | n == 4 = sumlist 0 (combinations_calc_4 azw p)

   | n == 5 = sumlist 0 (combinations_calc_5 azw p)

   | n == 6 = sumlist 0 (combinations_calc_6 azw p)

   | otherwise = sumlist 0 (combinations_calc_6 azw p)
-- end of old part for reference




sumlist sum [] = sum

sumlist sum (x:xs) = sumlist (sum+x) xs

-- hier laufzeitvariable



sumallsndfromtuplelist sum [] = sum

sumallsndfromtuplelist sum (x:xs) = sumallsndfromtuplelist (sum+(snd x)) xs


-- old part for reference
combinations_calc_1 azw p =  [(snd x) | x <-(createnewlistforp p), (fst x)<=azw+p]



combinations_calc_2 azw p =  [((snd x)*(snd y)) | x <-(createnewlistforp p), y <- probabilitylist, ((fst x)+(fst y))<=azw+p]



combinations_calc_3 azw p =  [((snd x)*(snd y)*(snd z)) | x <-(createnewlistforp p), y <- probabilitylist, z <- probabilitylist, ((fst x)+(fst y)+(fst z))<=azw+p]



combinations_calc_4 azw p =  [((snd x)*(snd y)*(snd z)*(snd a)) | x <-(createnewlistforp p), y <- probabilitylist, z <- probabilitylist, a <- probabilitylist, ((fst x)+(fst y)+(fst z)+(fst a))<=azw+p]



combinations_calc_5 azw p =  [((snd x)*(snd y)*(snd z)*(snd a)*(snd b)) | x <-(createnewlistforp p), y <- probabilitylist, z <- probabilitylist, a <- probabilitylist, b <- probabilitylist, ((fst x)+(fst y)+(fst z)+(fst a)+(fst b))<=azw+p]



combinations_calc_6 azw p =  [((snd x)*(snd y)*(snd z)*(snd a)*(snd b)*(snd c)) | x <- (createnewlistforp p), y <- (take azw probabilitylist), z <- (take azw probabilitylist), a <- (take azw probabilitylist), b <- (take azw probabilitylist), c <- (take azw probabilitylist), ((fst x)+(fst y)+(fst z)+(fst a)+(fst b)+(fst c))<=azw+p]
-- end of old part for reference


testlist = [0.00000001,0.00000002..0.9]

testlist3 = [1,2]

-- Zählervariablen: za, zb und zlist

--multiplywsk_for_combinations za zb [] [] [] = []

--multiplywsk_for_combinations za zb zlist [] [] = zlist

--multiplywsk_for_combinations 


-- helpmethods
checkweather_2_nums_added_are_lower_or_equal_to_num3 num1 num2 num3

   | (num1+num2)<=num3 = True

   | otherwise = False



add_to_list_if_check_true check num list

   | check==True = num:list

   | otherwise = list

   

add_tuple_to_list_if_check_true check num1 num2 list

   | check==True = (num1,num2):list

   | otherwise = list

   

simple_check num1 num2 num3 = checkweather_2_nums_added_are_lower_or_equal_to_num3 num1 num2 num3

   

   

-- actual Workaround Methods for replacing list comprehensions

multiplywsk_for_combinations za zb zlist lista listb

   | lista == [] && listb == [] = []

   | lista == [] || listb == [] = []

   | zb == ((length listb)-1) && za == ((length lista)-1) = ((lista !! za)*(listb !! zb)):zlist 

   | zb == ((length listb)-1) = multiplywsk_for_combinations (za+1) 0 (((lista !! za)*(listb !! zb)):zlist) lista listb

   | otherwise = multiplywsk_for_combinations za (zb+1) (((lista !! za)*(listb !! zb)):zlist) lista listb

   

--multiplywsk_for_combinations_tuplelists za zb zlist lista listb

--   | lista == [] && listb == [] = []

--   | lista == [] || listb == [] = []

--   | zb == ((length listb)-1) && za == ((length lista)-1) = ((snd(lista !! za))*(snd(listb !! zb))):zlist 

--   | zb == ((length listb)-1) = multiplywsk_for_combinations_tuplelists (za+1) 0 (((snd(lista !! za))*(snd(listb !! zb))):zlist) lista listb

--   | otherwise = multiplywsk_for_combinations_tuplelists za (zb+1) (((snd(lista !! za))*(snd(listb !! zb))):zlist) lista listb



multiplywsk_for_combinations_tuplelists za zb zlist lista listb azw

   | lista == [] || listb == [] = []

   | zb == ((length listb)-1) && za == ((length lista)-1) = add_tuple_to_list_if_check_true (simple_check (fst(lista !! za)) (fst(listb !! zb)) azw) ((fst(lista !! za))+(fst(listb !! zb))) ((snd(lista !! za))*(snd(listb !! zb))) zlist 

   | zb == ((length listb)-1) = multiplywsk_for_combinations_tuplelists (za+1) 0 (add_tuple_to_list_if_check_true (simple_check (fst(lista !! za)) (fst(listb !! zb)) azw) ((fst(lista !! za))+(fst(listb !! zb))) ((snd(lista !! za))*(snd(listb !! zb))) zlist) lista listb azw

   | otherwise = multiplywsk_for_combinations_tuplelists za (zb+1) (add_tuple_to_list_if_check_true (simple_check (fst(lista !! za)) (fst(listb !! zb)) azw) ((fst(lista !! za))+(fst(listb !! zb))) ((snd(lista !! za))*(snd(listb !! zb))) zlist) lista listb azw





testreturnfunktion n list innerlist azw

   | n == 0 = list --(multiplywsk_for_combinations_tuplelists 0 0 [] list innerlist azw)

   | otherwise = testreturnfunktion (n-1) (multiplywsk_for_combinations_tuplelists 0 0 [] list innerlist azw) innerlist azw

-- hier error weil liste die bei multiplywsk Function zurückkommt keine tuple list ist; da müssen noch die fst tuples wieder hinzugefügt werden innerhalb der Methode





--trampolinolike_construct n firstlist nextlist azw

--   | n>0 = testreturnfunktion n (trampolinolike_construct (n-1)) 

   

   





--add_to_list_if_check_true (simple_check (fst(lista !! za)) (fst(listb !! zb)) azw) ((snd(lista !! za))*(snd(listb !! zb))) zlist


