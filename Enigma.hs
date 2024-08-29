
module Enigma where
  import Data.Char  -- to use functions on characters
  import Data.Maybe -- breakEnigma uses Maybe type
  import Data.List
  import Debug.Trace


{- Part 1: Simulation of the Enigma -}

  type Rotor = (String,Int) -- the supplied type is not correct; fix it!
  type Reflector = [(Char,Char)] -- the supplied type is not correct; fix it!
  type Offsets = (Int,Int,Int) -- the supplied type is not correct; fix it!
  type Stecker = [(Char,Char)] -- the supplied type is not correct; fix it!
  
  data Enigma = SimpleEnigma Rotor Rotor Rotor Reflector Offsets
                | SteckeredEnigma Rotor Rotor Rotor Reflector Offsets Stecker

  {- encodeMessage : given a string and an enigma , encodes the string .
     encodeMessage "AlicE,,,123" (SimpleEnigma rotor3 rotor2 rotor1 reflectorB (0,0,25)) = NIQVD -}
  encodeMessage :: String -> Enigma -> String
  encodeMessage [] _ = []
  encodeMessage str enigma = encodeMessageSan (strip str) enigma

  {- encodeMessageSan: given given a sanitised string and an enigma ,
     encodes the string . Offsets are incremented after each character is encoded.
     Has 2 bindings, one for SimpleEnigma and one for SteckeredEnigma.
     encodeMessageSan "ALICE" (SimpleEnigma rotor3 rotor2 rotor1 reflectorB (0,0,25)) = NIQVD -}
  encodeMessageSan :: String -> Enigma -> String
  encodeMessageSan [] _ = [] 
  encodeMessageSan (x:xs) (SimpleEnigma rtrL rtrM rtrR ref (offL,offM,offR)) =
          (encodeChar x (rtrL,rtrM,rtrR) (oL,oM,oR) ref) : encodeMessageSan xs enigma
           where 
                oR = incOffset(offR)
                oM = if (oR == (snd rtrR)) then incOffset(offM) else (offM)
                oL = if ((oM == (snd rtrM)) && (oR == (snd rtrR))) then incOffset(offL) else (offL)
                enigma = (SimpleEnigma rtrL rtrM rtrR ref (oL,oM,oR))
  encodeMessageSan (x:xs) (SteckeredEnigma rtrL rtrM rtrR ref (offL,offM,offR) stckr) =
          (encodeCharSteckered x (rtrL,rtrM,rtrR) (oL,oM,oR) ref stckr) : encodeMessageSan xs enigma
           where 
                oR = incOffset(offR)
                oM = if (oR == (snd rtrR)) then incOffset(offM) else (offM)
                oL = if ((oM == (snd rtrM)) && (oR == (snd rtrR))) then incOffset(offL) else (offL)
                enigma = (SteckeredEnigma rtrL rtrM rtrR ref (oL,oM,oR) stckr)


  {- encodeChar : given rotors, offsets , reflector and a Char, simulates 
     passing the char fully through an unsteckered enigma machine 
     encodeChar 'A' (rotor3,rotor2,rotor1) (0,0,0) reflectorB = 'N' -}
  encodeChar :: Char -> (Rotor,Rotor,Rotor) -> Offsets -> Reflector -> Char 
  encodeChar x (rtrL,rtrM,rtrR) (offL,offM,offR) ref = 
          (shiftCharRev rtrs offs (switch ref (shiftChar rtrs offs x)))
                  where 
                    rtrs = (rtrL,rtrM,rtrR)
                    offs = (offL,offM,offR)
  
  {- encodeCharSteckered : given rotors, offsets , reflector and a Char, simulates 
     passing the char fully through a steckered enigma machine 
     encodeCharSteckered 'A' (rotor3,rotor2,rotor1) (0,0,0)  reflectorB plugboard = 'F' -}
  encodeCharSteckered :: Char -> (Rotor,Rotor,Rotor) -> Offsets -> Reflector -> Stecker -> Char
  encodeCharSteckered x (rtrL,rtrM,rtrR) (offL,offM,offR) ref stckr = 
          (switch stckr (shiftCharRev rtrs offs (switch ref (shiftChar rtrs offs (switch stckr x)))))
                  where 
                    rtrs = (rtrL,rtrM,rtrR)
                    offs = (offL,offM,offR)
  
  {- shiftChar: given rotors, offsets and a char , passes char forwards through rotors
     with the given offset applied.
     shiftChar (rotor3,rotor2,rotor1) (0,0,0) 'A' = 'G' -}
  shiftChar :: (Rotor,Rotor,Rotor) -> Offsets -> Char -> Char
  shiftChar (rtrL,rtrM,rtrR) (offL,offM,offR) x = 
          (shiftRotor rtrL offL (shiftRotor rtrM offM (shiftRotor rtrR offR x)))
  
  {- shiftCharRev: given rotors, offsets and a char , passes char backwards through rotors
     with the given offset applied.
     shiftCharRev (rotor3,rotor2,rotor1) (0,0,0) 'L' = 'N' -}
  shiftCharRev :: (Rotor,Rotor,Rotor) -> Offsets -> Char -> Char
  shiftCharRev (rtrL,rtrM,rtrR) (offL,offM,offR) x = 
          (shiftRotorRev rtrR offR (shiftRotorRev rtrM offM (shiftRotorRev rtrL offL x)))
  

  {- shiftRotor : given a rotor, an offset and a character, returns the 
     character that it is mapped to in the rotor with the offset applied.
     shiftRotor rotor1 0 'A' = 'E'
   -}
  shiftRotor :: Rotor -> Int -> Char -> Char 
  shiftRotor rtr offst x = decLetter newLetterRotor offst
          where 
             newLetterRotor = rotorString !! (((alphaPos x) + offst) `mod` 26)
             rotorString = fst rtr 

  {- shiftRotorRev : given a rotor, an offset and a character, returns the 
     character that it is reverse mapped to in the rotor with the offset applied.
     Opposite of shiftRotor.
     shiftRotorRev rotor1 0 'E' = 'A'
   -}
  shiftRotorRev :: Rotor -> Int -> Char -> Char 
  shiftRotorRev rtr offst x = int2let((pos-offst) `mod` 26)
          where 
            pos = elemIndex' f rotorString
            f = advanceLetter x offst
            rotorString = fst rtr  
  
  {- switch : given a reflector/plugboard and a character, returns the 
     character that it is mapped to in the reflector. If the character is not
    in the reflector, it is returned unchanged. 
    switch reflectorB 'A' = 'Y'
   -}
  switch :: Reflector -> Char -> Char
  switch [] n = toUpper n  
  switch (x:xs) n | ((toUpper n) == (fst x)) = snd x
                  | ((toUpper n) == (snd x)) = fst x
                  | otherwise = switch xs n 

{- Part 2: Finding the Longest Menu -}

  type Menu = [Int] 
  type Crib = [(Char,Char)] 
   
  {- longestMenu : given a crib, returns the longest menu in the crib
     longestMenu cribTest =    ---cribTest defined below 
      [[13,14,19,22,1,0,5,21,12,7,4,3,6,8,18,16,9],
      [13,14,19,22,1,0,8,12,7,4,3,6,5,21,18,16,9],
      [13,14,19,22,4,3,6,5,21,12,7,1,0,8,18,16,9],
      [13,14,19,22,4,3,6,8,12,7,1,0,5,21,18,16,9]] -}
  longestMenu :: Crib -> [Menu]
  longestMenu crb = getLongestMenu(getMenuForEach 0 crb)

  {- getLongestMenu : given a list of menus, returns the longest menus in the list
     getLongestMenu [[1,2,3],[1,2],[1,2,3,4],[2,3,4,1]] = [[1,2,3,4],[2,3,4,1]] -}
  getLongestMenu :: [Menu] -> [Menu] 
  getLongestMenu [] = [] 
  getLongestMenu menus = filter (\x -> length x == maxLen) menus -- filters out menus that are not of the longets length 
              where 
                   maxLen = length (head (mergesort cmpr menus)) -- gets length of longest menu

  {- getMenuForEach : given an index and a crib, returns menu for each index, starting 
     from given index until end of list.
     Tested with crib provided in main. Example too long to write here -}
  getMenuForEach :: Int -> Crib -> [Menu]
  getMenuForEach ind crb | (ind == length crb) = [] 
                         | otherwise = getMenus [ind] [] crb ++ getMenuForEach (ind + 1) crb
  
  {- getMenus : given a list of indexes, a menu and a crib, returns a list of menus for 
     given indexes. menu used for recursively adding on to the menu. 
     getMenus [1] [] cribTest -}
  getMenus :: [Int] -> Menu -> Crib -> [Menu] 
  getMenus [] menu crb = [menu]
  -- below checks if the index is already in the menu. Prevents infinite recursion. 
  getMenus [x] menu crb = if (elem x menu) then [menu] else getMenus (getOccuranceIndPlain (getCipherChar x crb) crb) (menu ++ [x]) crb
  getMenus (x:xs) menu crb | (elem x menu) = getMenus xs menu crb
                           | otherwise = getMenus (getOccuranceIndPlain (getCipherChar x crb) crb) (menu ++ [x]) crb ++ getMenus xs menu crb 
  
  {- getOccuranceIndPlain : given a cipher character and a crib, returns a
    list of the indexes in the plain where that character occurs.
    getOccuranceIndPlain 'T' cribTest = [2,3]  ---- cribTest is defined below 
  -}
  getOccuranceIndPlain :: Char -> Crib -> [Int]
  getOccuranceIndPlain _ [] = []
  getOccuranceIndPlain chara (x:xs) | (chara == (fst x)) = 0 : (map (+1) (getOccuranceIndPlain chara xs))
                              | otherwise = map (+1) (getOccuranceIndPlain chara xs)

  
  {- getCipherChar : given an index and a crib, returns the cipher character at that index.
     If the index is greater than the length of the crib, it returns a space.
     getCipherChar 4 cribTest = 'T'  ---- cribTest is defined below 
   -}
  getCipherChar :: Int -> Crib -> Char 
  getCipherChar ind crib | ind > (length crib)-1 = ' '
                         | otherwise = snd (crib !! ind)


  {- mergesort : given a list of lists, returns a list of lists sorted by length, descendingly .
     uses helper functions merge and cmpr to sort. 
     mergesort cmpr [[1,2],[4,5,6],[0]] = [[4,5,6],[1,2],[0]] -}
  mergesort :: (Ord a, Show a) => (a->a->Bool) -> [a] -> [a]  
  mergesort cmp [] = [] 
  mergesort cmp [x] = [x]  
  mergesort cmp xs = merge cmp (mergesort cmp ys) (mergesort cmp zs)
      where
          (ys, zs) = (take n xs, drop n xs)
          n = length xs `div` 2

  {- merge : helper function for mergesort. 
     Code provided by Emma Norling in week 3 lab . 
      -}
  merge :: (Ord a, Show a) => (a->a->Bool) -> [a] -> [a] -> [a]
  merge cmp [] ys = ys 
  merge cmp xs [] = xs
  merge cmp (x:xs) (y:ys) 
      | cmp x y = x : merge cmp xs (y:ys) 
      | otherwise = y : merge cmp (x:xs) ys

  {- cmpr : helper function for mergesort. Code provided by Emma Norling in week 3 lab 
     Edited to compare the lengths of the lists and sort descendingly. 
     cmpr [1,3] [4,5,6] = False 
     cmpr [1,2,3,4] [3,4] = True -}
  cmpr :: Ord a => [a] -> [a] -> Bool 
  cmpr x y | (length y) < (length x) = True 
           | otherwise = False

  

{- Part 3: Simulating the Bombe -}
  
  breakEnigma :: Crib -> Maybe (Offsets, Stecker)
  breakEnigma _ = Nothing

  

{- Useful definitions and functions -}

   -- substitution cyphers for the Enigma rotors
   -- as pairs of (wirings, knock-on position)
   -- knock-on position is where it will cause the next left wheel to
   -- advance when it moves past this position
 
        --"ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  rotor1=("EKMFLGDQVZNTOWYHXUSPAIBRCJ",17::Int)
  rotor2=("AJDKSIRUXBLHWTMCQGZNPYFVOE",5::Int)
  rotor3=("BDFHJLCPRTXVZNYEIWGAKMUSQO",22::Int)
  rotor4=("ESOVPZJAYQUIRHXLNFTGKDCMWB",10::Int)
  rotor5=("VZBRGITYUPSDNHLXAWMJQOFECK",0::Int)

  {- the standard Enigma reflector (Reflector B)
    swapped A<->Y, B<->R, C<->U,D<->H, E<->Q, F<->S, G<->L, 
            I<->P, J<->X, K<->N, M<->O, T<->Z,V<->W
  -}
  reflectorB= [('A','Y'),
              ('B','R'),
              ('C','U'),
              ('D','H'),
              ('E','Q'),
              ('F','S'),
              ('G','L'),
              ('I','P'),
              ('J','X'),
              ('K','N'),
              ('M','O'),
              ('T','Z'),
              ('V','W')]


  --plugboard = [('F','T'),('D','U'),('V','A'),('K','W'),('H','Z'),('I','X')] 

  cribTest1 = "WETTERVORHERSAGEBISKAYA"
  messageTest1 = "RWIVTYRESXBFOGKUHQBAISE"
  cribTest = zip cribTest1 messageTest1

  smallCrib = [('W','R'),('R','Y'),('R','S'),('H','X'), ('S','B'),('B','H')]

  {- alphaPos: given an uppercase letter, returns its index in the alphabet
     ('A' = position 0; 'Z' = position 25)
   -}
  alphaPos :: Char -> Int
  alphaPos c = (ord c) - ord 'A'

  {- int2let : given an integer, returns the corresponding uppercase letter
     (0 = 'A'; 25 = 'Z')
   -}
  int2let :: Int -> Char 
  int2let n = chr (ord 'A' + n)

  {- decLetter : given a letter and an integer, returns the letter that is
     the given number of places to the left of the given letter in the
     alphabet. 
     decLetter 'A' 1 = 'Z'
   -}
  decLetter :: Char -> Int -> Char 
  decLetter chr x = int2let (((alphaPos chr) - x) `mod` 26)

  {- advanceLetter : given a letter and an integer, returns the letter that is
     the given number of places to the right of the given letter in the
     alphabet. 
     advanceLetter 'Z' 1 = 'A' 
  -}
  advanceLetter :: Char -> Int -> Char 
  advanceLetter chr x = int2let (((alphaPos chr) + x) `mod` 26)
  
  {- incOffset : given an int, increases offset by 1 , but wraps around at 26
     incOffset 25 = 0 -}
  incOffset :: Int -> Int 
  incOffset pos = (pos + 1) `mod` 26

  {- strip: given a string, returns a sanitised string with only uppercase
     letters. 
     strip "Hello, World!" = "HELLOWORLD" -}
  strip :: [Char] -> [Char]
  strip [] = []
  strip (x:xs) 
         | isLetter x = (toUpper x): strip xs 
         | otherwise = strip xs

  {- elemIndex' : given an element and a list, returns the index of the first
      occurence of the element in the list. Turns the Maybe Int into an Int.
      Used in contexts where character is guaranteed to be in the list. 
      elemIndex' 'A' "HELLO" = 0 -}
  elemIndex' :: Char -> [Char] -> Int
  elemIndex' x  str = fromMaybe 0 (elemIndex (toUpper x) str)
