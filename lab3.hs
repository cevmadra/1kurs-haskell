type People = String
type Phone = String
type Descr = String
type DayMonthYear = (Int,Int,Int) 
data Records = Remind People DayMonthYear | 
                PhoneBook People Phone |
                Meet Descr DayMonthYear deriving Show

getByName:: [Records] -> People -> (Maybe Phone, Maybe DayMonthYear )
getByName l p = (getB l, getR l)
    where getB [] = Nothing
getB ( PhoneBook p' n :xs) 
    | p' == p = Just n
    | otherwise = getB xs
getB (_:xs) = getB xs
getR [] = Nothing
getR ( Remind p' d :xs) 
    | p' == p = Just d
    | otherwise = getR xs
getR (_:xs) = getR xs

unical n l
    | elem n l = l
    | otherwise = n:l

getByLetter:: [Records] -> Char -> [People]
getByLetter [] _ = []
getByLetter ( PhoneBook [] _ :xs) c = getByLetter xs c
getByLetter ( PhoneBook p@(n:_) _ :xs) c 
    | n == c = unical p (getByLetter xs c)
    | otherwise = getByLetter xs c
getByLetter ( Remind [] _ :xs) c = getByLetter xs c
getByLetter ( Remind p@(n:_) _ :xs) c 
    | n == c = unical p (getByLetter xs c)
    | otherwise = getByLetter xs c
getByLetter (_:xs) c = getByLetter xs c

getAssignment:: [Records] -> DayMonthYear -> [Records] 
getAssignment [] _ = []
getAssignment (r@(Remind _ d'):xs) d 
    | d == d' = r : getAssignment xs d
    | otherwise = getAssignment xs d
getAssignment (r@(Meet _ d'):xs) d 
    | d == d' = r : getAssignment xs d
    | otherwise = getAssignment xs d
getAssignment (_:xs) d = getAssignment xs d

testData = [
    Remind "Putin V.V." (7,10,1952), 
    Remind "Obama B.H." (4, 8,1961),
    PhoneBook "Putin V.V." "8 (800) 200-23-16",
    PhoneBook "KolodeznyDiver" "8 (495) 952-88-33",
    Meet "Spaceport Vandenberg" (4, 8,1961)
    ]

main = do
print $ getByName testData "Putin V.V."
print $ getByName testData "Obama B.H."
print $ getByName testData "KolodeznyDiver"
print $ getByLetter testData 'P'
print $ getAssignment testData (4, 8,1961)


type Name = String
type Day = Integer
type Month = Integer
type Year = Integer
type Birthday = (Day, Month, Year)
type Phone = String
type Description = String

data Records = B1 Name  Birthday
             | P1 Name Phone
             | Meeting Date Description deriving (Eq, Show)

type NoteBook = [Records]

getPhoneByName :: Name -> NoteBook -> Maybe Phone
getPhoneByName _ [] = Noting 
getPhoneByName n (x:xs) = case x of
    Birthday nc bd -> if n == nc then Just bd
                                 else getVithdayByName n xs
                    -> getBirthdayByName n xs
                    
getByName  :: Name -> NoteBook ->  (Maybe Phone, Maybe Birthday)
getByName n nb = (getPhoneByName n nb, getBirthdayByName n nb)

{- 2) -}

getByLetter :: Char -> NoteBook -> [Name]
getByLetter _ [] = []
getByLetter c (x:xs) = case x of
    Birthday n _ -> if head n == c then n:getByLetter c xs
                                   else getByLetter c xs
    Phone n _ ->  if head n == c then n:getByLetter c xs
                                 else getByLetter c xs
             -> getByLetter c xs
             
getAssignment :: Date -> NoteBook -> ([Description], [Phone])
getAssignment d nb = 
    let getMeetingByDate :: Date -> Notebook -> [Description]
        getMeetingByDate - [] = []
        getMeetingByDate d (x:xs) = case x of 
            Meeting dc ds -> if d == dc then ds:getMeetingByDate d xs
                                        else getMeetingByDate d xs
                        -> getMeetingByDate d xs
                        
getPhoneByDate :: Date -> NoteBook -> NoteBook -> [Phone]
getPhoneByDate - [] = []
getPhoneByDate (d,m,y) (x:xs) nb = case x of
    Birthday n (dc,mc) -> if (d == dc) && (m == mc)
                            then case getPhoneByName n nb of
                                Just p -> (n ++ ':' :p) :getPhoneByDate (d,m ,y) xs nb
                                    -> )(n ++ " - no phone"): getPhoneByDate (d,m ,y) xs nb
                            else getPhoneByDate (d,m,y) xs nb
                  -> getPhoneByDate (d,m,y) xs nb
    in (getMeetingByDate d nb, getPhoneByDate d nb nb)
    
    
