data Briq = Int8 Integer | Text String | Smbl String | List [Briq]

data Annt = Annt String

newtype AnntList = AnntList { getList :: [Annt] }

data Aexp = Aexp AnntList Briq

instance Show Briq where
    show (Int8 i) = show i
    show (Text s) = "\"" ++ s ++ "\""
    show (Smbl s) = s
    show (List []) = ";"
    show (List (x:xs)) = "[" ++ show x ++ showG xs ++ "]"

showG [] = ""
showG (x:xs) = " " ++ show x ++ showG xs

instance Show Annt where
    show (Annt s) = "@" ++ s

instance Show AnntList where
    show (AnntList []) = ""
    show (AnntList (x:xs)) = show x ++ " " ++ show (AnntList xs)

instance Show Aexp where
    show (Aexp annot briq) = show annot ++ show briq


main :: IO ()
main = do
    print $ AnntList [(Annt "print")]
    print $ Aexp (AnntList [(Annt "print"), (Annt "kick")]) (Text "wowow")
    print $ Aexp (AnntList []) (List [])
    print $ Aexp (AnntList []) (List [(Text "wowow"), (Smbl "make"), (Int8 53545)])
    print $ Aexp (AnntList [(Annt "print")]) (List [(Text "wowow"), (Smbl "make"), (Int8 53545)])
