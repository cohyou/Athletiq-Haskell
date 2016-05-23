data Briq = Int8 Integer | Text String | Smbl String

data Annot = Annot String

newtype AnnotList = AnnotList { getList :: [Annot] }

data Aexp = Aexp AnnotList Briq

instance Show Briq where
    show (Int8 i) = show i
    show (Text s) = "\"" ++ s ++ "\""
    show (Smbl s) = s

instance Show Annot where
    show (Annot s) = "@" ++ s

instance Show AnnotList where
    show (AnnotList []) = ""
    show (AnnotList (x:xs)) = show x ++ " " ++ show (AnnotList xs)

instance Show Aexp where
    show (Aexp annot briq) = show annot ++ show briq


main :: IO ()
main = do
    print $ AnnotList [(Annot "print")]
    print $ Aexp (AnnotList [(Annot "print"), (Annot "kick")]) (Text "wowow")
