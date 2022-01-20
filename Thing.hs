{-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE ForeignFunctionInterface #-}

module Thing where

-- import Foreign.C ( CString, newCString )

class Jonify a where
    jonify :: a -> String

preJName :: String
preJName = "{\""

midJName :: String
midJName = "\":"

postJName :: String
postJName = "}"

preJItem :: String
preJItem = "\""

postJItem :: String
postJItem = "\""

preJList :: String
preJList = "["

postJList :: String
postJList = "]"

instance Jonify String where
    jonify xs = xs

newtype Url = Url String
            deriving (Show, Eq)

jonifyIStr :: Jonify a => a -> String
jonifyIStr xs = preJItem ++ jonify xs ++ postJItem

jonifyIsStr :: Jonify a => [a] -> (a -> String) -> String
jonifyIsStr xs f = preJList ++ jonifyIsStrH xs ++ postJList
    where
        jonifyIsStrH [] = ""
        jonifyIsStrH [x] = f x
        jonifyIsStrH (x:xs) = f x ++ "," ++ jonifyIsStrH xs

jonifyNpStr :: String -> String
jonifyNpStr name = preJName ++ name ++ midJName

instance Jonify Url where
    jonify (Url link) = jonifyNpStr "Url" ++ jonifyIStr link ++ postJName

data Content    = Nothingg
                | Words [String]
                | Picture Url
                | Video Url
                | Audio Url
                deriving (Show, Eq)

instance Jonify Content where
    jonify Nothingg = jonifyNpStr "Nothing" ++ postJName
    jonify (Words ws) = jonifyNpStr "Words" ++ jonifyIsStr ws jonifyIStr ++ postJName
    jonify (Picture url) = jonifyNpStr "Picture" ++ jonify url ++ postJName
    jonify (Video url) = jonifyNpStr "Video" ++ jonify url ++ postJName
    jonify (Audio url) = jonifyNpStr "Audio" ++ jonify url ++ postJName

data Duration   = Done
                | Seconds Int
                | Minutes Int
                | Hours Int
                | Days Int
                deriving (Show, Eq)

instance Jonify Duration where
    jonify Done = jonifyNpStr "Done" ++ show 0 ++ postJName
    jonify (Seconds val) = jonifyNpStr "Seconds" ++ jonifyIStr (show val) ++ postJName
    jonify (Minutes val) = jonifyNpStr "Minutes" ++ jonifyIStr (show val) ++ postJName
    jonify (Hours val) = jonifyNpStr "Hours" ++ jonifyIStr (show val) ++ postJName
    jonify (Days val) = jonifyNpStr "Days" ++ jonifyIStr (show val) ++ postJName

newtype Height  = Height Int
                deriving (Show, Eq)

instance Jonify Height where
    jonify (Height val) = jonifyNpStr "Height" ++ jonifyIStr (show val) ++ postJName

newtype Width   = Width Int
                deriving (Show, Eq)

instance Jonify Width where
    jonify (Width val) = jonifyNpStr "Width" ++ jonifyIStr (show val) ++ postJName

newtype Position    = Position (Height, Width)
                    deriving (Show, Eq)

instance Jonify Position where
    jonify (Position (h,w)) =
        jonifyNpStr "Position" ++ preJList ++
        jonify h ++ "," ++ jonify w ++
        postJList ++ postJName


newtype Element =   Element (Position, Duration, Content)
                    deriving (Show, Eq)

instance Jonify Element where
    jonify (Element (pos, dur, con)) =
        jonifyNpStr "Element" ++ preJList ++
        jonify pos ++ "," ++ jonify dur ++ "," ++ jonify con ++
        postJList ++ postJName

data Canvas     = One [Element]
                | Many [Canvas]
                deriving (Show, Eq)

instance Jonify Canvas where
    jonify (One es)     = jonifyNpStr "One" ++ jonifyIsStr es jonify ++ postJName
    jonify (Many es)    = jonifyNpStr "Many" ++ jonifyIsStr es jonify ++ postJName

-- canvasStr :: IO CString
-- canvasStr = (newCString . show) things

-- foreign export ccall canvasStr :: IO CString

toCanvas :: [String] -> Canvas
toCanvas xs = One (map (\s -> Element (Position (Height 0, Width 0), Seconds 5, Words s)) [xs])

manyCanvas :: [[String]] -> Canvas
manyCanvas xs = Many $ map toCanvas xs

things :: Canvas
things = manyCanvas [["Hello", "This", "Is", "A", "Test"], ["Another test"]]
