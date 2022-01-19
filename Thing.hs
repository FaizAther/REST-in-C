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

newtype Url = Url String
            deriving (Show, Eq)

jonifyIStr :: String -> String
jonifyIStr xs = preJItem ++ xs ++ postJItem

jonifyIsStr :: [String] -> String
jonifyIsStr xs = preJList ++ jonifyIsStrH xs ++ postJList
    where
        jonifyIsStrH [] = ""
        jonifyIsStrH [x] = jonifyIStr x
        jonifyIsStrH (x:xs) = jonifyIStr x ++ "," ++ jonifyIsStrH xs

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
    jonify (Words ws) = jonifyNpStr "Words" ++ jonifyIsStr ws ++ postJName
    jonify (Picture url) = jonifyNpStr "Picture" ++ jonify url ++ postJName
    jonify (Video url) = jonifyNpStr "Video" ++ jonify url ++ postJName
    jonify (Audio url) = jonifyNpStr "Audio" ++ jonify url ++ postJName

data Duration   = Done
                | Seconds Int Duration
                | Minutes Int Duration
                | Hours Int Duration
                | Days Int Duration
                deriving (Show, Eq)

newtype Height  = Height Int
                deriving (Show, Eq)

newtype Width   = Width Int
                deriving (Show, Eq)


newtype Position    = Position (Height, Width)
                    deriving (Show, Eq)


newtype Element = Element (Position, Duration, Content)
                deriving (Show, Eq)


data Canvas     = One [Element]
                | Many [Canvas]
                deriving (Show, Eq)


-- canvasStr :: IO CString
-- canvasStr = (newCString . show) things

-- foreign export ccall canvasStr :: IO CString

toCanvas :: [String] -> Canvas
toCanvas xs = One (map (\s -> Element (Position (Height 0, Width 0), Done, Words s)) [xs])

things :: Canvas
things = toCanvas ["Hello", "This", "Is", "A", "Test"]
