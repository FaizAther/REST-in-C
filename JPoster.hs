-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE ForeignFunctionInterface #-}

module JPoster where

import JParser
  ( JParse (runJParser),
    jonVal,
  )
import JonVal (JonVal (JonList, JonLit, JonMap, JonNum))

-- import Foreign.C ( CString, newCString )

class Jonify a where
  jonify :: a -> String

preJName :: String
preJName = "{\""

preJName' :: String
preJName' = "{"

midJName' :: String
midJName' = ":"

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

commaJ :: String
commaJ = ","

colonJ :: String
colonJ = ":"

emptyJ :: String
emptyJ = ""

newtype Url = Url String
  deriving (Show, Eq)

validUrl :: String -> Maybe Url
validUrl input = do
  (leftover, obj) <- runJParser jonVal input
  case obj of
    JonVal.JonMap [(key, value)] ->
      if key == "Url"
        then case value of
          JonVal.JonLit s -> Just (Url s)
          _ -> Nothing
        else Nothing
    _ -> Nothing

jonifyIStr :: Jonify a => a -> String
jonifyIStr xs = preJItem ++ jonify xs ++ postJItem

jonifyIStr' :: [Char] -> [Char]
jonifyIStr' xs = preJItem ++ xs ++ postJItem

jonifyIsStr :: Jonify a => [a] -> (a -> String) -> String
jonifyIsStr xs f = preJList ++ jonifyIsStrH xs ++ postJList
  where
    jonifyIsStrH [] = emptyJ
    jonifyIsStrH [x] = f x
    jonifyIsStrH (x : xs) = f x ++ commaJ ++ jonifyIsStrH xs

jonifyNpStr :: String -> String
jonifyNpStr name = preJName ++ name ++ midJName

jonifyNpStr' :: String -> String
jonifyNpStr' name = preJItem ++ name ++ midJName

nameJUrl :: String
nameJUrl = "Url"

instance Jonify Url where
  jonify (Url link) = jonifyNpStr nameJUrl ++ jonifyIStr' link ++ postJName
  -- jonify = show 

data Content
  = Nil
  | Words [String]
  | Picture Url
  | Video Url
  | Audio Url
  | Link Url
  deriving (Show, Eq)

validContent :: String -> Maybe Content
validContent input = do
  (left, obj) <- runJParser jonVal input
  case obj of
    JonVal.JonMap [("Content", value)] ->
      case value of
        JonVal.JonLit "Nil" -> Just Nil
        _ -> Nothing
    JonVal.JonMap [("Words", JonVal.JonList js)] -> Just $ Words $ f js
    JonVal.JonMap [("Video", JonVal.JonMap [("Url", JonVal.JonLit link)])] -> Just $ Video $ Url link
    JonVal.JonMap [("Picture", JonVal.JonMap [("Url", JonVal.JonLit link)])] -> Just $ Picture $ Url link
    JonVal.JonMap [("Audio", JonVal.JonMap [("Url", JonVal.JonLit link)])] -> Just $ Audio $ Url link
    JonVal.JonMap [("Link", JonVal.JonLit link)] -> Just $ Link $ Url link
    _ -> Nothing
  where
    f :: [JonVal.JonVal] -> [String]
    f (JonVal.JonLit s : js) = s : f js
    f (_ : js) = f js
    f _ = []

nameJContent :: String
nameJContent = "Content"

nameJNth :: String
nameJNth = "Nil"

nameJWds :: String
nameJWds = "Words"

nameJPic :: String
nameJPic = "Picture"

nameJVid :: String
nameJVid = "Video"

nameJAud :: String
nameJAud = "Audio"

instance Jonify Content where
  -- jonify Nil = jonifyNpStr nameJContent ++ jonifyIStr nameJNth ++ postJName
  jonify Nil = show $ JonMap [("Content", JonLit "Nil")]
  -- jonify (Words ws) = jonifyNpStr nameJWds ++ jonifyIsStr ws jonifyIStr ++ postJName
  jonify (Words ws) = show $ JonMap $ [("Words", JonList (map JonLit ws))]
  jonify (Picture url) = jonifyNpStr nameJPic ++ jonify url ++ postJName
  -- jonify (Picture url) = show $ JonMap [("Picture", JonLit $ show url)]
  jonify (Video url) = jonifyNpStr nameJVid ++ jonify url ++ postJName
  -- jonify (Video url) = show $ JonMap [("Video", JonLit $ show url)]
  jonify (Audio url) = jonifyNpStr nameJAud ++ jonify url ++ postJName
  jonify (Link url) = jonify url

-- jonify (Audio url) = show $ JonMap [("Audio", JonLit $ show url)]

testCon :: String -> Maybe Content
testCon xs =
  let val = jonify <$> validContent xs
   in case val of
        Just xs -> validContent xs
        _ -> Nothing

testCon1 :: Maybe Content
testCon1 = testCon "{\"Content\":\"Nil\"}"

testCon2 :: Maybe Content
testCon2 = testCon "{\"Picture\":{\"Url\":\"aba\"}}"

testCon3 :: Maybe Content
testCon3 = testCon "{\"Words\":[\"aba\", \"kdaba\"]}"

data Duration
  = Done
  | Seconds Int
  | Minutes Int
  | Hours Int
  | Days Int
  deriving (Show, Eq)

validDuration :: String -> Maybe Duration
validDuration input = do
  (left, obj) <- runJParser jonVal input
  case obj of
    JonVal.JonMap [("Duration", value)] ->
      case value of
        JonVal.JonLit "Done" -> Just Done
        _ -> Nothing
    JonVal.JonMap [("Seconds", JonVal.JonNum n)] -> Just $ Seconds n
    JonVal.JonMap [("Minutes", JonVal.JonNum n)] -> Just $ Seconds n
    JonVal.JonMap [("Hours", JonVal.JonNum n)] -> Just $ Seconds n
    JonVal.JonMap [("Days", JonVal.JonNum n)] -> Just $ Seconds n
    _ -> Nothing

testDur0 :: Maybe Duration
testDur0 = validDuration "{\"Duration\":\"Done\"}"

testDur1 :: Maybe Duration
testDur1 = validDuration "{\"Seconds\":1}"

nameJDn :: String
nameJDn = "Done"

nameJZ :: String
nameJZ = show 0

nameJSec :: String
nameJSec = "Seconds"

nameJMin :: String
nameJMin = "Minutes"

nameJHrs :: String
nameJHrs = "Hours"

nameJDys :: String
nameJDys = "Days"

instance Jonify Duration where
  jonify Done = jonifyNpStr nameJDn ++ nameJZ ++ postJName
  jonify (Seconds val) = jonifyNpStr nameJSec ++ show val ++ postJName
  jonify (Minutes val) = jonifyNpStr nameJSec ++ show val ++ postJName
  jonify (Hours val) = jonifyNpStr nameJHrs ++ show val ++ postJName
  jonify (Days val) = jonifyNpStr nameJDys ++ show val ++ postJName

newtype Height = Height Int
  deriving (Show, Eq)

nameJHeight :: String
nameJHeight = "Height"

instance Jonify Height where
  jonify (Height val) =
    --jonifyNpStr nameJHeight ++
    jonifyIStr' nameJHeight ++ colonJ
      ++ show val

--Thing.++ postJName

newtype Width = Width Int
  deriving (Show, Eq)

nameJWidth :: String
nameJWidth = "Width"

instance Jonify Width where
  jonify (Width val) =
    --jonifyNpStr nameJWidth ++
    jonifyIStr' nameJWidth ++ colonJ
      ++ show val

--Thing.++ postJName

newtype Position = Position (Height, Width)
  deriving (Show, Eq)

nameJPos :: String
nameJPos = "Position"

instance Jonify Position where
  jonify (Position (h, w)) =
    --jonifyNpStr nameJPos ++ preJList ++
    jonifyIStr' nameJPos ++ midJName' ++ preJName'
      ++ jonify h
      ++ commaJ
      ++ jonify w
      ++ postJName

--Thing.++ postJName
--Thing.++ postJList
--Thing.++ postJName

newtype Element = Element (Position, Duration, Content)
  deriving (Show, Eq)

testElem0 :: Element
testElem0 = Element (Position (Height 0, Width 0), Seconds 1, Words ["hi"])

nameJElm :: String
nameJElm = "Element"

instance Jonify Element where
  jonify (Element (pos, dur, con)) =
    jonifyNpStr nameJElm --Thing.++ preJList
      ++ preJName'
      ++ jonify pos
      ++ commaJ
      ++ jonifyIStr' "Time"
      ++ midJName'
      ++ jonify dur
      ++ commaJ
      ++ jonifyIStr' "Content"
      ++ midJName'
      ++ jonify con
      --Thing.++ postJList
      ++ postJName
      ++ postJName

data Canvas
  = One [Element]
  | Many [Canvas]
  deriving (Show, Eq)

nameJOne :: String
nameJOne = "One"

nameJMny :: String
nameJMny = "Many"

instance Jonify Canvas where
  jonify (One es) = jonifyNpStr nameJOne ++ jonifyIsStr es jonify ++ postJName
  jonify (Many es) = jonifyNpStr nameJMny ++ jonifyIsStr es jonify ++ postJName

-- canvasStr :: IO CString
-- canvasStr = (newCString . unString . jonify) things1

-- foreign export ccall canvasStr :: IO CString

appendCanvas :: Element -> Canvas -> Canvas
appendCanvas elem (One es) = One (es ++ [elem])
appendCanvas _ rest = rest

toElemW :: [String] -> Element
toElemW s = Element (Position (Height 0, Width 0), Seconds 5, Words s)

toCanvasW :: [[String]] -> Canvas
toCanvasW xs = One (map toElemW xs)

manyCanvas :: [[String]] -> Canvas
manyCanvas xs = Many $ map toCanvasW [xs]

things1 :: Canvas
things1 = toCanvasW [
    ["Hello", "This", "Is", "A", "Test1"], ["Good", "bye", "end", "of", "Test1"],
    ["Hello", "This", "Is", "A", "Test2"], ["Good", "bye", "end", "of", "Test2"]
  ]

things2 :: Canvas
things2 = appendCanvas (Element (Position (Height 0, Width 0), Seconds 5, Video (Url "http://www.2022contrary.xyz/demo/assets/dummy.mp4"))) things1

things3 :: Canvas
things3 = appendCanvas (Element (Position (Height 0, Width 0), Seconds 5, Link (Url "assets/dummy.mp4"))) things2

things0 :: Canvas
things0 = manyCanvas [["Hello", "This", "Is", "A", "Test"], ["Another test"]]