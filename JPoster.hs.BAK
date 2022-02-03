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
  jonify :: a -> JonStr

preJName :: JonStr
preJName = JonStr "{\""

preJName' :: JonStr
preJName' = JonStr "{"

midJName' :: JonStr
midJName' = JonStr ":"

midJName :: JonStr
midJName = JonStr "\":"

postJName :: JonStr
postJName = JonStr "}"

preJItem :: JonStr
preJItem = JonStr "\""

postJItem :: JonStr
postJItem = JonStr "\""

preJList :: JonStr
preJList = JonStr "["

postJList :: JonStr
postJList = JonStr "]"

commaJ :: JonStr
commaJ = JonStr ","

colonJ :: JonStr
colonJ = JonStr ":"

emptyJ :: JonStr
emptyJ = JonStr ""

newtype JonStr = JonStr {unJonStr :: String}
  deriving (Eq)

instance Show JonStr where
  show = show . unJonStr

instance Jonify JonStr where
  jonify xs = xs

(++) :: JonStr -> JonStr -> JonStr
(++) xs ys = JonStr $ unJonStr xs Prelude.++ unJonStr ys

newtype Url = Url JonStr
  deriving (Show, Eq)

validUrl :: String -> Maybe Url
validUrl input = do
  (leftover, obj) <- runJParser jonVal input
  case obj of
    JonVal.JonMap [(key, value)] ->
      if key == "Url"
        then case value of
          JonVal.JonLit s -> Just (Url (JonStr s))
          _ -> Nothing
        else Nothing
    _ -> Nothing

jonifyIStr :: Jonify a => a -> JonStr
jonifyIStr xs = preJItem JPoster.++ jonify xs JPoster.++ postJItem

jonifyIsStr :: Jonify a => [a] -> (a -> JonStr) -> JonStr
jonifyIsStr xs f = preJList JPoster.++ jonifyIsStrH xs JPoster.++ postJList
  where
    jonifyIsStrH [] = emptyJ
    jonifyIsStrH [x] = f x
    jonifyIsStrH (x : xs) = f x JPoster.++ commaJ JPoster.++ jonifyIsStrH xs

jonifyNpStr :: JonStr -> JonStr
jonifyNpStr name = preJName JPoster.++ name JPoster.++ midJName

jonifyNpStr' :: JonStr -> JonStr
jonifyNpStr' name = preJItem JPoster.++ name JPoster.++ midJName

nameJUrl :: JonStr
nameJUrl = JonStr "Url"

instance Jonify Url where
  -- jonify (Url link) = jonifyNpStr nameJUrl JPoster.++ jonifyIStr link JPoster.++ postJName
  jonify (Url link) = JonStr $ show $ JonMap [("Url", JonLit (unJonStr link))]

data Content
  = Nil
  | Words [JonStr]
  | Picture Url
  | Video Url
  | Audio Url
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
    JonVal.JonMap [("Video", JonVal.JonMap [("Url", JonVal.JonLit link)])] -> Just $ Video $ Url $ JonStr link
    JonVal.JonMap [("Picture", JonVal.JonMap [("Url", JonVal.JonLit link)])] -> Just $ Picture $ Url $ JonStr link
    JonVal.JonMap [("Audio", JonVal.JonMap [("Url", JonVal.JonLit link)])] -> Just $ Audio $ Url $ JonStr link
    _ -> Nothing
  where
    f :: [JonVal.JonVal] -> [JonStr]
    f (JonVal.JonLit s : js) = JonStr s : f js
    f (_ : js) = f js
    f _ = []

nameJContent :: JonStr
nameJContent = JonStr "Content"

nameJNth :: JonStr
nameJNth = JonStr "Nil"

nameJWds :: JonStr
nameJWds = JonStr "Words"

nameJPic :: JonStr
nameJPic = JonStr "Picture"

nameJVid :: JonStr
nameJVid = JonStr "Video"

nameJAud :: JonStr
nameJAud = JonStr "Audio"

instance Jonify Content where
  -- jonify Nil = jonifyNpStr nameJContent JPoster.++ jonifyIStr nameJNth JPoster.++ postJName
  jonify Nil = JonStr $ show $ JonMap [("Content", JonLit "Nil")]
  jonify (Words ws) = jonifyNpStr nameJWds JPoster.++ jonifyIsStr ws jonifyIStr JPoster.++ postJName
  -- jonify (Words ws) = JonStr $ show $ JonList (map (JonLit . unJonStr) ws)
  jonify (Picture url) = jonifyNpStr nameJPic JPoster.++ jonify url JPoster.++ postJName
  -- jonify (Picture url) = JonStr $ show $ JonMap [("Picture", JonLit $ show url)]
  jonify (Video url) = jonifyNpStr nameJVid JPoster.++ jonify url JPoster.++ postJName
  -- jonify (Video url) = JonStr $ show $ JonMap [("Video", JonLit $ show url)]
  jonify (Audio url) = jonifyNpStr nameJAud JPoster.++ jonify url JPoster.++ postJName
  -- jonify (Audio url) = JonStr $ show $ JonMap [("Audio", JonLit $ show url)]

testCon :: String -> Maybe Content
testCon xs =
  let val = jonify <$> validContent xs
   in case val of
        Just (JonStr xs) -> validContent xs
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

nameJDn :: JonStr
nameJDn = JonStr "Done"

nameJZ :: JonStr
nameJZ = JonStr $ show 0

nameJSec :: JonStr
nameJSec = JonStr "Seconds"

nameJMin :: JonStr
nameJMin = JonStr "Minutes"

nameJHrs :: JonStr
nameJHrs = JonStr "Hours"

nameJDys :: JonStr
nameJDys = JonStr "Days"

instance Jonify Duration where
  jonify Done = jonifyNpStr nameJDn JPoster.++ nameJZ JPoster.++ postJName
  jonify (Seconds val) = jonifyNpStr nameJSec JPoster.++ (JonStr . show) val JPoster.++ postJName
  jonify (Minutes val) = jonifyNpStr nameJSec JPoster.++ (JonStr . show) val JPoster.++ postJName
  jonify (Hours val) = jonifyNpStr nameJHrs JPoster.++ (JonStr . show) val JPoster.++ postJName
  jonify (Days val) = jonifyNpStr nameJDys JPoster.++ (JonStr . show) val JPoster.++ postJName

newtype Height = Height Int
  deriving (Show, Eq)

nameJHeight :: JonStr
nameJHeight = JonStr "Height"

instance Jonify Height where
  jonify (Height val) = --jonifyNpStr nameJHeight JPoster.++
    jonifyIStr nameJHeight JPoster.++ colonJ JPoster.++
    (JonStr . show) val
    --Thing.++ postJName

newtype Width = Width Int
  deriving (Show, Eq)

nameJWidth :: JonStr
nameJWidth = JonStr "Width"

instance Jonify Width where
  jonify (Width val) = --jonifyNpStr nameJWidth JPoster.++ 
    jonifyIStr nameJWidth JPoster.++ colonJ JPoster.++
    (JonStr . show) val
    --Thing.++ postJName

newtype Position = Position (Height, Width)
  deriving (Show, Eq)

nameJPos :: JonStr
nameJPos = JonStr "Position"

instance Jonify Position where
  jonify (Position (h, w)) =
    --jonifyNpStr nameJPos JPoster.++ preJList JPoster.++ 
      jonifyIStr nameJPos JPoster.++ midJName' JPoster.++ preJName'
      JPoster.++ jonify h
      JPoster.++ commaJ
      JPoster.++ jonify w
      JPoster.++ postJName
      --Thing.++ postJName
      --Thing.++ postJList
      --Thing.++ postJName

newtype Element = Element (Position, Duration, Content)
  deriving (Show, Eq)

testElem0 :: Element
testElem0 = Element (Position (Height 0, Width 0), Seconds 1, Words [JonStr "hi"])

nameJElm :: JonStr
nameJElm = JonStr "Element"

instance Jonify Element where
  jonify (Element (pos, dur, con)) =
    jonifyNpStr nameJElm --Thing.++ preJList
      JPoster.++ preJName'
      JPoster.++ jonify pos
      JPoster.++ commaJ
      JPoster.++ jonifyIStr (JonStr "Time") JPoster.++ midJName' JPoster.++ jonify dur
      JPoster.++ commaJ
      JPoster.++ jonifyIStr (JonStr "Content") JPoster.++ midJName' JPoster.++ jonify con
      --Thing.++ postJList
      JPoster.++ postJName
      JPoster.++ postJName

data Canvas
  = One [Element]
  | Many [Canvas]
  deriving (Show, Eq)

nameJOne :: JonStr
nameJOne = JonStr "One"

nameJMny :: JonStr
nameJMny = JonStr "Many"

instance Jonify Canvas where
  jonify (One es) = jonifyNpStr nameJOne JPoster.++ jonifyIsStr es jonify JPoster.++ postJName
  jonify (Many es) = jonifyNpStr nameJMny JPoster.++ jonifyIsStr es jonify JPoster.++ postJName

-- canvasStr :: IO CString
-- canvasStr = (newCString . unJonStr . jonify) things1

-- foreign export ccall canvasStr :: IO CString

toElem :: [JonStr] -> Element
toElem s = Element (Position (Height 0, Width 0), Seconds 5, Words s)

toCanvas :: [[JonStr]] -> Canvas
toCanvas xs = One (map toElem xs)

manyCanvas :: [[JonStr]] -> Canvas
manyCanvas xs = Many $ map toCanvas [xs]

things1 :: Canvas
things1 = toCanvas $ (JonStr <$>) <$> [["Hello", "This", "Is", "A", "Test"], ["Another", "Test"]]

things0 :: Canvas
things0 = manyCanvas [fmap JonStr ["Hello", "This", "Is", "A", "Test"], fmap JonStr ["Another test"]]