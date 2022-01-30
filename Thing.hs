-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE ForeignFunctionInterface #-}

module Thing where

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
jonifyIStr xs = preJItem Thing.++ jonify xs Thing.++ postJItem

jonifyIsStr :: Jonify a => [a] -> (a -> JonStr) -> JonStr
jonifyIsStr xs f = preJList Thing.++ jonifyIsStrH xs Thing.++ postJList
  where
    jonifyIsStrH [] = emptyJ
    jonifyIsStrH [x] = f x
    jonifyIsStrH (x : xs) = f x Thing.++ commaJ Thing.++ jonifyIsStrH xs

jonifyNpStr :: JonStr -> JonStr
jonifyNpStr name = preJName Thing.++ name Thing.++ midJName

nameJUrl :: JonStr
nameJUrl = JonStr "Url"

instance Jonify Url where
  jonify (Url link) = jonifyNpStr nameJUrl Thing.++ jonifyIStr link Thing.++ postJName

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
  jonify Nil = jonifyNpStr nameJContent Thing.++ jonifyIStr nameJNth Thing.++ postJName
  jonify (Words ws) = jonifyNpStr nameJWds Thing.++ jonifyIsStr ws jonifyIStr Thing.++ postJName
  jonify (Picture url) = jonifyNpStr nameJPic Thing.++ jonify url Thing.++ postJName
  jonify (Video url) = jonifyNpStr nameJVid Thing.++ jonify url Thing.++ postJName
  jonify (Audio url) = jonifyNpStr nameJAud Thing.++ jonify url Thing.++ postJName

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
  jonify Done = jonifyNpStr nameJDn Thing.++ nameJZ Thing.++ postJName
  jonify (Seconds val) = jonifyNpStr nameJSec Thing.++ jonifyIStr ((JonStr . show) val) Thing.++ postJName
  jonify (Minutes val) = jonifyNpStr nameJSec Thing.++ jonifyIStr ((JonStr . show) val) Thing.++ postJName
  jonify (Hours val) = jonifyNpStr nameJHrs Thing.++ jonifyIStr ((JonStr . show) val) Thing.++ postJName
  jonify (Days val) = jonifyNpStr nameJDys Thing.++ jonifyIStr ((JonStr . show) val) Thing.++ postJName

newtype Height = Height Int
  deriving (Show, Eq)

nameJHeight :: JonStr
nameJHeight = JonStr "Height"

instance Jonify Height where
  jonify (Height val) = jonifyNpStr nameJHeight Thing.++ jonifyIStr ((JonStr . show) val) Thing.++ postJName

newtype Width = Width Int
  deriving (Show, Eq)

nameJWidth :: JonStr
nameJWidth = JonStr "Width"

instance Jonify Width where
  jonify (Width val) = jonifyNpStr nameJWidth Thing.++ jonifyIStr ((JonStr . show) val) Thing.++ postJName

newtype Position = Position (Height, Width)
  deriving (Show, Eq)

nameJPos :: JonStr
nameJPos = JonStr "Position"

instance Jonify Position where
  jonify (Position (h, w)) =
    jonifyNpStr nameJPos Thing.++ preJList
      Thing.++ jonify h
      Thing.++ commaJ
      Thing.++ jonify w
      Thing.++ postJList
      Thing.++ postJName

newtype Element = Element (Position, Duration, Content)
  deriving (Show, Eq)

nameJElm :: JonStr
nameJElm = JonStr "Element"

instance Jonify Element where
  jonify (Element (pos, dur, con)) =
    jonifyNpStr nameJElm Thing.++ preJList
      Thing.++ jonify pos
      Thing.++ commaJ
      Thing.++ jonify dur
      Thing.++ commaJ
      Thing.++ jonify con
      Thing.++ postJList
      Thing.++ postJName

data Canvas
  = One [Element]
  | Many [Canvas]
  deriving (Show, Eq)

nameJOne :: JonStr
nameJOne = JonStr "One"

nameJMny :: JonStr
nameJMny = JonStr "Many"

instance Jonify Canvas where
  jonify (One es) = jonifyNpStr nameJOne Thing.++ jonifyIsStr es jonify Thing.++ postJName
  jonify (Many es) = jonifyNpStr nameJMny Thing.++ jonifyIsStr es jonify Thing.++ postJName

-- -- canvasStr :: IO CString
-- -- canvasStr = (newCString . jonify) things

-- -- foreign export ccall canvasStr :: IO CString

toCanvas :: [JonStr] -> Canvas
toCanvas xs = One (map (\s -> Element (Position (Height 0, Width 0), Seconds 5, Words s)) [xs])

manyCanvas :: [[JonStr]] -> Canvas
manyCanvas xs = Many $ map toCanvas xs

things :: Canvas
things = manyCanvas [fmap JonStr ["Hello", "This", "Is", "A", "Test"], fmap JonStr ["Another test"]]