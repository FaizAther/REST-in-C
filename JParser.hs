module JParser where

-- Copying tsoding
-- https://www.youtube.com/watch?v=N9RUqGYuGfw

import Control.Applicative ((<|>), Alternative, empty)
import Data.Char (isDigit)

data JonVal
  = JonNul
  | JonBool Bool
  | JonNum  Integer
  | JonStr  String
  | JonList [JonVal]
  | JonMap  [(String, JonVal)]
  deriving (Show, Eq)

newtype JParse a = JParser
  {
    runJParser :: String -> Maybe (String, a)
  }

instance Functor JParse where
--fmap :: (a -> b) -> JParse a -> JParse b
  fmap f (JParser mfa) = JParser $ \input -> do
                                        (valS, a') <- mfa input
                                        pure (valS , f a')

instance Applicative JParse where
  pure a = JParser $ \input -> Just (input, a)
--(<*>) :: JParse (a -> b) -> JParse a -> JParse b
  (<*>) (JParser mfab) (JParser ma) = JParser $ \input -> do
                                                    (valS, fab') <- mfab input
                                                    (valS', a' ) <- ma valS
                                                    pure (valS', fab' a')

instance Monad JParse where
  (>>=) (JParser fa) fjb = JParser $ \input -> do
                                            (valS, a') <- fa input
                                            (JParser fmb) <- Just $ fjb a'
                                            fmb valS

instance Alternative JParse where
  empty = JParser $ const Nothing
  (<|>) (JParser a0) (JParser a1) = JParser $ \input -> a0 input <|> a1 input

-- null
jonNul :: JParse JonVal
jonNul = JonNul <$ parseStr "null"

-- true, false
jonBool :: JParse JonVal
jonBool = f <$> (parseStr "true" <|> parseStr "false")
  where f "true" = JonBool True
        f "false" = JonBool False
        f _ = undefined

span' :: (a -> Bool) -> [a] -> Maybe ([a], [a])
span' f ls =
  let (match, rest) = span f ls in
    if null match then Nothing
    else Just (rest, match)

parseSpan :: (Char -> Bool) -> JParse String
parseSpan f = JParser $ \input -> 
                                span' f input

-- 1 2 3
jonNum :: JParse JonVal
jonNum = f <$> parseSpan isDigit
  where f xs = JonNum $ read xs

jonStr :: JParse JonVal
jonStr = JonStr <$> (parseCh '"' *> parseSpan (/= '"') <* parseCh '"')

parseCh :: Char -> JParse Char
parseCh ch = JParser singleCh
  where singleCh (x:xs) = if x == ch then Just (xs, x) else Nothing
        singleCh _ = Nothing

parseStr :: String -> JParse String
parseStr = traverse parseCh
  -- where
  --   charPs :: [JParse Char]
  --   charPs = fmap parseCh str

jonVal :: JParse JonVal
jonVal = jonNul <|> jonBool <|> jonNum <|> jonStr
