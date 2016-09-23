{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main)
  where

import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Expr
import Text.Megaparsec.Char
import Text.Megaparsec.String -- input stream is of type ‘String’
import qualified Data.Map as M 
import qualified Text.Megaparsec.Lexer as L

data Graphics = Graphics {g_x :: Double
                         ,g_y :: Double
                         ,g_h :: Double
                         ,g_w :: Double
                         ,g_fill :: T.Text
                         ,g_outline :: T.Text
                         ,g_type :: T.Text
                         }
      deriving (Show)

data Property = Property [Char] [Char]
  deriving (Show)

propToTup (Property k v) = (k,v)

parseProperty :: Parser Property
parseProperty = do
  many spaceChar
  x <- many letterChar
  many spaceChar
  y <- (try numbery) <|> (try stringy)
  if (null x || null y) then fail "heh"
  else return $ Property x y

numbery :: Parser [Char]
numbery = show <$> L.number

stringy :: Parser [Char]
stringy = do 
  char '\"'
  v <- many (alphaNumChar <|> oneOf ['#'])
  char '\"'
  return v
      

parseGraphics :: Parser Graphics
parseGraphics = do
  string "graphics"
  spaceChar
  char '['
  space
  props <- sepEndBy1 parseProperty (newline) 
  char ']'
  let m = M.fromList $ map propToTup props
  let mm = (M.!) m
  return $ Graphics (read $ mm "x") (read $ mm "y") (read $ mm "h") (read $ mm "w")
                    (T.pack $ mm "fill") (T.pack $ mm "outline") (T.pack $ mm "type")

test = "graphics [" ++
    "\n  x        0.0" ++
    "\n  y        1279.66" ++
    "\n  h        20" ++
    "\n  w        60" ++
    "\n  fill     \"#cccccc\""++
    "\n  outline  \"#000000\""++
    "\n  type     \"ellipse\""++
    "\n]"


test2 = 
    "\n  x        0.0" ++
    "\n  y        1279.66" ++
    "\n  h        20" ++
    "\n  w        60" -- ++
--     "\n  fill     \"#cccccc\""++
--     "\n  outline  \"#000000\""++
--     "\n  type     \"ellipse\""


main = do
       -- print $ parse parseProperty "" "x \"#two\""
       print $ parse parseGraphics "" test

       -- print $ parse (sepBy parseProperty newline) "" test2
