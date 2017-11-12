module Lib
    ( someFunc
    ) where

import Control.Applicative
import qualified Data.Attoparsec.Text as A
import Data.Text

someFunc :: IO ()
someFunc = putStrLn "someFunc"

newtype Whitespace = Whitespace Text
    deriving Show
data Attribute = Attribute
    { attrLeadingWhitespace :: Whitespace
    , attrName :: Text
    , attrValue :: Text 
    } deriving Show
data Element = Element
    { elemLeadingWhitespace :: Whitespace
    , elemName :: Text
    , elemAttributes :: [Attribute]
    , elemAttrTrailingWhitespace :: Whitespace
    , elemBody :: ElementBody
    } deriving Show
data ElementBody = ElementBody
    { bodyLeadingWhitespace :: Whitespace
    , bodyElements :: [Element]
    , bodyTrailingWhitespace :: Whitespace
    } deriving Show

whitespace :: A.Parser Whitespace
whitespace = do
    text <- A.takeWhile $ A.inClass " \t\r\n"
    return (Whitespace text)

name :: A.Parser Text
name = do
    name_ <- A.takeWhile1 $ A.inClass "-_a-zA-Z0-9"
    return name_

value :: A.Parser Text
value = valueSingle <|> valueDouble
    where
        valueSingle :: A.Parser Text
        valueSingle = do
            A.char '\''
            value_ <- A.takeWhile $ A.notInClass "\n\r"
            A.char '\''
            return value_
        valueDouble :: A.Parser Text
        valueDouble = do
            A.char '"'
            value_ <- A.takeWhile $ A.notInClass "\n\r"
            A.char '"'
            return value_

attribute :: A.Parser Attribute
attribute = do
    attrLeadingWhitespace <- whitespace
    attrName <- name
    A.char '='
    attrValue <- value
    return $ Attribute attrLeadingWhitespace attrName attrValue

element :: A.Parser Element
element = do
    elemLeadingWhitespace <- whitespace
    A.char '<'
    elemName <- name
    elemAttributes <- A.many' attribute
    elemAttrTrailingWhitespace <- whitespace
    A.char '>'
    elemBody <- elementBody
    A.char '<'
    A.char '/'
    A.string elemName
    A.char '>'
    return $ Element elemLeadingWhitespace elemName elemAttributes elemAttrTrailingWhitespace elemBody

elementBody :: A.Parser ElementBody
elementBody = do
    bodyLeadingWhitespace <- whitespace
    bodyElements <- A.many' element
    bodyTrailingWhitespace <- whitespace
    return $ ElementBody bodyLeadingWhitespace bodyElements bodyTrailingWhitespace

