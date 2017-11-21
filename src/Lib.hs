module Lib
    ( someFunc
    , element
    ) where

import           Control.Applicative
import qualified Data.Attoparsec.Text as A
import           Data.Text

someFunc :: IO ()
someFunc = putStrLn "someFunc"

newtype Whitespace = Whitespace Text
instance Show Whitespace where
    show (Whitespace text) = unpack text

data AttrValue =
      SingleQuoted Text
    | DoubleQuoted Text
instance Show AttrValue where
    show (SingleQuoted value) = "'" ++ unpack value ++ "'"
    show (DoubleQuoted value) = "\"" ++ unpack value ++ "\""

data Attribute = Attribute
    { attrLeadingWhitespace :: Whitespace
    , attrName              :: Text
    , attrValue             :: AttrValue
    } deriving Show
data Element = Element
    { elemLeadingWhitespace      :: Whitespace
    , elemName                   :: Text
    , elemAttributes             :: [Attribute]
    , elemAttrTrailingWhitespace :: Whitespace
    , elemBody                   :: ElementBody
    } deriving Show
data ElementBody = ElementBody
    { bodyLeadingWhitespace  :: Whitespace
    , bodyElements           :: [Element]
    , bodyTrailingWhitespace :: Whitespace
    } deriving Show

noWhitespace :: Whitespace
noWhitespace = Whitespace $ pack ""

noElementBody :: ElementBody
noElementBody = ElementBody noWhitespace [] noWhitespace

whitespace :: A.Parser Whitespace
whitespace = do
    text <- A.takeWhile $ A.inClass " \t\r\n"
    return (Whitespace text)

name :: A.Parser Text
name = A.takeWhile1 $ A.inClass "-_a-zA-Z0-9"

value :: A.Parser AttrValue
value = valueSingle <|> valueDouble
    where
        valueSingle :: A.Parser AttrValue
        valueSingle = do
            A.char '\''
            value_ <- A.takeWhile $ A.notInClass "'\n\r"
            A.char '\''
            return $ SingleQuoted value_
        valueDouble :: A.Parser AttrValue
        valueDouble = do
            A.char '"'
            value_ <- A.takeWhile $ A.notInClass "\"\n\r"
            A.char '"'
            return $ DoubleQuoted value_

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
    nextChar <- A.peekChar'
    if nextChar == '/'
        then do
            A.char '/'
            A.char '>'
            return $ Element elemLeadingWhitespace elemName elemAttributes elemAttrTrailingWhitespace noElementBody
        else do
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
