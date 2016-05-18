import System.IO
import Control.Exception
import Data.Char

import Control.Monad.State

{-
parseTest p s = do
    print $ fst $ p s
    `catch` \(SomeException e) ->
        putStr $ show e
-}
parseTest p s = do
    print $ evalState p s
    `catch` \(SomeException e) ->
        putStr $ show e

{- anyChar (x:xs) = (x, xs) -}
anyChar :: State String Char
anyChar = state $ anyChar where
    anyChar (x:xs) = (x, xs)

{- satisfy f (x:xs) | f x = (x, xs) -}
satisfy :: (Char -> Bool) -> State String Char
satisfy f = state $ satisfy
    where satisfy (x:xs) | f x = (x, xs)

char c = satisfy (== c)
digit  = satisfy isDigit
letter = satisfy isLetter

{-
test3 xs0 =
    let (x1, xs1) = letter xs0
        (x2, xs2) = digit  xs1
        (x3, xs3) = digit  xs2
    in ([x1, x2, x3], xs3)
-}
test3 = do
    x1 <- letter
    x2 <- digit
    x3 <- digit
    return [x1, x2, x3]

{-
test4 xs0 =
    let (x1, xs1) = (char '[') xs0
        (x2, xs2) = digit xs1
        (x3, xs3) = (char ' ') xs2
    in ([x1, x2, x3], xs3)
-}
test4 = do
    x1 <- (char '[')
    x2 <- digit
    x3 <- (char ' ')
    return [x1, x2, x3]

main = do
    handle <- openFile "parse.iq" ReadMode
    text <- hGetContents handle
    parseTest test4 text
    parseTest test3 "abc"
    parseTest test3 "123"
    parseTest test3 "a23"
    parseTest test3 "a234"
    hClose handle
