module Main where

import Data.Char
import Data.List
import Data.Maybe
import Data.String
import Data.ByteString.Char8 qualified as B
import Control.Applicative
import System.IO
import System.Environment
import Data.FileEmbed (embedStringFile)

import Network.HTTP.Req

main :: IO ()
main =
  do args <- getArgs
     case args of
       [] -> repl
       ["-"] -> enc
       ["-r"] -> raw
       _ -> help

help = putStr $ unlines
  [ "ran with no options runs a repl"
  , "options:"
  , "\t-\tgobble all stdin, encode it, send and decode the response"
  , "\t-r\tgobble all stdin and send it raw, decode the response" ]

repl =
  do hSetBuffering stdin LineBuffering
     go
  where
    go =
      do hPutStr stderr "> "
         hFlush stderr
         inp <- hGetLine stdin
         resp <- r (encode inp)
         putStr (decode resp)
         hFlush stdout
         hPutStr stderr "\n"
         hFlush stderr
         go

enc =
  do inp <- hGetContents stdin
     resp <- r (encode inp)
     putStr (decode resp)
     hFlush stdout

raw =
  do inp <- hGetContents stdin
     resp <- r inp
     putStr (decode resp)
     hFlush stdout

-- encoding / decoding strings

t = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!\"#$%&'()*+,-./:;<=>?@[\\]^_`|~ \n"

encode = ('S':) . mapMaybe f
  where
    f c = fmap (chr . (33+)) (findIndex (c==) t) <|> Just c

decode ('S':xs) = (f . ord) `map` xs
  where
    f n | n < 33 || n > 126 = chr n
        | otherwise = t !! (n - 33)
decode ys = ys

-- http requests

token :: IsString a => a
token = $(embedStringFile ".teamtoken")

r :: String -> IO String
r body = runReq defaultHttpConfig $ do
  r <- req
    POST
    (https "boundvariable.space" /: "communicate")
    (ReqBodyBs $ B.pack body)
    bsResponse
    bearer
  pure $ B.unpack $ responseBody r
  where
    bearer = header "Authorization" (B.unwords ["Bearer",token])
