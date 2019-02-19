{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.IO.Class         ( liftIO )
import           System.Random                  ( randomRIO )
import           Data.List                      ( delete )
import           Network.Wai.Middleware.RequestLogger
                                                ( logStdoutDev )
import           Network.Wai.Middleware.Static  ( staticPolicy
                                                , noDots
                                                , (>->)
                                                , addBase
                                                )
import           Web.Scotty              hiding ( delete )

main :: IO ()
main = scotty 3000 $ do
  middleware logStdoutDev
  middleware $ staticPolicy (noDots >-> addBase "frontend")

  get "/" $ file "index.html"

  get "/syns/:num" $ do
    n    <- param "num"
    syns <- liftIO $ generateSyns n
    json syns

  post "/postSyns/:xs" $ do
    xs <- param "xs"
    liftIO $ processWordsResults xs
    json ["back" :: String] -- TODO možda ne treba

processWordsResults :: String -> IO ()
processWordsResults xs = do
  let ys = pairUp $ words xs
  mapM_ (\x -> appendFile "./UserTimes.txt" $ x ++ "\n") ys
  return ()

pairUp :: [String] -> [String]
pairUp []                = []
pairUp [x              ] = []
pairUp (x         : [y]) = []
pairUp (x : y : z : zs ) = unwords (x : y : [z]) : pairUp zs

generateSyns :: Int -> IO [String]
generateSyns n = do
  allNouns   <- getWords "amerNouns50000.txt"
  nouns      <- sample n allNouns
  commonSyns <- getWords "goodSyns50.txt"
  addAttributes nouns commonSyns

dataFolder :: FilePath
dataFolder = "data/"

getWords :: FilePath -> IO [String]
getWords path =
  concatMap (tail . words) . lines <$> readFile (dataFolder ++ path)

addAttributes :: [String] -> [String] -> IO [String]
addAttributes xs commonSyns = mapM (newAttribute commonSyns) xs

newAttribute :: [String] -> String -> IO String
newAttribute commonSyns x = do
  attr <- sample1 =<< getWords "amerAdjectives10000.txt"
  let syn = attr ++ " " ++ x
  if syn `elem` commonSyns then newAttribute commonSyns x else return syn

sample1 :: [a] -> IO a
sample1 xs = do
  let l = length xs - 1
  idx <- randomRIO (0, l)
  return $ xs !! idx

sample :: Eq a => Int -> [a] -> IO [a]
sample 0 xs = return []
sample n xs = do
  let l = min n (length xs)
  val <- sample1 xs
  (:) <$> pure val <*> sample (l - 1) (delete val xs)
