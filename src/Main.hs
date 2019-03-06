module Main where

import           Criterion.Main
import qualified Data.Text      as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO
import qualified Hash

nLazy = Hash.digestnLazy
nStrict = Hash.digestnStrict
d2n = Hash.digest2n

nLazyIO = Hash.digestnLazyFile
nStrictIO = Hash.digestnStrictFile
d2nIO = Hash.digest2nFile

exampleLazy n = TL.pack $ unlines $ replicate n "Lorem ipsum and stuff"
exampleStrict n = T.pack $ unlines $ replicate n "Lorem ipsum and stuff"

writeExampleFile n = do
  let contents = exampleLazy n
  let path = "/tmp/hash-poc-example-" ++ show n
  TLIO.writeFile path contents
  return path


main :: IO ()
main = do
  file1 <- writeExampleFile 1
  file10 <- writeExampleFile 10
  file100 <- writeExampleFile 100     -- about 2k
  file1000 <- writeExampleFile 1000   -- about 21k
  file2000 <- writeExampleFile 2000   -- about 43k
  file3000 <- writeExampleFile 3000   -- about 64k
  file4000 <- writeExampleFile 4000   -- about 86k
  file5000 <- writeExampleFile 5000   -- about 107k
  file6000 <- writeExampleFile 6000   -- about 129k
  defaultMain [ bgroup "100" [ bench "lazy" $ whnf nLazy (exampleLazy 100)
                             , bench "strict" $ whnf nStrict (exampleStrict 100)
                             , bench "2n" $ whnf d2n (exampleStrict 100)
                             , bench "lazyIO" $ nfIO $ nLazyIO file100
                             , bench "strictIO" $ nfIO $ nStrictIO file100
                             , bench "2nIO" $ nfIO $ d2nIO file100
                             ]
              , bgroup "1000" [ bench "lazy" $ whnf nLazy (exampleLazy 1000)
                              , bench "strict" $ whnf nStrict (exampleStrict 1000)
                              , bench "lazyIO" $ nfIO $ nLazyIO file1000
                              , bench "strictIO" $ nfIO $ nStrictIO file1000
                              ]
              , bgroup "2000" [ bench "lazy" $ whnf nLazy (exampleLazy 2000)
                              , bench "strict" $ whnf nStrict (exampleStrict 2000)
                              , bench "lazyIO" $ nfIO $ nLazyIO file2000
                              , bench "strictIO" $ nfIO $ nStrictIO file2000
                              ]
              , bgroup "3000" [ bench "lazy" $ whnf nLazy (exampleLazy 3000)
                              , bench "strict" $ whnf nStrict (exampleStrict 3000)
                              , bench "lazyIO" $ nfIO $ nLazyIO file3000
                              , bench "strictIO" $ nfIO $ nStrictIO file3000
                              ]
              , bgroup "4000" [ bench "lazy" $ whnf nLazy (exampleLazy 4000)
                              , bench "strict" $ whnf nStrict (exampleStrict 4000)
                              , bench "lazyIO" $ nfIO $ nLazyIO file4000
                              , bench "strictIO" $ nfIO $ nStrictIO file4000
                              ]
              , bgroup "5000" [ bench "lazy" $ whnf nLazy (exampleLazy 5000)
                              , bench "strict" $ whnf nStrict (exampleStrict 5000)
                              , bench "lazyIO" $ nfIO $ nLazyIO file5000
                              , bench "strictIO" $ nfIO $ nStrictIO file5000
                              ]
              , bgroup "6000" [ bench "lazy" $ whnf nLazy (exampleLazy 6000)
                              , bench "strict" $ whnf nStrict (exampleStrict 6000)
                              , bench "lazyIO" $ nfIO $ nLazyIO file6000
                              , bench "strictIO" $ nfIO $ nStrictIO file6000
                              ]
              ]
