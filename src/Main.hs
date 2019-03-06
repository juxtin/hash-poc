module Main where

import           Criterion.Main
import qualified Data.Text      as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO
import qualified Hash

nLazy = Hash.digestnLazy (const True)
nStrict = Hash.digestnStrict (const True)
d2n = Hash.digest2n (const True)

nLazyIO = flip Hash.digestnLazyFile (const True)
nStrictIO = flip Hash.digestnStrictFile (const True)
d2nIO = flip Hash.digest2nFile (const True)

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
  file10000 <- writeExampleFile 10000 -- about 215k
  defaultMain [ bgroup "lazy" [ bench "1" $ whnf nLazy (exampleLazy 1)
                              , bench "10" $ whnf nLazy (exampleLazy 10)
                              , bench "100" $ whnf nLazy (exampleLazy 100)
                              , bench "10000" $ whnf nLazy (exampleLazy 10000)
                              ]
              , bgroup "strict" [ bench "1" $ whnf nStrict (exampleStrict 1)
                                , bench "10" $ whnf nStrict (exampleStrict 10)
                                , bench "100" $ whnf nStrict (exampleStrict 100)
                                , bench "1000" $ whnf nStrict (exampleStrict 1000)
                                , bench "10000" $ whnf nStrict (exampleStrict 10000)
                                ]
              , bgroup "2n" [ bench "1" $ whnf d2n (exampleStrict 1)
                            , bench "10" $ whnf d2n (exampleStrict 10)
                            , bench "100" $ whnf d2n (exampleStrict 100)
                            , bench "1000" $ whnf d2n (exampleStrict 1000)
                            , bench "10000" $ whnf d2n (exampleStrict 10000)
                            ]
              , bgroup "lazyIO" [ bench "1" $ nfIO $ nLazyIO file1
                                , bench "10" $ nfIO $ nLazyIO file10
                                , bench "100" $ nfIO $ nLazyIO file100
                                , bench "1000" $ nfIO $ nLazyIO file1000
                                , bench "10000" $ nfIO $ nLazyIO file10000
                                ]
              , bgroup "strictIO" [ bench "1" $ nfIO $ nStrictIO file1
                                  , bench "10" $ nfIO $ nStrictIO file10
                                  , bench "100" $ nfIO $ nStrictIO file100
                                  , bench "1000" $ nfIO $ nStrictIO file1000
                                  , bench "10000" $ nfIO $ nStrictIO file10000
                                  ]
              , bgroup "2nIO" [ bench "1" $ nfIO $ d2nIO file1
                              , bench "10" $ nfIO $ d2nIO file10
                              , bench "100" $ nfIO $ d2nIO file100
                              , bench "1000" $ nfIO $ d2nIO file1000
                              , bench "10000" $ nfIO $ d2nIO file10000
                              ]
              ]
