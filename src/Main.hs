module Main where

import           Criterion.Main
import qualified Data.Text      as T
import qualified Data.Text.Lazy as TL
import qualified Hash

nLazy = Hash.digestnLazy (const True)
nStrict = Hash.digestnStrict (const True)
d2n = Hash.digest2n (const True)

exampleLazy n = TL.replicate n $ TL.pack "Lorem ipsum and stuff"
exampleStrict n = T.replicate n $ T.pack "Lorem ipsum and stuff"

main :: IO ()
main = do
  defaultMain [ bgroup "lazy" [ bench "1" $ whnf nLazy (exampleLazy 1)
                              , bench "10" $ whnf nLazy (exampleLazy 10)
                              , bench "100" $ whnf nLazy (exampleLazy 100)
                              , bench "1000" $ whnf nLazy (exampleLazy 1000)
                              ]
              , bgroup "strict" [ bench "1" $ whnf nStrict (exampleStrict 1)
                                , bench "10" $ whnf nStrict (exampleStrict 10)
                                , bench "100" $ whnf nStrict (exampleStrict 100)
                                , bench "1000" $ whnf nStrict (exampleStrict 1000)
                                ]
              , bgroup "2n" [ bench "1" $ whnf d2n (exampleStrict 1)
                            , bench "10" $ whnf d2n (exampleStrict 10)
                            , bench "100" $ whnf d2n (exampleStrict 100)
                            , bench "1000" $ whnf d2n (exampleStrict 1000)
                            ]
              ]
