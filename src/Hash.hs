module Hash
    ( digestnLazy
    , digestnStrict
    , digest2n
    ) where

import qualified Crypto.Hash.SHA512 as SHA512
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Base16 as B16

type Filter = SHA512.Ctx -> Char -> SHA512.Ctx

idFilter :: Filter
idFilter ctx char = SHA512.update ctx $ B8.singleton char

toFilter :: (Char -> Bool) -> Filter
toFilter f ctx c =
  if f c
  then SHA512.update ctx $ B8.singleton c
  else ctx

digestnLazy' :: Filter -> TL.Text -> B8.ByteString
digestnLazy' filter text =
  B16.encode $ SHA512.finalize ctx
  where
    ctx = TL.foldl filter ctx0 text
    ctx0 = SHA512.init

digestnLazy :: (Char -> Bool) -> TL.Text -> B8.ByteString
digestnLazy f = digestnLazy' (toFilter f)

digestnStrict' :: Filter -> T.Text -> B8.ByteString
digestnStrict' filter text =
  B16.encode $ SHA512.finalize ctx
  where
    ctx = T.foldl filter ctx0 text
    ctx0 = SHA512.init

digestnStrict :: (Char -> Bool) -> T.Text -> B8.ByteString
digestnStrict f = digestnStrict' (toFilter f)

digest2n :: (Char -> Bool) -> T.Text -> B8.ByteString
digest2n f t = B16.encode $ SHA512.hash $ TE.encodeUtf8 $ T.filter f t
