module Hash
    ( digestnLazy
    , digestnStrict
    , digest2n
    , digestnLazyFile
    , digestnStrictFile
    , digest2nFile
    ) where

import qualified Crypto.Hash.SHA512 as SHA512
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Base16 as B16

type Filter = SHA512.Ctx -> Char -> SHA512.Ctx

toFilter :: (Char -> Bool) -> Filter
toFilter f ctx c =
  if f c
  then SHA512.update ctx $ B8.singleton c
  else ctx

class HashCtx a where
  getCtx :: a -> SHA512.Ctx
  update :: a -> B8.ByteString -> a

instance HashCtx SHA512.Ctx where
  getCtx = id
  update = SHA512.update

data WsHashCtx =
  WsHashCtx { ctx' :: SHA512.Ctx
            , acceptWs :: Bool
            }

instance HashCtx WsHashCtx where
  getCtx = ctx'
  update wsCtx bs =
    let newCtx = SHA512.update (ctx' wsCtx) bs in
      wsCtx { ctx' = newCtx }

setAccept :: Bool -> WsHashCtx -> WsHashCtx
setAccept x wsCtx = wsCtx { acceptWs = x }

normalize :: WsHashCtx -> Char -> WsHashCtx
normalize wsCtx@(WsHashCtx _ True) char =
  if isWS char
  then setAccept False $ update wsCtx $ B8.singleton ' '
  else setAccept True  $ update wsCtx $ B8.singleton char
normalize wsCtx@(WsHashCtx _ False) char =
  if isWS char
  then wsCtx
  else setAccept True $ update wsCtx $ B8.singleton char

isWS :: Char -> Bool
isWS = flip elem [' ', '\t', '\n', '\r']

digestnLazy' :: HashCtx ctx => ctx -> (ctx -> Char -> ctx) -> TL.Text -> B8.ByteString
digestnLazy' initCtx f text =
  B16.encode $ SHA512.finalize $ getCtx ctx
  where
    ctx = TL.foldl f initCtx text

digestnLazy :: TL.Text -> B8.ByteString
digestnLazy text =
  let initCtx = WsHashCtx SHA512.init True in
    digestnLazy' initCtx normalize text

digestnStrict' :: HashCtx ctx => ctx -> (ctx -> Char -> ctx) -> T.Text -> B8.ByteString
digestnStrict' initCtx f text =
  B16.encode $ SHA512.finalize $ getCtx ctx
  where
    ctx = T.foldl f initCtx text

digestnStrict :: T.Text -> B8.ByteString
digestnStrict text =
  let initCtx = WsHashCtx SHA512.init True in
    digestnStrict' initCtx normalize text

digest2n :: T.Text -> B8.ByteString
digest2n t = B16.encode $ SHA512.hash $ TE.encodeUtf8 $ T.reverse $ fst $ T.foldl f init t
  where
    init = (mempty :: T.Text, True)
    f :: (T.Text, Bool) -> Char -> (T.Text, Bool)
    f (text, True) char
      | isWS char = (' ' `T.cons` text, False)
      | otherwise = (char `T.cons` text, True)
    f (text, False) char
      | isWS char = (text, False)
      | otherwise = (char `T.cons` text, True)

digestnLazyFile :: FilePath -> IO B8.ByteString
digestnLazyFile path = digestnLazy <$> TLIO.readFile path

digestnStrictFile :: FilePath ->  IO B8.ByteString
digestnStrictFile path = digestnStrict <$> TIO.readFile path

digest2nFile :: FilePath -> IO B8.ByteString
digest2nFile path = digest2n <$> TIO.readFile path
