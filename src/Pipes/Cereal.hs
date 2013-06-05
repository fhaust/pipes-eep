


module Pipes.Cereal where


import Control.Proxy
import Data.Serialize (Serialize, Result(..))
import qualified Data.Serialize as S

import Data.ByteString (ByteString)
import Data.Maybe

decodeD :: (Proxy p, Serialize a, Monad m) => () -> Pipe p ByteString a m ()
decodeD () = runIdentityP $ loop Nothing Nothing
  where 
    loop mk mbin = do
      bin <- maybe (request ()) return mbin
      case fromMaybe (S.runGetPartial S.get) mk bin of
        Fail reason -> fail reason
        Partial k   -> loop (Just k) Nothing
        Done c bin' -> do
          respond c
          loop Nothing (Just bin')

--    let rs = S.runGetPartial S.get bs
--    case rs of
--      Partial f -> 


encodeD :: (Proxy p, Serialize a, Monad m) => () -> Pipe p a ByteString m ()
encodeD () = runIdentityP . forever $ do
  x <- request ()
  respond . S.encode $ x

