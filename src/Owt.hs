module Owt where

import Conduit
import Control.Exception.Lifted (SomeException, try)
import Control.Lens
import Data.Aeson (decode, encode)
import Data.Aeson qualified as A
import Data.Base64.Types qualified as B64
import Data.ByteString qualified as BS
import Data.ByteString.Base64 qualified as B64
import Data.ByteString.Char8 qualified as C8
import Data.ByteString.Lazy qualified as BSL
import Data.Conduit.Combinators qualified as CC
import Data.Default
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Deriving.Aeson
import Network.HTTP.Req hiding (decompress)
import Network.HTTP.Req.Conduit (responseBodySource)
import Relude.Unsafe qualified as U

data OwtRequest = OwtRequest
  { _owtRequestCodeB64 :: Text,
    _owtRequestFnName :: Text,
    _owtRequestKwargsB64 :: Text
  }
  deriving (Show, Eq, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "_owtRequest", CamelToSnake]] OwtRequest

makeLenses ''OwtRequest

instance Default OwtRequest where
  def =
    OwtRequest
      { _owtRequestCodeB64 = "",
        _owtRequestKwargsB64 = "",
        _owtRequestFnName = "run"
      }

data OwtClient scheme = OwtClient
  { _owtClientAddress :: Url scheme,
    _owtClientPort :: Int
  }

makeLenses ''OwtClient

data OwtError = OwtError Text deriving (Show)

class OwtOptions method scheme a where
  owtOptions :: a -> OwtRequest -> Option scheme

instance OwtOptions POST scheme (OwtClient scheme) where
  owtOptions client _ = port $ client ^. owtClientPort

instance OwtOptions GET scheme (OwtClient scheme) where
  owtOptions client request =
    (port $ client ^. owtClientPort)
      <> ("code_b64" =: (request ^. owtRequestCodeB64))
      <> ("fn_name" =: (request ^. owtRequestFnName))
      <> ("kwargs_b64" =: (request ^. owtRequestKwargsB64))

type family OwtRequestBodyF (method :: Type) where
  OwtRequestBodyF GET = NoReqBody
  OwtRequestBodyF POST = ReqBodyJson OwtRequest

class (HttpMethod method) => OwtRequestBody method where
  owtRequestBody :: OwtRequest -> OwtRequestBodyF method

instance OwtRequestBody GET where
  owtRequestBody = const NoReqBody

instance OwtRequestBody POST where
  owtRequestBody = ReqBodyJson

class (HttpMethod method) => OwtMethod method where
  owtMethod :: method

instance OwtMethod GET where
  owtMethod = GET

instance OwtMethod POST where
  owtMethod = POST

class (HttpMethod method) => Owt (method :: Type) out a where
  owt :: (MonadIO m, ToJSON kwargs) => Text -> kwargs -> a -> m (Either OwtError out)
  default owt :: (MonadIO m, ToJSON kwargs) => Text -> kwargs -> a -> m (Either OwtError out)
  owt code kwargs client = do
    let request =
          OwtRequest
            { _owtRequestCodeB64 = (B64.extractBase64 $ B64.encodeBase64 $ TE.encodeUtf8 code),
              _owtRequestKwargsB64 = (B64.extractBase64 $ B64.encodeBase64 $ BSL.toStrict . A.encode $ kwargs),
              _owtRequestFnName = "run"
            }
    owt' @method @out @a request client

  owt' :: (MonadIO m) => OwtRequest -> a -> m (Either OwtError out)

instance
  ( HttpMethod method,
    OwtMethod method,
    OwtRequestBody method,
    HttpBody (OwtRequestBodyF method),
    HttpBodyAllowed (AllowsBody method) (ProvidesBody (OwtRequestBodyF method))
  ) =>
  Owt method ByteString (OwtClient scheme)
  where
  owt' request client = do
    rE <-
      liftIO $
        try $
          runReq defaultHttpConfig $
            req
              (owtMethod @method)
              (client ^. owtClientAddress)
              (owtRequestBody @method request)
              bsResponse
              (owtOptions @POST @scheme client request)
    return $ case rE of
      Left (e :: SomeException) -> Left $ OwtError $ show e
      Right r -> Right $ responseBody r

instance (HttpMethod method, Owt method ByteString a) => Owt method Text a where
  owt' request client = do
    r <- owt' @method @ByteString @a request client
    return $ r <&> decodeUtf8
