module Owt where

import Conduit
import Control.Exception.Lifted (SomeException, throwIO, try)
import Control.Lens
import Control.Monad.Base (MonadBase)
import Control.Monad.Trans.Control (MonadBaseControl)
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
import Deriving.Aeson.Stock
import Network.HTTP.Client qualified as L
import Network.HTTP.Req hiding (decompress)
import Network.HTTP.Req.Conduit (responseBodySource)
import Relude.Unsafe qualified as U
import Text.URI

data OwtRequest = OwtRequest
  { _owtRequestCodeB64 :: !Text,
    _owtRequestKwargsB64 :: !Text,
    _owtRequestFnName :: !Text
  }
  deriving (Show, Eq, Generic)
  deriving
    (FromJSON, ToJSON)
    via PrefixedSnake "_owtRequest" OwtRequest

makeLenses ''OwtRequest

instance Default OwtRequest where
  def =
    OwtRequest
      { _owtRequestCodeB64 = "",
        _owtRequestKwargsB64 = "",
        _owtRequestFnName = "run"
      }

data OwtClient scheme = OwtClient
  { _owtClientAddress :: !(Url scheme),
    _owtClientPort :: !(Option scheme)
  }

makeLenses ''OwtClient

mkOwtClient :: (MonadThrow m) => Text -> m (Either (OwtClient 'Http) (OwtClient 'Https))
mkOwtClient address = do
  uri <- mkURI address
  return $ case useURI uri of
    Nothing -> error $ "Invalid address: " <> show address
    Just (Left (u, p)) -> Left (OwtClient u p)
    Just (Right (u, p)) -> Right (OwtClient u p)

newtype OwtError = OwtError Text deriving (Show, Eq)

instance Exception OwtError

class OwtOptions method scheme a where
  owtOptions :: a -> OwtRequest -> Option scheme

instance OwtOptions POST scheme (OwtClient scheme) where
  owtOptions client _ = client ^. owtClientPort

instance OwtOptions GET scheme (OwtClient scheme) where
  owtOptions client request =
    client ^. owtClientPort
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

base64EncodeText :: Text -> Text
base64EncodeText = B64.extractBase64 . B64.encodeBase64 . TE.encodeUtf8

class (ToJSON a) => ToPython a where
  toPython :: a -> Text

instance (ToJSON a) => ToPython a where
  toPython = fixBooleans . TE.decodeUtf8 . BSL.toStrict . A.encode
    where
      fixBooleans = T.replace ":true" ":True" . T.replace ":false" ":False"

class
  ( HttpMethod method,
    MonadBaseControl IO m
  ) =>
  Owt (method :: Type) out m client
  where
  owt :: (ToPython kwargs) => Text -> kwargs -> client -> m out
  default owt :: (ToPython kwargs) => Text -> kwargs -> client -> m out
  owt code kwargs client = do
    let request =
          OwtRequest
            { _owtRequestCodeB64 = base64EncodeText code,
              _owtRequestKwargsB64 = base64EncodeText (toPython kwargs),
              _owtRequestFnName = "run"
            }
    owt' @method @out @m @client request client

  owt' :: OwtRequest -> client -> m out

instance (Owt method out m clientA, Owt method out m clientB) => Owt method out m (Either clientA clientB) where
  owt' request (Left client) = owt' @method request client
  owt' request (Right client) = owt' @method request client

handleReqError :: (MonadBaseControl IO m) => m a -> m a
handleReqError action = do
  rE <- try action
  case rE of
    Left (e :: HttpException) -> throwIO (OwtError (show e))
    Right r -> return r

instance
  ( HttpMethod (method :: Type),
    OwtMethod method,
    OwtRequestBody method,
    OwtOptions method scheme (OwtClient scheme),
    HttpBody (OwtRequestBodyF method),
    HttpBodyAllowed (AllowsBody method) (ProvidesBody (OwtRequestBodyF method)),
    MonadBaseControl IO m,
    MonadIO m
  ) =>
  Owt method ByteString m (OwtClient scheme)
  where
  owt' request client = handleReqError $ do
    liftIO $
      runReq defaultHttpConfig $
        req
          (owtMethod @method)
          (client ^. owtClientAddress)
          (owtRequestBody @method request)
          bsResponse
          (owtOptions @method @scheme client request)
          <&> responseBody

instance (HttpMethod method, MonadBaseControl IO m, Owt method ByteString m client) => Owt method Text m client where
  owt' request client =
    owt' @method @ByteString @m @client request client <&> decodeUtf8

type OwtStreamHandler m a =
  ConduitT ByteString Void (ResourceT m) a

type OwtStream sm m a = OwtStreamHandler sm a -> m a

type OwtStreamIO a = OwtStream IO IO a

instance (MonadIO m) => MonadHttp (ConduitM i o (ResourceT m)) where
  handleHttpException = liftIO . throwIO

instance
  ( HttpMethod method,
    HttpBody (OwtRequestBodyF method),
    HttpBodyAllowed
      (AllowsBody method)
      (ProvidesBody (OwtRequestBodyF method)),
    OwtMethod method,
    OwtRequestBody method,
    OwtOptions method scheme (OwtClient scheme),
    MonadBaseControl IO m,
    MonadIO m,
    MonadUnliftIO m
  ) =>
  Owt method (OwtStream m m a) m (OwtClient scheme)
  where
  owt' request client = return $ \(handler :: OwtStreamHandler m a) ->
    handleReqError $
      runConduitRes $
        req'
          (owtMethod @method)
          (client ^. owtClientAddress)
          (owtRequestBody @method request)
          (owtOptions @method client request <> responseTimeout (1000000 * 300))
          (\request manager -> bracketP (L.responseOpen request manager) L.responseClose responseBodySource)
          .| handler
