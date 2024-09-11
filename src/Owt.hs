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
import Web.HttpApiData

class (ToJSON a) => ToPython a where
  toPython :: a -> Text
  default toPython :: (a ~ Text) => a -> Text
  toPython = id

instance (ToJSON a) => ToPython a where
  toPython = fixBooleans . TE.decodeUtf8 . BSL.toStrict . A.encode
    where
      fixBooleans = T.replace ":true" ":True" . T.replace ":false" ":False"

base64EncodeText :: Text -> Text
base64EncodeText = B64.extractBase64 . B64.encodeBase64 . TE.encodeUtf8

base64DecodeText :: Text -> Text
base64DecodeText = TE.decodeUtf8 . B64.decodeBase64 . (B64.assertBase64 @'B64.StdPadded) . TE.encodeUtf8

newtype Code = Code {unCode :: Text} deriving (Show, Eq, ToJSON, FromJSON, Generic)

newtype CodeB64 = CodeB64 {unCodeB64 :: Text} deriving (Show, Eq, ToJSON, FromJSON, Generic)

instance ToHttpApiData CodeB64 where
  toUrlPiece = unCodeB64

class HasCode c where
  codeText :: c -> Text
  default codeText :: c -> Text
  codeText = unCode . code

  code :: c -> Code
  default code :: c -> Code
  code = Code . codeText

  codeB64 :: c -> CodeB64
  default codeB64 :: c -> CodeB64
  codeB64 = CodeB64 . base64EncodeText . codeText

instance HasCode Code where
  code = id

instance HasCode CodeB64 where
  codeText = base64DecodeText . unCodeB64

instance HasCode Text where
  code = Code

newtype Kwargs = Kwargs {unKwargs :: Text} deriving (Show, Eq, ToJSON, FromJSON, Generic)

newtype KwargsB64 = KwargsB64 {unKwargsB64 :: Text} deriving (Show, Eq, ToJSON, FromJSON, Generic)

instance ToHttpApiData KwargsB64 where
  toUrlPiece = unKwargsB64

class HasKwargs a where
  kwargsText :: a -> Text
  default kwargsText :: a -> Text
  kwargsText = base64DecodeText . unKwargsB64 . kwargsB64

  kwargs :: a -> Kwargs
  default kwargs :: (ToPython a) => a -> Kwargs
  kwargs = Kwargs . toPython

  kwargsB64 :: a -> KwargsB64
  default kwargsB64 :: a -> KwargsB64
  kwargsB64 = KwargsB64 . base64EncodeText . unKwargs . kwargs

instance HasKwargs Kwargs where
  kwargs = id

instance HasKwargs KwargsB64 where
  kwargsB64 = id

instance HasKwargs Text where
  kwargs = Kwargs

toKwargs :: (ToPython a) => a -> Kwargs
toKwargs = Kwargs . toPython

newtype FnName = FnName {unFnName :: Text} deriving (Show, Eq, ToJSON, FromJSON, Generic)

instance ToHttpApiData FnName where
  toUrlPiece = unFnName

class HasFnName a where
  fnName :: a -> FnName
  default fnName :: a -> FnName
  fnName = const $ FnName "run"

class (HasCode o, HasKwargs o, HasFnName o) => IsOwt o

instance (HasCode o, HasKwargs o, HasFnName o) => IsOwt o

data SimpleOwt c k where
  SimpleOwt :: (HasCode c, HasKwargs k) => c -> k -> SimpleOwt c k

instance (HasCode c) => HasCode (SimpleOwt c k) where
  codeText (SimpleOwt c _) = codeText c

instance HasKwargs (SimpleOwt c k) where
  kwargs (SimpleOwt _ k) = kwargs k

instance HasFnName (SimpleOwt c k)

data OwtRequest = OwtRequest
  { _owtRequestCodeB64 :: !CodeB64,
    _owtRequestKwargsB64 :: !KwargsB64,
    _owtRequestFnName :: !FnName
  }
  deriving (Show, Eq, Generic)
  deriving
    (FromJSON, ToJSON)
    via PrefixedSnake "_owtRequest" OwtRequest

makeLenses ''OwtRequest

instance HasCode OwtRequest where
  codeB64 = view owtRequestCodeB64

instance HasKwargs OwtRequest where
  kwargsB64 = view owtRequestKwargsB64

instance HasFnName OwtRequest where
  fnName = view owtRequestFnName

toRequest :: (IsOwt o) => o -> OwtRequest
toRequest o =
  OwtRequest
    { _owtRequestCodeB64 = codeB64 o,
      _owtRequestKwargsB64 = kwargsB64 o,
      _owtRequestFnName = fnName o
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

class OwtOptions method scheme client where
  owtOptions :: (IsOwt o) => client -> o -> Option scheme

instance OwtOptions POST scheme (OwtClient scheme) where
  owtOptions client _ = client ^. owtClientPort

instance OwtOptions GET scheme (OwtClient scheme) where
  -- owtOptions :: (IsOwt o k) => client -> o -> Option scheme
  owtOptions client o =
    client ^. owtClientPort
      <> ("code_b64" =: codeB64 o)
      <> ("kwargs_b64" =: kwargsB64 o)
      <> ("fn_name" =: fnName o)

type family OwtRequestBodyF (method :: Type) a where
  OwtRequestBodyF GET _ = NoReqBody
  OwtRequestBodyF POST a = ReqBodyJson a

class (HttpMethod method) => OwtRequestBody method where
  owtRequestBody :: (IsOwt o) => o -> OwtRequestBodyF method o

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

class
  ( HttpMethod method,
    MonadBaseControl IO m
  ) =>
  Owt (method :: Type) out m client
  where
  owt :: (IsOwt o) => client -> o -> m out
  default owt :: (IsOwt o) => client -> o -> m out
  owt client o = owt' @method @out @m @client (toRequest o) client

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
    HttpBody (OwtRequestBodyF method OwtRequest),
    HttpBodyAllowed (AllowsBody method) (ProvidesBody (OwtRequestBodyF method OwtRequest)),
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

type OwtStream' sm m a = OwtStreamHandler sm a -> m a

type OwtStream m a = OwtStream' m m a

instance (MonadIO m) => MonadHttp (ConduitM i o (ResourceT m)) where
  handleHttpException = liftIO . throwIO

instance
  ( HttpMethod method,
    HttpBody (OwtRequestBodyF method OwtRequest),
    HttpBodyAllowed
      (AllowsBody method)
      (ProvidesBody (OwtRequestBodyF method OwtRequest)),
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
