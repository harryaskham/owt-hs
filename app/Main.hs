module Main where

import Network.HTTP.Req
import Options.Applicative
import Owt
import Text.URI

cliParser :: ParserInfo (IO (Either (OwtClient 'Http) (OwtClient 'Https), OwtRequest))
cliParser =
  flip
    info
    fullDesc
    ( do
        address <-
          ( strOption
              ( long "address"
                  <> short 'a'
                  <> help "Owt server address"
              )
            )
        request <-
          OwtRequest
            <$> ( strOption
                    ( long "code"
                        <> short 'c'
                        <> help "Python code to run"
                    )
                )
            <*> ( strOption
                    ( long "kwargs"
                        <> short 'k'
                        <> help "Python kwargs to supply"
                        <> value "{}"
                    )
                )
            <*> ( strOption
                    ( long "fn-name"
                        <> short 'f'
                        <> help "Python function name to run"
                        <> value "run"
                    )
                )
        return $ do
          uri <- mkURI address
          let client = case useURI uri of
                Nothing -> error $ "Invalid address: " <> show address
                Just (Left (u, p)) -> Left (OwtClient u p)
                Just (Right (u, p)) -> Right (OwtClient u p)
          return (client, request)
    )

main :: IO ()
main = do
  ioCR <- customExecParser (prefs showHelpOnEmpty) cliParser
  (clientE, request) <- ioCR
  let handleResponse :: Either OwtError ByteString -> IO ()
      handleResponse = \case
        Left e -> print e
        Right r -> putBS r
  case clientE of
    Left client -> owt' @GET request client >>= handleResponse
    Right client -> owt' @GET request client >>= handleResponse
