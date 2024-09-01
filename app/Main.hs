module Main where

import Network.HTTP.Req
import Options.Applicative
import Owt
import Text.URI

cliParser :: ParserInfo (IO (Either (OwtClient 'Http) (OwtClient 'Https), Text, Text))
cliParser =
  info
    ( do
        address <-
          ( strOption
              ( long "address"
                  <> short 'a'
                  <> help "Owt server address"
              )
            )
        code <-
          strOption
            ( long "code"
                <> short 'c'
                <> help "Python code to run"
            )
        kwargs <-
          strOption
            ( long "kwargs"
                <> short 'k'
                <> help "Python kwargs to supply"
                <> value "{}"
            )
        return $ do
          uri <- mkURI address
          let client = case useURI uri of
                Nothing -> error $ "Invalid address: " <> show address
                Just (Left (u, p)) -> Left (OwtClient u p)
                Just (Right (u, p)) -> Right (OwtClient u p)
          return (client, code, kwargs)
    )
    fullDesc

main :: IO ()
main = do
  ioCR <- customExecParser (prefs showHelpOnEmpty) cliParser
  (clientE, code, kwargs) <- ioCR
  putTextLn $ "Code:\n" <> code
  putTextLn $ "Kwargs:\n" <> kwargs
  let handleResponse :: Either OwtError ByteString -> IO ()
      handleResponse = \case
        Left e -> print e
        Right r -> putBS r
  case clientE of
    Left client -> owt @POST code kwargs client >>= handleResponse
    Right client -> owt @POST code kwargs client >>= handleResponse
