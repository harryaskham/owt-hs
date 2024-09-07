module Main where

import Conduit
import Data.Text qualified as T
import Network.HTTP.Req
import Options.Applicative
import Owt

cliParser :: ParserInfo (Text, Text, Text, Bool, Bool, Either GET POST)
cliParser =
  info
    ( do
        address <-
          ( strOption
              ( long "address"
                  <> short 'a'
                  <> help "Owt server address"
                  <> value "http://localhost:9876/owt"
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
        verbose <- switch (long "verbose" <> short 'v' <> help "Verbose output")
        stream <- switch (long "stream" <> short 's' <> help "Stream output")
        method <-
          strOption (long "method" <> short 'm' <> help "HTTP method" <> value "POST")
            <&> ( T.toLower >>> \case
                    "get" -> Left GET
                    "post" -> Right POST
                    _ -> error "Invalid method"
                )
        return (address, code, kwargs, verbose, stream, method)
    )
    fullDesc

main :: IO ()
main = do
  ( address,
    code,
    kwargs,
    verbose,
    stream,
    method
    ) <-
    customExecParser (prefs showHelpOnEmpty) cliParser
  when verbose $ do
    putTextLn $ "Code:\n" <> code
    putTextLn $ "\nKwargs:\n" <> kwargs
  client <- mkOwtClient address
  if stream
    then do
      let owtF = case method of
            Left GET -> owt @GET @(OwtStream IO IO ())
            Right POST -> owt @POST @(OwtStream IO IO ())
      go <- owtF code kwargs client
      go $ foldMapMC putBS
    else do
      let owtF = case method of
            Left GET -> owt @GET
            Right POST -> owt @POST
      owtF code kwargs client >>= putBS
