{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeOperators              #-}

module Bitcoin where

import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Either
import           Control.Monad.Trans.Reader
import           Data.Aeson
import           Data.Aeson.Types           hiding (Options)
import qualified Data.ByteString.Base64     as B64 (encode)
import qualified Data.ByteString.Char8      as BC (pack)
import           Data.Monoid                ((<>))
import           Data.Proxy
import           Data.Text                  (Text)
import           Data.Text.Encoding         (decodeUtf8)
import           Servant.API
import           Servant.Client

-----------------------------------------------------------------------------

type Host = String
type Port = Int

data BitcoindRequest =
    GetBlockCount
  | GetBlockHash Int
  | GetInfo
    deriving (Read,Show)

data Config = Config
  { configUsername :: String
  , configPassword :: String
  , configHost :: Host
  , configPort :: Port
  } deriving Show

-----------------------------------------------------------------------------

type BitcoinT = ReaderT (Config,Text) (EitherT ServantError IO)

-----------------------------------------------------------------------------

runBitcoinT :: Config -> BitcoinT a -> IO (Either ServantError a)
runBitcoinT cfg@Config{..} = runEitherT . flip runReaderT (cfg,authText)
 where
  authText = decodeUtf8 ("Basic " <> B64.encode unpw)
  unpw     = BC.pack (configUsername <> ":" <> configPassword)

-----------------------------------------------------------------------------

bitcoindRequest :: Text -> [Value] -> Value
bitcoindRequest method params = object
  [ "jsonrpc" .= ("1.0" :: Text)
  , "method"  .= method
  , "params"  .= params
  , "id"      .= ("jsonrpc" :: Text)
  ]

instance ToJSON BitcoindRequest where
  toJSON r = case r of
    GetBlockCount  -> bitcoindRequest "getblockcount" []
    GetBlockHash i -> bitcoindRequest "getblockhash" [toJSON i]
    GetInfo        -> bitcoindRequest "getinfo" []

-----------------------------------------------------------------------------

type API = Header "Authorization" Text
           :> ReqBody '[JSON] BitcoindRequest
           :> Post '[JSON] Value

api :: Proxy API
api = Proxy

callBitcoind :: Host
                -> Port 
                -> Maybe Text
                -> BitcoindRequest
                -> EitherT ServantError IO Value
callBitcoind host port = client api (BaseUrl Http host port)

-----------------------------------------------------------------------------

bd :: BitcoindRequest -> BitcoinT Value
bd req = do
  (Config{..},authText) <- ask
  lift $ callBitcoind configHost configPort (Just authText) req

parseResponse :: FromJSON a => Value -> Parser a
parseResponse = withObject "response" $ \o -> do
  r <- o .: "result"
  return r

decodeResponse :: FromJSON a => Value -> BitcoinT (Maybe a)
decodeResponse = return . parseMaybe parseResponse

getBlockCount :: BitcoinT (Maybe Int)
getBlockCount = bd GetBlockCount >>= decodeResponse

getBlockHash :: Int -> BitcoinT (Maybe Text)
getBlockHash = bd . GetBlockHash >=> decodeResponse

getInfo :: BitcoinT Value
getInfo = bd GetInfo




