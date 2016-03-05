{-# LANGUAGE RecordWildCards #-}

module Client where

import           Control.Monad
import           Control.Monad.IO.Class
import           Options.Applicative

import           Bitcoin

-----------------------------------------------------------------------------

data Options = Options
  { optionsCommand :: BitcoindRequest
  , optionsConfig :: Config
  } deriving Show

getBlockHashP :: Parser BitcoindRequest
getBlockHashP = GetBlockHash <$> (argument auto (metavar "BLOCKDEPTH"))

options :: Parser Options
options = Options
  <$> subparser
      (   command "getbestblockhash"
          (info (pure GetBestBlockHash)
                (progDesc "Get best block hash"))
      <>  command "getblockcount"
          (info (pure GetBlockCount)
                (progDesc "Get block count"))
      <>  command "getblockchaininfo"
          (info (pure GetBlockchainInfo)
                (progDesc "Get blockchain info"))
      <>  command "getblockhash"
          (info getBlockHashP
                (progDesc "Get block hash"))
      <>  command "getinfo"
          (info (pure GetInfo)
                (progDesc "Get info"))
      ) 
  <*> (Config 
      <$> strOption
            ( long "username"
              <> short 'u'
              <> metavar "USERNAME"
              <> help "RPC username"
              <> value "bitcoinrpc")
      <*> strOption
            ( long "password"
              <> short 'p'
              <> metavar "PASSWORD"
              <> help "RPC password"
              <> value "")
      <*> strOption
            ( long "host"
              <> short 'h'
              <> metavar "HOST"
              <> help "RPC host"
              <> value "127.0.0.1")
      <*> option auto 
            ( long "port"
              <> short 't'
              <> metavar "PORT"
              <> help "RPC port"
              <> value 18332)
      )

-----------------------------------------------------------------------------

main :: IO ()
main = execParser opts >>= run
 where
  opts = info (helper <*> options)
              (fullDesc
               <> progDesc "Invoke bitcoind rpc api"
               <> header "bitcoind client")

run :: Options -> IO ()
run Options{..} = void . runBitcoinT optionsConfig $ 
  bd optionsCommand >>= liftIO . print