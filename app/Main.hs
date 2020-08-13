module Main
  ( main
  ) where

import           Control.Applicative            ( (<**>) )
import           MpvChat                        ( Config(Config)
                                                , runMpvChat
                                                )
import           MpvChat.Prelude
import qualified Network.Twitch                as Tv
import           Options.Applicative            ( Parser
                                                , ParserInfo
                                                , auto
                                                , execParser
                                                , fullDesc
                                                , help
                                                , helper
                                                , info
                                                , long
                                                , metavar
                                                , option
                                                , progDesc
                                                , short
                                                , showDefault
                                                , str
                                                , value
                                                )

auth :: Parser Tv.Auth
auth = Tv.Auth <$> cid where
  cid = option str $
    long "client-id" <>
    metavar "ID" <>
    help "twitch client id"

config :: Parser Config
config = Config <$> ipc <*> auth <*> por <*> hls where
  ipc = option str $
    long "ipc-path" <>
    metavar "PATH" <>
    help "mpv ipc path"
  por = option auto $
    short 'p' <>
    long "port" <>
    metavar "PORT" <>
    value 8192 <>
    help "server port" <>
    showDefault
  hls = map setFromList $ many $ option str $
    long "highlight" <>
    metavar "NAME" <>
    help "highlight user"

program :: ParserInfo Config
program = info (config <**> helper) $
  fullDesc <>
  progDesc "mpv chat"

main :: IO ()
main = runMpvChat =<< execParser program
