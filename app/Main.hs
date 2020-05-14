module Main
  ( main
  ) where

import           Network.Twitch.Chat            ( Config(Config)
                                                , run
                                                )
import qualified Network.Twitch.Twitch         as Tv

import           Control.Applicative            ( (<**>) )
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
config = Config <$> ipc <*> auth <*> por where
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

program :: ParserInfo Config
program = info (config <**> helper) $
  fullDesc <>
  progDesc "mpv chat"

main :: IO ()
main = run =<< execParser program
