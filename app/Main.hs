module Main
  ( main,
  )
where

import Control.Applicative ((<**>))
import MpvChat
  ( Config (Config),
    runMpvChat,
  )
import Options.Applicative
  ( Parser,
    ParserInfo,
    argument,
    auto,
    execParser,
    fullDesc,
    help,
    helper,
    info,
    long,
    metavar,
    option,
    progDesc,
    short,
    showDefault,
    str,
    value,
  )

config :: Parser Config
config = Config <$> ipc <*> por
  where
    ipc =
      argument str $
        metavar "PATH"
          <> help "mpv ipc path"
    por =
      option auto $
        short 'p'
          <> long "port"
          <> metavar "PORT"
          <> value 8192
          <> help "server port"
          <> showDefault

program :: ParserInfo Config
program =
  info (config <**> helper) $
    fullDesc
      <> progDesc "mpv chat"

main :: IO ()
main = runMpvChat =<< execParser program
