# Twitch Mpv Chat Viewer

Disclaimer: not affiliated with Twitch or its services in any way.

## Build

Haskell [Stack](https://docs.haskellstack.org/en/stable/README/) is required.

``` shell
stack build
```

## Dependencies

Download [TwitchDownloaderCLI](https://github.com/lay295/TwitchDownloader) and
put the executable in `$PATH`.

## Usage

In project root directory, run:

``` shell
# download chat and import into db
./import.py <twitch_vod_url>
# start mpv with ipc server
mpv --input-ipc-server=mpv <twitch_vod_url> --idle=yes --force-window=yes
# start chat viewer server
stack exec -- mpv-chat mpv
# open chat in browser
firefox http://localhost:8192/
```

See `stack exec -- mpv-chat --help` for more options.
