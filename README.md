# Twitch Mpv Chat Viewer

Disclaimer: not affiliated with Twitch or its services in any way.

## Build

Haskell [Stack](https://docs.haskellstack.org/en/stable/README/) is required.

``` shell
stack build
```

## Usage

TODO: document chat import process

In project root directory, run:

``` shell
mpv --input-ipc-server=mpv_ipc <twitch_vod_url>
stack exec -- mpv-chat --ipc-path=mpv_ipc
firefox http://localhost:8192/
```

See `stack exec -- mpv-chat --help` for more options.
