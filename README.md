# Twitch Mpv Chat Viewer

Disclaimer: not affiliated with Twitch or its services in any way.

## Build

Haskell [Stack](https://docs.haskellstack.org/en/stable/README/) is required.

``` shell
stack build
```

## Usage

A Twitch client ID is required; see Twitch developer documentation for how to
acquire one.

In project root directory, run:

``` shell
mpv --input-ipc-server=mpv_ipc <twitch_vod_url>
stack exec -- mpv-chat --ipc-path=mpv_ipc --client-id=<twitch_client_id>
firefox http://localhost:8192/
```

See `stack exec -- mpv-chat --help` for more options.
