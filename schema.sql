CREATE TABLE IF NOT EXISTS channel(
    id INTEGER PRIMARY KEY,
    name TEXT NOT NULL
) STRICT;

CREATE TABLE IF NOT EXISTS game(
    id INTEGER PRIMARY KEY,
    display_name TEXT NOT NULL,
    box_art_url TEXT NOT NULL
) STRICT;

CREATE TABLE IF NOT EXISTS video(
    id INTEGER PRIMARY KEY,
    title TEXT NOT NULL,
    created_at TEXT NOT NULL,
    start INTEGER NOT NULL,
    end INTEGER NOT NULL,
    length INTEGER NOT NULL,
    view_count INTEGER,
    game TEXT,
    channel_id INTEGER NOT NULL REFERENCES channel(id),
    content_type TEXT NOT NULL
) STRICT;

CREATE TABLE IF NOT EXISTS chapter(
    id TEXT NOT NULL PRIMARY KEY,
    video_id INTEGER NOT NULL REFERENCES video(id),
    start_milliseconds INTEGER NOT NULL,
    length_milliseconds INTEGER NOT NULL,
    type TEXT NOT NULL,
    description TEXT NOT NULL,
    sub_description TEXT NOT NULL,
    thumbnail_url TEXT NOT NULL,
    game_id INTEGER NOT NULL REFERENCES game(id)
) STRICT;

CREATE TABLE IF NOT EXISTS user(
    id INTEGER PRIMARY KEY,
    display_name TEXT NOT NULL,
    name TEXT NOT NULL,
    bio TEXT,
    created_at TEXT NOT NULL,
    updated_at TEXT NOT NULL,
    logo TEXT NOT NULL
) STRICT;

CREATE TABLE IF NOT EXISTS comment(
    id TEXT NOT NULL PRIMARY KEY,
    created_at TEXT NOT NULL,
    content_id INTEGER NOT NULL REFERENCES video(id),
    commenter INTEGER NOT NULL REFERENCES user(id),
    bits_spent INTEGER NOT NULL,
    fragments TEXT NOT NULL,
    user_badges TEXT NOT NULL,
    user_color TEXT
) STRICT;

CREATE INDEX IF NOT EXISTS comment_index
    ON comment(content_id, created_at);

CREATE TABLE IF NOT EXISTS emote(
    id TEXT NOT NULL,
    image_scale INTEGER NOT NULL,
    data BLOB NOT NULL,
    url TEXT,
    width INTEGER NOT NULL,
    height INTEGER NOT NULL,
    PRIMARY KEY(id)
) STRICT;

CREATE TABLE IF NOT EXISTS emote_third_party(
    name TEXT NOT NULL,
    id TEXT NOT NULL,
    image_scale INTEGER NOT NULL,
    data BLOB NOT NULL,
    url TEXT,
    width INTEGER NOT NULL,
    height INTEGER NOT NULL,
    PRIMARY KEY(name)
) STRICT;

CREATE TABLE IF NOT EXISTS twitch_badge(
    name TEXT NOT NULL,
    version INTEGER NOT NULL,
    title TEXT NOT NULL,
    description TEXT NOT NULL,
    bytes BLOB NOT NULL,
    PRIMARY KEY(name, version)
) STRICT;

CREATE TABLE IF NOT EXISTS twitch_bits(
    prefix TEXT NOT NULL,
    tier INTEGER NOT NULL,
    id TEXT NOT NULL,
    image_scale INTEGER NOT NULL,
    data BLOB NOT NULL,
    name TEXT NOT NULL,
    url TEXT,
    width INTEGER NOT NULL,
    height INTEGER NOT NULL,
    PRIMARY KEY(prefix, tier)
) STRICT;

CREATE TABLE IF NOT EXISTS follow(
    id INTEGER PRIMARY KEY,
    name TEXT NOT NULL,
    highlight INTEGER NOT NULL DEFAULT 0
) STRICT;
