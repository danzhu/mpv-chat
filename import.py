#!/usr/bin/env python3

from base64 import b64decode
from contextlib import closing
from datetime import datetime
from hashlib import sha3_256
from pathlib import Path
from typing import Any
import json
import sqlite3
import sys


INIT = """\
PRAGMA foreign_keys = ON;
PRAGMA journal_mode = WAL;
PRAGMA synchronous = NORMAL;
"""


def as_json(obj: object) -> str:
    return json.dumps(obj, separators=(",", ":"))


def as_datetime(val: str) -> str:
    d = datetime.fromisoformat(val).replace(tzinfo=None)
    return d.isoformat(
        sep=" ",
        timespec=(
            "seconds"
            if d.microsecond == 0
            else "milliseconds"
            if d.microsecond % 1000 == 0
            else "microseconds"
        ),
    )


def load(conn: sqlite3.Connection, data: Any) -> None:
    first = data["comments"][0]
    channel_id = int(first["channel_id"])
    content_type = first["content_type"]

    streamer = data["streamer"]
    streamer_id = int(streamer["id"])
    assert streamer_id == channel_id
    conn.execute(
        "INSERT INTO channel VALUES(?, ?) ON CONFLICT(id) DO NOTHING",
        (
            streamer_id,
            streamer["name"],
        ),
    )

    video = data["video"]
    video_id = video["id"]

    print("removing old data")
    conn.execute("DELETE FROM twitch_badge WHERE video_id = ?", (video_id,))
    conn.execute("DELETE FROM emote_third_party WHERE video_id = ?", (video_id,))
    conn.execute("DELETE FROM comment WHERE content_id = ?", (video_id,))
    conn.execute("DELETE FROM chapter WHERE video_id = ?", (video_id,))
    conn.execute("DELETE FROM video WHERE id = ?", (video_id,))

    print("writing video")
    conn.execute(
        "INSERT INTO video VALUES(?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
        (
            video_id,
            video["title"],
            as_datetime(video["created_at"]),
            video["start"],
            video["end"],
            video["length"],
            video.get("viewCount"),
            video.get("game"),
            channel_id,
            content_type,
        ),
    )
    print("writing games")
    conn.executemany(
        "INSERT INTO game VALUES(?, ?, ?) ON CONFLICT(id) DO NOTHING",
        (
            (
                chapter["gameId"],
                chapter["gameDisplayName"],
                chapter["gameBoxArtUrl"],
            )
            for chapter in video["chapters"]
        ),
    )
    print("writing chapters")
    conn.executemany(
        "INSERT INTO chapter VALUES(?, ?, ?, ?, ?, ?, ?, ?, ?)",
        (
            (
                chapter["id"],
                video_id,
                chapter["startMilliseconds"],
                chapter["lengthMilliseconds"],
                chapter["type"],
                chapter["description"],
                chapter["subDescription"],
                chapter["thumbnailUrl"],
                chapter["gameId"],
            )
            for chapter in video["chapters"]
        ),
    )
    print("writing users")
    conn.executemany(
        "INSERT INTO user VALUES(?, ?, ?, ?, ?, ?, ?)"
        " ON CONFLICT(id) DO UPDATE SET"
        " display_name = excluded.display_name,"
        " name = excluded.name,"
        " bio = excluded.bio,"
        " updated_at = excluded.updated_at,"
        " logo = excluded.logo"
        " WHERE excluded.updated_at > updated_at",
        (
            (
                commenter["_id"],
                commenter["display_name"],
                commenter["name"],
                commenter["bio"],
                as_datetime(commenter["created_at"]),
                as_datetime(commenter["updated_at"]),
                commenter["logo"],
            )
            for comment in data["comments"]
            for commenter in (comment["commenter"],)
        ),
    )

    def write_comment(comment):
        content_id = comment["content_id"]
        message = comment["message"]
        fragments = message["fragments"]
        assert int(comment["channel_id"]) == channel_id
        assert comment["content_type"] == content_type
        assert content_id == video_id
        assert message["body"] == "".join(f["text"] for f in fragments)
        return (
            comment["_id"],
            as_datetime(comment["created_at"]),
            content_id,
            comment["commenter"]["_id"],
            message["bits_spent"],
            as_json(fragments),
            as_json(message["user_badges"]),
            message["user_color"],
        )

    print("writing comments")
    conn.executemany(
        "INSERT INTO comment VALUES(?, ?, ?, ?, ?, ?, ?, ?)",
        map(write_comment, data["comments"]),
    )

    embedded = data["embeddedData"]
    if embedded is None:
        return

    def as_file(b64: str) -> str:
        data = b64decode(b64)
        id = sha3_256(data).hexdigest()
        conn.execute(
            "INSERT INTO file(id, data) VALUES (?, ?) ON CONFLICT(id) DO NOTHING",
            (id, data),
        )
        return id

    print("writing emotes")
    assert all(e["name"] is None for e in embedded["firstParty"])
    # TODO: update
    conn.executemany(
        "INSERT INTO emote VALUES(?, ?, ?, ?, ?) ON CONFLICT(id) DO NOTHING",
        (
            (
                emote["id"],
                emote["imageScale"],
                as_file(emote["data"]),
                emote["width"],
                emote["height"],
            )
            for emote in embedded["firstParty"]
        ),
    )
    print("writing third party emotes")
    conn.executemany(
        "INSERT INTO emote_third_party VALUES(?, ?, ?, ?, ?, ?, ?)",
        (
            (
                video_id,
                emote["name"],
                emote["id"],
                emote["imageScale"],
                as_file(emote["data"]),
                emote["width"],
                emote["height"],
            )
            for emote in embedded["thirdParty"]
        ),
    )
    print("writing twitch badges")
    conn.executemany(
        "INSERT INTO twitch_badge VALUES(?, ?, ?, ?, ?, ?)",
        (
            (
                video_id,
                badge["name"],
                ver_name,
                ver["title"],
                ver["description"],
                as_file(ver["bytes"]),
            )
            for badge in embedded["twitchBadges"]
            for ver_name, ver in badge["versions"].items()
        ),
    )
    print("writing twitch bits")
    conn.executemany(
        "INSERT INTO twitch_bits VALUES(?, ?, ?, ?, ?, ?, ?, ?)"
        " ON CONFLICT(prefix, tier) DO NOTHING",
        (
            (
                bits["prefix"],
                tier_name,
                tier["id"],
                tier["imageScale"],
                as_file(tier["data"]),
                tier["name"],
                tier["width"],
                tier["height"],
            )
            for bits in embedded["twitchBits"]
            for tier_name, tier in bits["tierList"].items()
        ),
    )


def main() -> None:
    ROOT = Path(sys.path[0])

    print("loading json")
    p = Path(sys.argv[1])
    with p.open(encoding="utf-8") as f:
        data = json.load(f)

    with closing(sqlite3.connect(ROOT / "twitch.db", isolation_level=None)) as conn:
        conn.executescript(INIT)
        conn.execute("BEGIN")
        try:
            load(conn, data)
        except Exception:
            conn.execute("ROLLBACK")
            raise
        else:
            print("committing")
            conn.execute("COMMIT")


if __name__ == "__main__":
    main()
