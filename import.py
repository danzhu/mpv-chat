#!/usr/bin/env python3

import json
import re
import socket
import sqlite3
import subprocess
import sys
from base64 import b64decode
from contextlib import closing
from datetime import datetime
from hashlib import sha3_256
from pathlib import Path
from typing import Any

ROOT = Path(sys.path[0])
URL_RE = re.compile(r"https://www\.twitch\.tv/videos/(\d+)")
URL_FMT = "https://www.twitch.tv/videos/{id}"


def as_json(obj: object) -> str:
    return json.dumps(obj, separators=(",", ":"))


def as_datetime(val: str) -> str:
    d = datetime.fromisoformat(val).replace(tzinfo=None)
    return d.isoformat(
        sep=" ",
        timespec=(
            "seconds"
            if d.microsecond == 0
            else "milliseconds" if d.microsecond % 1000 == 0 else "microseconds"
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
    users = {
        commenter["_id"]: commenter
        for comment in data["comments"]
        for commenter in (comment["commenter"],)
    }
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
                uid,
                user["display_name"],
                user["name"],
                user["bio"],
                as_datetime(user["created_at"]),
                as_datetime(user["updated_at"]),
                user["logo"],
            )
            for uid, user in users.items()
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


def download_chat(id: str, p: Path) -> None:
    print("downloading chat")
    subprocess.run(
        [
            ROOT / "chat/TwitchDownloaderCLI",
            "chatdownload",
            "--id",
            id,
            "-o",
            p,
            "--embed-images",
        ],
        check=True,
    )


def import_chat(conn: sqlite3.Connection, p: Path) -> None:
    print("loading json")
    with p.open(encoding="utf-8") as f:
        data = json.load(f)

    conn.execute("BEGIN")
    try:
        load(conn, data)
    except Exception:
        conn.execute("ROLLBACK")
        raise
    else:
        print("committing")
        conn.execute("COMMIT")


def start_video(id: str, ipc_path: Path) -> None:
    print("starting video")
    with socket.socket(socket.AF_UNIX, socket.SOCK_STREAM) as s:
        s.connect(bytes(ipc_path))
        url = URL_FMT.format(id=id)
        msg = as_json({"command": ["loadfile", url]}) + "\n"
        s.sendall(msg.encode())
        s.shutdown(socket.SHUT_RDWR)


def main() -> None:
    from argparse import ArgumentParser

    p = ArgumentParser()
    p.add_argument("id", help="video id or url")
    args = p.parse_args()
    id: str = args.id

    with closing(sqlite3.connect(ROOT / "twitch.db", isolation_level=None)) as conn:
        conn.execute("PRAGMA foreign_keys = ON")
        conn.execute("PRAGMA journal_mode = WAL")
        conn.execute("PRAGMA synchronous = NORMAL")

        match = URL_RE.fullmatch(id)
        if match is not None:
            id = match.group(1)

        path = ROOT / f"chat/{id}.json"
        stored = bool(
            conn.execute("SELECT 1 FROM video WHERE id = ?", (id,)).fetchone()
        )

        if not stored:
            if not path.exists():
                download_chat(id, path)
            import_chat(conn, path)

    ipc_path = ROOT / "mpv"
    if ipc_path.exists():
        start_video(id, ipc_path)


if __name__ == "__main__":
    main()
