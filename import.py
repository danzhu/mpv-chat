#!/usr/bin/env python3

import json
import logging
import re
import sqlite3
import subprocess
import sys
from base64 import b64decode
from contextlib import closing
from datetime import datetime
from hashlib import sha3_256
from pathlib import Path
from typing import Any

logger = logging.getLogger("__name__")

ROOT = Path(sys.path[0])
SCHEMA = ROOT / "schema.sql"
STMT_RE = re.compile(r"\n*;\n*")
URL_RE = re.compile(r"https://www\.twitch\.tv/videos/(\d+)(\?.*)?")
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

    logger.info("writing video")
    conn.execute(
        "INSERT INTO video VALUES(?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
        (
            video_id,
            video["title"],
            video.get("description"),
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
    logger.info("writing games")
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
    logger.info("writing chapters")
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
    logger.info("writing users")
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

    logger.info("writing comments")
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

    logger.info("writing emotes")
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
    logger.info("writing third party emotes")
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
    logger.info("writing twitch badges")
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
    logger.info("writing twitch bits")
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


def download_video(id: int) -> None:
    logger.info("downloading video")
    subprocess.run(
        ["yt-dlp", "--embed-metadata", URL_FMT.format(id=id)],
        cwd="vod",
        check=True,
    )


def download_chat(id: int, p: Path) -> None:
    logger.info("downloading chat")
    subprocess.run(
        [
            "TwitchDownloaderCLI",
            "chatdownload",
            "--id",
            str(id),
            "-o",
            p,
            "--embed-images",
        ],
        check=True,
    )


def remove_chat(conn: sqlite3.Connection, id: int) -> None:
    logger.info("removing old data")
    conn.execute("DELETE FROM twitch_badge WHERE video_id = ?", (id,))
    conn.execute("DELETE FROM emote_third_party WHERE video_id = ?", (id,))
    conn.execute("DELETE FROM comment WHERE content_id = ?", (id,))
    conn.execute("DELETE FROM chapter WHERE video_id = ?", (id,))
    conn.execute("DELETE FROM video WHERE id = ?", (id,))


def import_chat(conn: sqlite3.Connection, p: Path) -> None:
    logger.info("loading json")
    with p.open(encoding="utf-8") as f:
        data = json.load(f)
    load(conn, data)


def main() -> None:
    from argparse import ArgumentParser, BooleanOptionalAction

    p = ArgumentParser()
    p.add_argument("id", help="video id or url")
    p.add_argument(
        "--video",
        action=BooleanOptionalAction,
        default=False,
        help="download video as well",
    )
    p.add_argument("-f", "--force", action=BooleanOptionalAction, default=False)
    args = p.parse_args()
    id_str: str = args.id
    video: bool = args.video
    force: bool = args.force

    logging.basicConfig(format="[%(levelname)s] %(message)s", level=logging.INFO)

    match = URL_RE.fullmatch(id_str)
    if match is not None:
        id_str = match.group(1)
    try:
        id = int(id_str)
    except ValueError:
        print(f"invalid video id {id_str!r}")
        sys.exit(2)

    logger.info(f"video id {id}")

    if video:
        download_video(id)

    with closing(sqlite3.connect(ROOT / "twitch.db", isolation_level=None)) as conn:
        conn.execute("PRAGMA foreign_keys = ON")
        conn.execute("PRAGMA journal_mode = WAL")
        conn.execute("PRAGMA synchronous = NORMAL")

        conn.execute("BEGIN")
        try:
            logger.info("updating schema")
            stmts = STMT_RE.split(SCHEMA.read_text())
            for stmt in stmts:
                if not stmt:
                    continue
                conn.execute(stmt)

            stored = bool(
                conn.execute("SELECT 1 FROM video WHERE id = ?", (id,)).fetchone()
            )
            if stored and not force:
                logger.info("already stored")
            else:
                path = ROOT / f"chat/{id}.json"
                if path.exists():
                    logger.info("already downloaded")
                else:
                    download_chat(id, path)
                if force:
                    remove_chat(conn, id)
                import_chat(conn, path)
        except Exception:
            conn.execute("ROLLBACK")
            raise
        else:
            logger.info("committing")
            conn.execute("COMMIT")


if __name__ == "__main__":
    main()
