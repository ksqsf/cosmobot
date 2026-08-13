#!/usr/bin/env python3
"""Minimal protocol fixture; later phases add the trusted Python frontend."""

import io
import json
import sys

MAX_RPC_BYTES = 4 * 1024 * 1024


def read_frame(stream):
    frame = stream.readline(MAX_RPC_BYTES + 2)
    if not frame.endswith(b"\n"):
        raise ValueError("JSON-RPC frame is missing its terminating newline")
    payload = frame[:-1]
    if len(payload) > MAX_RPC_BYTES:
        raise ValueError("JSON-RPC frame exceeds 4 MiB")
    if b"\n" in payload:
        raise ValueError("JSON-RPC frame contains an unescaped newline")
    return json.loads(payload)


def write_frame(stream, message):
    payload = json.dumps(message, ensure_ascii=False, allow_nan=False, separators=(",", ":")).encode()
    if len(payload) > MAX_RPC_BYTES:
        raise ValueError("JSON-RPC frame exceeds 4 MiB")
    stream.write(payload + b"\n")
    stream.flush()


def serve_once(stdin=sys.stdin.buffer, stdout=sys.stdout.buffer):
    request = read_frame(stdin)
    write_frame(
        stdout,
        {
            "jsonrpc": "2.0",
            "id": request["id"],
            "result": {"kind": "completed", "content": request["params"]["code"]},
        },
    )


def self_test():
    request = {
        "jsonrpc": "2.0",
        "id": "host:run",
        "method": "python.run",
        "params": {"code": "print('你好')\nprint('line two')"},
    }
    request_frame = io.BytesIO()
    write_frame(request_frame, request)
    request_frame.seek(0)
    response_frame = io.BytesIO()
    serve_once(request_frame, response_frame)
    response_frame.seek(0)
    assert read_frame(response_frame) == {
        "jsonrpc": "2.0",
        "id": "host:run",
        "result": {
            "kind": "completed",
            "content": "print('你好')\nprint('line two')",
        },
    }

    completed = {"kind": "completed", "content": "你好\nline two\n"}
    failed = {"kind": "failed", "message": "cannot continue"}
    tool_results = [
        {"ok": True, "content": "done"},
        {
            "ok": False,
            "failure": {
                "category": "permission_denied",
                "message": "denied",
                "detail": "tool is hidden",
            },
        },
    ]
    for example in (completed, failed, tool_results):
        frame = io.BytesIO()
        write_frame(frame, example)
        frame.seek(0)
        assert read_frame(frame) == example

    for invalid in (b"{}", b"{not json}\n", b" " * (MAX_RPC_BYTES + 1) + b"\n"):
        try:
            read_frame(io.BytesIO(invalid))
        except (ValueError, json.JSONDecodeError):
            pass
        else:
            raise AssertionError("invalid frame was accepted")


if __name__ == "__main__":
    if sys.argv[1:] == ["--self-test"]:
        self_test()
    else:
        serve_once()
