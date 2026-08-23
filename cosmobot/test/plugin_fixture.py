#!/usr/bin/env python3

import json
import os
from pathlib import Path
import sys
import time
import tomllib


def send(message):
    sys.stdout.write(json.dumps(message, separators=(",", ":")) + "\n")
    sys.stdout.flush()


def response(request_id, result):
    send({"jsonrpc": "2.0", "id": request_id, "result": result})


def error(request_id, code, message, data=None):
    payload = {"code": code, "message": message}
    if data is not None:
        payload["data"] = data
    send({"jsonrpc": "2.0", "id": request_id, "error": payload})


config_path = Path(os.environ["COSMOBOT_PLUGIN_CONFIG"])
config = tomllib.loads(config_path.read_text())
fixture = config.get("fixture", {})
mode = fixture.get("mode", "normal")
finalize_path = fixture.get("finalize_path")
late_path = fixture.get("late_path")
transient_marker = fixture.get("transient_marker")

for raw_line in sys.stdin:
    request = json.loads(raw_line)
    request_id = request.get("id")
    method = request.get("method")
    params = request.get("params") or {}

    if method == "plugin.initialize":
        if mode == "crash":
            raise SystemExit(23)
        if mode == "malformed":
            sys.stdout.write("{not-json}\n")
            sys.stdout.flush()
            continue
        if mode == "malformed-manifest":
            response(request_id, {"protocolVersion": "1.0.0"})
            continue
        if mode == "transient-always" or (
            mode == "transient-once" and transient_marker and not Path(transient_marker).exists()
        ):
            if transient_marker:
                Path(transient_marker).write_text("failed once")
            error(request_id, -32000, "transient startup", {"transient": True})
            if mode == "transient-once":
                os._exit(24)
            continue
        response(
            request_id,
            {
                "protocolVersion": "1.0.0",
                "pluginVersion": "1.2.3",
                "routes": [
                    {
                        "id": "echo",
                        "help": {"label": "!echo <text>", "description": "Echo text."},
                        "filter": "echo-command",
                        "disposition": "stop",
                        "access": "allowed",
                    }
                ],
                "filters": {"echo-command": {"command": "!echo"}},
                "tools": [
                    {
                        "name": "echo",
                        "description": "Echo text.",
                        "schema": {
                            "type": "object",
                            "properties": {"text": {"type": "string"}},
                            "required": ["text"],
                            "additionalProperties": False,
                        },
                    }
                ],
                "requestedCapabilities": ["chat"],
            },
        )
        if mode == "leader-exit":
            child = os.fork()
            if child == 0:
                time.sleep(1)
                if late_path:
                    Path(late_path).write_text("survived")
                raise SystemExit(0)
            os._exit(24)
        if mode == "transient-process-once" and transient_marker and not Path(transient_marker).exists():
            Path(transient_marker).write_text("failed once")
            time.sleep(0.05)
            os._exit(75)
    elif method == "plugin.route.invoke":
        if mode == "route-timeout":
            time.sleep(3)
        invocation_id = params["invocationId"]
        callback_id = "callback:" + invocation_id
        send(
            {
                "jsonrpc": "2.0",
                "id": callback_id,
                "method": "chat.reply",
                "params": {"invocationId": invocation_id, "text": params.get("arguments", "")},
            }
        )
        next(sys.stdin)  # synchronized host callback response
        response(request_id, {"status": "success", "content": "", "imageUrls": []})
        if mode == "late-callback":
            time.sleep(0.1)
            late_id = "late:" + invocation_id
            send(
                {
                    "jsonrpc": "2.0",
                    "id": late_id,
                    "method": "chat.reply",
                    "params": {"invocationId": invocation_id, "text": "late"},
                }
            )
            late_response = json.loads(next(sys.stdin))
            if late_path:
                Path(late_path).write_text(json.dumps(late_response))
    elif method == "plugin.tool.invoke":
        arguments = params.get("arguments")
        if not isinstance(arguments, dict) or not isinstance(arguments.get("text"), str):
            error(request_id, -32602, "text must be a string")
        else:
            response(
                request_id,
                {"status": "success", "content": arguments["text"], "imageUrls": []},
            )
    elif method == "plugin.shutdown":
        if finalize_path:
            with Path(finalize_path).open("a") as output:
                output.write("finalize\n")
        response(request_id, None)
        break
    else:
        error(request_id, -32601, "method not found")
