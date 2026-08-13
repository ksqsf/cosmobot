#!/usr/bin/env python3
"""Trusted standard-library supervisor for one run_python invocation."""

import json
import ctypes
import os
import selectors
import signal
import socket
import sys
import traceback
import types

MAX_RPC_BYTES = 4 * 1024 * 1024
MAX_CONTROL_BYTES = 8 * 1024
MAX_STDOUT_BYTES = 1024 * 1024
MAX_STDERR_BYTES = 64 * 1024
TRUNCATION_MARKER = b"\n[output truncated]\n"
PR_SET_DUMPABLE = 4


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
    payload = json.dumps(
        message, ensure_ascii=False, allow_nan=False, separators=(",", ":")
    ).encode()
    if len(payload) > MAX_RPC_BYTES:
        raise ValueError("JSON-RPC frame exceeds 4 MiB")
    write_all(stream, payload + b"\n")
    stream.flush()


def write_all(stream, data):
    remaining = memoryview(data)
    while remaining:
        written = stream.write(remaining)
        if not written:
            raise EOFError("JSON-RPC stream closed during write")
        remaining = remaining[written:]


def bounded_text(value, label, allow_empty=True):
    if not isinstance(value, str):
        raise TypeError(f"{label} must be a string")
    if not allow_empty and not value:
        raise ValueError(f"{label} must not be empty")
    if len(value) > MAX_CONTROL_BYTES or len(value.encode()) > MAX_CONTROL_BYTES:
        raise ValueError(f"{label} exceeds 8 KiB")
    return value


class RunToolException(Exception):
    def __init__(self, name, index, failure, results):
        self.name = name
        self.index = index
        self.failure = failure
        self.results = results
        super().__init__(failure["message"])


class BackToAgent(BaseException):
    def __init__(self, prompt):
        self.prompt = bounded_text(prompt, "BackToAgent prompt")
        super().__init__(self.prompt)


class _Fail(BaseException):
    def __init__(self, message):
        self.message = bounded_text(message, "failure message", allow_empty=False)
        super().__init__(self.message)


class ChildClient:
    def __init__(self, channel):
        self.channel = channel

    def run(self, calls):
        write_frame(self.channel, {"kind": "tools", "calls": calls})
        response = read_frame(self.channel)
        if isinstance(response, dict) and "error" in response:
            failure = response["error"]
            validate_failure(failure)
            raise RunToolException(None, None, failure, [])
        results = response.get("results") if isinstance(response, dict) else None
        if not isinstance(results, list) or len(results) != len(calls):
            raise ValueError("nested tool result count does not match its request")
        for result in results:
            validate_result(result)
        return results


def validate_result(result):
    if not isinstance(result, dict) or type(result.get("ok")) is not bool:
        raise ValueError("invalid nested tool result envelope")
    if result["ok"]:
        if set(result) != {"ok", "content"} or not isinstance(result["content"], str):
            raise ValueError("invalid nested tool success envelope")
    else:
        failure = result.get("failure")
        if set(result) != {"ok", "failure"} or not isinstance(failure, dict):
            raise ValueError("invalid nested tool failure envelope")
        validate_failure(failure)


def validate_failure(failure):
    if not isinstance(failure, dict) or set(failure) != {
        "category",
        "message",
        "detail",
    } or not all(isinstance(failure[key], str) for key in failure):
        raise ValueError("invalid nested tool failure detail")


def validate_calls(calls):
    if not isinstance(calls, list) or not calls:
        raise ValueError("calls must be a non-empty list")
    if len(calls) > 16:
        raise ValueError("calls accepts at most 16 entries")
    for call in calls:
        if not isinstance(call, dict) or set(call) != {"name", "args"}:
            raise ValueError("each call must contain exactly name and args")
        if not isinstance(call["name"], str) or not call["name"]:
            raise ValueError("tool name must be a non-empty string")
    json.dumps(calls, ensure_ascii=False, allow_nan=False)
    return calls


def install_module(client):
    module = types.ModuleType("cosmobot")

    def run_tools(calls):
        calls = validate_calls(calls)
        results = client.run(calls)
        for index, result in enumerate(results):
            if not result["ok"]:
                raise RunToolException(
                    calls[index]["name"], index, result["failure"], results
                )
        return results

    def run_tool(name, args_json):
        return run_tools([{"name": name, "args": args_json}])[0]

    def complete(content=""):
        raise BackToAgent(content)

    def fail(message):
        raise _Fail(message)

    module.RunToolException = RunToolException
    module.BackToAgent = BackToAgent
    module.run_tool = run_tool
    module.run_tools = run_tools
    module.complete = complete
    module.fail = fail
    sys.modules["cosmobot"] = module


def reject_file_urls(event, args):
    if event == "urllib.Request" and str(args[0]).lower().startswith("file:"):
        raise PermissionError("file:// URLs are disabled in run_python")


def parse_run_request(request):
    if not isinstance(request, dict) or request.get("jsonrpc") != "2.0":
        raise ValueError("invalid python.run JSON-RPC request")
    if request.get("id") != "host:run" or request.get("method") != "python.run":
        raise ValueError("unexpected Python worker request")
    if set(request) != {"jsonrpc", "id", "method", "params"}:
        raise ValueError("invalid python.run request fields")
    params = request["params"]
    if not isinstance(params, dict) or set(params) != {"code"}:
        raise ValueError("python.run params must contain exactly code")
    if not isinstance(params["code"], str):
        raise ValueError("python.run code must be a string")
    return params["code"]


def child_main(code, channel_fd, stdout_fd, stderr_fd):
    null_fd = os.open("/dev/null", os.O_RDONLY)
    os.dup2(null_fd, 0)
    os.close(null_fd)
    os.dup2(stdout_fd, 1)
    os.dup2(stderr_fd, 2)
    os.closerange(3, channel_fd)
    os.closerange(channel_fd + 1, os.sysconf("SC_OPEN_MAX"))
    channel = os.fdopen(channel_fd, "r+b", buffering=0)
    sys.stdout = sys.__stdout__ = os.fdopen(os.dup(1), "w", encoding="utf-8")
    sys.stderr = sys.__stderr__ = os.fdopen(os.dup(2), "w", encoding="utf-8")
    sys.addaudithook(reject_file_urls)
    install_module(ChildClient(channel))
    try:
        exec(compile(code, "<run_python>", "exec"), {})
    except BackToAgent as completed:
        write_frame(channel, {"kind": "completed", "content": completed.prompt})
    except _Fail as failed:
        write_frame(channel, {"kind": "failed", "message": failed.message})
    except BaseException as error:
        traceback.print_exception(error)
        sys.stderr.flush()
        raise SystemExit(1)
    else:
        sys.stdout.flush()
        write_frame(channel, {"kind": "fallthrough"})


class Capture:
    def __init__(self, limit):
        self.limit = limit
        self.data = bytearray()
        self.truncated = False

    def append(self, chunk):
        if self.truncated:
            return
        if len(self.data) + len(chunk) <= self.limit:
            self.data.extend(chunk)
            return
        marker = TRUNCATION_MARKER[: self.limit]
        prefix_limit = self.limit - len(marker)
        self.data.extend(chunk[: max(0, prefix_limit - len(self.data))])
        del self.data[prefix_limit:]
        self.data[:] = bytes(self.data).decode(errors="ignore").encode()
        self.data.extend(marker)
        self.truncated = True

    def text(self):
        return bytes(self.data).decode(errors="ignore")


def run_child(code, host_in, host_out, host_stderr):
    if ctypes.CDLL(None, use_errno=True).prctl(PR_SET_DUMPABLE, 0, 0, 0, 0) != 0:
        raise OSError(ctypes.get_errno(), "failed to protect Python supervisor fds")
    parent_channel, child_channel = socket.socketpair()
    stdout_read, stdout_write = os.pipe()
    stderr_read, stderr_write = os.pipe()
    pid = os.fork()
    if pid == 0:
        parent_channel.close()
        os.close(stdout_read)
        os.close(stderr_read)
        try:
            child_main(code, child_channel.detach(), stdout_write, stderr_write)
        except BaseException:
            traceback.print_exc()
            os._exit(1)
        else:
            os._exit(0)
    child_channel.close()
    os.close(stdout_write)
    os.close(stderr_write)
    try:
        return supervise(pid, parent_channel, stdout_read, stderr_read, host_in, host_out, host_stderr)
    except BaseException:
        try:
            os.kill(pid, signal.SIGKILL)
        except ProcessLookupError:
            pass
        try:
            os.waitpid(pid, 0)
        except ChildProcessError:
            pass
        raise


def supervise(pid, channel, stdout_fd, stderr_fd, host_in, host_out, host_stderr):
    selector = selectors.DefaultSelector()
    channel_fd = channel.detach()
    channel_file = os.fdopen(channel_fd, "r+b", buffering=0)
    selector.register(channel_fd, selectors.EVENT_READ, "control")
    selector.register(stdout_fd, selectors.EVENT_READ, "stdout")
    selector.register(stderr_fd, selectors.EVENT_READ, "stderr")
    stdout = Capture(MAX_STDOUT_BYTES)
    stderr = Capture(MAX_STDERR_BYTES)
    terminal = None
    next_id = 1
    try:
        while selector.get_map():
            for key, _ in selector.select():
                terminal, next_id = consume_event(
                    key,
                    selector,
                    channel_fd,
                    channel_file,
                    stdout,
                    stderr,
                    host_in,
                    host_out,
                    terminal,
                    next_id,
                )
    finally:
        selector.close()
    _, status = os.waitpid(pid, 0)
    if terminal is None or status != 0:
        if stderr.data:
            host_stderr.write(bytes(stderr.data))
            host_stderr.flush()
        return None
    if terminal["kind"] == "fallthrough":
        return {
            "kind": "completed",
            "content": stdout.text() or "Python completed successfully.",
        }
    if terminal["kind"] == "completed":
        return {
            "kind": "completed",
            "content": bounded_text(terminal.get("content"), "completion"),
        }
    return {
        "kind": "failed",
        "message": bounded_text(terminal.get("message"), "failure message", False),
    }


def consume_event(
    key,
    selector,
    channel_fd,
    channel_file,
    stdout,
    stderr,
    host_in,
    host_out,
    terminal,
    next_id,
):
    if key.data == "control":
        try:
            message = read_frame(channel_file)
        except (EOFError, ValueError, json.JSONDecodeError):
            selector.unregister(channel_fd)
            channel_file.close()
            return terminal, next_id
        kind = message.get("kind") if isinstance(message, dict) else None
        if kind == "tools" and terminal is None:
            calls = validate_calls(message.get("calls"))
            write_frame(
                host_out,
                {
                    "jsonrpc": "2.0",
                    "id": next_id,
                    "method": "tools.run",
                    "params": {"calls": calls},
                },
            )
            response = read_frame(host_in)
            if response.get("jsonrpc") != "2.0" or response.get("id") != next_id:
                raise ValueError("invalid tools.run response")
            if "error" in response:
                write_frame(
                    channel_file,
                    {
                        "error": {
                            "category": "external_service_unavailable",
                            "message": "Host rejected the nested tool request.",
                            "detail": str(response["error"])[:500],
                        }
                    },
                )
                return terminal, next_id + 1
            results = response.get("result")
            if not isinstance(results, list):
                raise ValueError("tools.run response has no result array")
            write_frame(channel_file, {"results": results})
            return terminal, next_id + 1
        if kind in {"fallthrough", "completed", "failed"} and terminal is None:
            return message, next_id
        raise ValueError("invalid child protocol message")
    chunk = os.read(key.fd, 65536)
    if chunk:
        (stdout if key.data == "stdout" else stderr).append(chunk)
    else:
        selector.unregister(key.fileobj)
        os.close(key.fd)
    return terminal, next_id


def serve_once(
    stdin=sys.stdin.buffer, stdout=sys.stdout.buffer, stderr=sys.stderr.buffer
):
    code = parse_run_request(read_frame(stdin))
    result = run_child(code, stdin, stdout, stderr)
    if result is None:
        return False
    response = {"jsonrpc": "2.0", "id": "host:run", "result": result}
    write_frame(stdout, fit_frame(response))
    return True


def fit_frame(response):
    if len(json.dumps(response, ensure_ascii=False, separators=(",", ":")).encode()) <= MAX_RPC_BYTES:
        return response
    result = response.get("result", {})
    if result.get("kind") != "completed":
        raise ValueError("terminal JSON-RPC frame exceeds 4 MiB")
    marker = TRUNCATION_MARKER.decode()
    empty = {**response, "result": {**result, "content": ""}}
    budget = MAX_RPC_BYTES - len(
        json.dumps(empty, ensure_ascii=False, separators=(",", ":")).encode()
    )
    marker_size = json_string_bytes(marker)
    used = 0
    prefix = []
    for char in result["content"]:
        size = json_string_bytes(char)
        if used + size + marker_size > budget:
            break
        prefix.append(char)
        used += size
    return {
        **response,
        "result": {**result, "content": "".join(prefix) + marker},
    }


def json_string_bytes(text):
    size = 0
    for char in text:
        codepoint = ord(char)
        if char in {'"', "\\"}:
            size += 2
        elif codepoint < 0x20:
            size += 2 if char in "\b\f\n\r\t" else 6
        else:
            size += len(char.encode())
    return size


def main():
    if sys.argv[1:]:
        raise SystemExit("usage: cosmobot_worker.py")
    return 0 if serve_once() else 1


if __name__ == "__main__":
    raise SystemExit(main())
