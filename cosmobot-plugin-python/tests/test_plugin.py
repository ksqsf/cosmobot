import asyncio
import dataclasses
import json
import os
from pathlib import Path
import sys
import unittest

from cosmobot_plugin import (
    Context,
    InvalidArguments,
    Plugin,
    ProtocolError,
    TransientFailure,
    schema_for,
)


@dataclasses.dataclass
class Args:
    name: str = dataclasses.field(metadata={"description": "A name."})
    count: int = 1
    excited: bool | None = None


class MemoryWriter:
    def __init__(self):
        self.lines = []

    async def write(self, line):
        self.lines.append(json.loads(line))


class PluginTests(unittest.IsolatedAsyncioTestCase):
    def make_plugin(self):
        plugin = Plugin("2.0", ["chat"])

        @plugin.command("!hello", "Say hello.")
        async def hello(context, arguments):
            return arguments

        @plugin.tool("greet", "Greet someone.")
        async def greet(context: Context, arguments: Args) -> str:
            return arguments.name * arguments.count

        return plugin

    async def test_manifest_defaults_and_schema(self):
        manifest = self.make_plugin().manifest()
        self.assertEqual(manifest["routes"][0]["id"], "hello")
        self.assertEqual(manifest["routes"][0]["disposition"], "stop")
        self.assertEqual(manifest["routes"][0]["access"], "allowed")
        self.assertEqual(manifest["routes"][0]["help"], {"label": "!hello", "description": "Say hello."})
        self.assertEqual(manifest["filters"], {"hello": {"command": "!hello"}})
        schema = manifest["tools"][0]["schema"]
        self.assertEqual(schema["required"], ["name"])
        self.assertEqual(schema["properties"]["name"]["description"], "A name.")
        self.assertEqual(schema["properties"]["excited"]["anyOf"][-1], {"type": "null"})

    async def test_general_route_filter(self):
        plugin = Plugin("1")

        @plugin.route(
            "images",
            "images",
            "Handle image events.",
            {"all": [{"platform": "matrix"}, {"mention": True}]},
            access="public",
            disposition="continue",
        )
        async def images(context, arguments):
            pass

        manifest = plugin.manifest()
        self.assertEqual(manifest["filters"]["images"]["all"][1], {"mention": True})
        self.assertEqual(manifest["routes"][0]["access"], "public")

    async def test_command_syntax(self):
        for command, valid in [
            ("!echo", True),
            ("/echo", False),
            ("!hello world", False),
            ("!", False),
        ]:
            with self.subTest(command=command):
                plugin = Plugin("1")
                if valid:
                    decorator = plugin.command(command, "Echo.")

                    async def handler(context, arguments):
                        pass

                    decorator(handler)
                    self.assertEqual(plugin.routes[0].id, "echo")
                else:
                    with self.assertRaises(ValueError):
                        plugin.command(command, "Invalid.")

    async def test_tool_provider_identifier_rejects_double_underscore(self):
        plugin = Plugin("1")
        with self.assertRaisesRegex(ValueError, "invalid"):
            plugin.tool("ambiguous__name", "Invalid.")

    async def test_tool_text_and_argument_failure(self):
        plugin = self.make_plugin()
        await plugin._dispatch("plugin.initialize", {})
        result = await plugin._dispatch(
            "plugin.tool.invoke",
            {"invocationId": "i", "tool": "greet", "arguments": {"name": "Hi", "count": 2}, "timeoutSeconds": 1},
        )
        self.assertEqual(result, {"status": "success", "content": "HiHi", "imageUrls": []})
        with self.assertRaises(InvalidArguments):
            await plugin._dispatch(
                "plugin.tool.invoke",
                {"invocationId": "i", "tool": "greet", "arguments": {"name": 3}, "timeoutSeconds": 1},
            )
        with self.assertRaises(InvalidArguments):
            await plugin._dispatch(
                "plugin.tool.invoke",
                {"invocationId": "i", "tool": "greet", "arguments": {"name": "Hi"}},
            )

    async def test_lifecycle_exactly_once(self):
        events = []
        plugin = self.make_plugin()

        async def initialize():
            events.append("initialize")
            return 42

        async def finalize(state):
            events.append(f"finalize:{state}")

        plugin.lifecycle(initialize, finalize)
        await plugin._dispatch("plugin.initialize", {})
        await plugin._dispatch("plugin.shutdown", {})
        await plugin._stop()
        self.assertEqual(events, ["initialize", "finalize:42"])

    async def test_startup_transience_is_explicit(self):
        async def finalize(state):
            pass

        async def transient_initialize():
            raise TransientFailure("try again")

        transient = Plugin("1").lifecycle(transient_initialize, finalize)
        transient._writer = MemoryWriter()
        await transient._handle_request(
            {"jsonrpc": "2.0", "id": 1, "method": "plugin.initialize", "params": {}}
        )
        self.assertEqual(transient._writer.lines[0]["error"]["data"], {"transient": True})

        async def ordinary_initialize():
            raise RuntimeError("broken")

        ordinary = Plugin("1").lifecycle(ordinary_initialize, finalize)
        ordinary._writer = MemoryWriter()
        with self.assertLogs("cosmobot_plugin", level="ERROR"):
            await ordinary._handle_request(
                {"jsonrpc": "2.0", "id": 2, "method": "plugin.initialize", "params": {}}
            )
        self.assertEqual(ordinary._writer.lines[0]["error"]["data"], {"transient": False})

    async def test_ordinary_invocation_failure_remains_transient(self):
        plugin = Plugin("1")

        @plugin.command("!fail", "Fail.")
        async def fail(context, arguments):
            raise RuntimeError("failed")

        await plugin._dispatch("plugin.initialize", {})
        plugin._writer = MemoryWriter()
        with self.assertLogs("cosmobot_plugin", level="ERROR"):
            await plugin._handle_request(
                {
                    "jsonrpc": "2.0",
                    "id": 3,
                    "method": "plugin.route.invoke",
                    "params": {
                        "invocationId": "failure",
                        "routeId": "fail",
                        "message": {},
                        "timeoutSeconds": 1,
                    },
                }
            )
        self.assertEqual(plugin._writer.lines[0]["error"]["data"], {"kind": "transient"})

    async def test_shutdown_cancels_handlers_before_finalize(self):
        events = []
        started = asyncio.Event()
        plugin = Plugin("1")

        @plugin.command("!wait", "Wait.")
        async def wait(context, arguments):
            try:
                started.set()
                await asyncio.Event().wait()
            finally:
                events.append("handler stopped")

        async def initialize():
            return None

        async def finalize(state):
            events.append("finalized")

        plugin.lifecycle(initialize, finalize)
        await plugin._dispatch("plugin.initialize", {})
        invocation = asyncio.create_task(
            plugin._dispatch(
                "plugin.route.invoke",
                {"invocationId": "i", "routeId": "wait", "message": {}, "timeoutSeconds": 1},
            )
        )
        await started.wait()
        await plugin._dispatch("plugin.shutdown", {})
        self.assertTrue(invocation.cancelled())
        self.assertEqual(events, ["handler stopped", "finalized"])

    async def test_callback_capability_and_expiry(self):
        plugin = self.make_plugin()
        plugin._writer = MemoryWriter()
        context = Context(plugin, "inv-1", {"text": "hello", "raw": {"unstable": True}})
        task = asyncio.create_task(context.reply("answer"))
        await asyncio.sleep(0)
        request = plugin._writer.lines[0]
        self.assertEqual(request["method"], "chat.reply")
        self.assertEqual(request["params"]["invocationId"], "inv-1")
        plugin._handle_response({"jsonrpc": "2.0", "id": request["id"], "result": "message-id"})
        self.assertEqual(await task, "message-id")
        context.active = False
        with self.assertRaises(RuntimeError):
            await context.reply("late")
        with self.assertRaises(PermissionError):
            await Context(Plugin("1"), "i", {}).llm("no")

        media_plugin = Plugin("1", ["media"])
        media_plugin._writer = MemoryWriter()
        media_context = Context(media_plugin, "inv-2", {})
        media_call = asyncio.create_task(media_context.media("media:123"))
        await asyncio.sleep(0)
        media_request = media_plugin._writer.lines[0]
        self.assertEqual(media_request["params"], {"invocationId": "inv-2", "ref": "media:123"})
        media_plugin._handle_response(
            {"jsonrpc": "2.0", "id": media_request["id"], "result": {"mimeType": None, "size": None}}
        )
        self.assertEqual(await media_call, {"mimeType": None, "size": None})

    async def test_requests_execute_concurrently(self):
        plugin = Plugin("1")
        entered = 0
        both = asyncio.Event()

        @plugin.command("!wait", "Wait.")
        async def wait(context, arguments):
            nonlocal entered
            entered += 1
            if entered == 2:
                both.set()
            await asyncio.wait_for(both.wait(), 0.5)

        input_lines = [
            {"jsonrpc": "2.0", "id": 1, "method": "plugin.initialize", "params": {}},
            {"jsonrpc": "2.0", "id": 2, "method": "plugin.route.invoke", "params": {"invocationId": "a", "routeId": "wait", "message": {}, "timeoutSeconds": 1}},
            {"jsonrpc": "2.0", "id": 3, "method": "plugin.route.invoke", "params": {"invocationId": "b", "routeId": "wait", "message": {}, "timeoutSeconds": 1}},
        ]

        class Reader:
            async def readline(self, size):
                await asyncio.sleep(0)
                if input_lines:
                    return json.dumps(input_lines.pop(0)).encode() + b"\n"
                await both.wait()
                await asyncio.sleep(0.01)
                return b""

        writer = MemoryWriter()
        await plugin.run(Reader(), writer)
        self.assertEqual(entered, 2)
        self.assertEqual({line["id"] for line in writer.lines}, {1, 2, 3})

    async def test_invalid_json_and_oversize_lines(self):
        class Reader:
            def __init__(self, lines):
                self.lines = lines

            async def readline(self, size):
                return self.lines.pop(0) if self.lines else b""

        writer = MemoryWriter()
        await Plugin("1").run(Reader([b"not json\n"]), writer)
        self.assertEqual(writer.lines[0]["error"]["code"], -32700)
        with self.assertRaises(ProtocolError):
            await Plugin("1").run(Reader([b"x" * (1024 * 1024 + 1)]), MemoryWriter())

    async def test_real_stdin_initialize_invoke_shutdown(self):
        project = Path(__file__).parents[1]
        environment = dict(os.environ)
        environment["PYTHONPATH"] = str(project / "src")
        process = await asyncio.create_subprocess_exec(
            sys.executable,
            str(project / "examples" / "echo.py"),
            stdin=asyncio.subprocess.PIPE,
            stdout=asyncio.subprocess.PIPE,
            stderr=asyncio.subprocess.PIPE,
            env=environment,
        )
        assert process.stdin is not None
        assert process.stdout is not None
        assert process.stderr is not None

        async def send(message):
            process.stdin.write(json.dumps(message).encode() + b"\n")
            await process.stdin.drain()

        async def receive():
            return json.loads(await asyncio.wait_for(process.stdout.readline(), 2))

        await send({"jsonrpc": "2.0", "id": 1, "method": "plugin.initialize", "params": {}})
        initialized = await receive()
        self.assertEqual(initialized["result"]["routes"][0]["id"], "echo")

        await send(
            {
                "jsonrpc": "2.0",
                "id": 2,
                "method": "plugin.route.invoke",
                "params": {
                    "invocationId": "smoke",
                    "routeId": "echo",
                    "message": {},
                    "arguments": "hello",
                    "timeoutSeconds": 1,
                },
            }
        )
        callback = await receive()
        self.assertEqual(callback["method"], "chat.reply")
        self.assertEqual(callback["params"], {"invocationId": "smoke", "text": "hello"})
        await send({"jsonrpc": "2.0", "id": callback["id"], "result": "message-id"})
        self.assertEqual((await receive())["id"], 2)

        await send({"jsonrpc": "2.0", "id": 3, "method": "plugin.shutdown", "params": {}})
        self.assertEqual((await receive())["id"], 3)
        process.stdin.close()
        stderr_task = asyncio.create_task(process.stderr.read())
        try:
            returncode = await asyncio.wait_for(process.wait(), 5)
        finally:
            if process.returncode is None:
                process.kill()
                await process.wait()
        stderr = await stderr_task
        self.assertEqual(returncode, 0)
        self.assertEqual(stderr, b"")

    async def test_real_stdin_rejects_overrun(self):
        project = Path(__file__).parents[1]
        environment = dict(os.environ)
        environment["PYTHONPATH"] = str(project / "src")
        process = await asyncio.create_subprocess_exec(
            sys.executable,
            str(project / "examples" / "echo.py"),
            stdin=asyncio.subprocess.PIPE,
            stdout=asyncio.subprocess.PIPE,
            stderr=asyncio.subprocess.PIPE,
            env=environment,
        )
        _, stderr = await asyncio.wait_for(
            process.communicate(b"x" * (1024 * 1024 + 2) + b"\n"), 5
        )
        self.assertNotEqual(process.returncode, 0)
        self.assertIn(b"JSON-RPC line exceeds 1 MiB", stderr)

    async def test_shared_route_invoke_fixture(self):
        fixture_path = (
            Path(__file__).parents[1]
            / ".."
            / "cosmobot"
            / "protocol-fixtures"
            / "route-invoke.json"
        )
        line = fixture_path.read_bytes().rstrip(b"\n") + b"\n"
        completed = asyncio.Event()
        seen = {}
        plugin = Plugin("1")

        @plugin.command("!echo", "Echo.")
        async def echo(context, arguments):
            seen.update(
                arguments=arguments,
                platform=context.message["platform"],
                event_kind=context.message["eventKind"],
                raw=context.message["raw"],
            )
            return arguments

        await plugin._dispatch("plugin.initialize", {})

        class Reader:
            async def readline(self, size):
                nonlocal line
                if line:
                    current, line = line, b""
                    return current
                await completed.wait()
                return b""

        class Writer(MemoryWriter):
            async def write(self, output):
                await super().write(output)
                if self.lines[-1].get("id") == "host:7:1":
                    completed.set()

        writer = Writer()
        await plugin.run(Reader(), writer)
        self.assertEqual(
            seen,
            {
                "arguments": "hello",
                "platform": "telegram",
                "event_kind": "created",
                "raw": None,
            },
        )
        self.assertEqual(
            writer.lines[-1],
            {"jsonrpc": "2.0", "id": "host:7:1", "result": "hello"},
        )

    async def test_transient_process_failure_exits_75(self):
        project = Path(__file__).parents[1]
        environment = dict(os.environ)
        environment["PYTHONPATH"] = str(project / "src")
        process = await asyncio.create_subprocess_exec(
            sys.executable,
            "-c",
            "from cosmobot_plugin import transient_process_failure; transient_process_failure('retry me')",
            stdout=asyncio.subprocess.PIPE,
            stderr=asyncio.subprocess.PIPE,
            env=environment,
        )
        stdout, stderr = await asyncio.wait_for(process.communicate(), 5)
        self.assertEqual(process.returncode, 75)
        self.assertEqual(stdout, b"")
        self.assertIn(b"transient process failure: retry me", stderr)

    async def test_timeout_is_required_and_waits_for_handler_cleanup(self):
        stopped = asyncio.Event()
        plugin = Plugin("1")

        @plugin.command("!slow", "Wait forever.")
        async def slow(context, arguments):
            try:
                await asyncio.Event().wait()
            finally:
                stopped.set()

        await plugin._dispatch("plugin.initialize", {})
        with self.assertRaises(InvalidArguments):
            await plugin._dispatch(
                "plugin.route.invoke",
                {"invocationId": "missing", "routeId": "slow", "message": {}},
            )
        with self.assertRaises(InvalidArguments):
            await plugin._dispatch(
                "plugin.route.invoke",
                {"invocationId": "zero", "routeId": "slow", "message": {}, "timeoutSeconds": 0},
            )

        plugin._writer = MemoryWriter()
        await plugin._handle_request(
            {
                "jsonrpc": "2.0",
                "id": 7,
                "method": "plugin.route.invoke",
                "params": {
                    "invocationId": "timeout",
                    "routeId": "slow",
                    "message": {},
                    "timeoutSeconds": 0.001,
                },
            }
        )
        self.assertTrue(stopped.is_set())
        self.assertEqual(plugin._writer.lines[0]["error"]["code"], -32000)
        self.assertEqual(plugin._writer.lines[0]["error"]["message"], "plugin invocation timed out")

    def test_schema_function(self):
        self.assertEqual(schema_for(Args)["properties"]["count"], {"type": "integer"})


if __name__ == "__main__":
    unittest.main()
