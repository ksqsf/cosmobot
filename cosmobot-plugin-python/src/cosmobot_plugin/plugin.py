from __future__ import annotations

import asyncio
import dataclasses
import inspect
import json
import logging
import math
import sys
import types
import typing
from collections.abc import Awaitable, Callable, Mapping, Sequence
from typing import Any, Literal, NoReturn, TypeVar, get_args, get_origin, get_type_hints

PROTOCOL_VERSION = "1.0.0"
MAX_LINE_BYTES = 1024 * 1024
CAPABILITIES = frozenset({"chat", "llm", "agent", "media"})

log = logging.getLogger("cosmobot_plugin")
T = TypeVar("T")


class ProtocolError(Exception):
    pass


class HostError(Exception):
    def __init__(self, code: int, message: str, data: Any = None):
        super().__init__(message)
        self.code, self.data = code, data


class InvalidArguments(Exception):
    pass


class TransientFailure(Exception):
    """An explicitly retryable startup or invocation failure."""


class PermanentFailure(Exception):
    """An explicitly non-retryable startup or invocation failure."""


@dataclasses.dataclass(frozen=True)
class _Route:
    id: str
    label: str
    help: str
    filter: Mapping[str, Any]
    handler: Callable[..., Awaitable[Any]]
    access: str
    disposition: str


@dataclasses.dataclass(frozen=True)
class _Tool:
    name: str
    description: str
    arguments: type[Any] | None
    handler: Callable[..., Awaitable[Any]]


class Context:
    """Host callbacks valid only for the current handler invocation."""

    def __init__(self, plugin: Plugin, invocation_id: str, message: Mapping[str, Any]):
        self._plugin = plugin
        self.invocation_id = invocation_id
        self.message = message
        self.active = True

    @property
    def state(self) -> Any:
        return self._plugin.state

    async def _call(self, capability: str, method: str, **params: Any) -> Any:
        if not self.active:
            raise RuntimeError("invocation context has expired")
        if capability not in self._plugin.capabilities:
            raise PermissionError(f"plugin did not declare the {capability!r} capability")
        return await self._plugin._host_call(
            f"{capability}.{method}", {"invocationId": self.invocation_id, **params}
        )

    async def reply(self, text: str) -> Any:
        return await self._call("chat", "reply", text=text)

    async def referenced_message(self) -> Any:
        return await self._call("chat", "referenced")

    async def llm(self, prompt: str) -> Any:
        return await self._call("llm", "complete", prompt=prompt)

    async def agent(self, prompt: str) -> Any:
        return await self._call("agent", "run", prompt=prompt)

    async def media(self, reference: str) -> Any:
        return await self._call("media", "resolve", ref=reference)


class Plugin:
    def __init__(self, version: str, capabilities: Sequence[str] = ()):
        if not version.strip():
            raise ValueError("plugin version must not be empty")
        unknown = set(capabilities) - CAPABILITIES
        if unknown:
            raise ValueError(f"unknown capabilities: {', '.join(sorted(unknown))}")
        self.version = version
        self.capabilities = tuple(dict.fromkeys(capabilities))
        self.routes: list[_Route] = []
        self.tools: list[_Tool] = []
        self.state: Any = None
        self._initialize: Callable[[], Awaitable[Any]] | None = None
        self._finalize: Callable[[Any], Awaitable[None]] | None = None
        self._initialized = False
        self._finalized = False
        self._pending: dict[int, asyncio.Future[Any]] = {}
        self._tasks: set[asyncio.Task[Any]] = set()
        self._active_handlers: set[asyncio.Task[Any]] = set()
        self._next_id = 1
        self._write_lock = asyncio.Lock()
        self._writer: Any = None
        self._stopping = False

    def command(
        self,
        command: str,
        help: str,
        *,
        access: str = "allowed",
        disposition: str = "stop",
    ) -> Callable[[Callable[..., Awaitable[Any]]], Callable[..., Awaitable[Any]]]:
        route_id = command[1:] if command.startswith("!") else ""
        if not _is_provider_identifier(route_id):
            raise ValueError(f"invalid command: {command!r}")
        return self.route(
            route_id,
            command,
            help,
            {"command": command},
            access=access,
            disposition=disposition,
        )

    def route(
        self,
        route_id: str,
        label: str,
        help: str,
        route_filter: Mapping[str, Any],
        *,
        access: str = "allowed",
        disposition: str = "stop",
    ) -> Callable[[Callable[..., Awaitable[Any]]], Callable[..., Awaitable[Any]]]:
        if not route_id.strip() or any(route.id == route_id for route in self.routes):
            raise ValueError(f"invalid or duplicate route id: {route_id!r}")
        if not label.strip() or not help.strip():
            raise ValueError("route label and help must not be empty")
        _validate_filter(route_filter)
        if disposition not in {"stop", "continue"}:
            raise ValueError("disposition must be 'stop' or 'continue'")
        if access not in {"public", "allowed", "superuser"}:
            raise ValueError("access must be 'public', 'allowed', or 'superuser'")

        def register(handler: Callable[..., Awaitable[Any]]) -> Callable[..., Awaitable[Any]]:
            _require_async(handler)
            self.routes.append(
                _Route(route_id, label, help, dict(route_filter), handler, access, disposition)
            )
            return handler

        return register

    def tool(
        self,
        name: str,
        description: str,
        arguments: type[Any] | None = None,
    ) -> Callable[[Callable[..., Awaitable[Any]]], Callable[..., Awaitable[Any]]]:
        if not _is_provider_identifier(name) or any(tool.name == name for tool in self.tools):
            raise ValueError(f"invalid or duplicate tool name: {name!r}")
        if not description.strip():
            raise ValueError("tool description must not be empty")

        def register(handler: Callable[..., Awaitable[Any]]) -> Callable[..., Awaitable[Any]]:
            _require_async(handler)
            argument_type = arguments or _argument_annotation(handler)
            if argument_type is not None and not dataclasses.is_dataclass(argument_type):
                raise TypeError("tool arguments must be a dataclass")
            self.tools.append(_Tool(name, description, argument_type, handler))
            return handler

        return register

    def lifecycle(
        self,
        initialize: Callable[[], Awaitable[Any]],
        finalize: Callable[[Any], Awaitable[None]],
    ) -> Plugin:
        _require_async(initialize)
        _require_async(finalize)
        self._initialize, self._finalize = initialize, finalize
        return self

    def manifest(self) -> dict[str, Any]:
        return {
            "protocolVersion": PROTOCOL_VERSION,
            "pluginVersion": self.version,
            "routes": [
                {
                    "id": route.id,
                    "help": {"label": route.label, "description": route.help},
                    "filter": route.id,
                    "access": route.access,
                    "disposition": route.disposition,
                }
                for route in self.routes
            ],
            "filters": {route.id: route.filter for route in self.routes},
            "tools": [
                {
                    "name": tool.name,
                    "description": tool.description,
                    "schema": schema_for(tool.arguments) if tool.arguments else _empty_schema(),
                }
                for tool in self.tools
            ],
            "requestedCapabilities": list(self.capabilities),
        }

    async def run(self, reader: Any = None, writer: Any = None) -> None:
        self._writer = writer or sys.stdout.buffer
        transport = None
        if reader is None:
            reader = asyncio.StreamReader(limit=MAX_LINE_BYTES + 1)
            protocol = asyncio.StreamReaderProtocol(reader)
            transport, _ = await asyncio.get_running_loop().connect_read_pipe(
                lambda: protocol, sys.stdin.buffer
            )
            read_line = lambda: _read_stream_line(reader)
        else:
            read_line = lambda: _maybe_await(reader.readline, MAX_LINE_BYTES + 1)
        try:
            while not self._stopping:
                line = await read_line()
                if not line:
                    break
                if len(line) > MAX_LINE_BYTES:
                    raise ProtocolError("JSON-RPC line exceeds 1 MiB")
                try:
                    message = json.loads(line)
                except (json.JSONDecodeError, UnicodeDecodeError) as error:
                    await self._send_error(None, -32700, "parse error", str(error))
                    continue
                if not isinstance(message, dict):
                    await self._send_error(None, -32600, "invalid request")
                    continue
                if "method" in message:
                    if message.get("method") == "plugin.shutdown":
                        await self._handle_request(message)
                    else:
                        task = asyncio.create_task(self._handle_request(message))
                        self._tasks.add(task)
                        task.add_done_callback(self._tasks.discard)
                elif "id" in message and ("result" in message or "error" in message):
                    self._handle_response(message)
                else:
                    await self._send_error(message.get("id"), -32600, "invalid request")
        finally:
            if transport is not None:
                transport.close()
            await self._stop()

    async def _handle_request(self, request: Mapping[str, Any]) -> None:
        request_id = request.get("id")
        try:
            if request.get("jsonrpc") != "2.0" or not isinstance(request.get("method"), str):
                raise _RpcError(-32600, "invalid request")
            params = request.get("params", {})
            if not isinstance(params, dict):
                raise _RpcError(-32602, "params must be an object", "permanent")
            result = await self._dispatch(request["method"], params)
            if request_id is not None:
                await self._send({"jsonrpc": "2.0", "id": request_id, "result": result})
        except _RpcError as error:
            if request_id is not None:
                await self._send_error(request_id, error.code, str(error), error.data)
        except InvalidArguments as error:
            if request_id is not None:
                await self._send_error(request_id, -32602, str(error), {"kind": "permanent_argument"})
        except TransientFailure as error:
            if request_id is not None:
                data = (
                    {"transient": True}
                    if request.get("method") == "plugin.initialize"
                    else {"kind": "transient"}
                )
                await self._send_error(request_id, -32000, str(error), data)
        except PermanentFailure as error:
            if request_id is not None:
                data = (
                    {"transient": False}
                    if request.get("method") == "plugin.initialize"
                    else {"kind": "permanent"}
                )
                await self._send_error(request_id, -32001, str(error), data)
        except asyncio.TimeoutError:
            if request_id is not None:
                await self._send_error(request_id, -32000, "plugin invocation timed out", {"kind": "transient"})
        except asyncio.CancelledError:
            raise
        except Exception as error:
            log.exception("plugin handler failed")
            if request_id is not None:
                data = (
                    {"transient": False}
                    if request.get("method") == "plugin.initialize"
                    else {"kind": "transient"}
                )
                await self._send_error(request_id, -32000, str(error), data)

    async def _dispatch(self, method: str, params: Mapping[str, Any]) -> Any:
        if method == "plugin.initialize":
            if self._initialized:
                raise _RpcError(-32600, "plugin already initialized")
            if self._initialize:
                self.state = await self._initialize()
            self._initialized = True
            return self.manifest()
        if not self._initialized:
            raise _RpcError(-32002, "plugin is not initialized")
        if method == "plugin.route.invoke":
            return await self._invoke_route(params)
        if method == "plugin.tool.invoke":
            return await self._invoke_tool(params)
        if method == "plugin.shutdown":
            await self._cancel_handlers()
            await self._finalize_once()
            self._stopping = True
            return None
        raise _RpcError(-32601, f"method not found: {method}")

    async def _invoke_route(self, params: Mapping[str, Any]) -> Any:
        route_id = _required_string(params, "routeId")
        timeout = _required_timeout(params)
        route = next((item for item in self.routes if item.id == route_id), None)
        if route is None:
            raise _RpcError(-32602, f"unknown route: {route_id}", {"kind": "permanent_argument"})
        context = self._context(params)
        task = asyncio.current_task()
        if task is not None:
            self._active_handlers.add(task)
        try:
            value = await asyncio.wait_for(
                route.handler(context, params.get("arguments", "")), timeout
            )
            return {"handled": True} if value is None else value
        finally:
            context.active = False
            if task is not None:
                self._active_handlers.discard(task)

    async def _invoke_tool(self, params: Mapping[str, Any]) -> Any:
        name = _required_string(params, "tool")
        timeout = _required_timeout(params)
        tool = next((item for item in self.tools if item.name == name), None)
        if tool is None:
            raise InvalidArguments(f"unknown tool: {name}")
        raw_arguments = params.get("arguments", {})
        if not isinstance(raw_arguments, dict):
            raise InvalidArguments("tool arguments must be an object")
        arguments = _decode_dataclass(tool.arguments, raw_arguments) if tool.arguments else None
        context = self._context(params)
        task = asyncio.current_task()
        if task is not None:
            self._active_handlers.add(task)
        try:
            invocation = (
                tool.handler(context)
                if tool.arguments is None
                else tool.handler(context, arguments)
            )
            value = await asyncio.wait_for(invocation, timeout)
            return {"status": "success", "content": value, "imageUrls": []} if isinstance(value, str) else value
        finally:
            context.active = False
            if task is not None:
                self._active_handlers.discard(task)

    def _context(self, params: Mapping[str, Any]) -> Context:
        invocation_id = _required_string(params, "invocationId")
        message = params.get("message", {})
        if not isinstance(message, dict):
            raise InvalidArguments("message must be an object")
        return Context(self, invocation_id, message)

    async def _host_call(self, method: str, params: Mapping[str, Any]) -> Any:
        request_id = self._next_id
        self._next_id += 1
        future = asyncio.get_running_loop().create_future()
        self._pending[request_id] = future
        try:
            await self._send({"jsonrpc": "2.0", "id": request_id, "method": method, "params": params})
            return await future
        finally:
            self._pending.pop(request_id, None)

    def _handle_response(self, response: Mapping[str, Any]) -> None:
        future = self._pending.get(response.get("id"))
        if future is None or future.done():
            return
        error = response.get("error")
        if isinstance(error, dict):
            future.set_exception(
                HostError(error.get("code", -32000), error.get("message", "host call failed"), error.get("data"))
            )
        elif "result" in response:
            future.set_result(response["result"])
        else:
            future.set_exception(ProtocolError("malformed JSON-RPC response"))

    async def _send_error(self, request_id: Any, code: int, message: str, data: Any = None) -> None:
        error = {"code": code, "message": message}
        if data is not None:
            error["data"] = data
        await self._send({"jsonrpc": "2.0", "id": request_id, "error": error})

    async def _send(self, message: Mapping[str, Any]) -> None:
        line = json.dumps(message, separators=(",", ":"), ensure_ascii=False).encode() + b"\n"
        if len(line) > MAX_LINE_BYTES:
            raise ProtocolError("outgoing JSON-RPC line exceeds 1 MiB")
        async with self._write_lock:
            result = self._writer.write(line)
            if inspect.isawaitable(result):
                await result
            flush = getattr(self._writer, "flush", None)
            if flush:
                result = flush()
                if inspect.isawaitable(result):
                    await result
            drain = getattr(self._writer, "drain", None)
            if drain:
                result = drain()
                if inspect.isawaitable(result):
                    await result

    async def _finalize_once(self) -> None:
        if self._initialized and not self._finalized:
            self._finalized = True
            if self._finalize:
                await self._finalize(self.state)

    async def _cancel_handlers(self) -> None:
        current = asyncio.current_task()
        tasks = [task for task in self._active_handlers if task is not current and not task.done()]
        for task in tasks:
            task.cancel()
        if tasks:
            await asyncio.gather(*tasks, return_exceptions=True)

    async def _stop(self) -> None:
        current = asyncio.current_task()
        tasks = [task for task in self._tasks if task is not current and not task.done()]
        for task in tasks:
            task.cancel()
        if tasks:
            await asyncio.gather(*tasks, return_exceptions=True)
        await self._cancel_handlers()
        await self._finalize_once()
        for future in self._pending.values():
            if not future.done():
                future.set_exception(EOFError("host connection closed"))


@dataclasses.dataclass(frozen=True)
class _RpcError(Exception):
    code: int
    message: str
    data: Any = None

    def __str__(self) -> str:
        return self.message


def serve(plugin: Plugin) -> None:
    asyncio.run(plugin.run())


def serve_with(
    plugin: Plugin,
    initialize: Callable[[], Awaitable[Any]],
    finalize: Callable[[Any], Awaitable[None]],
) -> None:
    serve(plugin.lifecycle(initialize, finalize))


def transient_process_failure(message: str) -> NoReturn:
    print(f"transient process failure: {message}", file=sys.stderr, flush=True)
    raise SystemExit(75)


def schema_for(cls: type[Any]) -> dict[str, Any]:
    if not dataclasses.is_dataclass(cls):
        raise TypeError("tool arguments must be a dataclass")
    hints = get_type_hints(cls)
    properties, required = {}, []
    for field in dataclasses.fields(cls):
        properties[field.name] = _schema_type(hints.get(field.name, Any))
        if field.metadata.get("description"):
            properties[field.name]["description"] = field.metadata["description"]
        if field.default is dataclasses.MISSING and field.default_factory is dataclasses.MISSING:
            required.append(field.name)
    return {"type": "object", "properties": properties, "required": required, "additionalProperties": False}


def _schema_type(annotation: Any) -> dict[str, Any]:
    origin, args = get_origin(annotation), get_args(annotation)
    if origin is Literal:
        return {"enum": list(args)}
    if origin in (typing.Union, types.UnionType):
        return {"anyOf": [_schema_type(arg) for arg in args]}
    if origin in (list, Sequence):
        return {"type": "array", "items": _schema_type(args[0] if args else Any)}
    if origin in (dict, Mapping):
        return {"type": "object", "additionalProperties": _schema_type(args[1] if len(args) > 1 else Any)}
    if annotation is type(None):
        return {"type": "null"}
    if dataclasses.is_dataclass(annotation):
        return schema_for(annotation)
    json_type = {str: "string", int: "integer", float: "number", bool: "boolean"}.get(annotation)
    return {"type": json_type} if json_type else {}


def _decode_dataclass(cls: type[T] | None, values: Mapping[str, Any]) -> T:
    if cls is None:
        raise InvalidArguments("tool has no argument type")
    fields = {field.name: field for field in dataclasses.fields(cls)}
    unknown = set(values) - fields.keys()
    if unknown:
        raise InvalidArguments(f"unknown arguments: {', '.join(sorted(unknown))}")
    hints = get_type_hints(cls)
    decoded: dict[str, Any] = {}
    for name, field in fields.items():
        if name not in values:
            if field.default is dataclasses.MISSING and field.default_factory is dataclasses.MISSING:
                raise InvalidArguments(f"missing argument: {name}")
            continue
        try:
            decoded[name] = _decode_value(hints.get(name, Any), values[name])
        except (TypeError, ValueError) as error:
            raise InvalidArguments(f"invalid argument {name}: {error}") from error
    return cls(**decoded)


def _decode_value(annotation: Any, value: Any) -> Any:
    origin, args = get_origin(annotation), get_args(annotation)
    if annotation is Any:
        return value
    if origin is Literal:
        if value not in args:
            raise ValueError(f"expected one of {args!r}")
        return value
    if origin in (typing.Union, types.UnionType):
        for choice in args:
            try:
                return _decode_value(choice, value)
            except (TypeError, ValueError, InvalidArguments):
                pass
        raise TypeError("value does not match any allowed type")
    if origin in (list, Sequence):
        if not isinstance(value, list):
            raise TypeError("expected array")
        return [_decode_value(args[0] if args else Any, item) for item in value]
    if origin in (dict, Mapping):
        if not isinstance(value, dict):
            raise TypeError("expected object")
        return {str(key): _decode_value(args[1] if len(args) > 1 else Any, item) for key, item in value.items()}
    if dataclasses.is_dataclass(annotation):
        if not isinstance(value, dict):
            raise TypeError("expected object")
        return _decode_dataclass(annotation, value)
    if annotation in (str, int, float, bool):
        valid = (
            isinstance(value, (int, float)) and not isinstance(value, bool)
            if annotation is float
            else isinstance(value, annotation) and not (annotation is int and isinstance(value, bool))
        )
        if not valid:
            raise TypeError(f"expected {annotation.__name__}")
    return value


def _empty_schema() -> dict[str, Any]:
    return {"type": "object", "properties": {}, "required": [], "additionalProperties": False}


def _argument_annotation(handler: Callable[..., Any]) -> type[Any] | None:
    parameters = list(inspect.signature(handler).parameters.values())
    if len(parameters) < 2:
        return None
    return get_type_hints(handler).get(parameters[1].name)


def _require_async(function: Callable[..., Any]) -> None:
    if not inspect.iscoroutinefunction(function):
        raise TypeError("plugin handlers and lifecycle functions must be async")


def _required_string(values: Mapping[str, Any], key: str) -> str:
    value = values.get(key)
    if not isinstance(value, str) or not value:
        raise InvalidArguments(f"{key} must be a non-empty string")
    return value


def _required_timeout(values: Mapping[str, Any]) -> float:
    value = values.get("timeoutSeconds")
    if (
        not isinstance(value, (int, float))
        or isinstance(value, bool)
        or not math.isfinite(value)
        or value <= 0
    ):
        raise InvalidArguments("timeoutSeconds must be a positive number")
    return float(value)


def _is_identifier(value: str) -> bool:
    return bool(value) and all(character.isascii() and character.isalnum() or character in "_-" for character in value)


def _is_provider_identifier(value: str) -> bool:
    return _is_identifier(value) and "__" not in value


def _validate_filter(route_filter: Mapping[str, Any]) -> None:
    nodes = 0

    def validate(value: Any, depth: int) -> None:
        nonlocal nodes
        nodes += 1
        if depth > 8 or nodes > 64:
            raise ValueError("route filter exceeds protocol bounds")
        if not isinstance(value, Mapping) or len(value) != 1:
            raise ValueError("route filter nodes must contain one operator")
        operator, argument = next(iter(value.items()))
        if operator in {"all", "any"}:
            if not isinstance(argument, list) or not argument:
                raise ValueError(f"{operator} route filter must be a non-empty list")
            for child in argument:
                validate(child, depth + 1)
        elif operator == "not":
            validate(argument, depth + 1)
        elif operator not in {"command", "prefix", "platform", "event", "chatKind", "reply", "mention", "access"}:
            raise ValueError(f"unknown route filter operator: {operator}")

    validate(route_filter, 1)


async def _maybe_await(function: Callable[..., Any], *args: Any) -> Any:
    if inspect.iscoroutinefunction(function):
        return await function(*args)
    return await asyncio.to_thread(function, *args)


async def _read_stream_line(reader: asyncio.StreamReader) -> bytes:
    try:
        return await reader.readline()
    except (asyncio.LimitOverrunError, ValueError) as error:
        raise ProtocolError("JSON-RPC line exceeds 1 MiB") from error
