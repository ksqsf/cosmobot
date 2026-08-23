"""Standalone, dependency-free author SDK for cosmobot plugins."""

from .plugin import (
    Context,
    HostError,
    InvalidArguments,
    PermanentFailure,
    Plugin,
    ProtocolError,
    TransientFailure,
    schema_for,
    serve,
    serve_with,
    transient_process_failure,
)

__all__ = [
    "Context",
    "HostError",
    "InvalidArguments",
    "PermanentFailure",
    "Plugin",
    "ProtocolError",
    "TransientFailure",
    "schema_for",
    "serve",
    "serve_with",
    "transient_process_failure",
]
