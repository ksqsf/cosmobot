#!/usr/bin/env python3
from cosmobot_plugin import Context, Plugin, serve

plugin = Plugin("1.0.0", ["chat"])


@plugin.command("!echo", "Repeat the supplied text.")
async def echo(context: Context, arguments: str) -> None:
    await context.reply(arguments)


if __name__ == "__main__":
    serve(plugin)
