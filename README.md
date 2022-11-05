# Trace Helper

trace_helper is a library designed to make dealing with Erlang traces easier.

Traces are received and parsed by a worker in the background until stopped.

Traces are parsed into useable records (`include/trace_helper.hrl`) allowing for pattern matching without accounting for every possible variation of trace message.