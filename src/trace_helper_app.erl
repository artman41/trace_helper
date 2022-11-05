-module(trace_helper_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    trace_helper_registry:new(),
	trace_helper_sup:start_link().

stop(_State) ->
	ok.
