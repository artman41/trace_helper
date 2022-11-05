-module(trace_helper).

-export([
    start_trace/2, start_trace/3,
    stop_trace/1
]).

-define(is_MFA_matchspec(MFA), (is_tuple(MFA) andalso (tuple_size(MFA) =:= 3) andalso (is_atom(element(1, MFA)) andalso is_atom(element(2, MFA)) andalso is_atom(element(3, MFA))))).

start_trace(PidPort, Flags) when is_pid(PidPort) orelse is_port(PidPort) andalso is_list(Flags) ->
    start_trace(PidPort, Flags, {'_', '_', '_'}).

start_trace(PidPort, Flags, MFA) when is_pid(PidPort) orelse is_port(PidPort) andalso is_list(Flags) andalso ?is_MFA_matchspec(MFA) ->
    trace_helper_sup:start_child(PidPort, Flags, MFA).

stop_trace(PidId) when is_pid(PidId) orelse is_integer(PidId)->
    trace_helper_worker_srv:stop(PidId).