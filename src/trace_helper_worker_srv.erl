-module(trace_helper_worker_srv).
-behaviour(gen_server).

-include("trace_helper.hrl").

%% API.
-export([start_link/3, stop/1]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
    pidport :: pid() | port(),
    trace_args :: list(),
    messages :: queue:queue(traced_msg())
}).

-define(is_MFA_matchspec(MFA), (is_tuple(MFA) andalso (tuple_size(MFA) =:= 3) andalso (is_atom(element(1, MFA)) andalso is_atom(element(2, MFA)) andalso is_atom(element(3, MFA))))).

%% API.

start_link(PidPort, TraceArgs, MFA) when (is_pid(PidPort) orelse is_port(PidPort)) andalso is_list(TraceArgs) andalso ?is_MFA_matchspec(MFA) ->
    Id = trace_helper_registry:next_id(),
    gen_server:start_link({via, trace_helper_registry, Id}, ?MODULE, [Id, PidPort, TraceArgs, MFA], []).

-spec stop(pid() | Id :: integer()) -> queue:queue(traced_msg()).
stop(Pid) when is_pid(Pid) ->
    {dictionary, Dict} = process_info(Pid, dictionary),
    case proplists:get_value('$initial_call', Dict) of
        {?MODULE, init, 1} ->
            gen_server:call(Pid, stop_trace);
        _ ->
            erlang:error(badarg)
    end;
stop(Id) when is_integer(Id) ->
    gen_server:call({via, trace_helper_registry, Id}, stop_trace).

%% gen_server.

init([Id, PidPort, TraceArgs, MFA]) ->
    erlang:process_flag(trap_exit, true),
    put('$id', Id),
    erlang:trace(PidPort, true, [{tracer, self()} | TraceArgs]),
    erlang:trace_pattern(MFA, true, [local]),
    {ok, #state{pidport = PidPort, trace_args = TraceArgs, messages = queue:new()}}.

handle_call(stop_trace, _From, State = #state{messages = Messages}) ->
    {stop, {shutdown, stopping_trace}, Messages, State};
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(Msg, State = #state{trace_args = TraceArgs, messages = Messages}) when is_tuple(Msg) andalso (element(1, Msg) =:= trace orelse element(1, Msg) =:= trace_ts) ->
    {noreply, State#state{messages = handle_trace(Msg, Messages, TraceArgs)}};
handle_info(Info, State) ->
    error_logger:info_msg("Ignoring ~p~n", [Info]),
    {noreply, State}.

terminate(_Reason, #state{pidport = PidPort, trace_args = TraceArgs}) ->
    erlang:trace(PidPort, false, TraceArgs),
    trace_helper_registry:unregister_name(get('$id')),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% INTERNAL
handle_trace(Msg, Messages, Flags) ->
    Rec = parse_message(Msg, Flags),
    case Rec of
        undefined ->
            Messages;
        _ ->
            queue:in(Rec, Messages)
    end.

parse_message(Msg0, Flags) ->
    TimestampFlags = [all, timestamp, cpu_timestamp, monotonic_timestamp, strict_monotonic_timestamp],
    Msg1 =
        case TimestampFlags -- Flags of
            TimestampFlags -> 
                Msg0;
            _ when element(1, Msg0) =:= trace_ts ->
                error_logger:info_msg("Msg: ~p~n", [Msg0]),
                {Timestamp, Trace} = convert_trace_ts(Msg0),
                erlang:put('$timestamp', Timestamp),
                Trace
        end,
    case lists:member(all, Flags) of
        true ->
            %% For whatever reason, some kind of number is thrown on the end for 'all'
            FixedMsg = list_to_tuple(lists:sublist(tuple_to_list(Msg1), tuple_size(Msg1)-1)),
            parse_message(FixedMsg, Flags -- [all]);
        false ->
            case erlang:get('$timestamp') of
                undefined ->
                    parse_message_(Msg1);
                Timestamp_ ->
                    erlang:erase('$timestamp'),
                    #timestamped{timestamp = Timestamp_, data = parse_message_(Msg1)}
            end
    end.

parse_message_(TraceMsg) ->
    case TraceMsg of
        {trace, PidPort, send, Msg, To} ->
            #send{msg = Msg, to = To, exists = true};
        {trace, PidPort, send_to_non_existing_process, Msg, To} ->
            #send{msg = Msg, to = To, exists = false};
        {trace, PidPort, 'receive', Msg} ->
            #recv{msg = Msg};
        {trace, Pid, call, {Mod, Fun, Args}} ->
            #call{mod = Mod, function = Fun, args = Args};
        {trace, Pid, return_to, {Mod, Fun, Arity}} ->
            #return{direction = to, mod = Mod, function = Fun, arity = Arity};
        {trace, Pid, return_from, {Mod, Fun, Arity}, ReturnValue} ->
            #return{direction = from, mod = Mod, function = Fun, arity = Arity};
        {trace, Pid, exception_from, {Mod, Fun, Arity}, {Class, Value}} ->
            #exception{mod = Mod, function = Fun, arity = Arity, class = Class, value = Value};
        {trace, Pid, spawn, ChildPid, {Mod, Fun, Args}} ->
            #spawn{pid = ChildPid, mod = Mod, function = Fun, args = Args};
        {trace, Pid, spawned, ParentPid, {Mod, Fun, Args}} ->
            #spawned{pid = ParentPid, mod = Mod, function = Fun, args = Args};
        {trace, Pid, exit, Reason} ->
            #exit{reason = Reason};
        {trace, PidPort, register, RegName} ->
            #register{name = RegName};
        {trace, PidPort, unregister, RegName} ->
            #unregister{name = RegName};
        {trace, Pid, link, Pid2} ->
            #link{target = Pid2};
        {trace, Pid, unlink, Pid2} ->
            #unlink{target = Pid2};
        {trace, PidPort, getting_linked, Pid2} ->
            #getting_linked{from = Pid2};
        {trace, PidPort, getting_unlinked, Pid2} ->
            #getting_unlinked{from = Pid2};
        {trace, Port, open, Pid, Driver} ->
            #open{port = Port, driver = Driver};
        {trace, Port, closed, Reason} ->
            #closed{reason = Reason};
        {trace, PidPort, MsgType, Command} when MsgType =:= in orelse MsgType =:= in_exiting ->
            (case Command of
                0 ->
                    #in{command = undefined};
                _ ->
                    #in{command = Command}
            end)#in{exiting = MsgType =:= in_exiting};
        {trace, Pid, MsgType, Command} when MsgType =:= out orelse MsgType =:= out_exiting orelse MsgType =:= out_exited->
            (case Command of
                0 ->
                    #out{command = undefined};
                _ ->
                    #out{command = Command}
            end)#out{exiting = MsgType =:= out_exiting, exited = MsgType =:= out_exited};
        {trace, Pid, MsgType, Info} when MsgType =:= gc_minor_start orelse MsgType =:= gc_minor_end orelse MsgType =:= gc_major_start orelse MsgType =:= gc_major_end ->
            (case MsgType of
                gc_minor_start -> #gc{type = minor, phase = start};
                gc_minor_end   -> #gc{type = minor, phase = 'end'};
                gc_major_start -> #gc{type = major, phase = start};
                gc_major_end   -> #gc{type = major, phase = 'end'}
            end)#gc{info = Info};
        {trace, Pid, gc_max_heap_size, Info} ->
            #gc_max_heap_size{info = Info};
        _ ->
            error_logger:info_msg("Unknown: Msg: ~p~n", [TraceMsg]),
            undefined
    end.

convert_trace_ts(Msg) when element(1, Msg) =:= trace_ts ->
    [trace_ts | Rest] = tuple_to_list(Msg),
    {Timestamp, Data} = convert_trace_ts_(Rest),
    {Timestamp, list_to_tuple([trace | Data])}.

convert_trace_ts_([Timestamp]) ->
    {Timestamp, []};
convert_trace_ts_([Head | Tail]) ->
    {Timestamp, Acc} = convert_trace_ts_(Tail),
    {Timestamp, [Head | Acc]}.