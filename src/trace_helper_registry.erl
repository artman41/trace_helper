-module(trace_helper_registry).

-export([next_id/0]).
-export([new/0, all/0]).
-export([whereis_name/1, register_name/2, unregister_name/1, send/2]).

-record(entry, {
    worker_id :: non_neg_integer(),
    worker_pid :: pid()
}).

-define(TABLE, ?MODULE).

next_id() ->
    ets:update_counter(?TABLE, counter, {#entry.worker_pid, 1}).

new() ->
    ets:new(?TABLE, [public, named_table, {keypos, #entry.worker_id}, {write_concurrency, false}, {read_concurrency, true}]),
    %% Abusing the fact that we have a table to store a counter in there
    ets:insert(?TABLE, #entry{worker_id = counter, worker_pid = 0}).

all() ->
    ets:tab2list(?TABLE).

-spec whereis_name(WorkerId :: non_neg_integer()) -> pid() | undefined.
whereis_name(WorkerId) when is_integer(WorkerId) andalso WorkerId > 0 ->
    case ets:lookup(?TABLE, WorkerId) of
        [#entry{worker_id = WorkerId, worker_pid = Pid}] ->
            Pid;
        _ ->
            undefined
    end.

-spec register_name(WorkerId :: any(), pid()) -> yes | no.
register_name(WorkerId, Pid) when is_integer(WorkerId) andalso WorkerId > 0 ->
    case ets:insert_new(?TABLE, #entry{worker_id = WorkerId, worker_pid = Pid}) of
        true -> yes;
        false -> no
    end.

-spec unregister_name(Name :: any()) -> true.
unregister_name(WorkerId) when is_integer(WorkerId) andalso WorkerId > 0 ->
    ets:delete(?TABLE, WorkerId).

-spec send(WorkerId :: non_neg_integer(), Msg :: any()) -> pid().
send(WorkerId, Msg) ->
    case whereis_name(WorkerId) of
        Pid when is_pid(Pid) ->
            Pid ! Msg,
            Pid;
        undefined ->
            exit({badarg, {WorkerId, Msg}})
    end.