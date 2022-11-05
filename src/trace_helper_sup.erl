-module(trace_helper_sup).
-behaviour(supervisor).

-export([start_link/0, start_child/3]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(PidPort, Flags, MFAMatchSpec) ->
    supervisor:start_child(?MODULE, [PidPort, Flags, MFAMatchSpec]).

init([]) ->
    SupFlags = #{
        strategy => simple_one_for_one, 
        intensity => 50, 
        period => 10
    },
    Children = [
        #{
            id => trace_helper_worker_srv,
            start => {trace_helper_worker_srv, start_link, []},
            restart => transient,
            shutdown => 2000,
            type => worker
        }
    ],
    {ok, {SupFlags, Children}}.