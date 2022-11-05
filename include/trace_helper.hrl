-record(send, {msg :: any(), to :: any(), exists :: boolean()}).
-record(recv, {msg :: any()}).
-record(call, {mod :: module(), function :: atom(), args :: list()}).
-record(return, {direction :: to | from, mod :: module(), function :: atom(), arity :: non_neg_integer()}).
-record(exception, {mod :: module(), function :: atom(), arity :: non_neg_integer(), class :: atom(), value :: any()}).
-record(spawn, {pid :: pid(), mod :: module(), function :: atom(), args :: list() | any()}).
-record(spawned, {pid :: pid(), mod :: module(), function :: atom(), args :: list() | any()}).
-record(exit, {reason :: any()}).
-record(register, {name :: any()}).
-record(unregister, {name :: any()}).
-record(link, {target :: pid()}).
-record(unlink, {target :: pid()}).
-record(getting_linked, {from :: pid()}).
-record(getting_unlinked, {from :: pid()}).
-record(open, {port :: port(), driver :: atom()}).
-record(closed, {reason :: any()}).
-record(in, {command :: undefined | {module(), atom(), list()} | atom(), exiting :: boolean()}).
-record(out, {command :: undefined | {module(), atom(), list()} | atom(), exiting :: boolean(), exited :: boolean()}).
-record(gc, {type :: minor | major, phase :: start | 'end', info :: proplists:proplist()}).
-record(gc_max_heap_size, {info :: proplists:proplist()}).
-record(timestamped, {timestamp :: os:timestamp(), data :: traced_msg()}).

-type traced_msg() :: #send{}
                    | #recv{} 
                    | #call{} 
                    | #return{} 
                    | #exception{} 
                    | #spawn{} 
                    | #spawned{} 
                    | #exit{} 
                    | #register{} 
                    | #unregister{} 
                    | #link{} 
                    | #unlink{} 
                    | #getting_linked{} 
                    | #getting_unlinked{} 
                    | #open{} 
                    | #closed{} 
                    | #in{} 
                    | #out{} 
                    | #gc{} 
                    | #gc_max_heap_size{}
                    | #timestamped{}.