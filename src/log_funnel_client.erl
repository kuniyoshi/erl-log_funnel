-module(log_funnel_client).
-export([open/1, reopen/0, append/1, queue_len/0]).
-define(WORKER, log_funnel_worker).

open(Filename) ->
    ok = gen_server:call(?WORKER, {open, Filename}).

reopen() ->
    gen_server:cast(?WORKER, reopen).

append(Message) ->
    gen_server:cast(?WORKER, {append, Message}).

queue_len() ->
    {message_queue_len, Len} = erlang:process_info(whereis(?WORKER), message_queue_len),
    Len.
