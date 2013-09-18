-module(disc_note_client).
-export([open/1, reopen/0, append/1]).
-define(WORKER, disc_note_worker).

open(Filename) ->
    ok = gen_server:call(?WORKER, {open, Filename}).

reopen() ->
    gen_server:cast(?WORKER, reopen).

append(Message) ->
    gen_server:cast(?WORKER, {append, Message}).
