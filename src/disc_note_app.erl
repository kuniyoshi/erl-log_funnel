-module(disc_note_app).
-behaviour(application).
-export([start/2, stop/1]).
-export([open/1, reopen/0, append/1]).
-define(WORKER, disc_note_worker).

start(_StartType, _StartArgs) ->
    disc_note_sup:start_link().

stop(_State) ->
    ok.

open(Filename) ->
    ok = gen_server:call(?WORKER, {open, Filename}).

reopen() ->
    gen_server:cast(?WORKER, reopen).

append(Message) ->
    gen_server:cast(?WORKER, {drain, Message}).
