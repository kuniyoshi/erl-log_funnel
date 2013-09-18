-module(disc_note_worker).
-behaviour(gen_server).
-include_lib("eunit/include/eunit.hrl").
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% ------------------------------------------------------------------
%% application Function Exports
%% ------------------------------------------------------------------

-export([open/1,
         reopen/0,
         append/1]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
    ?debugMsg("init"),
    {ok, Args}.

handle_call({open, Filename}, _From, State) ->
    ?debugMsg("handle_call with filename"),
    ?debugVal(Filename),
    {ok, State2} = handle_open(Filename, State),
    {reply, ok, State2}.

handle_cast(reopen, State) ->
    ?debugMsg("reopen"),
    {ok, State2} = handle_reopen(State),
    {noreply, State2};
handle_cast({close, IoDevice}, State) ->
    ?debugMsg("close"),
    timer:sleep(1000),  % wait other process which may be still writing.
    ok = file:close(IoDevice),
    {noreply, State};
handle_cast({logging, Message}, State) ->
    ?debugMsg("handle_cast"),
    io:format("logging~n"),
    {ok, State2} = handle_append(Message, State),
    {noreply, State2}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    ?debugMsg("terminate"),
    ok = handle_terminate(State).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

handle_open(Filename, State) ->
    {ok, IoDevice} = file:open(Filename, [append]),
    State2 = lists:keystore(io_device, 1, State, {io_device, IoDevice}),
    State3 = lists:keystore(filename, 1, State2, {filename, Filename}),
    {ok, State3}.

handle_reopen(State) ->
    {io_device, IoDevice} = lists:keyfind(io_device, 1, State),
    {filename, Filename} = lists:keyfind(filename, 1, State),
    {ok, State2} = handle_open(Filename, State),
    ok = file:close(IoDevice),
    {ok, State2}.

handle_append(Message, State) when is_binary(Message) ->
    handle_append(binary_to_list(Message), State);
handle_append(Message, State) ->
    ?debugVal(Message),
    ?debugVal(State),
    Message2 = lib:nonl(Message),
    ?debugVal(Message2),
    {io_device, IoDevice} = lists:keyfind(io_device, 1, State),
    ?debugVal(IoDevice),
    ok = file:write(IoDevice, string:concat(Message2, "\n")),
    {ok, State}.

handle_terminate(State) ->
    {io_device, IoDevice} = lists:keyfind(io_device, 1, State),
    ok = file:close(IoDevice).

open(Filename) ->
    gen_server:call(?MODULE, {open, Filename}).

reopen() ->
    gen_server:cast(?MODULE, reopen).

append(Message) ->
    gen_server:cast(?MODULE, {append, Message}).
