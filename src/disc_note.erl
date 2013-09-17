-module(disc_note).
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
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
    ?debugMsg("init"),
    {ok, Args}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(Msg, State) ->
    ?debugMsg("handle_cast"),
    State2 = case Msg of
        reopen ->
           {ok, NewState} = reopen(State),
            NewState;
        {logging, Msg2} ->
            {ok, NewState} = note(Msg2, State),
            NewState
    end,
    {noreply, State2}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    ?debugMsg("terminate"),
    ok = close_note(State).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

note(Msg, State) ->
    {ok, State}.

close_note(State) ->
    {ok, State}.

reopen(State) ->
    {ok, State}.
