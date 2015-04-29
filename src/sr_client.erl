%%%----------------------------------------------------------------------------
%%% @author Martin Wiso <martin@wiso.cz>
%%% @doc
%%% @end
%%%----------------------------------------------------------------------------
-module(sr_client).

-behaviour(websocket_client_handler).

-include("sr.hrl").

%% API
-export([ start_link/1
        , send/1,
          send/2
        ]).

%% websocket_client_handler callbacks
-export([ init/2
         , websocket_handle/3
         , websocket_info/3
         , websocket_terminate/3
        ]).

-define(SERVER, ?MODULE).

%%%============================================================================
%%% API
%%%============================================================================
-spec start_link(binary()) -> {ok, pid()} | {error, any()}.
start_link(WSS) ->
    websocket_client:start_link(?b2l(WSS), ?MODULE, []).

-spec send(binary()) -> ok.
send(Payload) ->
    send(self(), Payload).

-spec send(pid(), binary()) -> ok.
send(Pid, Payload) ->
    websocket_client:cast(Pid, {text, Payload}).

%%%============================================================================
%%% websocket_client_handler callbacks
%%%============================================================================
init([], _ConnState) ->
    {ok, []}.

websocket_handle({text, JSON}, _ConnState, State) ->
    Map = parse_json(JSON),
    case maps:get(<<"type">>, Map) of
        <<"hello">> ->
            {reply, ok, State};
        _Other ->
            {reply, {ok, Map}, State}
    end;
websocket_handle(Msg, _ConnState, State) ->
    {reply, {error, Msg}, State}.

websocket_info(_Info, _ConnState, State) ->
    {reply, {text, <<>>}, State}.

websocket_terminate(_Reason, _ConnState, _State) ->
    ok.

%%%============================================================================
%%% Internal functions
%%%============================================================================
parse_json(Bin) ->
    jiffy:decode(Bin, [return_maps]).
