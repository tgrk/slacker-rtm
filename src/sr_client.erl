%%%----------------------------------------------------------------------------
%%% @author Martin Wiso <martin@wiso.cz>
%%% @doc
%%% @end
%%%----------------------------------------------------------------------------
-module(sr_client).

-behaviour(websocket_client_handler).

-include("sr.hrl").

%% API
-export([start_link/1]).

%% websocket_client_handler callbacks
-export([init/2,
         websocket_handle/3,
         websocket_info/3,
         websocket_terminate/3
        ]).

-define(SERVER, ?MODULE).

%%%============================================================================
%%% API
%%%============================================================================
start_link(WSS) ->
    websocket_client:start_link(?b2l(WSS), ?MODULE, []).

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
        Other ->
            {reply, {error, Other}, State}
    end;
websocket_handle(Msg, _ConnState, State) ->
    error_logger:info_msg("Received msg ~p~n", [Msg]),
    {reply, unknown, State}.

websocket_info(Info, _ConnState, State) ->
    error_logger:info_msg("Received info ~p~n", [Info]),
    {reply, {text, <<"erlang message received">>}, State}.

websocket_terminate(Reason, _ConnState, State) ->
    error_logger:info_msg("Websocket closed in state ~p wih reason ~p~n",
              [State, Reason]),
    ok.

%%%============================================================================
%%% Internal functions
%%%============================================================================
parse_json(Bin) ->
    jiffy:decode(Bin, [return_maps]).
