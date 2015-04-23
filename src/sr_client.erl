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

%%%============================================================================
%%% API
%%%============================================================================
-spec start_link(binary()) -> {ok, any()} | {error, any()}.
start_link(WSS) ->
    error_logger:info_msg("rtm start ~p~n", [WSS]),
    websocket_client:start_link(?b2l(WSS), ?MODULE, []).

%%%============================================================================
%%% gen_server callbacks
%%%============================================================================
init([], ConnState) ->
    error_logger:info_msg("rtm init ~p~n", [ConnState]),
    {ok, []}.

websocket_handle(Msg, ConnState, State) ->
    error_logger:info_msg("RTM handle ~p ~p, ~p~n",
                          [Msg, ConnState, State]),
    {reply, <<>>, State}.

websocket_info(Msg, ConnState, State) ->
    error_logger:info_msg("RTM info ~p ~p, ~p~n", [Msg, ConnState, State]),
    {reply, <<>>, State}.

websocket_terminate(Reason, _ConnState, State) ->
    error_logger:info_msg("RTM closed in state ~p wih reason ~p~n",
                          [State, Reason]),
    ok.

%%%============================================================================
%%% Internal functions
%%%============================================================================
