%%%----------------------------------------------------------------------------
%%% @author Martin Wiso <martin@wiso.cz>
%%% @doc
%%% @end
%%%----------------------------------------------------------------------------
-module(sr_sup).

-behaviour(supervisor).

-include("sr.hrl").

%% API
-export([start_link/0, start_client/1]).

%% Supervisor callbacks
-export([init/1]).

-define(CHILD(I, Type, Params),
    {I, {I, start_link, Params}, permanent, 5000, Type, [I]}).

%%%============================================================================
%%% API functions
%%%============================================================================
-spec start_link() -> {ok, pid()} | ignore | {error, any()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec start_client(binary()) -> pid() | no_return().
start_client(WSS) ->
    case supervisor:start_child(?MODULE, ?CHILD(sr_client, worker, [WSS])) of
        {error, Reason} ->
            throw({unable_to_start_client, Reason});
        {ok, Pid} ->
            Pid
    end.

%%%============================================================================
%%% Supervisor callbacks
%%%============================================================================
init([]) ->
    {ok, {{one_for_one, 5, 10}, []}}.
