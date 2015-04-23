%%%----------------------------------------------------------------------------
%%% @author Martin Wiso <martin@wiso.cz>
%%% @doc
%%% @end
%%%----------------------------------------------------------------------------
-module(sr_app).

-behaviour(application).

%% API
-export([start/2, stop/1]).

-include("sr.hrl").

%%%============================================================================
%%% Application callbacks
%%%============================================================================
-spec start(normal | {takeover, node()} | {failover, node()}, term()) ->
                   {ok, pid()} | {ok, pid(), term()} | {error, any()}.
start(_StartType, _StartArgs) ->
    sr_sup:start_link().

-spec stop(term()) -> ok.
stop(_State) ->
    ok.
