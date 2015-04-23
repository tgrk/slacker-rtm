%%%----------------------------------------------------------------------------
%%% @author Martin Wiso <martin@wiso.cz>
%%% @doc
%%% @end
%%%----------------------------------------------------------------------------
-module(sr).

%% API
-export([ connect/1
        , start/0
        , stop/0
        ]).

-include("sr.hrl").

-define(BASE_URL, "https://slack.com/").

%% Types
-type scope() :: atom() | identify | client | read | post | admin.

%%%============================================================================
%%% API
%%%============================================================================
-spec connect(binary()) -> {ok, pid()} | {error, any()}.
connect(Token) ->
    {ok, _Headers, {Response}} = call_api(<<"rtm.start">>,
                                          [{token, to_binary(Token)}]),
    case proplists:get_value(<<"ok">>, Response, <<"false">>) of
        true  ->
            WSS   = proplists:get_value(<<"url">>, Response),
            case sr_sup:start_client(WSS) of
                Pid when is_pid(Pid) ->
                    {ok, Pid};
                Error ->
                    Error
            end;
        false ->
            {error, {unable_to_connect, Response}}
    end.

%%%============================================================================
%%% Application callbacks
%%%============================================================================
-spec start() -> {ok, pid()} | {ok, pid(), term()} | {error, any()}.
start() ->
    [ensure_started(D) || D <- deps()],
    ok.

-spec stop() -> ok.
stop() ->
    [application:stop(D) || D <- deps()],
    ok.

%%%============================================================================
%%% Internal functionality
%%%============================================================================
deps() ->
    [crypto, ssl, inets, jiffy, websocket_client, ?APP].

ensure_started(App) ->
    case application:ensure_all_started(App) of
        {ok, _Deps} ->
            ok;
        {error, {already_started, App}} ->
            ok;
        {error, _} = Error ->
            throw({error, {unable_to_start, App, Error}})
    end.

call_api(UrlType, Args) when is_list(Args) ->
    Url = get_url(UrlType) ++ "?" ++ http_flatten_args(Args),
    error_logger:info_msg("slacker-rtm: get url=~p~n", [Url]),
    case http_request(Url, get) of
        {ok, Headers, Response} ->
            {ok, Headers, parse_response(Headers, Response)};
        {error, Reason} ->
            {error, Reason}
    end.

parse_response(Headers, Response) ->
    case get_content_type(Headers) of
        "text/html" ->
            Response;
        "application/json" ->
            jiffy:decode(to_binary(Response))
    end.

get_content_type(Headers) ->
    ContentType = proplists:get_value("content-type", Headers, "text/html"),
    [Type | _] = string:tokens(ContentType, ";"),
    Type.

http_request(Url, get) ->
    case httpc:request(get, {Url, []}, [{timeout, 60000}], []) of
        {ok, {{_, 200, _}, Headers, Response}} ->
            {ok, Headers, Response};
        {ok, {{_, Status, _}, Headers, Response}} ->
            {error, {Status, Headers, Response}}
    end.

get_url(Method) ->
    ?BASE_URL ++ "api/" ++ to_list(Method).

http_flatten_args(Args) ->
    string:join([to_list(K) ++ "=" ++ http_uri:encode(to_list(V))
                 || {K, V} <- Args], "&").

to_binary(Value) when is_list(Value) ->
    list_to_binary(Value);
to_binary(Value) ->
    Value.

to_list(Value) when is_binary(Value) ->
    ?b2l(Value);
to_list(Value) when is_integer(Value) ->
    ?i2l(Value);
to_list(Value) when is_atom(Value) ->
    ?a2l(Value);
to_list(Value) ->
    Value.
