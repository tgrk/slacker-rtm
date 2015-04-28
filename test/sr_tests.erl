%%%----------------------------------------------------------------------------
%%% @author Martin Wiso <martin@wiso.cz>
%%% @doc
%%% @end
%%%----------------------------------------------------------------------------
-module(sr_tests).

-compile(export_all).

-include("sr.hrl").
-include_lib("eunit/include/eunit.hrl").

%%%=============================================================================
sr_test_() ->
    {setup,
     fun ()  -> sr:start() end,
     fun (_) -> sr:stop() end,
     [
      {"Connect without token", {timeout, 60, fun test_invalid_connect/0}},
      {"Connect with token",    {timeout, 60, fun test_valid_connect/0}}
     ]
    }.

%%%=============================================================================
test_invalid_connect() ->
    {Code, {unable_to_connect, Map}} = sr:connect(<<>>),
    ?assertEqual(error, Code),
    ?assertEqual(false, maps:get(<<"ok">>, Map)),
    ?assertEqual(<<"not_authed">>, maps:get(<<"error">>, Map)),
    ok.

test_valid_connect() ->
    {ok, Token} = read_api_token(),
    {Code, Pid} = sr:connect(Token),
    ?assertEqual(ok, Code),
    ?assert(is_pid(Pid)).

%%%============================================================================
%%% Internal functionality
%%%============================================================================
read_api_token() ->
    {ok, [PL]} = file:consult("../test/api_token.term"),
    {ok, proplists:get_value(token, PL)}.
