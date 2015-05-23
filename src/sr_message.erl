%%%----------------------------------------------------------------------------
%%% @author Martin Wiso <martin@wiso.cz>
%%% @doc
%%% Helper for formatting messages in Slack way
%%% @end
%%%----------------------------------------------------------------------------
-module(sr_message).

-include("sr.hrl").

%% API
-export([ format/3
        , format/4
        , format/5
        , format_table/4
        , format_table/5
        ]).

%% Types
-type field_title() :: atom() | title | value.
-type field()       :: {field_title(), binary()} | map().
-type fields()      :: [field()].
-export_type([fields/0]).

%%%============================================================================
%%% API
%%%============================================================================
-spec format(binary(), binary(), binary()) -> binary().
format(Title, Message, Color) ->
    to_json(#{<<"fallback">>  => Message,
              <<"title">>     => Title,
              <<"text">>      => Message,
              <<"color">>     => Color}).

-spec format(binary(), binary(), binary(), binary()) -> binary().
format(Title, Message, Color, IconUrl) ->
    to_json(#{<<"fallback">>  => Message,
              <<"title">>     => Title,
              <<"text">>      => Message,
              <<"icon_url">>  => IconUrl,
              <<"color">>     => Color}).

-spec format(binary(), binary(), binary(), binary(), binary()) -> binary().
format(Title, Message, Color, URL, IconUrl) ->
    to_json(#{<<"fallback">>  => Message,
              <<"title">>     => Title,
              <<"text">>      => Message,
              <<"icon_url">>  => IconUrl,
              <<"image_url">> => URL,
              <<"color">>     => Color}).

-spec format_table(binary(), binary(), fields(), binary()) -> binary().
format_table(Title, Message, Fields, Color) ->
    to_json(#{<<"fallback">>  => Message,
              <<"title">>     => Title,
              <<"text">>      => Message,
              <<"fields">>    => lists:map(fun format_field/1, Fields),
              <<"color">>     => Color}).

-spec format_table(binary(), binary(), fields(), binary(), binary()) -> binary().
format_table(Title, Message, Fields, Color, IconUrl) ->
    to_json(#{<<"fallback">>  => Message,
              <<"title">>     => Title,
              <<"text">>      => Message,
              <<"fields">>    => lists:map(fun format_field/1, Fields),
              <<"icon_url">>  => IconUrl,
              <<"color">>     => Color}).


%%%============================================================================
%%% Internal functions
%%%============================================================================
format_field(Field) when is_map(Field) ->
    maps:put(<<"short">>, true, Field);
format_field(Field) ->
    #{<<"title">> => proplists:get_value(title, Field),
      <<"value">> => proplists:get_value(value, Field),
      <<"short">> => true
     }.

to_json(Struct) ->
    jiffy:encode([Struct]).
