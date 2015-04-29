%%%----------------------------------------------------------------------------
%%% @author Martin Wiso <martin@wiso.cz>
%%% @doc
%%% Helper for formatting messages in Slack way
%%% @end
%%%----------------------------------------------------------------------------
-module(sr_message).

-include("sr.hrl").

%% API
-export([ format/4
        , format/5
        , format_table/5
        ]).

%% Types
-type field_title() :: atom() | title | value.
-type field()       :: {field_title(), binary()}.
-type fields()      :: [field()].
-export_type([fields/0]).

%%%============================================================================
%%% API
%%%============================================================================
-spec format(binary(), binary(), binary(), binary()) -> binary().
format(Title, Message, Color, IconUrl) ->
    to_json({[{<<"fallback">>, ?l2b(Message)},
              {<<"title">>,    ?l2b(Title)},
              {<<"text">>,     ?l2b(Message)},
              {<<"icon_url">>, IconUrl},
              {<<"color">>,    Color}
             ]}).

-spec format(binary(), binary(), binary(), binary(), binary()) -> binary().
format(Title, Message, Color, URL, IconUrl) ->
    to_json({[{<<"fallback">>,  ?l2b(Message)},
              {<<"title">>,     ?l2b(Title)},
              {<<"text">>,      ?l2b(Message)},
              {<<"icon_url">>,  IconUrl},
              {<<"image_url">>, ?l2b(URL)},
              {<<"color">>,     Color}
             ]}).


-spec format_table(binary(), binary(), fields(), binary(), binary()) -> binary().
format_table(Title, Message, Fields, Color, IconUrl) ->
    to_json({[{<<"fallback">>, ?l2b(Message)},
              {<<"title">>,    ?l2b(Title)},
              {<<"text">>,     ?l2b(Message)},
              {<<"fields">>,   lists:map(fun format_field/1, Fields)},
              {<<"icon_url">>, IconUrl},
              {<<"color">>,    Color}
             ]}).


%%%============================================================================
%%% Internal functions
%%%============================================================================
format_field(Field) ->
    {[{<<"title">>, proplists:get_value(title, Field)},
      {<<"value">>, proplists:get_value(value, Field)},
      {<<"short">>, true}
     ]}.

to_json(Struct) ->
    jiffy:encode([Struct]).
