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
        , format/6
        ]).

%%%============================================================================
%%% API
%%%============================================================================
format(Title, Message, Color, IconUrl) ->
    to_json({[{<<"fallback">>, ?l2b(Message)},
              {<<"title">>,    ?l2b(Title)},
              {<<"text">>,     ?l2b(Message)},
              {<<"icon_url">>, IconUrl},
              {<<"color">>,    Color}
             ]}).

format(Title, Message, Fields, Color, IconUrl) ->
    to_json({[{<<"fallback">>, ?l2b(Message)},
              {<<"title">>,    ?l2b(Title)},
              {<<"text">>,     ?l2b(Message)},
              format_fields(Fields),
              {<<"icon_url">>, IconUrl},
              {<<"color">>,    Color}
             ]}).

format(Title, Message, [], Color, URL, IconUrl) ->
    to_json({[{<<"fallback">>,  ?l2b(Message)},
              {<<"title">>,     ?l2b(Title)},
              {<<"text">>,      ?l2b(Message)},
              {<<"icon_url">>,  IconUrl},
              {<<"image_url">>, ?l2b(URL)},
              {<<"color">>,     Color}
             ]}).


%%%============================================================================
%%% Internal functions
%%%============================================================================
format_fields(Fields) ->
    {<<"fields">>, lists:map(fun format_field/1, Fields)}.

format_field(Field) ->
    {[{<<"title">>, proplists:get_value(title, Field)},
      {<<"value">>, proplists:get_value(value, Field)},
      {<<"short">>, true}
     ]}.

to_json(Struct) ->
    jiffy:encode([Struct]).
