%%% @doc Verifies that encoding/decoding results in the same value.
%%%
%%% To make it a bit easier to test this only tests non-empty binary keys and
%%% values, as percent encoding changes the output otherwise (as it should).
%%% @end
-module(prop_restc_body).
-include_lib("proper/include/proper.hrl").

prop_json_as_maps() ->
    ?FORALL(
        Object,
        object(),
        Object =:=
            restc_body:decode(
                <<"application/json">>,
                restc_body:encode(json, Object),
                [return_maps]
            )
    ).

prop_json_as_proplists() ->
    ?FORALL(
        Object,
        proplist(),
        Object =:=
            restc_body:decode(
                <<"application/json">>,
                restc_body:encode(json, Object),
                []
            )
    ).

prop_percent_as_maps() ->
    ?FORALL(
        Object,
        object(),
        Object =:=
            restc_body:decode(
                <<"application/x-www-form-urlencoded">>,
                restc_body:encode(percent, Object),
                [return_maps]
            )
    ).

prop_percent_as_proplists() ->
    ?FORALL(
        Object,
        proplist(),
        Object =:=
            restc_body:decode(
                <<"application/x-www-form-urlencoded">>,
                restc_body:encode(percent, Object),
                []
            )
    ).

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%
object() ->
    map(non_empty_text(), non_empty_text()).

proplist() ->
    list({non_empty_text(), non_empty_text()}).

non_empty_text() ->
    non_empty(utf8()).
