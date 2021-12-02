-module(restc_body).

-export([encode/2, decode/3]).

encode(json, Body) ->
    jsx:encode(Body);
encode(percent, Body) when is_map(Body) ->
    hackney_url:qs(maps:to_list(Body), []);
encode(percent, Body) ->
    hackney_url:qs(Body, []);
encode(xml, Body) ->
    lists:flatten(xmerl:export_simple(Body, xmerl_xml)).

decode(_, <<>>, Opts) ->
    case proplists:get_bool(return_maps, Opts) of
        true -> #{};
        _ -> []
    end;
decode(<<"application/json">>, Body, Opts0) ->
    Opts =
        case lists:member(return_maps, Opts0) of
            true -> [return_maps];
            false -> []
        end,
    jsx:decode(Body, Opts);
decode(<<"application/xml">>, Body, _Opts) ->
    {ok, Data, _} = erlsom:simple_form(binary_to_list(Body)),
    Data;
decode(<<"text/xml">>, Body, Opts) ->
    decode(<<"application/xml">>, Body, Opts);
decode(<<"image/png">>, Body, _Opts) ->
    Body;
decode(<<"application/x-www-form-urlencoded">>, Body, Opts) ->
    KeyValueList = hackney_url:parse_qs(Body),
    case proplists:get_bool(return_maps, Opts) of
        true -> maps:from_list(KeyValueList);
        _ -> KeyValueList
    end;
decode(_, Body, _Opts) ->
    Body.
