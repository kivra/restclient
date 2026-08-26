-module(restc_body).

-export([encode/2, decode/3]).

%% jsx represented the empty JSON object as [{}] rather than []; the latter is
%% an empty array. That distinction is part of the public decoding contract, so
%% it is preserved here now that the OTP `json` module does the work.
-define(EMPTY_OBJECT, [{}]).

encode(json, Body) ->
    iolist_to_binary(json:encode(Body, fun encode_json/2));
encode(percent, Body) when is_map(Body) ->
    hackney_url:qs(maps:to_list(Body), []);
encode(percent, Body) ->
    hackney_url:qs(Body, []);
encode(xml, Body) ->
    lists:flatten(xmerl:export_simple(Body, xmerl_xml));
encode(multi, Body) ->
    {multipart, Body}.

decode(_, <<>>, Opts) ->
    case return_maps(Opts) of
        true -> #{};
        false -> []
    end;
decode(<<"application/json">>, Body, Opts) ->
    {Decoded, _Acc, _Rest} = json:decode(Body, ok, json_decoders(Opts)),
    Decoded;
decode(<<"application/xml">>, Body, _Opts) ->
    {ok, Data, _} = erlsom:simple_form(binary_to_list(Body)),
    Data;
decode(<<"text/xml">>, Body, Opts) ->
    decode(<<"application/xml">>, Body, Opts);
decode(<<"image/png">>, Body, _Opts) ->
    Body;
decode(<<"application/x-www-form-urlencoded">>, Body, Opts) ->
    KeyValueList = hackney_url:parse_qs(Body),
    case return_maps(Opts) of
        true -> maps:from_list(KeyValueList);
        false -> KeyValueList
    end;
decode(_, Body, _Opts) ->
    Body.

%%% INTERNAL ===================================================================

return_maps(Opts) ->
    proplists:get_bool(return_maps, Opts).

%% json:encode/2 encodes a proplist as an array of two-element arrays, so
%% objects given as proplists need to be recognised before falling back to the
%% default encoder that handles maps, lists and scalars.
encode_json(?EMPTY_OBJECT, _Encode) ->
    <<"{}">>;
encode_json([{_, _} | _] = Proplist, Encode) ->
    json:encode_key_value_list(Proplist, Encode);
encode_json(Other, Encode) ->
    json:encode_value(Other, Encode).

json_decoders(Opts) ->
    case return_maps(Opts) of
        true ->
            #{object_finish => fun finish_map/2};
        false ->
            #{
                object_start => fun(_Acc) -> [] end,
                object_push => fun push_member/3,
                object_finish => fun finish_proplist/2
            }
    end.

%% The default object_finish keeps the first of a set of duplicate keys; jsx
%% kept the last. Reversing before building the map restores that.
finish_map(Members, OldAcc) ->
    {maps:from_list(lists:reverse(Members)), OldAcc}.

push_member(Key, Value, Members) ->
    [{Key, Value} | Members].

finish_proplist([], OldAcc) ->
    {?EMPTY_OBJECT, OldAcc};
finish_proplist(Members, OldAcc) ->
    {lists:reverse(Members), OldAcc}.
