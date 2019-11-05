-module(restc_body).

-export([encode/2, decode/2]).

encode(json, Body) ->
  jsx:encode(Body);
encode(percent, Body) ->
  lists:map(fun({K, V}) ->
                {restc_util:to_binary(K), restc_util:to_binary(V)}
            end, Body),
  binary_to_list(hackney_url:qs(Body, []));
encode(xml, Body) ->
  lists:flatten(xmerl:export_simple(Body, xmerl_xml)).

decode(_, <<>>)                      -> [];
decode(<<"application/json">>, Body) -> jsx:decode(Body);
decode(<<"application/xml">>, Body)  ->
  {ok, Data, _} = erlsom:simple_form(binary_to_list(Body)),
  Data;
decode(<<"text/xml">>, Body)  -> decode(<<"application/xml">>, Body);
decode(<<"image/png">>, Body) -> Body;
decode(_, Body)               -> Body.
