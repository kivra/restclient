-module(restc_body).

-export([encode/2, decode/3]).

encode(json, Body) ->
  jsx:encode(Body);
encode(percent, Body) ->
  lists:map(fun({K, V}) ->
                {restc_util:to_binary(K), restc_util:to_binary(V)}
            end, Body),
  binary_to_list(hackney_url:qs(Body, []));
encode(xml, Body) ->
  lists:flatten(xmerl:export_simple(Body, xmerl_xml)).

decode(_, <<>>, _Opts)                      -> [];
decode(<<"application/json">>, Body, Opts)  -> jsx:decode(Body, Opts);
decode(<<"application/xml">>, Body, _Opts)  ->
  {ok, Data, _} = erlsom:simple_form(binary_to_list(Body)),
  Data;
decode(<<"text/xml">>, Body, Opts)   ->
  decode(<<"application/xml">>, Body, Opts);
decode(<<"image/png">>, Body, _Opts) -> Body;
decode(_, Body, _Opts)               -> Body.
