-module(prop_restc_body).
-include_lib("proper/include/proper.hrl").

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_decode_encode_json() ->
    ?FORALL(Obj, object_proplist(),
            Obj =:= restc_body:decode(<<"application/json">>,
                                      restc_body:encode(json, Obj))).

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%
object_proplist() ->
    list({key(), value()}).

key() ->
    utf8().

value() ->
    oneof([utf8(), integer()]).
