-module(restc_util).

-export([string_to_binary/1, to_binary/1]).

string_to_binary(V) when is_binary(V) -> V;
string_to_binary(V) when is_list(V)   -> list_to_binary(V).

to_binary(V) when is_integer(V) -> integer_to_binary(V);
to_binary(V) when is_atom(V)    -> atom_to_binary(V, utf8);
to_binary(V)                    -> string_to_binary(V).
