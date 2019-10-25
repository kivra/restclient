-module(prop_restc).
-include_lib("proper/include/proper.hrl").

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_test() ->
    ?FORALL({S,H,P,Q}, {scheme(), host(), path(), query_elements()},
        begin
          BaseUrlBin = <<S/binary, "://", H/binary>>,
          BaseUrl = binary_to_list(BaseUrlBin),
          Path = binary_to_list(P),
          Query = lists:map(fun({K, V}) ->
                                {binary_to_list(K), binary_to_list(V)}
                            end, Q),
          Oracle = construct_url_oracle(BaseUrl, Path, Query),
          RestC0 = restc:construct_url(BaseUrl, Path, Query, [return_binary]),

          RestC = case P of
                    P when P =:= <<>>; P =:= <<"/">> ->
                      {match, [[Left, Right]]} =
                        re:run(RestC0,
                               <<"(https?://.*)/(\\??.*)">>,
                               [global, {capture, all_but_first, binary}]),
                      <<Left/binary, Right/binary>>;
                    _ -> RestC0
                  end,
          erlang:display({Oracle, binary_to_list(RestC)}),
          string:lowercase(Oracle) =:= string:lowercase(binary_to_list(RestC))
        end).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%

construct_url_oracle(FullPath, Query) when is_binary(FullPath) ->
  construct_url_oracle(binary_to_list(FullPath), Query);
construct_url_oracle(FullPath, Query) when is_list(FullPath) ->
  {S, N, P, _, _} = mochiweb_util:urlsplit(FullPath),
  Q = mochiweb_util:urlencode(Query),
  mochiweb_util:urlunsplit({S, N, P, Q, []}).

construct_url_oracle(SchemeNetloc, Path, Query) when is_binary(SchemeNetloc) ->
  construct_url_oracle(binary_to_list(SchemeNetloc), Path, Query);
construct_url_oracle(SchemeNetloc, Path, Query) when is_binary(Path) ->
  construct_url_oracle(SchemeNetloc, binary_to_list(Path), Query);
construct_url_oracle(SchemeNetloc, Path, Query) when is_list(SchemeNetloc),
                                              is_list(Path) ->
  {S, N, P1, _, _} = mochiweb_util:urlsplit(SchemeNetloc),
  {_, _, P2, _, _} = mochiweb_util:urlsplit(Path),
  P = path_cat(P1, P2),
  urlunsplit(S, N, P, Query).

urlunsplit(S, N, P, Query) ->
  Q = mochiweb_util:urlencode(Query),
  mochiweb_util:urlunsplit({S, N, P, Q, []}).

path_cat(P1, P2) ->
  UL = lists:append(path_fix(P1), path_fix(P2)),
  ["/"++U || U <- UL].

path_fix(S) ->
  PS = mochiweb_util:path_split(S),
  path_fix(PS, []).

path_fix({[], []}, Acc) ->
  lists:reverse(Acc);
path_fix({[], T}, Acc) ->
  path_fix(mochiweb_util:path_split(T), Acc);
path_fix({H, T}, Acc) ->
  path_fix(mochiweb_util:path_split(T), [H|Acc]).

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%
scheme() ->
  oneof([<<"http">>, <<"https">>]).

host() ->
  oneof([<<"localhost:9839">>, <<"kivra.com">>, <<"api.kivra.com">>, <<"kivra:80">>, <<"kivra">>,
         <<"1.0.2.3:47">>, <<"1.9.0.2">>, <<"kivra.com/user">>]).
path() ->
  ?LET(PathElems, list(utf8()),
      iolist_to_binary(lists:join(<<"/">>, PathElems))).

query_elements() ->
  list({utf8(), utf8()}).

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
