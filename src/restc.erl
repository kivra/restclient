-module(restc).

-export([request/2, request/3, request/4, request/5, request/6]).
-export([construct_url/2, construct_url/3]).

-type method()       :: head | get | put | post | trace | options | delete.
-type url()          :: string().
-type headers()      :: [header()].
-type header()       :: {string(), string()}.
-type querys()       :: [qry()].
-type qry()          :: {string(), string()}.
-type status_codes() :: [status_code()].
-type status_code()  :: integer().
-type reason()       :: term().
-type content_type() :: json | xml.
-type property()     :: atom() | tuple().
-type proplist()     :: [property()].
-type body()         :: proplist().
-type response()     :: {ok, Status::status_code(), Headers::headers(), Body::body()} |
                        {error, Status::status_code(), Headers::headers(), Body::body()} |
                        {error, Reason::reason()}.


%%% API ========================================================================


-spec request(Method::method(), Url::url()) -> Response::response().
request(Method, Url) ->
    request(Method, json, Url, [], [], []).

-spec request(Method::method(), Url::url(), Expect::status_codes()) -> Response::response().
request(Method, Url, Expect) ->
    request(Method, json, Url, Expect, [], []).

-spec request(Method::method(), Type::content_type(), Url::url(),
              Expect::status_codes()) -> Response::response().
request(Method, Type, Url, Expect) ->
    request(Method, Type, Url, Expect, [], []).

-spec request(Method::method(), Type::content_type(), Url::url(),
              Expect::status_codes(), Headers::headers()) -> Response::response().
request(Method, Type, Url, Expect, Headers) ->
    request(Method, Type, Url, Expect, Headers, []).

-spec request(Method::method(), Type::content_type(), Url::url(),
              Expect::status_codes(), Headers::headers(), Body::body()) -> Response::response().
request(Method, Type, Url, Expect, Headers, Body) ->
    Accept = {"Accept", get_ctype(Type)++", */*;q=0.9"},
    Request = get_request(Url, Type, [Accept|Headers], Body),
    Response = parse_response(httpc:request(Method, Request, [], [])),
    case Response of
        {ok, Status, H, B} ->
            case check_expect(Status, Expect) of
                true -> Response;
                false -> {error, Status, H, B}
            end;
        Error ->
            Error
    end.

-spec construct_url(FullPath::url(), Query::querys()) -> Url::url().
construct_url(FullPath, Query) ->
    {S, N, P, _, _} = mochiweb_util:urlsplit(FullPath),
    urlunsplit(S, N, P, Query).

-spec construct_url(FullPath::url(), Path::url(), Query::querys()) -> Url::url().
construct_url(SchemeNetloc, Path, Query) ->
    {S, N, P1, _, _} = mochiweb_util:urlsplit(SchemeNetloc),
    {_, _, P2, _, _} = mochiweb_util:urlsplit(Path),
    P = path_cat(P1, P2),
    urlunsplit(S, N, P, Query).


%%% INTERNAL ===================================================================


check_expect(_Status, []) ->
    true;
check_expect(Status, Expect) ->
    lists:member(Status, Expect).

encode_body(json, Body) ->
    binary_to_list(iolist_to_binary(mochijson2:encode(Body)));
encode_body(xml, Body) ->
    lists:flatten(xmerl:export_simple(Body, xmerl_xml));
encode_body(_, Body) ->
   encode_body(json, Body).

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

get_request(Url, _Type, Headers, []) ->
    {Url, Headers};
get_request(Url, Type, Headers, Body) ->
    SendBody = encode_body(Type, Body),
    {Url, Headers, [], SendBody}.

parse_response({ok, {{_, Status, _}, Headers, Body}}) ->
    Type = proplists:get_value("content-type", Headers),
    Body2 = parse_body(Type, Body),
    {ok, Status, Headers, Body2};
parse_response({error, Type}) ->
    {error, Type}.

parse_body("application/json", Body) -> mochijson2:decode(Body, [{format, proplist}]);
parse_body("application/xml", Body) -> erlsom:simple_form(Body);
parse_body("text/xml", Body) -> parse_body("application/xml", Body);
parse_body(_, Body) -> Body.

get_ctype(json) -> "application/json";
get_ctype(xml) -> "application/xml";
get_ctype(_) -> get_ctype(json).

