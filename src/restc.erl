-module(restc).

-export([request/2, request/3, request/4, request/5, request/6]).

-type method()       :: head | get | put | post | trace | options | delete.
-type url()          :: string().
-type headers()      :: [header()].
-type header()       :: {string(), string()}.
-type status_codes() :: [status_code()].
-type status_code()  :: integer().
-type reason()       :: term().
-type content_type() :: json | xml.
-type body()         :: string() | binary().
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
    Request = get_request(Url, [Accept|Headers], Body),
    Response = parse_response(httpc:request(Method, Request, [], [])),
    case Response of
        {ok, Status, H, B} ->
             case lists:member(Status, Expect) of
                true -> Response;
                false -> {error, Status, H, B}
            end;
        Error ->
            Error
    end.


%%% INTERNAL ===================================================================


get_request(Url, Headers, []) ->
    {Url, Headers};
get_request(Url, Headers, Body) ->
    {Url, Headers, [], Body}.

parse_response({ok, {{_, Status, _}, Headers, Body}}) ->
    Type = proplists:get_value("content-type", Headers),
    Body2 = parse_body(Type, Body),
    {ok, Status, Headers, Body2};
parse_response({error, Type}) ->
    {error, Type}.

parse_body("application/json", Body) -> mochijson2:decode(Body, [{format, proplist}]);
parse_body("application/xml", Body) -> erlsom:simple_form(Body);
parse_body("text/xml", Body) -> parse_body("application/xml", Body);
parse_body(_, Body) -> parse_body("application/json", Body).

get_ctype(json) -> "application/json";
get_ctype(xml) -> "application/xml";
get_ctype(_) -> get_ctype(json).

