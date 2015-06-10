%% ----------------------------------------------------------------------------
%%
%% restclient: An erlang REST Client library
%%
%% Copyright (c) 2012 KIVRA
%%
%% Permission is hereby granted, free of charge, to any person obtaining a
%% copy of this software and associated documentation files (the "Software"),
%% to deal in the Software without restriction, including without limitation
%% the rights to use, copy, modify, merge, publish, distribute, sublicense,
%% and/or sell copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
%% DEALINGS IN THE SOFTWARE.
%%
%% ----------------------------------------------------------------------------

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
-type content_type() :: json | xml | percent.
-type property()     :: atom() | tuple().
-type proplist()     :: [property()].
-type body()         :: proplist().
-type response()     :: {ok, Status::status_code(), Headers::headers(), Body::body()} |
                        {error, Status::status_code(), Headers::headers(), Body::body()} |
                        {error, Reason::reason()}.

-define(DEFAULT_ENCODING, json).
-define(DEFAULT_CTYPE, "application/json").


%%% API ========================================================================


-spec request(Method::method(), Url::url()) -> Response::response().
request(Method, Url) ->
    request(Method, ?DEFAULT_ENCODING, Url, [], [], []).

-spec request(Method::method(), Url::url(), Expect::status_codes()) -> Response::response().
request(Method, Url, Expect) ->
    request(Method, ?DEFAULT_ENCODING, Url, Expect, [], []).

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
    Headers1 = [{"Accept", get_accesstype(Type)++", */*;q=0.9"} | Headers],
    Headers2 = [{"Content-Type", get_ctype(Type)} | Headers1],
    Request = get_request(Url, Type, Headers2,  Body),
    Response = parse_response(httpc:request(Method, Request,
                                            [], [{body_format, binary}])),
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
    jsx:to_json(Body);
encode_body(percent, Body) ->
    mochiweb_util:urlencode(Body);
encode_body(xml, Body) ->
    lists:flatten(xmerl:export_simple(Body, xmerl_xml));
encode_body(_, Body) ->
   encode_body(?DEFAULT_ENCODING, Body).

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

get_request(Url, _, Headers, []) ->
    {Url, Headers};
get_request(Url, Type, Headers, Body) ->
    SendBody = encode_body(Type, Body),
    {Url, Headers, get_ctype(Type), SendBody}.

parse_response({ok, {{_, Status, _}, Headers, Body}}) ->
    Type = case lists:keyfind("content-type", 1, Headers) of
        false    -> ?DEFAULT_CTYPE;
        {_, Val} -> Val
    end,
    CType = case string:tokens(Type, ";") of
        [CVal]    -> CVal;
        [CVal, _] -> CVal
    end,
    Body2 = parse_body(CType, Body),
    {ok, Status, Headers, Body2};
parse_response({error, Type}) ->
    {error, Type}.

parse_body([], Body)                 -> Body;
parse_body(_, [])                    -> [];
parse_body(_, <<>>)                  -> [];
parse_body("application/json", Body) -> jsx:decode(Body);
parse_body("application/xml", Body)  ->
    {ok, Data, _} = erlsom:simple_form(binary_to_list(Body)),
    Data;
parse_body("text/xml", Body) -> parse_body("application/xml", Body);
parse_body(_, Body)          -> Body.

get_accesstype(json)    -> "application/json";
get_accesstype(xml)     -> "application/xml";
get_accesstype(percent) -> "application/json";
get_accesstype(_)       -> get_ctype(?DEFAULT_ENCODING).

get_ctype(json)    -> "application/json";
get_ctype(xml)     -> "application/xml";
get_ctype(percent) -> "application/x-www-form-urlencoded";
get_ctype(_)       -> get_ctype(?DEFAULT_ENCODING).
