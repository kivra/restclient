%% ----------------------------------------------------------------------------
%%
%% restc: Erlang Rest Client
%%
%% Copyright (c) 2012-2014 KIVRA
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

-export([request/1]).
-export([request/2]).
-export([request/3]).
-export([request/4]).
-export([request/5]).
-export([request/6]).
-export([request/7]).

-export([construct_url/2]).
-export([construct_url/3]).

-type method()       :: head | get | put | patch | post | trace | options | delete.
-type url()          :: binary() | string().
-type headers()      :: [header()].
-type header()       :: {binary(), binary()}.
-type options()      :: [option()].
-type option()       :: {atom(), term()} | atom().
-type querys()       :: [qry()].
-type qry()          :: {string(), string()}.
-type status_codes() :: [status_code()].
-type status_code()  :: integer().
-type reason()       :: term().
-type content_type() :: json | xml | percent.
-type body()         :: binary() | jsx:json_term() | erlsom:simple_form().
-type response()     :: {ok, Status::status_code(), Headers::headers(), Body::body()} |
                        {error, Status::status_code(), Headers::headers(), Body::body()} |
                        {error, Reason::reason()}.

-define(DEFAULT_ENCODING, json).
-define(DEFAULT_CTYPE, <<"application/json">>).

-export_type([
     method/0
    ,url/0
    ,headers/0
    ,header/0
    ,options/0
    ,option/0
    ,querys/0
    ,qry/0
    ,status_codes/0
    ,status_code/0
    ,reason/0
    ,content_type/0
    ,body/0
    ,response/0
]).

%%% API ========================================================================


-spec request(Url::url()) -> Response::response().
request(Url) ->
    request(get, ?DEFAULT_ENCODING, Url, [], [], [], []).

-spec request(Method::method(), Url::url()) -> Response::response().
request(Method, Url) ->
    request(Method, ?DEFAULT_ENCODING, Url, [], [], [], []).

-spec request(Method::method(), Url::url(), Expect::status_codes()) -> Response::response().
request(Method, Url, Expect) ->
    request(Method, ?DEFAULT_ENCODING, Url, Expect, [], [], []).

-spec request(Method::method(), Type::content_type(), Url::url(),
              Expect::status_codes()) -> Response::response().
request(Method, Type, Url, Expect) ->
    request(Method, Type, Url, Expect, [], [], []).

-spec request(Method::method(), Type::content_type(), Url::url(),
              Expect::status_codes(), Headers::headers()) -> Response::response().
request(Method, Type, Url, Expect, Headers) ->
    request(Method, Type, Url, Expect, Headers, [], []).

-spec request(Method::method(), Type::content_type(), Url::url(),
              Expect::status_codes(), Headers::headers(), Body::body()) -> Response::response().
request(Method, Type, Url, Expect, Headers, Body) ->
    request(Method, Type, Url, Expect, Headers, Body, []).

-spec request(Method::method(), Type::content_type(), Url::url(),
    Expect::status_codes(), Headers::headers(), Body::body(), Options::options()) -> Response::response().
request(Method, Type, Url, Expect, Headers, Body, Options) ->
    AccessType = get_accesstype(Type),
    Headers1 = [{<<"Accept">>, <<AccessType/binary, ", */*;q=0.9">>} | Headers],
    Headers2 = [{<<"Content-Type">>, get_ctype(Type)} | Headers1],
    Retries = proplists:get_value(retries, Options, 0),
    request_loop(Method, Type, Url, Expect, Headers2, Body, Options, Retries).

request_loop(Method, Type, Url, Expect, Headers, Body, Options, Retries) ->
    Response = parse_response(do_request(Method, Type, Url, Headers, Body, Options)),
    case Response of
        {ok, Status, H, B} ->
            case check_expect(Status, Expect) of
                true -> Response;
                false when Retries > 0 ->
                    request_loop(Method, Type, Url, Expect, Headers, Body, Options, Retries-1);
                false ->
                    {error, Status, H, B}
            end;
        _Error when Retries > 0 ->
            request_loop(Method, Type, Url, Expect, Headers, Body, Options, Retries-1);
        Error ->
            Error
    end.

-spec construct_url(FullPath::url(), Query::querys()) -> Url::url().
construct_url(FullPath, Query) when is_binary(FullPath) ->
    construct_url(binary_to_list(FullPath), Query);
construct_url(FullPath, Query) when is_list(FullPath) ->
    {S, N, P, _, _} = mochiweb_util:urlsplit(FullPath),
    Q = mochiweb_util:urlencode(Query),
    mochiweb_util:urlunsplit({S, N, P, Q, []}).

-spec construct_url(FullPath::url(), Path::url(), Query::querys()) -> Url::url().
construct_url(SchemeNetloc, Path, Query) when is_binary(SchemeNetloc) ->
    construct_url(binary_to_list(SchemeNetloc), Path, Query);
construct_url(SchemeNetloc, Path, Query) when is_binary(Path) ->
    construct_url(SchemeNetloc, binary_to_list(Path), Query);
construct_url(SchemeNetloc, Path, Query) when is_list(SchemeNetloc),
                                              is_list(Path) ->
    {S, N, P1, _, _} = mochiweb_util:urlsplit(SchemeNetloc),
    {_, _, P2, _, _} = mochiweb_util:urlsplit(Path),
    P = path_cat(P1, P2),
    urlunsplit(S, N, P, Query).

%%% INTERNAL ===================================================================

do_request(post, Type, Url, Headers, Body, Options) ->
    Body2 = encode_body(Type, Body),
    hackney:request(post, Url, Headers, Body2, Options);
do_request(put, Type, Url, Headers, Body, Options) ->
    Body2 = encode_body(Type, Body),
    hackney:request(put, Url, Headers, Body2, Options);
do_request(patch, Type, Url, Headers, Body, Options) ->
    Body2 = encode_body(Type, Body),
    hackney:request(patch, Url, Headers, Body2, Options);
do_request(Method, _, Url, Headers, _, Options) when is_atom(Method) ->
    hackney:request(Method, Url, Headers, [], Options).

check_expect(_Status, []) ->
    true;
check_expect(Status, Expect) ->
    lists:member(Status, Expect).

encode_body(json, Body) ->
    jsx:encode(Body);
encode_body(percent, Body) ->
    mochiweb_util:urlencode(Body);
encode_body(xml, Body) ->
    lists:flatten(xmerl:export_simple(Body, xmerl_xml)).

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

parse_response({ok, 204, Headers, Client}) ->
    ok = hackney:close(Client),
    {ok, 204, Headers, []};
parse_response({ok, Status, Headers, Client}) ->
    Type = parse_type(get_key(<<"Content-Type">>, Headers, ?DEFAULT_CTYPE)),
    case hackney:body(Client) of
        {ok, Body}   -> {ok, Status, Headers, parse_body(Type, Body)};
        {error, _}=E -> E
    end;
parse_response({error, Type}) ->
    {error, Type}.

parse_type(Type) ->
    case binary:split(Type, <<";">>) of
        [CType, _] -> CType;
        _ -> Type
    end.

get_key(Key, Obj, Def) ->
    case lists:keyfind(Key, 1, Obj) of
        false      -> Def;
        {Key, Val} -> Val
    end.

parse_body(_, <<>>)                      -> [];
parse_body(<<"application/json">>, Body) -> jsx:decode(Body);
parse_body(<<"application/xml">>, Body)  ->
    {ok, Data, _} = erlsom:simple_form(binary_to_list(Body)),
    Data;
parse_body(<<"text/xml">>, Body) -> parse_body(<<"application/xml">>, Body);
parse_body(_, Body)          -> Body.

get_accesstype(json)    -> <<"application/json">>;
get_accesstype(xml)     -> <<"application/xml">>;
get_accesstype(percent) -> <<"application/json">>;
get_accesstype(_)       -> get_ctype(?DEFAULT_ENCODING).

get_ctype(json)    -> <<"application/json">>;
get_ctype(xml)     -> <<"application/xml">>;
get_ctype(percent) -> <<"application/x-www-form-urlencoded">>;
get_ctype(_)       -> get_ctype(?DEFAULT_ENCODING).

