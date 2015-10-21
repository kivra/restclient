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

-type method()       :: binary | head | get | put | post | trace | options | delete.
-type url()          :: binary() | string().
-type headers()      :: [header()].
-type header()       :: {binary(), binary()}.
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


%%% API ========================================================================


-spec request(Url::url()) -> Response::response().
request(Url) ->
    request(get, ?DEFAULT_ENCODING, Url, [], [], []).

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
    AccessType = get_accesstype(Type),
    Headers1 = [{<<"Accept">>, <<AccessType/binary, ", */*;q=0.9">>} | Headers],
    Headers2 = [{<<"Content-Type">>, get_ctype(Type)} | Headers1],
    Response = parse_response(do_request(Method, Type, Url, Headers2, Body)),
    case Response of
        {ok, Status, H, B} ->
            case check_expect(Status, Expect) of
                true -> Response;
                false -> {error, Status, H, B}
            end;
        Error ->
            Error
    end.


%%% INTERNAL ===================================================================


do_request(post, Type, Url, Headers, Body) ->
    Body2 = encode_body(Type, Body),
    hackney:request(post, Url, Headers, Body2);
do_request(put, Type, Url, Headers, Body) ->
    Body2 = encode_body(Type, Body),
    hackney:request(put, Url, Headers, Body2);
do_request(Method, _, Url, Headers, _) ->
    hackney:request(Method, Url, Headers).

check_expect(_Status, []) ->
    true;
check_expect(Status, Expect) ->
    lists:member(Status, Expect).

encode_body(json, Body) ->
    jsx:encode(Body);
encode_body(percent, Body) ->
    mochiweb_util:urlencode(Body);
encode_body(xml, Body) ->
    lists:flatten(xmerl:export_simple(Body, xmerl_xml));
encode_body(_, Body) ->
   encode_body(?DEFAULT_ENCODING, Body).

parse_response({ok, Status, Headers, Client}) ->
    Type = proplists:get_value(<<"Content-Type">>, Headers, ?DEFAULT_CTYPE),
    Type2 = parse_type(Type),
    {ok, Body} = hackney:body(Client),
    Body2 = parse_body(Type2, Body),
    {ok, Status, Headers, Body2};
parse_response({error, Type}) ->
    {error, Type}.

parse_type(Type) ->
    case binary:split(Type, <<";">>) of
        [CType, _] -> CType;
        _ -> Type
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
