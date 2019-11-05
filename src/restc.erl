%% ----------------------------------------------------------------------------
%%
%% restc: Erlang Rest Client
%%
%% Copyright (c) 2012-2019 KIVRA
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
-export([construct_url/4]).

-type method()       :: head    |
                        get     |
                        put     |
                        patch   |
                        post    |
                        trace   |
                        options |
                        delete.
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
-type content_type() :: json | xml | percent | png.
-type body()         :: binary() | jsx:json_term() | erlsom:simple_form().
-type response()     :: { ok, Status::status_code()
                        , Headers::headers()
                        , Body::body()}               |
                        { error, Status::status_code()
                        , Headers::headers()
                        , Body::body()}               |
                        { error, Reason::reason()}.

-define(DEFAULT_ENCODING, json).
-define(DEFAULT_CTYPE, <<"application/json">>).

-export_type([ method/0
             , url/0
             , headers/0
             , header/0
             , options/0
             , option/0
             , querys/0
             , qry/0
             , status_codes/0
             , status_code/0
             , reason/0
             , content_type/0
             , body/0
             , response/0
             ]).

%%% API ========================================================================


-spec request(Url::url()) -> Response::response().
request(Url) ->
  request(get, Url).

-spec request(Method::method(), Url::url()) -> Response::response().
request(Method, Url) ->
  request(Method, Url, []).

-spec request(Method::method(),
              Url::url(),
              Expect::status_codes()) -> Response::response().
request(Method, Url, Expect) ->
  request(Method, ?DEFAULT_ENCODING, Url, Expect).

-spec request(Method::method(),
              Type::content_type(),
              Url::url(),
              Expect::status_codes()) -> Response::response().
request(Method, Type, Url, Expect) ->
  request(Method, Type, Url, Expect, []).

-spec request(Method::method(),
              Type::content_type(),
              Url::url(),
              Expect::status_codes(),
              Headers::headers()) -> Response::response().
request(Method, Type, Url, Expect, Headers) ->
  request(Method, Type, Url, Expect, Headers, []).

-spec request(Method::method(),
              Type::content_type(),
              Url::url(),
              Expect::status_codes(),
              Headers::headers(),
              Body::body()) -> Response::response().
request(Method, Type, Url, Expect, Headers, Body) ->
  request(Method, Type, Url, Expect, Headers, Body, []).

-spec request(Method::method(),
              Type::content_type(),
              Url::url(),
              Expect::status_codes(),
              Headers::headers(),
              Body::body(),
              Options::options()) -> Response::response().
request(Method, Type, Url, Expect, Headers0, Body, Options) ->
  Headers1 = normalize_headers(Headers0),
  Headers = lists:usort([ accept(Headers1, Type)
                        , content_type(Headers1, Type) | Headers1]),
  Retries = proplists:get_value(retries, Options, 0),
  request_loop(Method, Type, Url, Expect, Headers, Body, Options, Retries).

request_loop(Method, Type, Url, Expect, Headers, Body, Options, Retries) ->
  Response =
    parse_response(do_request(Method, Type, Url, Headers, Body, Options)),
  case Response of
    {ok, Status, H, B} ->
      case check_expect(Status, Expect) of
        true -> Response;
        false when Retries > 0 ->
          request_loop(Method, Type, Url, Expect, Headers, Body, Options,
                       Retries-1);
        false ->
          {error, Status, H, B}
      end;
    _Error when Retries > 0 ->
      request_loop(Method, Type, Url, Expect, Headers, Body, Options,
                   Retries-1);
    Error ->
      Error
  end.

-spec construct_url(FullPath::url(), Query::querys()) -> Url::url().
construct_url(FullPath, Query) ->
  construct_url(FullPath, <<>>, Query).

-spec construct_url(BaseUrl::url(),
                    Path::url(),
                    Query::querys()) -> Url::url().
construct_url(BaseUrl, Path, Query) ->
  construct_url(BaseUrl, Path, Query, []).

-spec construct_url(BaseUrl::url(),
                    Path::url(),
                    Query::querys(),
                    Options::[option()]) -> Url::url().
construct_url(BaseUrl, Path, Query, Options) ->
  BaseUrlBin = s_to_binary(BaseUrl),
  PathBin    = s_to_binary(Path),
  QueryBin   = lists:map(fun({K,V}) -> {s_to_binary(K), s_to_binary(V)} end, Query),
  UrlBin     = hackney_url:make_url(BaseUrlBin, PathBin, QueryBin),
  case Options of
    [return_binary] -> UrlBin;
    [] -> binary_to_list(UrlBin)
  end.

%%% INTERNAL ===================================================================

%% Convert from string() to binary()
s_to_binary(V) when is_binary(V) -> V;
s_to_binary(V) when is_list(V)   -> list_to_binary(V).

to_binary(V) when is_integer(V) -> integer_to_binary(V);
to_binary(V) when is_atom(V)    -> atom_to_binary(V, utf8);
to_binary(V)                    -> s_to_binary(V).

normalize_headers(Headers) ->
  lists:map(fun({Key, Val}) ->
                {string:lowercase(Key), Val}
            end, Headers).

accept(Headers, Type) ->
  case lists:keyfind(<<"accept">>, 1, Headers) of
    {<<"accept">>, Accept} -> {<<"accept">>, Accept};
    false -> default_accept(Type)
  end.

default_accept(Type) ->
  AccessType = get_accesstype(Type),
  {<<"accept">>, <<AccessType/binary, ", */*;q=0.9">>}.

content_type(Headers, Type) ->
  case lists:keyfind(<<"content-type">>, 1, Headers) of
    {<<"content-type">>, ContentType} -> {<<"content-type">>, ContentType};
    false -> default_content_type(Type)
  end.

default_content_type(Type) ->
  {<<"content-type">>, get_ctype(Type)}.

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
  lists:map(fun({K, V}) ->
                {to_binary(K), to_binary(V)}
            end, Body),
  binary_to_list(hackney_url:qs(Body, []));
encode_body(xml, Body) ->
  lists:flatten(xmerl:export_simple(Body, xmerl_xml)).

parse_response({ok, 204, Headers, Client}) ->
  ok = hackney:close(Client),
  {ok, 204, Headers, []};
parse_response({ok, Status, Headers, Client}) ->
  NormalizedHeaders = normalize_headers(Headers),
  {<<"content-type">>, ContentType} =
    content_type(NormalizedHeaders, ?DEFAULT_CTYPE),
  Type = parse_type(ContentType),
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

parse_body(_, <<>>)                      -> [];
parse_body(<<"application/json">>, Body) -> jsx:decode(Body);
parse_body(<<"application/xml">>, Body)  ->
  {ok, Data, _} = erlsom:simple_form(binary_to_list(Body)),
  Data;
parse_body(<<"text/xml">>, Body)  -> parse_body(<<"application/xml">>, Body);
parse_body(<<"image/png">>, Body) -> Body;
parse_body(_, Body)               -> Body.

get_accesstype(json)    -> <<"application/json">>;
get_accesstype(xml)     -> <<"application/xml">>;
get_accesstype(percent) -> <<"application/json">>;
get_accesstype(png)     -> <<"image/png">>;
get_accesstype(_)       -> get_ctype(?DEFAULT_ENCODING).

get_ctype(json)    -> <<"application/json">>;
get_ctype(xml)     -> <<"application/xml">>;
get_ctype(percent) -> <<"application/x-www-form-urlencoded">>;
get_ctype(png)     -> <<"image/png">>;
get_ctype(_)       -> get_ctype(?DEFAULT_ENCODING).

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
