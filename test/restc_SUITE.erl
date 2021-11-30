%%%_* Module declaration ===============================================
-module(restc_SUITE).
-compile([export_all]).

-include_lib("stdlib/include/assert.hrl").

all() ->
  [ {group, upstream_pipethrough}
  , {group, expect_status}
  , {group, retries}
  , {group, request_body_encoding}
  , {group, response_body_decoding}
  , {group, accept_header_and_type}
  ].

groups() ->
  [{ upstream_pipethrough,
     [ upstream_returning_status_code__making_request__returns_the_same_status_code
     , upstream_returning_headers__making_request__returns_the_same_headers
     , upstream_returning_error__making_request__returns_the_same_error
     ]}
  ,{ expect_status,
     [ no_expected_status__making_request__returns_upstream_status
     , upstream_returning_unexpected_status__making_request__returns_error
     , upstream_returning_unexpected_status__making_request__returns_the_unexpected_status
     ]}
  ,{ retries,
     [ no_options__making_request__tries_only_once
     , two_retries_in_options__making_request__calls_three_times
     , retries_not_succeeding__making_request__returns_error
     , retries_eventually_succeeding__making_request__returns_result
     ]}
  ,{ request_body_encoding,
     [ type_is_json__making_request__sends_json_encoded_body
     , type_is_percent__making_request__sends_percent_encoded_body
     , type_is_xml__making_request__sends_xml_encoded_body
     , method_is_post__making_request__body_is_encoded
     , method_is_put__making_request__body_is_encoded
     , method_is_patch__making_request__body_is_encoded
     , method_is_something_else__making_request__body_is_empty_list
     ]}
  ,{ response_body_decoding,
     [ no_content_type_returned__making_json_request__decode_response_as_json
     , content_type_returned__making_json_request__decode_response_as_content_type
     , return_maps
     ]}
  ,{ accept_header_and_type,
     [ type_is_json__making_request_with_no_headers__accept_header_is_json
     , type_is_xml__making_request_with_no_headers__accept_header_is_xml
     , type_is_percent__making_request_with_no_headers__accept_header_is_json
     , type_is_png__making_request_with_no_headers__accept_header_is_png
     , type_is_json__making_request_with_xml_accept_header__accept_header_overrides_type
     ]}
  ].

init_per_testcase(_TestCase, Config) ->
  meck:new(hackney),
  Config.
end_per_testcase(_TestCase, _Config) -> meck:unload(hackney).

%%%_ * Tests - given, when, then ---------------------------------------
upstream_returning_status_code__making_request__returns_the_same_status_code(_Config) ->
  mock_hackney_success(200),
  ExpectedStatus = 200,

  {ok, ActualStatus, _, _} = restc:request(<<"http://any_url.com">>),

  ?assertEqual(ExpectedStatus, ActualStatus).

upstream_returning_headers__making_request__returns_the_same_headers(_Config) ->
  ExpectedHeaders = [{<<"Content-Type">>, <<"text/html">>}],
  mock_hackney_success(200, ExpectedHeaders, <<>>),

  {ok, _, ActualHeaders, _} = restc:request(<<"http://any_url.com">>),

  ?assertEqual(ExpectedHeaders, ActualHeaders).

upstream_returning_error__making_request__returns_the_same_error(_Config) ->
  mock_hackney_error(some_error),
  ExpectedError = some_error,

  {error, ActualError} = restc:request(<<"http://any_url.com">>),

  ?assertEqual(ExpectedError, ActualError).

no_expected_status__making_request__returns_upstream_status(_Config) ->
  mock_hackney_success(201),
  ExpectedStatus = [],
  ExpectedStatusCode = 201,

  {ok, ActualStatus, _, _} = restc:request(get, <<"http://any_url.com">>, ExpectedStatus),

  ?assertMatch(ExpectedStatusCode, ActualStatus).

upstream_returning_unexpected_status__making_request__returns_error(_Config) ->
  mock_hackney_success(500),

  Result = restc:request(get, <<"http://any_url.com">>, [200]),

  ?assertMatch({error, _, _, _}, Result).

upstream_returning_unexpected_status__making_request__returns_the_unexpected_status(_Config) ->
  mock_hackney_success(500, [], <<>>),
  ExpectedStatus = 500,

  {error, ActualStatus, _, _} = restc:request(get, <<"http://any_url.com">>, [200]),

  ?assertEqual(ExpectedStatus, ActualStatus).

no_options__making_request__tries_only_once(_Config) ->
  mock_hackney_success(500),
  ExpectedCalls = 1,

  restc:request(get, <<"http://any_url.com">>, [200]),

  ?assertEqual(ExpectedCalls, meck:num_calls(hackney, request, '_')).

two_retries_in_options__making_request__calls_three_times(_Config) ->
  mock_hackney_success(500),
  Options = [{retries, 2}],
  ExpectedCalls = 3,

  restc:request(get, json, <<"http://any_url.com">>, [200], [], [], Options),

  ?assertEqual(ExpectedCalls, meck:num_calls(hackney, request, '_')).

retries_not_succeeding__making_request__returns_error(_Config) ->
  mock_hackney_success(500),
  Options = [{retries, 2}],

  Result = restc:request(get, json, <<"http://any_url.com">>, [200], [], [], Options),

  ?assertMatch({error, _, _, _}, Result).

retries_eventually_succeeding__making_request__returns_result(_Config) ->
  mock_hackney_eventual_success(200, 2),
  Options = [{retries, 2}],
  ExpectedResult = {ok, 200, [], []},

  ActualResult = restc:request(get, json, <<"http://any_url.com">>, [200], [], [], Options),

  ?assertEqual(ExpectedResult, ActualResult).

type_is_json__making_request__sends_json_encoded_body(_Config) ->
  mock_hackney_success(200),
  Body = [{<<"any">>, <<"data">>}],

  restc:request(post, json, <<"http://any_url.com">>, [200], [], Body),

  EncodedBody = meck:capture(first, hackney, request, '_', 4, '_'),
  ?assert(jsx:is_json(EncodedBody)).

type_is_percent__making_request__sends_percent_encoded_body(_Config) ->
  mock_hackney_success(200),
  Body = [{<<"any">>, <<"data">>}],
  ExpectedBody = list_to_binary(mochiweb_util:urlencode(Body)),

  restc:request(post, percent, <<"http://any_url.com">>, [200], [], Body),

  ActualBody = meck:capture(first, hackney, request, '_', 4, '_'),
  ?assertEqual(ExpectedBody, ActualBody).

type_is_xml__making_request__sends_xml_encoded_body(_Config) ->
  mock_hackney_success(200),
  Body = [{any, [{something, ["data"]}]}],
  ExpectedBody = lists:flatten(xmerl:export_simple(Body, xmerl_xml)),

  restc:request(post, xml, <<"http://any_url.com">>, [200], [], Body),

  ActualBody = meck:capture(first, hackney, request, '_', 4, '_'),
  ?assertEqual(ExpectedBody, ActualBody).

method_is_post__making_request__body_is_encoded(_Config) ->
  mock_hackney_success(200),
  meck:new(jsx, [passthrough]),

  restc:request(post, json, <<"http://any_url.com">>, [200], [], [{<<"any">>, <<"data">>}]),

  ?assert(meck:called(jsx, encode, '_')).

method_is_put__making_request__body_is_encoded(_Config) ->
  mock_hackney_success(200),
  meck:new(jsx, [passthrough]),

  restc:request(put, json, <<"http://any_url.com">>, [200], [], [{<<"any">>, <<"data">>}]),

  ?assert(meck:called(jsx, encode, '_')).

method_is_patch__making_request__body_is_encoded(_Config) ->
  mock_hackney_success(200),
  meck:new(jsx, [passthrough]),

  restc:request(patch, json, <<"http://any_url.com">>, [200], [], [{<<"any">>, <<"data">>}]),

  ?assert(meck:called(jsx, encode, '_')).

method_is_something_else__making_request__body_is_empty_list(_Config) ->
  mock_hackney_success(200),
  ExpectedBody = [],

  restc:request(delete, json, <<"http://any_url.com">>, [200], [], [{<<"any">>, <<"data">>}]),

  ActualBody = meck:capture(first, hackney, request, '_', 4, '_'),
  ?assertEqual(ExpectedBody, ActualBody).

no_content_type_returned__making_json_request__decode_response_as_json(_Config) ->
  ExpectedResponseBody = [{<<"any">>, <<"data">>}],
  mock_hackney_success(200, [], jsx:encode(ExpectedResponseBody)),

  {ok, _, _, ActualResponseBody} = restc:request(get, <<"http://any_url.com">>),

  ?assertEqual(ExpectedResponseBody, ActualResponseBody).

content_type_returned__making_json_request__decode_response_as_content_type(_Config) ->
  ResponseBody = [{any, [{something, ["data"]}]}],
  EncodedResponseBody = list_to_binary(lists:flatten(xmerl:export_simple(ResponseBody, xmerl_xml))),
  ExpectedResponseBody = {"any", [], [{"something", [], ["data"]}]},
  mock_hackney_success(200, [{<<"Content-Type">>, <<"application/xml">>}], EncodedResponseBody),

  {ok, _, _, ActualResponseBody} = restc:request(get, <<"http://any_url.com">>),

  ?assertEqual(ExpectedResponseBody, ActualResponseBody).

return_maps(_Config) ->
  ResponseBody = #{<<"first_level">> => #{<<"second_level">> => [<<"a">>, <<"b">>]}},
  EncodedResponseBody = jsx:encode(ResponseBody),
  mock_hackney_success(200, [{<<"Content-Type">>, <<"application/json">>}], EncodedResponseBody),

  {ok, _, _, ActualResponseBody} =
    restc:request(get, na, <<"http://any_url.com">>, [], [], <<>>, [return_maps]),

  ?assertEqual(ResponseBody, ActualResponseBody).

type_is_json__making_request_with_no_headers__accept_header_is_json(_Config) ->
  mock_hackney_success(200),

  restc:request(get, json, <<"http://any_url.com">>, [200]),

  ActualHeaders = meck:capture(first, hackney, request, '_', 3, '_'),
  ?assertMatch([{<<"accept">>, <<"application/json", _/binary>>}, _], ActualHeaders).

type_is_xml__making_request_with_no_headers__accept_header_is_xml(_Config) ->
  mock_hackney_success(200),

  restc:request(get, xml, <<"http://any_url.com">>, [200]),

  ActualHeaders = meck:capture(first, hackney, request, '_', 3, '_'),
  ?assertMatch([{<<"accept">>, <<"application/xml", _/binary>>}, _], ActualHeaders).

type_is_percent__making_request_with_no_headers__accept_header_is_json(_Config) ->
  mock_hackney_success(200),

  restc:request(get, percent, <<"http://any_url.com">>, [200]),

  ActualHeaders = meck:capture(first, hackney, request, '_', 3, '_'),
  ?assertMatch([{<<"accept">>, <<"application/json", _/binary>>}, _], ActualHeaders).

type_is_png__making_request_with_no_headers__accept_header_is_png(_Config) ->
  mock_hackney_success(200),

  restc:request(get, png, <<"http://any_url.com">>, [200]),

  ActualHeaders = meck:capture(first, hackney, request, '_', 3, '_'),
  ?assertMatch([{<<"accept">>, <<"image/png", _/binary>>}, _], ActualHeaders).

type_is_json__making_request_with_xml_accept_header__accept_header_overrides_type(_Config) ->
  mock_hackney_success(200),
  Uri = <<"http://any_url.com">>,
  Headers = [{<<"Accept">>, <<"application/xml">>}],
  restc:request(get, json, Uri, [200], Headers),

  ActualHeaders = meck:capture(first, hackney, request, '_', 3, '_'),
  ?assertMatch([{<<"accept">>, <<"application/xml", _/binary>>}, _], ActualHeaders).

%%%_ * Helpers ---------------------------------------------------------
mock_hackney_success(Code) -> mock_hackney_success(Code, [], <<>>).

mock_hackney_success(Code, Headers, Body) ->
  meck:expect(hackney, request, ['_', '_', '_', '_', '_'], meck:val({ok, Code, Headers, client})),
  meck:expect(hackney, body, fun(client) -> {ok, Body} end).

mock_hackney_error(Error) ->
  meck:expect(hackney, request, ['_', '_', '_', '_', '_'], meck:val({error, Error})).

mock_hackney_eventual_success(Code, AmountOfErrors) ->
  ErrorCalls = error_calls(AmountOfErrors),
  meck:expect(hackney, request, ['_', '_', '_', '_', '_'],
              meck:seq(ErrorCalls ++ [{ok, Code, [], client}])),
  meck:expect(hackney, body, fun(client) -> {ok, <<>>} end).

error_calls(0) -> [];
error_calls(N) -> error_calls(N, []).
error_calls(0, Acc) -> Acc;
error_calls(N, Acc) ->
  error_calls(N-1, [{error, some_error}|Acc]).
