restclient(0.1.0) -- An erlang REST Client library
====================================

## DESCRIPTION

restclient is a library to help with consuming RESTful web services. It supports
encoding and decoding JSON, Percent and XML and comes with a convenience
function for working with urls and query parameters.

## USAGE

Include restclient as a rebar dependency with:

	{deps, [{restc, ".*", {git, "git://github.com/kivra/restclient.git", {tag, "0.1.0"}}}]}.

You have to start inets before using the client and if you want to use https make sure to start ssl before.
Then you can use the client as:

``` erlang
	Erlang R15B (erts-5.9) [source] [64-bit] [smp:8:8] [async-threads:0] [hipe] [kernel-poll:false]

	Eshell V5.9  (abort with ^G)
	1> application:start(inets).
	ok
	2> application:start(crypto).
	ok
	3> application:start(public_key).
	ok
	4> application:start(ssl).
	ok
	5> restc:request(get, "https://api.github.com").
	{ok,204,
	    [{"connection","keep-alive"},
	     {"date","Thu, 15 Mar 2012 22:32:47 GMT"},
	     {"etag","\"d41d8cd98f00b204e9800998ecf8427e\""},
	     {"server","nginx/1.0.13"},
	     {"status","204 No Content"},
	     {"x-ratelimit-limit","5000"},
	     {"x-ratelimit-remaining","4992"}],
	    []}
	6> restc:request(get, "https://api.github.com", [200]).
	{error,204,
	    [{"connection","keep-alive"},
	     {"date","Thu, 15 Mar 2012 22:32:47 GMT"},
	     {"etag","\"d41d8cd98f00b204e9800998ecf8427e\""},
	     {"server","nginx/1.0.13"},
	     {"status","204 No Content"},
	     {"x-ratelimit-limit","5000"},
	     {"x-ratelimit-remaining","4992"}],
	    []}
```

There's also convenience functions for working with urls and query string:

``` erlang
	7> restc:construct_url("http://www.example.com/te", "res/res1/res2", [{"q1", "qval1"}, {"q2", "qval2"}]).
	"http://www.example.com/te/res/res1/res2?q1=qval1&q2=qval2"
```

## License
The KIVRA restclient library uses an [MIT license](http://en.wikipedia.org/wiki/MIT_License). So go ahead and do what
you want!

Lots of fun!
