feedparser
==========

Erlang port for Mark Pilgrim's [Universal Feed Parser](http://feedparser.org/).

Dependencies
------------

rebar, python (>= 2.5), virtualenv, easy\_install

Build
-----
	$ rebar get-deps && rebar compile

Usage
-----

	$ erl -pa ebin/ -pa deps/poolboy/ebin/ -feedparser pool_size 1
	Erlang R14B04 (erts-5.8.5) [source] [smp:64:64] [rq:64] [async-threads:0] [kernel-poll:false]
	
	Eshell V5.8.5  (abort with ^G)
	1> inets:start().
	ok
	2> {ok, {_, _, Data}} = httpc:request(get, {"http://feedparser.org/docs/examples/atom10.xml", []}, [], [{body_format,binary}]).
	{ok,{{"HTTP/1.1",200,"OK"}, [],
	     <<"<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<feed xmlns=\"http://www.w3.org/2005/Atom\" xml:base=\"http:"...>>}}
	3> application:start(feedparser).
	ok
	4> feedparser:parse(Data).
	{ok,[{feed,[{updated,"2005-11-09T11:56:34Z"},
	            {subtitle,"For documentation <em>only</em>"}, ...]},
	     {encoding,"utf-8"},
	     {version,"atom10"},
	     {namespaces,[{'',"http://www.w3.org/2005/Atom"},
	                  {xhtml,"http://www.w3.org/1999/xhtml"}]},
	     {entries,[[{updated,"2005-11-09T11:56:34Z"},
	                {links,[...]},
	                {title,"First entry title"},
	                {authors,[[{href,"http://diveintomark.org/"},
	                           {name,"Mark Pilgrim"},
	                           {email,"mark@example.org"}]]},
	                {summary,"Watch out for nasty tricks"},
	                {content,[...]},
	                {href,"http://example.org/sam/"}, ...
	     ]]}]}
