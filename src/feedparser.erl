-module(feedparser).
-author('Sergey Urbanovich <sergey.urbanovich@gmail.com>').

-behavior(gen_server).
-behavior(poolboy_worker).

%% External exports
-export([
	start_link/1,
	parse/1, parse/2,
	bparse/1, bparse/2
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
	handle_info/2, code_change/3, terminate/2]).

-define(TIMEOUT, 120000).

-record(state, {port, queue = queue:new()}).

start_link(_) ->
	gen_server:start_link(?MODULE, [], []).

-type feedparser(String) :: {ok, [
	  {bozo, integer()}
	| {bozo_exception, String}
	| {feed, [
		  {author, String}
		| {author_detail, String}
		| {cloud, [
			  {domain, String}
			| {port, String | integer()}
			| {path, String}
			| {registerProcedure, String}
			| {protocol, String}
		  ]}
		| {contributors, [[
			  {name, String}
			| {href, String}
			| {email, String}
		  ]]}
		| {docs, String}
		| {errorreportsto, String}
		| {generator, String}
		| {icon, String}
		| {id, String}
		| {image, [
			  {title, String}
			| {href, String}
			| {link, String}
			| {width, integer()}
			| {height, integer()}
			| {description, String}
		  ]}
		| {info, String}
		| {language, String}
		| {license, String}
		| {link, String}
		| {links, [[
			  {ref, String}
			| {type, String}
			| {href, String}
			| {title, String}
		  ]]}
		| {logo, String}
		| {published, String}
		| {publisher, String}
		| {rights, String}
		| {subtitle, String}
		| {tags, [[
			  {term, String}
			| {scheme, String}
			| {label, String}
		  ]]}
		| {textinput, [
			  {title, String}
			| {link, String}
			| {name, String}
			| {description, String}
		  ]}
		| {title, String}
		| {ttl, String}
		| {updated, String}
	  ]}
	| {entries, [[
		  {author, String}
		| {author_detail, [
			  {name, String}
			| {href, String}
			| {email, String}
		  ]}
		| {comments, String}
		| {content, [[
			  {value, String}
			| {type, String}
			| {language, String}
			| {base, String}
		  ]]}
		| {contributors, [[
			  {name, String}
			| {href, String}
			| {email, String}
		  ]]}
		| {created, String}
		| {enclosures, [[
			  {href, String}
			| {length, String}
			| {type, String}
		  ]]}
		| {expired, String}
		| {id, String}
		| {license, String}
		| {link, String}
		| {links, [[
			  {ref, String}
			| {type, String}
			| {href, String}
			| {title, String}
		  ]]}
		| {published, String}
		| {publisher, String}
		| {source, [
			  {author, String}
			| {contributors, [[
				  {name, String}
				| {href, String}
				| {email, String}
			  ]]}
			| {icon, String}
			| {link, String}
			| {links, [[
				  {ref, String}
				| {type, String}
				| {href, String}
				| {title, String}
			  ]]}
			| {logo, String}
			| {rights, String}
			| {subtitle, String}
			| {title, String}
			| {updated, String}
		  ]}
		| {summary, String}
		| {tags, [[
			  {term, String}
			| {scheme, String}
			| {label, String}
		  ]]}
		| {title, String}
		| {updated, String}
		| {vcard, String}
		| {xfn, [
			  {relationships, String}
			| {href, String}
			| {name, String}
		  ]}
	  ]]}
	| {encoding, String}
	| {namespaces, [{atom(), String}]}
	| {version, String}
]} | {error, internal | timeout | exit}.

-type headers() :: [{binary(), binary()}].


-spec parse(binary()) -> feedparser(string()).
parse(Data) ->
	parse(Data, []).

-spec parse(binary(), headers()) -> feedparser(string()).
parse(Data, Headers) ->
	iparse(Data, Headers, string).


-spec bparse(binary()) -> feedparser(binary()).
bparse(Data) ->
	bparse(Data, []).

-spec bparse(binary(), headers()) -> feedparser(binary()).
bparse(Data, Headers) ->
	iparse(Data, Headers, binary).


%% @private
-spec iparse(binary(), headers(), string | binary)
		-> feedparser(binary() | string()).
iparse(Data, Headers, Type) ->
	Worker = poolboy:checkout(feedparser, true, infinity),
	try
		BinData = term_to_binary({parse, Data, Headers, Type}),
		RBinData = gen_server:call(Worker, {parse, BinData}, ?TIMEOUT),
		binary_to_term(RBinData)
	catch exit:{Error, _} ->
		{error, Error}
	after
		poolboy:checkin(feedparser, Worker)
	end.

%% ===================================================================
%% Gen_server callbacks
%% ===================================================================

init(_) ->
	Dir = case code:priv_dir(?MODULE) of
		{error, _} ->
			ModuleDir = filename:dirname(code:which(?MODULE)),
			filename:join( [ModuleDir, "..", "priv" ]);
		PrivDir ->
			PrivDir
	end,
	AbsDir = filename:absname(Dir),
	AbsScript = filename:join(AbsDir, "feedparser-port.py"),
	Port = open_port({spawn_executable, AbsScript},
		[{packet, 4}, binary, use_stdio, exit_status, {cd, AbsDir}]),
	{ok, #state{port=Port}}.

handle_call({parse, Data}, From, #state{port=Port, queue=Queue}=State) ->
	port_command(Port, Data),
	{noreply, State#state{queue=queue:in(From, Queue)}, ?TIMEOUT}.

handle_cast(_Request, State) ->
	{noreply, State}.

handle_info({Port, {data, Data}}, #state{port=Port, queue=FQueue}=State) ->
	{{value, From}, Queue} = queue:out(FQueue),
	gen_server:reply(From, Data),
	{noreply, State#state{queue=Queue}};
handle_info({Port, {exit_status, _Code}}, #state{port=Port}=State) ->
	{stop, exit, State};
handle_info(timeout, State) ->
	{stop, timeout, State};
handle_info(_Info, State) ->
	{noreply, State}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

terminate(_Reason, _State) ->
	ok.


-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

atom10_test() ->
	ok = application:start(feedparser),
	{ok, Data} = file:read_file("../atom10.xml"),
	{ok, Atom} = feedparser:parse(Data),
	{entries, [Entry]} = lists:keyfind(entries, 1, Atom),
	{title, "First entry title"} = lists:keyfind(title, 1, Entry),
	ok = application:stop(feedparser).

-endif.
