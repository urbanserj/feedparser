-module(feedparser).
-author('Sergey Urbanovich <sergey.urbanovich@gmail.com>').

-behavior(gen_server).
-behavior(poolboy_worker).

%% External exports
-export([
	start_link/1, parse/1, parse/2
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
	handle_info/2, code_change/3, terminate/2]).

-define(TIMEOUT, 120000).

-record(state, {port, queue = queue:new()}).

start_link(_) ->
	gen_server:start_link(?MODULE, [], []).

-spec parse(binary()) -> [
	  {bozo, integer()}
	| {bozo_exception, string()}
	| {feed, [
		  {author, string()}
		| {author_detail, string()}
		| {cloud, [
			  {domain, string()}
			| {port, string() | integer()}
			| {path, string()}
			| {registerProcedure, string()}
			| {protocol, string()}
		  ]}
		| {contributors, [[
			  {name, string()}
			| {href, string()}
			| {email, string()}
		  ]]}
		| {docs, string()}
		| {errorreportsto, string()}
		| {generator, string()}
		| {icon, string()}
		| {id, string()}
		| {image, [
			  {title, string()}
			| {href, string()}
			| {link, string()}
			| {width, integer()}
			| {height, integer()}
			| {description, string()}
		  ]}
		| {info, string()}
		| {language, string()}
		| {license, string()}
		| {link, string()}
		| {links, [[
			  {ref, string()}
			| {type, string()}
			| {href, string()}
			| {title, string()}
		  ]]}
		| {logo, string()}
		| {published, string()}
		| {publisher, string()}
		| {rights, string()}
		| {subtitle, string()}
		| {tags, [[
			  {term, string()}
			| {scheme, string()}
			| {label, string()}
		  ]]}
		| {textinput, [
			  {title, string()}
			| {link, string()}
			| {name, string()}
			| {description, string()}
		  ]}
		| {title, string()}
		| {ttl, string()}
		| {updated, string()}
	  ]}
	| {entries, [[
		  {author, string()}
		| {author_detail, [
			  {name, string()}
			| {href, string()}
			| {email, string()}
		  ]}
		| {comments, string()}
		| {content, [[
			  {value, string()}
			| {type, string()}
			| {language, string()}
			| {base, string()}
		  ]]}
		| {contributors, [[
			  {name, string()}
			| {href, string()}
			| {email, string()}
		  ]]}
		| {created, string()}
		| {enclosures, [[
			  {href, string()}
			| {length, string()}
			| {type, string()}
		  ]]}
		| {expired, string()}
		| {id, string()}
		| {license, string()}
		| {link, string()}
		| {links, [[
			  {ref, string()}
			| {type, string()}
			| {href, string()}
			| {title, string()}
		  ]]}
		| {published, string()}
		| {publisher, string()}
		| {source, [
			  {author, string()}
			| {contributors, [[
				  {name, string()}
				| {href, string()}
				| {email, string()}
			  ]]}
			| {icon, string()}
			| {link, string()}
			| {links, [[
				  {ref, string()}
				| {type, string()}
				| {href, string()}
				| {title, string()}
			  ]]}
			| {logo, string()}
			| {rights, string()}
			| {subtitle, string()}
			| {title, string()}
			| {updated, string()}
		  ]}
		| {summary, string()}
		| {tags, [[
			  {term, string()}
			| {scheme, string()}
			| {label, string()}
		  ]]}
		| {title, string()}
		| {updated, string()}
		| {vcard, string()}
		| {xfn, [
			  {relationships, string()}
			| {href, string()}
			| {name, string()}
		  ]}
	  ]]}
	| {encoding, string()}
	| {namespaces, [{atom(), string()}]}
	| {version, string()}
].
parse(Data) ->
	parse(Data, []).
parse(Data, Headers) ->
	Worker = poolboy:checkout(feedparser, true, infinity),
	try
		BinData = term_to_binary({parse, Data, Headers}),
		RBinData = gen_server:call(Worker, {parse, BinData}, ?TIMEOUT),
		binary_to_term(RBinData)
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
	{stop, port_exit, State};
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
