-module(feedparser).
-author('Sergey Urbanovich <sergey.urbanovich@gmail.com>').

-behavior(gen_server).

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

parse(Data) ->
	parse(Data, []).
parse(Data, Headers) ->
	Worker = poolboy:checkout(feedparser, true, infinity),
	try
		gen_server:call(Worker, {parse, Data, Headers}, ?TIMEOUT)
	after
		poolboy:checkin(feedparser, Worker)
	end.

%% ===================================================================
%% Gen_server callbacks
%% ===================================================================

init(_) ->
	Dir = case code:priv_dir(?MODULE) of
		{error, _} -> filename:join( [ filename:dirname(code:which(?MODULE)), "..", "priv" ] );
		PrivDir -> PrivDir
	end,
	AbsDir = filename:absname(Dir),
	Python = filename:join(AbsDir, "./python"),
	Port = open_port({spawn_executable, Python},
		[{packet, 4}, binary, use_stdio, exit_status, {cd, AbsDir}, {args, ["./feedparser-port.py"]}]
	),
	{ok, #state{port = Port}}.

handle_call({parse, Data, Headers}, From, #state{port = Port, queue = Queue} = State) ->
	port_command(Port, term_to_binary({parse, Data, Headers})),
	{noreply, State#state{queue = queue:in(From, Queue)}, ?TIMEOUT}.

handle_cast(_Request, State) ->
	{noreply, State}.

handle_info({Port, {data, Data}}, #state{port = Port, queue = FQueue} = State) ->
	{{value, From}, Queue} = queue:out(FQueue),
	gen_server:reply(From, binary_to_term(Data)),
	{noreply, State#state{queue = Queue}};
handle_info({Port, {exit_status, _Code}}, #state{port = Port} = State) ->
	{stop, port_exit, State};
handle_info(timeout, State) ->
	{stop, timeout, State};
handle_info(_Info, State) ->
	{noreply, State}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

terminate(_Reason, _State) ->
	ok.
