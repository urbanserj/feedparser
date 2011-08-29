-module(feedparser).
-behavior(gen_server).

%% External exports
-export([start/0, start_link/0]).
-export([parse/1, parse/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
	handle_info/2, code_change/3, terminate/2]).

-record(state, {port}).

-define(PROG,    "./python ./feedparser-port.py").
-define(RETRY,   1).
-define(TIMEOUT, 10000).

start() ->
	gen_server:start({local, ?MODULE}, ?MODULE, [], []).
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

parse(Data) ->
	parse(Data, []).

parse(Data, Headers) ->
	gen_server:call(?MODULE, {parse, Data, Headers}, ?TIMEOUT*?RETRY*2).

%% ===================================================================
%% Gen_server callbacks
%% ===================================================================

init(_) ->
	process_flag(trap_exit, true),
	{ok, #state{port = start_port()}}.

handle_call({parse, Data, Headers}, From, State) ->
	handle_call({parse, Data, Headers, ?RETRY}, From, State);
handle_call({parse, _Data, _Headers, 0}, _From, #state{port = Port} = State) ->
	{reply, {error, wtf}, State#state{port = restart_port(Port)}};
handle_call({parse, Data, Headers, Cnt}, From, #state{port = Port} = State) ->
	port_command(Port, term_to_binary({parse, Data, Headers})),
	receive
		{Port, {data, PData}} ->
			{reply, binary_to_term(PData), State};
		{'EXIT', Port, _Reason} ->
			handle_call({parse, Data, Headers, Cnt-1}, From, State#state{port = start_port()})
	after ?TIMEOUT ->
		handle_call({parse, Data, Headers, Cnt-1}, From, State#state{port = restart_port(Port)})
	end.

handle_cast(_Request, State) ->
	{noreply, State}.

handle_info({'EXIT', Port, _Reason}, #state{port = Port} = State) ->
	{noreply, State#state{port = start_port()}};
handle_info(_Info, State) ->
	% io:format("info: ~p~n", [_Info]),
	{noreply, State}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

terminate(_Reason, _State) ->
	ok.

%% ===================================================================

start_port() ->
	Dir = case code:priv_dir(?MODULE) of
		{error, _} -> filename:join( [ filename:dirname(code:which(?MODULE)), "..", "priv" ] );
		PrivDir -> PrivDir
	end,
	open_port({spawn, ?PROG}, [{packet, 4}, binary, use_stdio, {cd, Dir}]).

restart_port(Port) ->
	catch port_close(Port),
	start_port().
