
-module(feedparser_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	PoolSize = case application:get_env(feedparser, pool_size) of
		{ok, L} -> L;
		_ -> 4
	end,
    {ok, {{one_for_one, 10, 10}, [{feedparser,
		{poolboy, start_link, [[
			{name, {local, feedparser}},
			{worker_module, feedparser},
			{size, PoolSize},
			{max_overflow, 0}
		]]}, permanent, 5000, worker, [poolboy]}]}}.

