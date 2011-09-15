-module(feedparser_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	PoolSize = case application:get_env(feedparser, pool_size) of
		{ok, L} -> L;
		_ -> 4
	end,
	hottub:start_link(feedparser, PoolSize, feedparser, start_link, []).

stop(_State) ->
    ok.
