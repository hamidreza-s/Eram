-module(eram_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->

    application:ensure_started(lager),
    application:ensure_started(ranch),
    application:ensure_started(cowlib),
    application:ensure_started(cowboy),

    Dispatch = cowboy_router:compile(
		 [
		  {'_', [
			 {"/", cowboy_static, {priv_file, eram, "index.html"}},
			 {"/websocket", eram_websocket, []},
			 {"/static/[...]", cowboy_static, {priv_dir, eram, "static"}}
			]}
		 ]),
    {ok, _} = cowboy:start_http(http, 100,
				[{port, 8080}],
				[{env, [{dispatch, Dispatch}]}]),
    eram_sup:start_link().

stop(_State) ->
    ok.
