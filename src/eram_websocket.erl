-module(eram_websocket).

-export([init/3, websocket_init/3, websocket_handle/3,
	 websocket_info/3, websocket_terminate/3]).

-record(state, {loop_pid :: pid()}).

init(_, Req, Opts) ->
    {upgrade, protocol, cowboy_websocket, Req, Opts}.

websocket_init(_, Req, _Opts) ->
    {ok, LoopPID} = eram_loop:start_link(),
    {ok, Req, #state{loop_pid = LoopPID}}.

websocket_handle({text, Msg}, Req, State) ->
    lager:info("websocket incoming text: ~p (~p)", [Msg, State]),

    {ok, Response} = eram_loop:repl(State#state.loop_pid,
				      {evaluate, Msg}),

    lager:info("websocket outgoing text: ~p (~p)", [Response, State]),
    {reply, {text, Response}, Req, State};
websocket_handle(_Data, Req, State) ->
    {ok, Req, State}.

websocket_info(_Info, Req, State) ->
    {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
    ok.
