-module(eram_loop).
-behaviour(gen_server).

-export([start_link/0, repl/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {binding :: orddict:orddict()}).

repl(LoopRef, Msg) ->
    gen_server:call(LoopRef, Msg).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    Binding = erl_eval:new_bindings(),
    {ok, #state{binding = Binding}}.

handle_call({evaluate, Msg}, _From, State) ->
    BinRequest = binary_to_list(Msg),
    {ok, Evaluated, State1} = evaluate(BinRequest, State),
    {ok, Formated, State2} = format(Evaluated, State1),
    {reply, {ok, Formated}, State2};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% === private

evaluate(Request, State) ->
    {ok, Tokens, _} = erl_scan:string(Request, 0),
    try
	case erl_parse:parse_exprs(Tokens) of
	    {ok, Exprs} ->
		{value, Result, NewBinding} =
		    erl_eval:exprs(Exprs, State#state.binding),
		{ok, Result, State#state{binding = NewBinding}};
	    {error, ErrorReason} ->
		{_Line, _Module, Description} = ErrorReason,
		{ok, format_err_description(Description), State}
	end
    catch
	Type:Exception ->
	    {ok, {term_to_list({Type, Exception}), State}}
    end.

format(Response, State)
  when is_atom(Response) ->
    {ok, atom_to_binary(Response, utf8), State};
format(Response, State)
  when is_list(Response) ->
    {ok, list_to_binary(Response), State};
format(Response, State)
  when is_binary(Response) ->
    {ok, Response, State};
format(Response, State)
  when is_integer(Response) ->
    {ok, integer_to_binary(Response), State};
format(_, State) ->
    %% === @TODO: handle other types
    {ok, <<"unformatable response">>, State}.

format_err_description([String, Argument]) ->
   lists:flatten(io_lib:format("~s~s", [String, Argument])).

term_to_list(Term) ->
 lists:flatten(io_lib:format("~p", [Term])).
