-module(iota_ledger).

-include("iota.hrl").

-export([start_link/1, stop/0]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-export([is_approved/1]).


start_link(Args) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).


stop() ->
	gen_server:cast(?MODULE, stop).


is_approved(Hash) ->
	gen_server:call(?MODULE, {is_approved, Hash}).



init(_Args) ->
	{ok, Milestone} = build_snapshot(),
	{ok, #{milestone => Milestone, approvals => []}}.

handle_call({is_approved, Hash}, _From, State = #{approvals := Approvals}) ->
	Reply = lists:member(Hash, Approvals),
	{reply, Reply, State};
handle_call(Message, _From, State) ->
	{reply, {error, Message}, State}.


handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Message, State) ->
    {noreply, State}.


handle_info(_Message, State) ->
    {noreply, State}.


code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


terminate(_Reason, _State) ->
	ok.


build_snapshot() ->
	% TODO
	{ok, #milestone{}}.
