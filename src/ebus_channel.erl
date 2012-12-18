-module(ebus_channel).

-behaviour(gen_server).

-include("log.hrl").

-export([pid/1, publish/2, subscribe/1, unsubscribe/1]).

-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
    channel :: binary()
}).

-define(CHANNEL(Channel), {?MODULE, Channel}).
-define(PUBLISH(Message), {publish, Message}).
-define(SUBSCRIBE(Pid), {subscribe, Pid}).
-define(UNSUBSCRIBE(Pid), {unsubscribe, Pid}).

pid(Channel) ->
    case gproc:where({n, l, ?CHANNEL(Channel)}) of 
        undefined ->
            {error, undefined};
        Pid ->
            {ok, Pid}
    end.

publish(Pid, Message) ->
    gen_server:cast(Pid, ?PUBLISH(Message)).

subscribe(Pid) ->
    gen_server:cast(Pid, ?SUBSCRIBE(self())).

unsubscribe(Pid) ->
    gen_server:cast(Pid, ?UNSUBSCRIBE(self())).

%%% API
start_link(Channel) ->
    gen_server:start_link(?MODULE, [Channel], []).

%%% gen_server callbacks
init([Channel]) ->
    gproc:reg({n, l, ?CHANNEL(Channel)}), 
    {ok, #state{channel = Channel}}.

handle_call(Request, _From, State) ->
    ?ERROR("Strange call: ~p", [Request]),
    {reply, ok, State}.

handle_cast(?PUBLISH(Message), State) ->
    ?ERROR("Publish: ~p", [Message]),
    % publish routines
    {noreply, State};
handle_cast(?SUBSCRIBE(Subscriber), State) ->
    ?ERROR("Subscribe: ~p", [Subscriber]),
    % subscribe routines
    {noreply, State};
handle_cast(?UNSUBSCRIBE(Subscriber), State) ->
    ?ERROR("Unsubscribe: ~p", [Subscriber]),
    % unsubscribe routines
    {noreply, State};
handle_cast(Msg, State) ->
    ?ERROR("Strange cast: ~p", [Msg]),
    {noreply, State}.

handle_info(Info, State) ->
    ?ERROR("Strange info: ~p", [Info]),
    {noreply, State}.

terminate(_Reason, State) ->
    gproc:unreg({n, l, State#state.channel}), 
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
