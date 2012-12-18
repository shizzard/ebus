-module(ebus).

-export([start/1, stop/1, publish/2, subscribe/1, unsubscribe/1]).

-export([start_dev/0]).

-spec start(Channel::binary()) ->
    {ok, Channel::binary} | {error, Reason::any()}.
start(Channel) when is_binary(Channel) ->
    case supervisor:start_child(ebus_sup, [Channel]) of 
        {ok, _Pid} ->
            {ok, Channel};
        Error ->
            {error, Error}
    end.

-spec stop(Channel::binary()) ->
    {ok, Channel::binary} | {error, Reason::any()}.
stop(Channel) when is_binary(Channel) ->
    case ebus_channel:pid(Channel) of 
        {ok, Pid} ->
            supervisor:terminate_child(ebus_sup, Pid),
            {ok, Channel};
        {error, Reason} ->
            {error, Reason}
    end.

-spec publish(Channel::binary(), Message::any()) ->
    {ok, Channel::binary} | {error, Reason::any()}.
publish(Channel, Message) when is_binary(Channel) ->
    case ebus_channel:pid(Channel) of 
        {ok, Pid} ->
            ebus_channel:publish(Pid, Message),
            {ok, Channel};
        {error, Reason} ->
            {error, Reason}
    end.

-spec subscribe(Channel::binary()) ->
    {ok, Channel::binary} | {error, Reason::any()}.
subscribe(Channel) when is_binary(Channel) ->
    case ebus_channel:pid(Channel) of 
        {ok, Pid} ->
            ebus_channel:subscribe(Pid),
            {ok, Channel};
        {error, Reason} ->
            {error, Reason}
    end.

-spec unsubscribe(Channel::binary()) ->
    {ok, Channel::binary} | {error, Reason::any()}.
unsubscribe(Channel) when is_binary(Channel) ->
    case ebus_channel:pid(Channel) of 
        {ok, Pid} ->
            ebus_channel:unsubscribe(Pid),
            {ok, Channel};
        {error, Reason} ->
            {error, Reason}
    end.

start_dev() ->
    application:start(gproc),
    application:start(ebus).

% run_test() ->
%     io:format("== Test start ================================================"),
%     ebus:start(<<"channel">>),
%     [spawn(fun() -> test_worker(N) end) || N <- lists:seq(1, 1000)],
%     timer:sleep(100),
%     ebus:publish(<<"channel">>, {message, test}),
%     io:format("== Test stop =================================================").

% test_worker(N) ->
%     ebus:subscribe(<<"channel">>),
%     receive 
%         Msg ->
%             io:format("#~4B got a message: ~p~n", [N, Msg])
%     end.