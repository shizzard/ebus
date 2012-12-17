-module(ebus).

-export([start/1, stop/1, publish/2, subscribe/1, unsubscribe/1]).

-spec start(Channel::binary()) -> 
    ok | {error, duplicate}.
start(Channel) when is_binary(Channel) ->
    ok;
start(_Channel) ->
    throw(badarg).  

-spec stop(Channel::binary()) -> 
    ok | {error, not_found}.
stop(Channel) when is_binary(Channel) ->
    ok;
stop(_Channel) ->
    throw(badarg).

-spec publish(Channel::binary(), Message::any()) ->
    ok | {error, not_found}.
publish(Channel, Message) when is_binary(Channel) ->
    ok;
publish(_Channel, _Message) ->
    throw(badarg).

-spec subscribe(Channel::binary()) ->
    ok | {error, not_found}.
subscribe(Channel) when is_binary(Channel) ->
    ok;
subscribe(_Channel) ->
    throw(badarg). 

-spec unsubscribe(Channel::binary()) ->
    ok | {error, not_found} | {error, not_subscribed}.
unsubscribe(Channel) when is_binary(Channel) ->
    ok;
unsubscribe(_Channel) ->
    throw(badarg).
