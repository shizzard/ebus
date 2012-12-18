Erlang internal pubpub server
=============================

This server is very simple; what it does is:

 - handling subscribers pids in channel state
 - publishing a message in a simple list comprehension

Channel name is always binary string.

Usage
-----

    (ebus@shizz-laptop)1> C = <<"channel-1">>.
    <<"channel-1">>
    (ebus@shizz-laptop)2> ebus:start(C).
    {ok,<<"channel-1">>}
    (ebus@shizz-laptop)3> ebus:subscribe(C).
    {ok,<<"channel-1">>}
    (ebus@shizz-laptop)4> ebus:publish(C, {message, test}).
    {ok,<<"channel-1">>}
    (ebus@shizz-laptop)5> flush().
    Shell got {message,test}
    ok
    (ebus@shizz-laptop)6> ebus:unsubscribe(C).             
    {ok,<<"channel-1">>}
    (ebus@shizz-laptop)7> ebus:publish(C, {message, test}).
    {ok,<<"channel-1">>}
    (ebus@shizz-laptop)8> flush().                         
    ok
