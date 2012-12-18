console: compile
	erl -pa ebin deps/*/ebin -sname ebus -s ebus start_dev

clean:
	rebar clean

compile:
	rebar compile
