#compile

rebar3 compile


#run
erl -noshell -pa ebin _build/default/lib/*/ebin -s test2
