#compile
rebar3 compile


#run
erl -pa ebin _build/default/lib/*/ebin -s test -noshell
