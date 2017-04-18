
-module(test_handler).

-include_lib("task.hrl").
-export([init/2]).

init(Req, Opts) ->
	Method = cowboy_req:method(Req),
	HasBody = cowboy_req:has_body(Req),
	Req2 = maybe_echo(Method, HasBody, Req),
	{ok, Req2, Opts}.

maybe_echo(<<"POST">>, true, Req) ->
	io:format("pass1~n"),
	{ok, PostVals, Req2} = cowboy_req:body_qs(Req),
	Task = solve_req(PostVals),
	publiser:publish(Task),
	Echo  = <<"ok">>,
	echo(Echo, Req2);
maybe_echo(<<"POST">>, false, Req) ->
	cowboy_req:reply(400, [], <<"Missing body.">>, Req);
maybe_echo(_, _, Req) ->
	%% Method not allowed.
	cowboy_req:reply(405, Req).

echo(undefined, Req) ->
	cowboy_req:reply(400, [], <<"Missing echo parameter.">>, Req);
echo(Echo, Req) ->
	cowboy_req:reply(200, [
		{<<"content-type">>, <<"text/plain; charset=utf-8">>}
	], Echo, Req).


solve_req(PostVals) ->
	TimeLimit = proplists:get_value(<<"time_limit">>, PostVals),
	MemLimit = proplists:get_value(<<"mem_limit">>, PostVals),
	ProblemId = proplists:get_value(<<"problem_id">>, PostVals),
	SolutionId = proplists:get_value(<<"solution_id">>, PostVals),
	Code = 	 proplists:get_value(<<"code">>, PostVals),
	Lang = 	 proplists:get_value(<<"lang">>, PostVals),
	Task = #task{time_limit = TimeLimit,mem_limit = MemLimit,problem_id = ProblemId,solutoin_id = SolutionId,code = Code,lang = Lang},
	Task.




