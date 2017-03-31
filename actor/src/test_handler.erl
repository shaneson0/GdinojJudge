
-module(test_handler).
-export([init/2]).

-export([content_types_provided/2]).
-export([hello_to_html/2]).
-export([hello_to_json/2]).
-export([hello_to_text/2]).

-include("task.hrl").

init(Req, Opts) ->
	{cowboy_rest, Req, Opts}.

content_types_provided(Req, State) ->
	{[
		{<<"text/html">>, hello_to_html},
		{<<"application/json">>, hello_to_json},
		{<<"text/plain">>, hello_to_text}
	], Req, State}.

hello_to_html(Req, State) ->

	Body = <<"<html>
<head>
	<meta charset=\"utf-8\">
	<title>REST Hello World!</title>
</head>
<body>
	<p>REST Hello World as HTML!</p>
</body>
</html>">>,
	{Body, Req, State}.

hello_to_json(Req, State) ->
	Arguments = [time_limit , mem_limit , problem_id , solution_id , code , lang ] ,
	#{time_limit := Time_lim , mem_limit := Mem_lim , problem_id := Problem_id ,solution_id := Solution_id , code := Code , lang := Lang }
		= cowboy_req:match_qs(Arguments, Req),
	NewTask = #task{time_limit = Time_lim , mem_limit = Mem_lim , problem_id = Problem_id , solutoin_id = Solution_id , code = Code , lang = Lang } ,
	publiser:publish(NewTask),

	Body = <<"{\"success\":true,\"msg\":\"\",\"data\":\"\"}">>,
	{Body, Req, State}.
hello_to_text(Req, State) ->
	{<<"REST Hello World as text!">>, Req, State}.


