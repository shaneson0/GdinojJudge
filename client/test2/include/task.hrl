%%%-------------------------------------------------------------------
%%% @author csx
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. 一月 2017 下午8:43
%%%-------------------------------------------------------------------
-author("csx").


%%任务,用于发布到消息队列
-record( task , { time_limit , mem_limit , problem_id , solutoin_id , code , lang  } ).
