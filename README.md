GdinojJudge
=====

GdinojJudge，采用Erlang编写网络层，使用nif调用C编写的核心判题程序。目标是可以方便拓展判题系统，支持快速增加进行编译语言。

Prepare
-----
需要具备以下环境:（以后版本会支持一键部署全部环境）
1. gcc 4.84以上
2. erlang运行环境8.1以上
3. rabbitmq-server3.6.4以上

Run
-----

####actor

	cd actor
	rebar3 compile
	chmod +x start.sh
	./start.sh

####client

	cd client
	rebar3 compile
	chmod +x
	./start.sh
