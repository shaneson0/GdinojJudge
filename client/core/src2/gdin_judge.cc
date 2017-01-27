

#include "gdin_judge.h"
#include <stdio.h>
#include <iostream>
#include <unistd.h>
#include <sys/stat.h>
#include <sstream>
#include <sys/resource.h>
#include <sys/time.h>
#include <stdlib.h>
#include <sys/wait.h>
#include <sys/types.h>
#include <stdarg.h>

#define STD_MB 1048576


/*
	传入的参数
	problem_id
	time_lmt
	mem_lmt
	solution_id
	lang
	code
	

*/
gdin_judge::gdin_judge(int argc , char ** argv ) {

	// std::cout << "argc " << argc << std::endl;

	if ( argc == 9 )
	{
		 //read data

		 sscanf( argv[1] , "%d" , &(this->problem_id) ) ;
		 sscanf( argv[2] , "%d" , &(this->time_lmt) ) ;
		 sscanf( argv[3] , "%d" , &(this->mem_lmt) ) ;
		 sscanf( argv[4] , "%d" , &(this->solution_id )  ) ;
		 sscanf( argv[5] , "%d" , &(this->lang )) ;
		 sscanf( argv[6] , "%d" , &(this->judged_id )) ;
		 sscanf( argv[7] , "%d" , &(this->DEBUG )) ;
		 this->data_path = argv[8];
		 

		 //设置各个静态变量的初始化的值
		 this->LANG[0] = "c" ;
		 this->LANG[1] = "cc" ;
		 this->CODE_STORAGE = "/home/judge/temp" ;
		 this->DATA_STORAGE = "/home/judge/data/" ;
		 this->RUN_PATH = "/home/judge/run" ;


		 std::cout << this->problem_id << " " << this->time_lmt << " " << this->mem_lmt << " " << this->solution_id << std::endl; 

		 std::cout <<  this->lang << " " << this->data_path << std::endl ;
		 printf("argv[7] : %s , DEBUG : %d\n",argv[7] , this->DEBUG );

	}

}

gdin_judge::~gdin_judge() {
	
}



gdin_judge::gdin_judge() {

}

void gdin_judge::prepare() {

	//获得数据路径跟判题路径
	this->data_path = this->DATA_STORAGE + tostring(this->problem_id) ;
	this->RUN_PATH = this->RUN_PATH + tostring(this->judged_id) ;

	//改变当前路径
	chdir( (this->RUN_PATH).data() ) ;

	std::cout<< "prepare ok " << std::endl;


}

int gdin_judge::run() {


	//根据编译语言来使用不同的编译器

	int res ;

	switch( this->lang ) {
		case 0:	 res = Clang() ; break   ;
		case 1:  res = Cpplang() ; break ;
		default: res = -1 ;
	}

	return res ;

}



int gdin_judge::Clang() {

	int pid  ;

	//compile args
	const char * CP_C[] = { "gcc", "Main.c", "-o", "Main", "-fno-asm", "-Wall",
			"-lm", "--static", "-std=c99", "-DONLINE_JUDGE", NULL };

	//fork a process to compile
	pid = fork() ;
	if ( pid == 0 )
	{

		printf("is debug : %d\n",this->DEBUG );

		if( this->DEBUG ) {
			int size = 100 ;
			char buf[100] ;

			getcwd(buf , size) ;
			printf("path : %s\n", buf ); 
		}

		 //set compile process limit
		 struct rlimit LIM ;
		 LIM.rlim_max = 60 ;
		 LIM.rlim_cur = 60 ;
		 setrlimit(RLIMIT_CPU, &LIM);	

		 //set the alarm也就是说，一个进程最晚的运行时间是60s
		 alarm(60) ;


		 //set the max memory
		 LIM.rlim_max = STD_MB << 10 ;
		 LIM.rlim_cur = STD_MB << 10 ;
		 setrlimit(RLIMIT_AS,&LIM) ;

		 //ce output file
		 //重定向输出流，让判题结果直接输出到文本
		 freopen("ce.txt","w" , stdout ) ;

		 //给予权限
		 execute_cmd("chown judge *");

		 //设置uid和gid和真实uid
		 while(setgid(1536)!=0) sleep(1);
        while(setuid(1536)!=0) sleep(1); 
        while(setreuid(1536, 1536)!=0) sleep(1);

        //执行运行命令
        execvp(CP_C[0], (char * const *) CP_C);
        exit(0) ;


	} else {
		int status = 0;

		waitpid(pid, &status, 0);
		if (lang > 3 && lang < 7)
			status = get_file_size("ce.txt");
		if (DEBUG)
			printf("status=%d\n", status);
		return status;

	}

}

int gdin_judge::Cpplang() {

	return 1 ;
}


//这是一个通用的执行命令函数，使用了c语言可变参数的额特性
int gdin_judge::execute_cmd(const char * fmt ,... ) {
	char cmd[BUFFER_SIZE];

	int ret = 0;
	va_list ap;

	va_start(ap, fmt);
	vsprintf(cmd, fmt, ap);
	ret = system(cmd);
	va_end(ap);
	return ret;
}


//get the file size 
long gdin_judge::get_file_size(const char * filename) {
	struct stat f_stat;

	if (stat(filename, &f_stat) == -1) {
		return 0;
	}

	return (long) f_stat.st_size;
}



//辅助函数，int->string
std::string gdin_judge::tostring(int num) 
{
        std::ostringstream stm ;
        stm << num ;
        return stm.str() ;
}








