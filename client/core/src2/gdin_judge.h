


#ifndef GDIN_JUDGE
#define gdin_judgeJUDGE 


#define BUFFER_SIZE 200


//定义报错信息

#define NORMAL			1		//正常返回
#define ARGUMENT_FALUSE 2  		//传入参数错误		



#include <stdio.h>
#include <string>


class gdin_judge
{

public:
	gdin_judge();
	gdin_judge(int argc , char ** argv ) ;
	~gdin_judge();
	void prepare() ;
	int compile() ;
	int run() ;	


	int execute_cmd(const char * fmt, ...);				//
	long get_file_size(const char * filename ) ;		//获得文件大小

	int Clang() ;
	int Cpplang() ;

	std::string tostring(int ) ;

	/*

		judge函数

	*/
	void prepare_file() ;
	void system_ctl() ;
	void run_solution() ;
	void watch_solution() ;


	/*
		
		辅助函数
	
	*/

	//判断是否是.*in文件
	bool isInFile(const char fname[] );




	/* data */
private:

	std::string data_path ;
	int time_lmt ;
	int mem_lmt ;
	int lang ;          //0表示C , 1表示c++
	int problem_id ;
	int solution_id ;	
	int judged_id ;		//judged_id 用于识别判题进程
	int DEBUG ;


	std::string LANG[10] ;	
	std::string  CODE_STORAGE ;	
	std::string  DATA_STORAGE ;
	std::string  RUN_PATH ; 	//运行路径







};


#endif


