


#ifndef GDIN_JUDGE
#define gdin_judgeJUDGE 


#define BUFFER_SIZE 200
#define CALL_ARRAY_SZIE 512
#define STD_T_LIM 2
#define STD_F_LIM (STD_MB<<5)
#define STD_M_LIM (STD_MB<<7)

#define HOJ_MAX_LIMIT -1



//定义报错信息
#define NORMAL			1		//正常返回
#define ARGUMENT_FALUSE 2  		//传入参数错误	
	


//定义一下运行结果
#define OJ_WT0 0
#define OJ_WT1 1
#define OJ_CI 2
#define OJ_RI 3
#define OJ_AC 4
#define OJ_PE 5
#define OJ_WA 6
#define OJ_TL 7
#define OJ_ML 8
#define OJ_OL 9
#define OJ_RE 10
#define OJ_CE 11
#define OJ_CO 12
#define OJ_TR 13


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
	void prepare_file(char *filename , int namelen , char * userfile , char * outfile ) ;
	void system_ctl() ;
	void run_solution(int &	) ;
	void watch_solution(pid_t Pid , int topmemory , int &ACFlag, char *userfile , char *outfile , int &usedtime) ;
	int judge_solution( const char * file1 , const char * file2) ;
	int get_proc_status(int pid ,const char *mark) ;
	void print_runtime_error(char *) ;
	void init_syscalls_limits(int lang);
	void write_log(const char *fmt , ...) ;
	int compare(char *outfile , char * uesrfile);
	void delnextline(char s[]) ;

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

	//这个屏蔽数组用于屏蔽特定信号的数组
	int call_counter[CALL_ARRAY_SZIE] ;

};






#endif


