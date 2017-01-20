


#ifndef GDIN_JUDGE
#define gdin_judgeJUDGE 

#define MAX 20

//定义报错信息

#define NORMAL			1		//正常返回
#define ARGUMENT_FALUSE 2  		//传入参数错误		


//代码临时存储区
#define CODE_STORAGE ""



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
	static char language[][MAX] = { "c" , "cc" } ;


	/* data */
private:
	std::string code ;
	int time_lmt ;
	int mem_lmt ;
	int lang ;
	int problem_id ;
	int solution_id ;



};


#endif


