
/*

	edit by shanxuan




*/





#include <stdio.h>

void init_parameters(int argc , char ** argv , int & pid , int & solution_id , char * code_path ) 
{


}



//从Erlang端传入三个参数，一个是pid , 一个是代码的地址,一个是solutionid,一个是lang表示语言,一个是题目限制
int main( int argc , char ** argv ) 
{
	char 	work_path[100] ;		//工作路径
	char 	code_path[100] ; 		//编译代码路径
	int 	pid 		   ;		//problem_id
	int 	solution_id    ;		//solution_id

	//初始化获得各个参数
	init_parameters(argc , argv , pid , solution_id  , code_path );

	//进入代码路径
	chdir(work_path) ;

	//判断设置是否合理


	//compile
	compile() ;


	//run
	run();

	

	return 0;
}







