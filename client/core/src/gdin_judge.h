


##ifndef GDIN_JUDGE
#define gdin_judgeJUDGE 

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

	/* data */
private:
	std::string code ;
	std::string lang ;
	int time_lmt ;
	int mem_lmt ;
	int lang ;
	int problem_id ;
	int solution_id ;



};


#endif


