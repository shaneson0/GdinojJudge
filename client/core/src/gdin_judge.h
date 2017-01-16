


##ifndef GDIN_JUDGE
#define GDIN_JUDGE 

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
	int time_lmt ;
	int mem_lmt ;
	int lang ;


};


#endif


