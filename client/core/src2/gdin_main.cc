

#include <stdio.h>
#include "gdin_judge.h"



int main(int argc, char** argv) {

	gdin_judge * Judge = new gdin_judge(argc,argv) ;
	Judge->prepare() ;
	int res = Judge->run() ;
	printf("%d\n", res);

	return 0;
}
