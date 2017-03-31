

#include <stdio.h>
#include "gdin_judge.h"
#include <sys/syscall.h>


int main(int argc, char** argv) {

	gdin_judge * Judge = new gdin_judge(argc,argv) ;
	Judge->prepare() ;
	Judge->compile() ;
	int res = Judge->run() ;
	printf("%d\n", res);

	return 0;
}
