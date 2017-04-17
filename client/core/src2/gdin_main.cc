

#include <stdio.h>
#include "gdin_judge.h"
#include <sys/syscall.h>

#include <unistd.h>
#include <sys/ptrace.h>
#include <sys/wait.h>
#include <unistd.h>
#include <stdlib.h>




int main(int argc, char** argv) {
	gdin_judge * Judge = new gdin_judge(argc,argv) ;
	Judge->prepare() ;
	Judge->compile() ;
	// Judge->ptraceTest();
	int res = Judge->run() ;
	printf("%d\n", res);
	return 0;
}
