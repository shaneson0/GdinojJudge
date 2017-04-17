

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
#include <string.h>
#include <sys/user.h>
#include <sys/ptrace.h>
#include <string>
#include <sys/signal.h>
#include <dirent.h>
#include <time.h>
#include <ctype.h>
#include <assert.h>
using namespace std ;

#ifdef __i386
	#include "System32CallControlor.h"
#else
	#include "System64CallControlor.h"
#endif

#ifdef __i386
	#define REG_SYSCALL orig_eax
	#define REG_RET eax
	#define REG_ARG0 ebx
	#define REG_ARG1 ecx
#else
	#define REG_SYSCALL orig_rax
	#define REG_RET rax
	#define REG_ARG0 rdi
	#define REG_ARG1 rsi
#endif

//临时

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

		 //设置系统参数
		 init_syscalls_limits(this->lang);

	}

}

void gdin_judge::prepare_file(char *filename , int namelen , char * userfile , char * outfile ) {
	char fname[BUFFER_SIZE] ;
	strncpy( fname , filename , namelen-3 ) ;
	fname[namelen-3] = 0 ;
	string strogePath = this->DATA_STORAGE + tostring(this->problem_id) + "/"  ;
	string runpath = this->RUN_PATH ; 
	printf("fname : %s\n",fname);
	execute_cmd( "/bin/cp '%s/%s.in' %s/data.in" , strogePath.data() , fname , runpath.data()  ) ;
	execute_cmd( "/bin/cp '%s/%s.out' %s/data.out" , strogePath.data() , fname , runpath.data()  ) ;
	sprintf(userfile , "%s/user.out" , runpath.data()   ) ;
	sprintf(outfile , "%s/data.out" , runpath.data()  ) ;
	printf("prepare file outfile : %s\n",outfile );
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


/*

	运行代码
*/

void gdin_judge::ptraceTest()
{
	int status ;
	pid_t child ;
	long orig_eax;
	child = fork();
	if( child == 0 ) {
		ptrace(PTRACE_TRACEME, 0, NULL, NULL);  
        execl("/bin/ls", "ls", NULL);  
	}else {
		while(1) {
			wait(&status) ;
			printf("whileing of status : %d\n",status);
			if(WIFEXITED(status))  
              break;  
			ptrace(PTRACE_CONT, child, NULL, NULL);
		}
		printf("end of status : %d\n",status);
		// struct rusage ruse ;
		// printf("wait for son process ..\n") ;
		// status = 0 ;
		// wait4(child,&status , 0 , &ruse) ;
		// if( WIFSIGNALED(status) )
		// {
		// 	printf("end of signal.\n");
		// }
		// printf("pass1 status : %d\n");
		// orig_eax = ptrace(PTRACE_PEEKUSER,   
		// 				child, 4 * ORIG_EAX,   
		// 				NULL);  
        // printf("The child made a "  
        //        "system call %ld ", orig_eax);  
        // ptrace(PTRACE_CONT, child, NULL, NULL);
	}
}

void gdin_judge::system_ctl() {

}

void gdin_judge::run_solution(int &usedtime) {


	//设置它的调度优先级
	nice(19) ;
	//改变当前的工作路径，以便于直接运行代码
	int res = chdir(this->RUN_PATH.data());
	if( res == -1 ){
		perror("can't change dir\n") ;
		exit(0) ;
	}
	//-------------------
	//run
	//-------------------

	//改变项目跟路径
	chroot(this->RUN_PATH.data()) ;
	//设置各个id
	while (setgid(1536) != 0)
		sleep(1);
	while (setuid(1536) != 0)
		sleep(1);
	while (setresuid(1536, 1536, 1536) != 0)
		sleep(1);

	//对进程占用的资源进行控制
	//主要是涉及time limit , file limit , memory limit
	struct rlimit LIM ;
	LIM.rlim_cur = (this->time_lmt - usedtime / 1000 ) + 1;
	LIM.rlim_max = LIM.rlim_cur ;
	setrlimit(RLIMIT_CPU , &LIM);

	//取消之前的时钟
	alarm(0) ;
	//设置时钟
	alarm(this->time_lmt) ;

	//设置文件连接
	LIM.rlim_cur = STD_F_LIM ;
	LIM.rlim_max = STD_F_LIM + STD_MB ;
	setrlimit( RLIMIT_FSIZE , &LIM ) ;

	//设置进程数
	LIM.rlim_cur = LIM.rlim_max = 2 ;
	setrlimit( RLIMIT_NPROC , &LIM ) ;

	//设置stack区。控制栈空间
	LIM.rlim_cur = STD_MB << 6 ;
	LIM.rlim_max = STD_MB << 6 ;
	setrlimit( RLIMIT_STACK , &LIM ) ;

	//控制内存
	LIM.rlim_cur = STD_MB * this->mem_lmt / 2 * 3 ;
	LIM.rlim_max = STD_MB * this->mem_lmt * 2 ;
	setrlimit( RLIMIT_AS , &LIM ) ;

	//运行结果
	freopen("data.in","r",stdin);
	freopen("user.out","w",stdout);
	freopen("error.out","a+",stderr);
	ptrace(PTRACE_TRACEME, 0, NULL, NULL); 
	if( execl("./Main", "./Main", (char *) NULL) == -1 )
	{
		printf("run error!");
	}
	else 
		printf("run sucess..!\n");
	
	exit(0) ;

}

void gdin_judge::watch_solution(pid_t Pid , int topmemory , int &ACFlag, char *userfile , char *outfile , int &usedtime ) {


	int tempmemory ;
	int status ; 		//用于记录子进程的运行状态
	int exitcode ;		//用于获取后八位的结束码

	//获取程序现在正在使用的内存
	topmemory = get_proc_status(Pid ,"VmRSS:");
	struct user_regs_struct reg;
	struct rusage ruse ;
	while(1)
	{
		wait4(Pid,&status , 0 , &ruse) ;

		//获得当前进程运行过程中占用内存的峰值
		int tempmemory = get_proc_status(Pid, "VmPeak:") << 10; 
		if( tempmemory > topmemory )
			topmemory = tempmemory ;
		
		//判断是否出现内存溢出
		if( topmemory > this->mem_lmt * STD_MB ) {
			ACFlag = OJ_ML ;
			ptrace( PTRACE_KILL , Pid , NULL ,NULL ) ;
			break ;
		}

		if(WIFEXITED(status))  
			break; 

		// printf("check re ..\n");
		//检查是否RE
		int file_size = get_file_size("error.out");
		// printf("filesize : %d\n",file_size);
		if( file_size ) {
			ACFlag = OJ_RE ;
			ptrace( PTRACE_KILL , Pid , NULL , NULL );
			break ;
		}


		// printf("check output limit ..\n");
		//检查是否输出超出限制
		if( get_file_size(userfile) >  get_file_size(outfile) * 2 + 1024 ) {
			ACFlag = OJ_OL ;
			ptrace( PTRACE_KILL , Pid , NULL , NULL ) ;
			break ;
		}


		// printf("check exit code ..\n");
		//获取子进程的结束返回值
		//检查是否RE
		exitcode = WEXITSTATUS(status) ;
		//c / c++ 中0表示正常结束
		if( exitcode == 0 || exitcode == SIGTRAP ) {
				//normal and go 
		} else {
			switch(exitcode) {
				case SIGCHLD:
				case SIGALRM:
					alarm(0);	//清楚时钟
				case SIGKILL:
				case SIGXCPU:
					ACFlag = OJ_TL;
					break ;
				case SIGXFSZ:
					ACFlag = OJ_OL;	//单个文件的大小超出了操作系统原来的设置
					break ;
				default :
					ACFlag = OJ_RE ;
			}
			print_runtime_error(strsignal(exitcode)) ;
			ptrace( PTRACE_KILL , Pid , NULL , NULL ) ;
			break ;
		}

		//暂时不检查信号的启动å
		// printf("check signal ..\n");
		//检查子进程是否被信号突然中断
		if( WIFSIGNALED(status) ) {
			int sig = WTERMSIG(status) ;

			switch(sig) {
				case SIGCHLD :
				case SIGALRM :
					alarm(0) ;
				case SIGKILL :
				case SIGXCPU :
					ACFlag = OJ_TL ;
					break ;
				case SIGXFSZ :
					ACFlag = OJ_OL ;
					break ;
				default :
					ACFlag = OJ_RE ;
			}
			print_runtime_error(strsignal(exitcode)) ;
			break ;
		}
		
		//检查全部系统调用
		ptrace( PTRACE_GETREGS , Pid , NULL , &reg) ;
		if( call_counter[reg.REG_SYSCALL] ) {
			//这个信号允许使用
		} else {
			//发现非法程序调用
			ACFlag = OJ_RE ;
			char error[BUFFER_SIZE];
			sprintf(error,"ask shanxuan for solution : %d CallID : %ld . mail is 781244184@qq.com",solution_id, (long)reg.REG_SYSCALL);
			perror( error ) ;
			// write_log("ask shanxuan for solution : %d CallID : %ld . mail is 781244184@qq.com",solution_id, (long)reg.REG_SYSCALL) ;
			print_runtime_error(error);
			ptrace( PTRACE_KILL , Pid , NULL , NULL ) ;

		}


		ptrace(PTRACE_SYSCALL,   
				Pid, NULL, NULL); 
	}

	usedtime += (ruse.ru_utime.tv_sec * 1000 + ruse.ru_utime.tv_usec / 1000);
	usedtime += (ruse.ru_stime.tv_sec * 1000 + ruse.ru_stime.tv_usec / 1000);

	
}

int gdin_judge::run() {
	int Acflag = OJ_AC ;

	//Main Code
	std::string infile ;
	std::string outfile ;
	std::string userfile ;

	//OPEN DIRS
	DIR *dp ;
	dirent *dirp ;
	
	if( (dp = opendir((this->data_path).data() ) )== NULL ){
		perror("No suck file\n");
		exit(-1);
	}

	//read files and run
	int usertime = 0 ;
	while( Acflag && (dirp = readdir(dp)) != NULL )
	{
		//判断一下是否是in文件
		if( this->isInFile(dirp->d_name) ) {
			char userfile[BUFFER_SIZE] ;
			char outfile[BUFFER_SIZE] ;

			int namelen = strlen(dirp->d_name) ;
			this->prepare_file(dirp->d_name,namelen,userfile , outfile);
			pid_t AppPid = fork() ;
			if( AppPid == 0 )
			{
				// 子进程直接运行代码
				this->run_solution(usertime) ;
			} else 
			{
				//父进程监控子进程的运行结果
				this->watch_solution(AppPid,this->mem_lmt,Acflag,userfile,outfile , usertime ) ;
			}

		}
	}
	return Acflag ;
}

int gdin_judge::compile() {


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



//辅助函数,判断是否是in文件
bool gdin_judge::isInFile(const char fname[])
{
	int l = strlen(fname) ;
	if ( l <= 3 || strcmp( fname + l - 3 , ".in" ) != 0 )
	{
		/* code */
		return false  ;
	}

	return true ;

}

int gdin_judge::get_proc_status(int pid , const char * mark)
{
	FILE *pf ;
	char fn[BUFFER_SIZE] , buf[BUFFER_SIZE] ;
	int ret = 0 ;
	sprintf( fn , "proc/%d/status", pid);
	pf = fopen( fn , "fe" ) ;
	int m = strlen(mark) ;
	while( pf && fgets( buf , BUFFER_SIZE -1 , pf ) )
	{
		buf[strlen(buf) - 1] = 0 ;
		if( strncmp(buf , mark , m) == 0 )
			sscanf(buf + m + 1 ,"%d" , &ret ) ;
	}
	if( pf ) 
		fclose(pf) ;
	return ret ;
}


void gdin_judge::print_runtime_error(char * error)
{
	FILE *ferr = fopen("error.out", "a+");
	fprintf(ferr, "Runtime Error:%s\n", error);
	fclose(ferr);
}


void gdin_judge::init_syscalls_limits( int lang ) 
{
	int i;
	memset( this->call_counter , 0 , sizeof(this->call_counter) ) ;
	if( lang <= 1  ) { //C or C++ 
		for( i = 0 ; i == 0 || LANG_CV[i] ; i++ ) {
			call_counter[ LANG_CV[i] ] = HOJ_MAX_LIMIT ;
		}
	} else {
		//Nothing
	}
}



int gdin_judge::judge_solution(const char *file1 , const char *file2)
{
	printf("judge_solution : userfile : %s , outputfile : %s\n",file1,file2);
	//compare wr or ac
	FILE *f1 , *f2 ;
	char *s1,*s2;
	int PEFlag ;
	s1 = new char[STD_F_LIM+512] ;
	s2 = new char[STD_F_LIM+512] ;
	if( !(f1=fopen(file1,"re")) )
		return OJ_AC ;
	int file1size = get_file_size(file1);
	printf("file1 : %d\n",file1size);
	
	printf("get file1 ..\n");
	//get file1
	for( char * p1 = s1 ; fscanf(f1,"%s",p1) != EOF ;  )
	{
		// printf("s1 : %s , num : p1 - s1 : %d\n",p1,p1-s1);
		while( *p1 ) p1++ ;
	}
	fclose(f1) ;

	if( !(f2=fopen(file2,"re")) )
		return OJ_AC ;
	//get file2
	for( char * p2 = s2 ; fscanf(f2,"%s",p2) != EOF ;  )
		while( *p2 ) p2++ ;
	fclose(f2) ;

	printf("compare ... \n");
	//compare
	if( strcmp( s1,s2 ) != 0 )
	{
		delete []s1 ;
		delete []s2 ;
		return OJ_WA ;
	}
	else 
	{
		f1 = fopen(file1,"re") ;
		f2 = fopen(file2,"re") ;
		PEFlag = 0 ;
		while( PEFlag == 0 && fgets(s1,STD_F_LIM,f1) && fgets(s2,STD_F_LIM,f2) )
		{
			delnextline(s1) ; delnextline(s2) ;
			if( strcmp(s1 , s2) == 0 ) continue ;
			else {
				PEFlag = 1 ;
			}
		}
		delete []s1 ; 
		delete []s2 ;
		fclose(f1) ; fclose(f2) ;
		if( PEFlag )	return OJ_PE ;
		else 			return OJ_AC ;
	}


}


void gdin_judge::delnextline(char s[]) {
	int L ;
	L = strlen(s) ;
	while( L > 0 && ( s[L-1] == '\n' || s[L-1] == '\r' )  )
		s[--L] = 0 ;
}














