

#include "gdin_judge.h"
#include <stdio.h>
#include <iostream>


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

	if ( argc == 7 )
	{
		 //read data

		 sscanf( argv[1] , "%d" , &(this->problem_id) ) ;
		 sscanf( argv[2] , "%d" , &(this->time_lmt) ) ;
		 sscanf( argv[3] , "%d" , &(this->mem_lmt) ) ;
		 sscanf( argv[4] , "%d" , &(this->solution_id )  ) ;
		 sscanf( argv[5] , "%d" , &(this->lang )) ;
		 this->code = argv[6] ;

		 std::cout << this->problem_id << " " << this->time_lmt << " " << this->mem_lmt << " " << this->solution_id << std::endl; 

		 std::cout <<  this->lang << " " << this->code << std::endl ;

	}

}

gdin_judge::~gdin_judge() {
	
}



gdin_judge::gdin_judge() {

}

void gdin_judge::prepare() {

}

int gdin_judge::compile() {

}
