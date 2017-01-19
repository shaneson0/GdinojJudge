

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
gdin_judge::gdin_judge(int argc , char ** argc ) {

	if ( argc == 6 )
	{
		 //read data
		 sscanf( argc[0] , "%d" , &(this->problem_id) ) ;
		 sscanf( argc[1] , "%d" , &(this->time_lmt) ) ;
		 sscanf( argc[2] , "%d" , &(this->mem_lmt) ) ;
		 sscanf( argc[3] , "%d" , &(this->solution_id )  ) ;
		 sscanf( argc[4] , "%d" , &(this->lang )) ;
		 this->code = argc[5] ;

		 std::cout << this->problem_id << " " << this->time_lmt << " " << this->mem_lmt << " " << this->solution_id << std::endl; 

	}

}

gdin_judge::~gdin_judge() {
	
}



gdin_judge::gdin_judge() {

}

public void gdin_judge::prepare() {

}

public int gdin_judge::compile() {

}
