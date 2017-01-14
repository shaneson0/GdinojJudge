

#include<stdio.h>




int main(int argc, char** argv) {

	char work_dir[BUFFER_SIZE];
	//char cmd[BUFFER_SIZE];
	char user_id[BUFFER_SIZE];
	int solution_id = 1000;
	int runner_id = 0;
	int p_id, time_lmt, mem_lmt, lang, isspj, sim, sim_s_id, max_case_time = 0;

	init_parameters(argc, argv, solution_id, runner_id);


	//set work directory to start running & judging
	sprintf(work_dir, "%s/run%s/", oj_home, argv[2]);


	if (shm_run)
		mk_shm_workdir(work_dir);

	chdir(work_dir);
	if (!DEBUG)
		clean_workdir(work_dir);

	if (http_judge)
		system("/bin/ln -s ../cookie ./");
	get_solution_info(solution_id, p_id, user_id, lang);
	//get the limit

	if (p_id == 0) {
		time_lmt = 5;
		mem_lmt = 128;
		isspj = 0;
	} else {
		get_problem_info(p_id, time_lmt, mem_lmt, isspj);
	}
	//copy source file
	get_solution(solution_id, work_dir, lang);

	//java is lucky
	if (lang >= 3) {
		// the limit for java
		time_lmt = time_lmt + java_time_bonus;
		mem_lmt = mem_lmt + java_memory_bonus;
		// copy java.policy
		execute_cmd("/bin/cp %s/etc/java0.policy %s/java.policy", oj_home,
				work_dir);

	}

	//never bigger than judged set value;
	if (time_lmt > 300 || time_lmt < 1)
		time_lmt = 300;
	if (mem_lmt > 1024 || mem_lmt < 1)
		mem_lmt = 1024;

	if (DEBUG)
		printf("time: %d mem: %d\n", time_lmt, mem_lmt);

	// compile
	//      printf("%s\n",cmd);
	// set the result to compiling
	int Compile_OK;

	Compile_OK = compile(lang);
	if (Compile_OK != 0) {
		addceinfo(solution_id);
		update_solution(solution_id, OJ_CE, 0, 0, 0, 0, 0.0);
		update_user(user_id);
		update_problem(p_id);
		if (!http_judge)
			mysql_close(conn);
		if (!DEBUG)
			clean_workdir(work_dir);
		else
			write_log("compile error");
		exit(0);
	} else {
		update_solution(solution_id, OJ_RI, 0, 0, 0, 0, 0.0);
	}
	//exit(0);
	// run
	char fullpath[BUFFER_SIZE];
	char infile[BUFFER_SIZE];
	char outfile[BUFFER_SIZE];
	char userfile[BUFFER_SIZE];
	sprintf(fullpath, "%s/data/%d", oj_home, p_id); // the fullpath of data dir

	// open DIRs
	DIR *dp;
	dirent *dirp;
	// using http to get remote test data files
	if (p_id > 0 && http_judge)
		get_test_file(work_dir, p_id);
	if (p_id > 0 && (dp = opendir(fullpath)) == NULL) {

		write_log("No such dir:%s!\n", fullpath);
		if (!http_judge)
			mysql_close(conn);
		exit(-1);
	}

	int ACflg, PEflg;
	ACflg = PEflg = OJ_AC;
	int namelen;
	int usedtime = 0, topmemory = 0;

	//create chroot for ruby bash python
	if (lang == 4)
		copy_ruby_runtime(work_dir);
	if (lang == 5)
		copy_bash_runtime(work_dir);
	if (lang == 6)
		copy_python_runtime(work_dir);
	if (lang == 7)
		copy_php_runtime(work_dir);
	if (lang == 8)
		copy_perl_runtime(work_dir);
	if (lang == 9)
		copy_mono_runtime(work_dir);
	if (lang == 10)
		copy_objc_runtime(work_dir);
	if (lang == 11)
		copy_freebasic_runtime(work_dir);
	if (lang == 12)
		copy_guile_runtime(work_dir);
	if (lang == 15)
		copy_lua_runtime(work_dir);
	if (lang == 16)
		copy_js_runtime(work_dir);
	// read files and run
	// read files and run
	// read files and run
	double pass_rate = 0.0;
	int num_of_test = 0;
	int finalACflg = ACflg;
	if (p_id == 0) {  //custom input running
		printf("running a custom input...\n");
		get_custominput(solution_id, work_dir);
		init_syscalls_limits(lang);
		pid_t pidApp = fork();

		if (pidApp == 0) {
			run_solution(lang, work_dir, time_lmt, usedtime, mem_lmt);
		} else {
			watch_solution(pidApp, infile, ACflg, isspj, userfile, outfile,
					solution_id, lang, topmemory, mem_lmt, usedtime, time_lmt,
					p_id, PEflg, work_dir);

		}
		if (ACflg == OJ_TL) {
			usedtime = time_lmt * 1000;
		}
		if (ACflg == OJ_RE) {
			if (DEBUG)
				printf("add RE info of %d..... \n", solution_id);
			addreinfo(solution_id);
		} else {
			addcustomout(solution_id);
		}
		update_solution(solution_id, OJ_TR, usedtime, topmemory >> 10, 0, 0, 0);

		exit(0);
	}

	for (; (oi_mode || ACflg == OJ_AC|| ACflg == OJ_PE) && (dirp = readdir(dp)) != NULL;) {

		namelen = isInFile(dirp->d_name); // check if the file is *.in or not
		if (namelen == 0)
			continue;

		if(http_judge&&(!data_list_has(dirp->d_name)))
			continue;

		prepare_files(dirp->d_name, namelen, infile, p_id, work_dir, outfile,
				userfile, runner_id);
		init_syscalls_limits(lang);

		pid_t pidApp = fork();

		if (pidApp == 0) {

			run_solution(lang, work_dir, time_lmt, usedtime, mem_lmt);
		} else {

			num_of_test++;

			watch_solution(pidApp, infile, ACflg, isspj, userfile, outfile,
					solution_id, lang, topmemory, mem_lmt, usedtime, time_lmt,
					p_id, PEflg, work_dir);

			judge_solution(ACflg, usedtime, time_lmt, isspj, p_id, infile,
					outfile, userfile, PEflg, lang, work_dir, topmemory,
					mem_lmt, solution_id, num_of_test);
			if (use_max_time) {
				max_case_time =
						usedtime > max_case_time ? usedtime : max_case_time;
				usedtime = 0;
			}
			//clean_session(pidApp);
		}
		if (oi_mode) {
			if (ACflg == OJ_AC) {
				++pass_rate;
			}
			if (finalACflg < ACflg) {
				finalACflg = ACflg;
			}

			ACflg = OJ_AC;
		}
	}
	if (ACflg == OJ_AC && PEflg == OJ_PE)
		ACflg = OJ_PE;
	if (sim_enable && ACflg == OJ_AC && (!oi_mode || finalACflg == OJ_AC)
			&& lang < 5) { //bash don't supported
		sim = get_sim(solution_id, lang, p_id, sim_s_id);
	} else {
		sim = 0;
	}
	//if(ACflg == OJ_RE)addreinfo(solution_id);

	if ((oi_mode && finalACflg == OJ_RE) || ACflg == OJ_RE) {
		if (DEBUG)
			printf("add RE info of %d..... \n", solution_id);
		addreinfo(solution_id);
	}
	if (use_max_time) {
		usedtime = max_case_time;
	}
	if (ACflg == OJ_TL) {
		usedtime = time_lmt * 1000;
	}
	if (oi_mode) {
		if (num_of_test > 0)
			pass_rate /= num_of_test;
		update_solution(solution_id, finalACflg, usedtime, topmemory >> 10, sim,
				sim_s_id, pass_rate);
	} else {
		update_solution(solution_id, ACflg, usedtime, topmemory >> 10, sim,
				sim_s_id, 0);
	}
	if ((oi_mode && finalACflg == OJ_WA) || ACflg == OJ_WA) {
		if (DEBUG)
			printf("add diff info of %d..... \n", solution_id);
		if (!isspj)
			adddiffinfo(solution_id);
	}
	update_user(user_id);
	update_problem(p_id);
	clean_workdir(work_dir);

	if (DEBUG)
		write_log("result=%d", oi_mode ? finalACflg : ACflg);
	if (!http_judge)
		mysql_close(conn);
	if (record_call) {
		print_call_array();
	}
	closedir(dp);
	return 0;
}
