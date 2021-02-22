***************************************************************************
*                                  House cleaning
*************************************************************************** ; 
* Note: When using this file, beaware of the fact that most universities suscribe to annual updates of CRSP but daily updates of Compustat
    Thus, if Compustat has data and CRSP don't, the main observation indicator will change to the Compustat data;

proc delete data = _all_ ; run ; 

libname scratch "/scratch/cbs/tij"; 
libname project "~/Global Data";  

/* Get Project Macros and Helper Functions */
%include "~/helper_macros.sas";
%include "~/Global Data/project_macros.sas";

/* Choose Specific Characteristics */
proc import datafile = '~/Global Data/chars.xlsx'
	out  	=  chars_overview
	dbms  	=  xlsx
	replace;
 	sheet  	=  "chars";
 	range  	=  "A1:G154";
run;

proc sql noprint;
	select name_in_our
    into :chars_lvl1 separated by ' ' 
    from chars_overview
    where not missing(name_in_our);
quit;

%put We Have %nwords(&chars_lvl1.) level one chars;

/* All Characteristics */
proc sql noprint;
	select distinct lowcase(name) into :chars separated by ' '
	from dictionary.columns
	where lowcase(libname)='scratch' and lowcase(memname)='world_data'
		and name not in ('id', 'source', 'size_grp', 'obs_main', 'exch_main', 'primary_sec', 'gvkey', 'iid', 'permno', 'permco', 'excntry', 'curcd', 'fx',
		'sic', 'naics', 'gics', 'ff49',
		'common', 'comp_tpci', 'crsp_shrcd', 'comp_exchg', 'crsp_exchcd', 'crsp_sic', 'date', 'eom', 'adjfct', 'shares', 'me', 'me_lag',
		'me_company', 'prc_local', 'dolvol', 'ret', 'ret_local', 'ret_exc', 'ret_lag_dif'); /* Notice that I include prc as a characteristic*/
quit;

/* Check if any chars are missing */
%macro temp();
	%do i=1 %to %nwords(&chars_lvl1.);
		%let __c = %scan(&chars_lvl1., &i., %str(' '));
		%if %sysfunc(find(&chars., &__c.))=0 %then %do;
			%put ################ &__c. IS NOT IN THE DATASET!! ########################;
		%end;
	%end;
%mend;
%temp();

/* Capped Market Equity for Portfolio Weights */
proc sort data=scratch.world_data  out=world1; by id eom; run;

proc sql;
	create table world2 as
	select a.*, min(a.me, b.nyse_p80) as me_cap
	from world1 as a left join project.nyse_cutoff as b
	on a.eom = b.eom
	order by id, eom;
quit;

/* Winsorize at 0.1% */
%winsorize_own(inset=world2, outset=world3, sortvar=eom, vars=ret_exc ret me, perc_low=0.1, perc_high=99.9);

proc sort data=world3 out=world3; by id eom; run;

/* PORTFOLIO SORT ----------------------*/
* MACRO: prepare lagged data
	Prepare data and include screen;
%macro prepare_lags(data=,__char=, max_horizon=);
	* Start Empty Dataset for Tidy Format;
	data __pf1;
		format id $char20. eom YYMMDDN8. ret_exc 16.8 ret_lag_dif 8.0 lags 8.0 obs_main_l 1.0 exch_main_l 1.0 common_l 1.0 primary_sec_l 1.0 
		    comp_exchg_l 8.0 crsp_exchcd_l 8.0
			excntry_l $3. size_grp_l $char8. me_l 20.12 me_cap_l 20.12 signal_l 20.12;
		stop;  
	run;
	
	%do lags=1 %to &max_horizon.;
		data __signals; 
			set &data.(keep= source id eom ret_exc ret_lag_dif obs_main exch_main common primary_sec 
							 comp_exchg crsp_exchcd excntry size_grp me me_cap &__char.);
			by id;
			lags = &lags.;
			%let cols_lag = obs_main exch_main common primary_sec comp_exchg crsp_exchcd excntry size_grp me me_cap;
			%do i=1 %to %nwords(&cols_lag.); 
				%let col = %scan(&cols_lag., &i, %str(' '));
				&col._l = lag&lags.(&col.);
				if id ^= lag&lags.(id) or source ^= lag&lags.(source) or intck("month", lag&lags.(eom), eom)^=&lags. then
					&col._l = .;
			%end;
		    signal_l = lag&lags.(&__char);
			if id ^= lag&lags.(id) or source ^= lag&lags.(source) or intck("month", lag&lags.(eom), eom)^=&lags. then
					signal_l = .;
		    keep id eom ret_exc ret_lag_dif lags obs_main_l exch_main_l common_l primary_sec_l comp_exchg_l crsp_exchcd_l excntry_l size_grp_l me_l me_cap_l signal_l;
		run;
		proc append base=__pf1 data=__signals; run;
	%end;
	
	/* Screens */
	proc sql;
		create table __pf2 as
		select *
		from __pf1
		where obs_main_l = 1 and exch_main_l = 1 and common_l = 1 and primary_sec_l = 1 and ret_lag_dif = 1 and not missing(ret_exc) and not missing(me_l) and not missing(signal_l)
		order by excntry_l, size_grp_l, eom, lags;
	quit;
%mend;

* MACRO: cmp_func -------------
   This function takes a dataset, the name of a characteristic and the max lag between portfolio rebalancing
   and returns a characteristic managed portfolio for each rebalancing horizon between 1 and max_lag.
   The characteristic rank is used instead of the raw characteristic value;
%macro cmp_func(data=,__char=, max_horizon=);
	%let timer_start_cmp = %sysfunc(datetime());
	option nonotes;
	
	%prepare_lags(data=&data., __char = &__char., max_horizon = &max_horizon.); /* Prepare data, potentially with multiple lags in signal (Should be outside!)*/
	
	proc rank data=__pf2 out=__pf3 ties=mean;  /* Average in case of ties!! */
		by excntry_l size_grp_l eom lags;
		var signal_l;
		ranks char_rank;
	run;
	
	proc sql;
		create table __pf4 as 
		select *, char_rank /(count(char_rank) + 1) as p_rank
		from __pf3
		where not missing(signal_l)
		group by excntry_l, eom, size_grp_l, lags;
		
		create table __pf5 as 
		select *, (p_rank - mean(p_rank)) as p_rank_deviation
		from __pf4
		group by excntry_l, eom, size_grp_l, lags;
		
		create table __pf6 as 
		select *, p_rank_deviation / sum(abs(p_rank_deviation)) as weight  /* Rescale weights such that the units_invested is 1 in each portfolio*/
		from __pf5
		group by excntry_l, eom, size_grp_l, lags; 
		
		create table __cmp as 
		select "&__char." as characteristic, excntry_l as excntry, size_grp_l as size_grp, eom, lags,
			count(*) as n_stocks, 
			sum(weight*ret_exc) as ret_weighted,
			sum(signal_l*weight) as signal_weighted
		from __pf6
		where not missing(weight) 
		group by excntry, size_grp, eom, lags;
	quit;
	
	option notes;
	
	data _null_;
	  dur = datetime() - &timer_start_cmp;
	  put 30*'-' / ' TOTAL DURATION:' dur time13.2 / 30*'-';
	run; /* LAGS = 12: 16 min, LAGS=1: 2min */
%mend;	

* MACRO: hml_func -------------
   This function takes the name of a characteristic, the number of portfolios and the max lag between portfolio rebalancing
   and returns high minus low portfolios for each rebalancing horizon between 1 and max_lag
   Breakpoints can be based either on NYSE stocks (bps = nyse) or non-microcap i.e. only small, large and mega stocks (bps = non_micro);
%macro hml_func(data=,__char=, n_pf=, max_horizon=, bps=);
	%let timer_start_hml = %sysfunc(datetime());
	%let high_pf = %sysevalf(&n_pf. - 1); /* PROC Rank is zero based*/
	
	option nonotes;
	
	%prepare_lags(data=&data., __char = &__char., max_horizon = &max_horizon.);
	
	%if &bps. = 'nyse' %then %do;
		proc sql;
			create table nyse1 as
			select *
			from __pf2
			where crsp_exchcd_l=1 or comp_exchg_l=11
			order by lags, eom;
		quit;
		
		proc rank data=nyse1 groups=&n_pf. out=nyse2;  /* Average in case of ties!!*/
			by lags eom;
			var signal_l;
			ranks char_pf;
		run;	
		
		proc sql;
			create table nyse3 as 
			select lags, eom, char_pf, min(signal_l) as bp
			from nyse2
			group by lags, eom, char_pf;
		quit;
		
		proc transpose data=nyse3 out=nyse4(drop=_name_) prefix=bp;
		    by lags eom;
		    id char_pf;
		    var bp;
		run;
		
		proc sql;
			create table __pf3 as
			select a.*, b.*
			from __pf2 as a left join nyse4 as b
			on a.lags=b.lags and a.eom=b.eom;
		quit;
	%end;
	%if &bps. = 'non_micro' %then %do;
		proc sql;
			create table non_mc1 as
			select *
			from __pf2
			where size_grp_l in ('small', 'large', 'mega')
			order by eom, lags, excntry_l;
		quit;
		
		proc rank data=non_mc1 groups=&n_pf. out=non_mc2;
			by eom lags excntry_l;
			var signal_l;
			ranks char_pf;
		run;	
		
		proc sql;
			create table non_mc3 as 
			select excntry_l, lags, eom, char_pf, min(signal_l) as bp
			from non_mc2
			group by excntry_l, lags, eom, char_pf;
		quit;
		
		proc transpose data=non_mc3 out=non_mc4(drop=_name_) prefix=bp;
		    by excntry_l lags eom;
		    id char_pf;
		    var bp;
		run;
		
		proc sql;
			create table __pf3 as
			select a.*, b.*
			from __pf2 as a left join non_mc4 as b
			on a.excntry_l=b.excntry_l and a.lags=b.lags and a.eom=b.eom;
		quit;
	%end;
	
	data __pf4;
		set __pf3;
		if signal_l < bp1 then 
			char_pf = 0;
		if signal_l >= bp&high_pf. then 
			char_pf = &high_pf.;
		%do i=1 %to %sysevalf(&high_pf. - 1);
			%let j = %sysevalf(&i. + 1);
			if signal_l >= bp&i. and signal_l < bp&j. then
				char_pf = &i.;
		%end;
	run;
	
	proc sql;
		create table __pf5 as 
		select "&__char." as characteristic, excntry_l as excntry, eom, lags, char_pf,
			count(*) as n_stocks, 
			median(signal_l) as signal_median,
			mean(ret_exc) as ret_ew, 
			sum(ret_exc*me_l)/sum(me_l) as ret_vw,
			sum(ret_exc*me_cap_l)/sum(me_cap_l) as ret_vw_cap
		from __pf4
		where not missing(char_pf) 
		group by excntry, eom, lags, char_pf;
		
		create table __hml as 
		select characteristic, excntry, eom, lags, 
			sum(n_stocks) as n_stocks,
			min(n_stocks) as n_stocks_min,
			sum(ret_ew * (char_pf = &high_pf.)) - sum(ret_ew * (char_pf = 0)) as ret_ew,
			sum(ret_vw * (char_pf = &high_pf.)) - sum(ret_vw * (char_pf = 0)) as ret_vw,
			sum(ret_vw_cap * (char_pf = &high_pf.)) - sum(ret_vw_cap * (char_pf = 0)) as ret_vw_cap,
			sum(signal_median * (char_pf = &high_pf.)) - sum(signal_median * (char_pf = 0)) as signal
		from __pf5 
		where char_pf in (0, &high_pf.)
		group by characteristic, excntry, eom, lags
		having count(*)=2 and calculated signal ^= 0; /* Ensure that we have both a high and low portfolio*/
	quit;
	
	option notes;
	
	data _null_;
	  dur = datetime() - &timer_start_hml;
	  put 30*'-' / ' TOTAL DURATION:' dur time13.2 / 30*'-';
	run; /* LAGS = 12: 16 min, LAGS=1: 2min */
%mend;

* MACRO: block_func ------------------------------------------------------------------------------
   Combine the result of several characteristics to one data frame. This could be combined with
   block_apply, but I prefer to have the function separately;
%macro block_func(data=, char_vec=, start=, end=, n_pf=, max_horizon=, bps=);
	%put ################### Start block function  |  Start: &start.  |  End: &end.  #######################;
	
	* Start Empty Dataset to Append;
	data hml_pfs;
		format characteristic $char20. excntry $3. eom YYMMDDN8. lags 8.0 
			n_stocks 8.0 n_stocks_min 8.0 ret_ew 16.8 ret_vw 16.8 ret_vw_cap 16.8 signal 20.12;
		stop;  
	run;
	
	%do char_no=&start. %to &end.; 
		%let char = %scan(&char_vec., &char_no., %str(' '));
		%put Characteristic: &char. --> &char_no. out of %nwords(&char_vec.);
		*option nonotes;
		%hml_func(data=&data., __char=&char., n_pf = &n_pf., max_horizon = &max_horizon., bps = &bps.);
		*options notes;
		proc append base=hml_pfs data=__hml; run;
	%end;
%mend;

* MACRO: block_apply ------------------------------------------------------------------------------
   Apply Portfolio function to subset of variables and save this intermidiate step
   The benefit of this is that we avoid loosing work during if the server crashes for some reason;
%macro block_apply(data=,char_vec=, block_size=, first_char=, last_char=, n_pf=, max_horizon=, bps=);
	%let n_chars = %sysevalf(&last_char. - &first_char. + 1);
	%let blocks = %sysfunc(floor(&n_chars./&block_size.));
	%let mod = %sysfunc(mod(&n_chars., &block_size.));
	
	%if mod ^= 0 %then %do;
		%let blocks = %sysevalf(&blocks. + 1); 
	%end;
	
	%put Start Char: &first_char.  |  Last Char: &last_char.  |  Total Chars in Run: &n_chars.  |  Block Size: &block_size.  |  Blocks: &blocks.;
	
	%let start = &first_char.;
	%do b=1 %to &blocks.;
		%let end = %sysfunc(min(&start.+&block_size.-1, &last_char.));
		%block_func(data=&data., char_vec=&char_vec., start = &start., end = &end., n_pf = &n_pf., max_horizon = &max_horizon., bps = &bps.);
		%let data_name = ;
		data scratch.hml_sub_&start._&end.; 
			set hml_pfs;
		run;
		%let start = %sysevalf(&end.+1);
	%end;
%mend;

* MACRO: comb_files ------------------------------------------------------------------------------
   Since files are create individually, this macro combines the files for a final result
   Based on this macro https://documentation.sas.com/?docsetId=mcrolref&docsetTarget=n0js70lrkxo6uvn1fl4a5aafnlgt.htm&docsetVersion=9.4&locale=en;
%macro comb_files(dir, prefix, ext=sas7bdat);
	%local filrf rc did memcnt name i;
	%let rc = %sysfunc(filename(filrf,&dir));
	%let did = %sysfunc(dopen(&filrf));      
	
	%if &did eq 0 %then %do; 
	  %put Directory &dir cannot be open or does not exist;
	  %return;
	%end;
	
	%let l_p = %length(&prefix.); * Length Prefix;
	
	* Start Empty Dataset to Append;
	data hml;
		format characteristic $char20. excntry $3. eom YYMMDDN8. lags 8.0 
			n_stocks 8.0 n_stocks_min 8.0 ret_ew 16.8 ret_vw 16.8 ret_vw_cap 16.8 signal 20.12;
		stop;
	run;
	
	%do i = 1 %to %sysfunc(dnum(&did));
		%let name=%qsysfunc(dread(&did, &i));
	   	%if %qupcase(%qscan(&name, -1, .)) = %upcase(&ext) and %substr(&name., 1, &l_p.)=&prefix. %then %do;
	    	%put &dir./&name.;
	    	proc append base=hml data= "&dir./&name."; run;
	   	%end;
	%end;
	%let rc=%sysfunc(dclose(&did));
	%let rc=%sysfunc(filename(filrf));     
%mend comb_files;

* Create Portfolios HML Portfolios ----------------------------------------------------;
* Create data subset for computational efficiency;
data world_sub; 
	set world3;
	keep source id eom ret_exc ret_lag_dif 
		obs_main exch_main common primary_sec comp_exchg 
		crsp_exchcd excntry size_grp me me_cap 
		&chars_lvl1.;
run;

%let _timer_start = %sysfunc(datetime());
	%block_apply(data=world_sub, char_vec = &chars_lvl1., block_size = 10, first_char = 1, last_char = %nwords(&chars_lvl1.), n_pf=3, max_horizon=1, bps='non_micro');  /*last_char=%nwords(&chars_lvl1.)*/
data _null_;
  dur = datetime() - &_timer_start;
  put 30*'-' / ' TOTAL DURATION:' dur time13.2 / 30*'-';
run; 

* Combine and Save Data -----------------------------------------------;
%comb_files(dir=/scratch/cbs/tij, prefix=hml_sub_, ext=sas7bdat);

* Save as .csv;
proc export data=hml
    outfile="/scratch/cbs/tij/hml_pfs.csv"   
    dbms=CSV
    replace;
run;

* Zip file for easier download;
ods package (newzip) open nopf;
ods package (newzip) add file='/scratch/cbs/tij/hml_pfs.csv';
ods package (newzip) publish archive 
	properties (
		archive_name='hml_pfs.zip' 
		archive_path='/scratch/cbs/tij'
	);
ods package(newzip) close;
	
* Create CMP Portfolios --------------------------------------------------------------;
data us_data; set world3(where=(excntry = 'USA'));
%let _timer_start = %sysfunc(datetime());
	%let cmp_chars = &chars_lvl1.;
	%macro temp();
		* Start Empty Dataset to Append;
		data char_managed_pfs;
			format characteristic $char20. excntry $3. eom YYMMDDN8. size_grp $char8.  lags 8.0  
				n_stocks 8.0 ret_weighted 16.8 signal_weighted 20.12;
			stop;  
		run;
		
		* Append CMP Stats Iteratively;
		%do ci=1 %to %nwords(&cmp_chars.); 
			%let char = %scan(&cmp_chars., &ci, %str(' '));
			%put Characteristic: &char. --> &ci. out of %nwords(&cmp_chars.);
			%cmp_func(data=us_data,__char=&char., max_horizon = 1);
			proc append base=char_managed_pfs data=__cmp; run;
		%end;	
		
		data scratch.char_managed_pfs;
			set char_managed_pfs;
		run;
	%mend;
	%temp();

data _null_;
  dur = datetime() - &_timer_start;
  put 30*'-' / ' TOTAL DURATION:' dur time13.2 / 30*'-';
run;

* Save as .csv;
proc export data=scratch.char_managed_pfs
    outfile="/scratch/cbs/tij/cmp.csv"   
    dbms=CSV
    replace;
run;

* Zip file for easier download;
ods package (newzip) open nopf;
ods package (newzip) add file='/scratch/cbs/tij/cmp.csv';
ods package (newzip) publish archive 
	properties (
		archive_name='cmp.zip' 
		archive_path='/scratch/cbs/tij'
	);
ods package(newzip) close;	