* Market Chars: Monthly;
%let monthly_chars=
	/* Market Based Size Variables */
	market_equity
	
	/* Total Dividend Paid to Market Equity */
	div1m_me div3m_me div6m_me div12m_me
	
	/* Special Dividend Paid to Market Equity */
	divspc1m_me divspc12m_me
	
	/* Change in Shares Outstanding */
	chcsho_1m chcsho_3m chcsho_6m chcsho_12m
	
	/* Net Equity Payout */
	eqnpo_1m eqnpo_3m eqnpo_6m eqnpo_12m
	
	/* Momentum/Reversal */
	ret_1_0 ret_2_0 ret_3_0 ret_3_1 ret_6_0 ret_6_1 ret_9_0 ret_9_1 ret_12_0 ret_12_1 ret_12_7 ret_18_1 ret_24_1 ret_24_12
	ret_36_1 ret_36_12 ret_48_1 ret_48_12 ret_60_1 ret_60_12 ret_60_36
	
	/* Seasonality */
	seas_1_1an seas_2_5an seas_6_10an seas_11_15an seas_16_20an
	seas_1_1na seas_2_5na seas_6_10na seas_11_15na seas_16_20na
	
;

%put ### In total %nwords(&monthly_chars.) monthly characteristics will be created ###;
%macro market_chars_monthly(out=, data=, market_ret=, local_currency=);	
	%if &local_currency=1 %then %do;
		%let ret_var = ret_local;
	%end;
	%if &local_currency=0 %then %do;
		%let ret_var = ret;
	%end;
	
	/* Helper macro: Apply function lag0 to lag &n. */
	%macro apply_to_lastn(x=, _n=, func=);
		%let mv = &func.(&x.; 
		%do _i=1 %to &_n.-1;
			%let mv = &mv., lag&_i.(&x.);
		%end;
		%let mv = &mv.);
		&mv.;
	%mend apply_to_lastn;
	
	/* Get Important Variables */
	proc sql; 
		create table __monthly_chars1 as
		select a.id, a.date, a.eom, a.me, a.shares, a.adjfct, 
			a.prc, a.ret, a.ret_local, a.&ret_var. as ret_x,  
			a.div_tot, a.div_cash, a.div_spc, a.dolvol,
			a.ret_lag_dif, (a.ret_local = 0) as ret_zero, 
			a.ret_exc, b.mkt_vw_exc									/* Currently, Excess return is in USD because we lack RF for most markets */		
		from &data. as a left join &market_ret. as b
		on a.excntry=b.excntry and a.eom=b.eom
		order by a.id, a.eom;
	quit;
	
	* Ensure that there is a lag of 1 month between each obs;
	proc sql;
		create table __stock_coverage as 
		select id, min(eom) as start_date, max(eom) as end_date
		from __monthly_chars1
		group by id;
	quit;
	
	%expand(data=__stock_coverage, out=__full_range, id_vars=id, start_date=start_date, end_date=end_date, freq='month', new_date_name=eom);
	
	proc sql;
		create table __monthly_chars2 as
		select a.id, a.eom, missing(b.id) as obs_miss,
			b.me, b.shares, b.adjfct, b.prc, b.ret, b.ret_local, b.ret_x, b.ret_lag_dif,
			b.div_tot, b.div_cash, b.div_spc, b.dolvol, b.ret_zero, b.ret_exc, b.mkt_vw_exc
		from __full_range as a left join __monthly_chars1 as b
		on a.id=b.id and a.eom=b.eom
		order by id, eom;
	quit;
	
	* Cummulative Return Index;
	data __monthly_chars3;
		set __monthly_chars2;
		by id;
		retain ri_x;  /* Local or USD depending on &local_currency.*/
		retain ri;	  /* USD */
		retain count;
		if first.id then do;
			ri_x = sum(1, ret_x); /* Most will have missing return for the first observation. In that case this evaluates to 1*/
			ri = sum(1, ret);
			count = 1;
		end;
		else do;
			ri_x = ri_x*sum(1, ret_x); /* By using sum instead of 1+ret missing returns are set to 0 */
			ri = ri*sum(1, ret);
			count = count+1;
		end;
	run;
	
	/* Set non-standard returns to missing */
	data __monthly_chars4; 
		set __monthly_chars3;
		ret_miss = missing(ret_x) or ret_zero=1 or ret_lag_dif^=1; /* Important Screen, see [1]*/
		if ret_miss = 1 then do;
			ret_x = .;
			ret=.;
			ret_local=.;
			ret_exc =.;
			mkt_vw_exc = .;
		end;
		drop obs_miss ret_zero ret_lag_dif;
	run; 
	
	/* Create variables */
	proc sort nodup data=__monthly_chars4; by id eom; run;
	data __monthly_chars5;
		set __monthly_chars4; 
		by id eom;
		/* Market Equity */
		market_equity = me;
		
		/* Dividend to Price */
		%let div_range = 1 3 6 12; /* 24, 36*/
		%do i=1 %to %sysfunc(countw(&div_range.));  
			%let n = %scan(&div_range., &i.);
			div_sum = %apply_to_lastn(x=div_tot*shares, _n=&n., func=sum);
			div&n.m_me = div_sum/me; 
			if count < &n. then 
				div&n.m_me = .;
			drop div_sum;
		%end;
		
		/* Special Dividends */
		%let div_spc_range = 1 12;
		%do i=1 %to %sysfunc(countw(&div_spc_range.));  
			%let n = %scan(&div_spc_range., &i.);
			div_spc_sum = %apply_to_lastn(x=div_spc*shares, _n=&n., func=sum);
			divspc&n.m_me = div_spc_sum/me; 
			if count < &n. then 
				divspc&n.m_me = .;
			drop div_spc_sum;
		%end;
		
		/* Change in Shares Outstanding (Market Based Proxy for Net Share Issuance)*/
		%let chcsho_lags = 1 3 6 12;
		%do i=1 %to %sysfunc(countw(&chcsho_lags.));  
			%let chcsho_lag = %scan(&chcsho_lags.,&i.);
			chcsho_&chcsho_lag.m = (shares*adjfct)/lag&chcsho_lag.(shares*adjfct)-1;
			if count <= &chcsho_lag. then 
				chcsho_&chcsho_lag.m=.; 
		%end;
		
		/* Net Equity Payout (Market based stock buyback+dividend-stock issuance)*/
		%let eqnpo_lags = 1 3 6 12;
		%do i=1 %to %sysfunc(countw(&eqnpo_lags.));  
			%let eqnpo_lag = %scan(&eqnpo_lags.,&i.);
			eqnpo_&eqnpo_lag.m = log(ri/lag&eqnpo_lag.(ri))-log(me/lag&eqnpo_lag.(me));
			if count <= &eqnpo_lag. then 
				eqnpo_&eqnpo_lag.m=.; 
		%end;
		
		
		/* Momentum/Reversal */
		%let from_lags = 1 2 3 3 6 6 9 9 12 12 12 18 24 24 36 36 48 48 60 60 60;
		%let to_lags   = 0 0 0 1 0 1 0 1 0  1  7  1  1 12  1 12 12  1  1 12 36;
		%do j=1 %to %sysfunc(countw(&from_lags.));  
			%let from = %scan(&from_lags., &j.);
			%let to = %scan(&to_lags., &j.);
			ret_&from._&to. = lag&to.(ri_x)/lag&from.(ri_x)-1;
			if count <= &from. or missing(lag&from.(ret_x)) or missing(lag&to.(ret_x)) then /* For explanation of missing requirements, see [2] */
				ret_&from._&to.=.; 
		%end;
		
		/* Seasonality: Heston and Sadka (2008) */
		%seasonality(start_year=1, end_year=1);
		%seasonality(start_year=2, end_year=5);
		%seasonality(start_year=6, end_year=10);
		%seasonality(start_year=11, end_year=15);
		%seasonality(start_year=16, end_year=20);
		
		/* Drop Uneccesary Variables */
		drop me shares adjfct shares adjfct prc ret ret_local ret_x
			div_tot div_cash div_spc dolvol ret_exc mkt_vw_exc ret_miss ri_x ri count;
	run;	
	
	proc sort data=__monthly_chars5 out=&out.; by id eom; run;
%mend;

/* Calculate CAPM beta over a rolling window */
%macro market_beta(out=, data=, fcts=, __n =, __min=); 
	proc sql;
		create table __msf1 as 
		select a.id, a.eom, a.ret_exc, a.ret_lag_dif, b.mktrf
		from &data. as a left join &fcts. as b
		on a.excntry=b.excntry and a.eom=b.eom
		where a.ret_local^=0 and not missing(a.ret_exc) and a.ret_lag_dif=1 and not missing(b.mktrf);
	quit;	
	%winsorize_own(inset=__msf1, outset=__msf2, sortvar=eom, vars=ret_exc, perc_low=0.1, perc_high=99.9); /* Winsorize returns at 0.1% and 99.9% */
	proc sort data=__msf2; by id eom; run;
	
	proc sql;
		create table month_ends as 
		select distinct eom
		from __msf2
		order by eom;
	quit;
	
	* Divide data into __n groups;
	proc sql;
		create table dates_apply as 
		select *, mod(monotonic(), &__n.) as grp
		from month_ends;
	quit;
	
	* Helper macro: If first group, save &new. as &base. otherwise, append &new. to &base.;
	%macro save_or_append(base=, new=);
		%if &__grp. = 0 %then %do;
			data &base.; set &new.; run;
		%end;
		%else %do;
			proc append base=&base. data=&new.; run;
		%end;
	%mend;
	
	%do __grp=0 %to %eval(&__n. - 1);
		%put ############### GROUP %eval(&__grp.+1) out of &__n. ###############; 
		* Prepare data;
		proc sql;
			create table calc_dates as
			select a.eom, b.eom as calc_date
			from dates_apply as a left join dates_apply(where=(grp = &__grp.)) as b
			on a.eom > intnx("month", b.eom, -&__n., "e") and a.eom <= b.eom;
		quit;
		
		proc sql;
			create table calc_data as 
			select a.*, b.calc_date
			from __msf2 as a left join calc_dates as b
			on a.eom = b.eom
			where not missing(b.calc_date)  
			group by a.id, b.calc_date
			having count(*) >= &__min.
			order by a.id, b.calc_date;
		quit;
		
		proc reg data=calc_data outest=__capm1 edf NOPRINT;
			by id calc_date;
			model ret_exc=mktrf;
		run;
		proc sql;
			create table __capm2 as 
			select id, calc_date as eom, mktrf as beta_&__n.m, sqrt(_rmse_**2 * _edf_ / (_edf_ + 1)) as ivol_capm_&__n.m
			from __capm1
			where (_edf_ + 2) >= &__min.;
		quit;
		%save_or_append(base=op_capm, new=__capm2);
	%end;
	proc sort data=op_capm out=&out. nodup; by id eom; run; 
	proc delete data=op_capm; run;
%mend;

/* MACRO: RESIDUAL MOMENTUM --------------
- Rolling regressions over &__n. months. used to calculate residual momentum. 
- Currently I have only implemented FF3 but I could easily extend it to CAPM and HXZ4
- Residual momentum is typically calculated over a shorter horizon than that used to estimate parameters.
  The number of months to include return data from, is indicated by &incl. The number of months to skip within that period,
  is indicated by &skip. Both incl and skip can be list, but they need to be of the same length,
*/
%macro residual_momentum(out=, data=, fcts=, type=, __n =, __min=, incl=, skip=); /* first_lag and last_lag can be a list but must have equal length. Type in (market, ff3, hxz4) */
	proc sql;
		create table __msf1 as 
		select a.id, a.eom, a.ret_exc, a.ret_lag_dif, b.mktrf, b.hml, b.smb_ff, b.roe, b.inv, b.smb_hxz
		from &data. as a left join &fcts. as b
		on a.excntry=b.excntry and a.eom=b.eom
		where a.ret_local^=0 and not missing(a.ret_exc) and not missing(b.mktrf) and ret_lag_dif=1; 
	quit;	
	%winsorize_own(inset=__msf1, outset=__msf2, sortvar=eom, vars=ret_exc, perc_low=0.1, perc_high=99.9); /* Winsorize returns at 0.1% and 99.9% */
	proc sort data=__msf2; by id eom; run;
	
	proc sql;
		create table month_ends as 
		select distinct eom
		from __msf2
		order by eom;
	quit;
	
	* Divide data into __n groups;
	proc sql;
		create table dates_apply as 
		select *, mod(monotonic(), &__n.) as grp
		from month_ends;
	quit;
	
	* Helper macro: If first group, save &new. as &base. otherwise, append &new. to &base.;
	%macro save_or_append(base=, new=);
		%if &__grp. = 0 %then %do;
			data &base.; set &new.; run;
		%end;
		%else %do;
			proc append base=&base. data=&new.; run;
		%end;
	%mend;
	
	%do __grp=0 %to %eval(&__n. - 1);
		%put ############### GROUP %eval(&__grp.+1) out of &__n. ###############; 
		* Prepare data;
		proc sql;
			create table calc_dates as
			select a.eom, b.eom as calc_date
			from dates_apply as a left join dates_apply(where=(grp = &__grp.)) as b
			on a.eom > intnx("month", b.eom, -&__n., "e") and a.eom <= b.eom;
		quit;
		
		proc sql;
			create table calc_data as 
			select a.*, b.calc_date
			from __msf2 as a left join calc_dates as b
			on a.eom = b.eom
			where not missing(b.calc_date)  
			group by a.id, b.calc_date
			having count(*) >= &__min. 
			order by a.id, b.calc_date;
		quit;	
		
		* Fama and French (1993) 3 factor model;
		%if %sysfunc(find(&type., ff3)) >= 1 %then %do;
			proc reg data=calc_data(where=(not missing(hml) and not missing(smb_ff))) NOPRINT;
				by id calc_date;
				model ret_exc=mktrf smb_ff hml;
				output out=__ff3_res1
					   residual=res;  
			run;
			%do __i=1 %to %nwords(&incl.);
				%let __in = %scan(&incl., &__i., %str(' '));
				%let __sk = %scan(&skip., &__i., %str(' '));
				proc sql;
					create table __ff3_res2 as 
					select *, (eom > intnx("month", calc_date, -&__in., "e") and eom <= intnx("month", calc_date, -&__sk., "e")) as incl  /* Incl=1 --> obs is included in momentum calculation */ 
					from __ff3_res1 
					group by id, calc_date
					having count(res) >= &__min. 
					order by id, calc_date, eom;
	 			
	 				create table __ff3_res3 as 
	 				select id, calc_date as eom, mean(res) / std(res) as resff3_&__in._&__sk.
	 				from __ff3_res2
	 				where incl = 1
	 				group by id, calc_date;
	 			quit;
	 			
				%save_or_append(base=op_&__in._&__sk., new=__ff3_res3);
			%end;
		%end;
	%end;
	/* Output */
	%do __i=1 %to %nwords(&incl.);
		%let __in = %scan(&incl., &__i., %str(' '));
		%let __sk = %scan(&skip., &__i., %str(' '));
		proc sort data=op_&__in._&__sk. out=&out._&__in._&__sk. nodup; by id eom; run; 
		proc delete data=op_&__in._&__sk.; run; 
	%end;
%mend;





	
	
	
	

	

/* MACRO FOR DAILY CHARS ----------------------------------------------

*/
%macro prepare_daily(data=, fcts=);
	/* Start timer */
	%let __prep_start = %sysfunc(datetime());
	/* Prepare stock level data */
	proc sql;
		create table dsf1 as 
		select a.excntry, a.id, a.date, a.eom, a.prc / a.adjfct as prc_adj, a.ret, a.ret_exc, a.dolvol as dolvol_d, a.shares, a.tvol,
			   b.mktrf, b.hml, b.smb_ff, b.roe, b.inv, b.smb_hxz,
			   a.ret_lag_dif, a.bidask,
			   sum(a.ret_local = 0) as zero_obs  /* Some firms have almost inclusively zero returns. These should be excluded */
		from &data. as a left join &fcts. as b
		on a.excntry = b.excntry and a.date = b.date
		where not missing(b.mktrf) /* not missing mktrf ensures that we look at trading days*/
		group by a.id, a.eom; 
	
		update dsf1
		set ret_exc = .,
		    ret = .
		where ret_lag_dif > 14;  /* Only used returns based on prices that are not more than two weeks old */
		
		alter table dsf1
		drop ret_lag_dif, bidask;
	quit;
	
	%winsorize_own(inset=dsf1, outset=dsf2, sortvar=eom, vars=ret_exc ret, perc_low=0.1, perc_high=99.9); /* Winsorize returns at 0.1% and 99.9% */
	%winsorize_own(inset=dsf2, outset=dsf3, sortvar=eom, vars=tvol dolvol_d, perc_low=0, perc_high=99.9); /* Winsorize volume at at 99.9% */
	proc sort data=dsf3; by id date; run;
	
	* Create lead/lagged market returns (For dimson beta);
	proc sql;
		create table mkt_lead_lag1 as 
		select excntry, date, intnx('month',date,0,'E') as eom format=YYMMDDN8., mktrf
		from &fcts.
		order by excntry, date desc;
	quit;
	
	data mkt_lead_lag2;
		set mkt_lead_lag1;
		mktrf_ld1 = lag(mktrf);
		if excntry ^= lag(excntry) or eom ^= lag(eom) then mktrf_ld1 = .;  /* Eom condition is to avoid look-ahead bias */
	run;
	
	proc sort data=mkt_lead_lag2 out=mkt_lead_lag3; by excntry date; run;
	
	data mkt_lead_lag4;
		set mkt_lead_lag3;
		mktrf_lg1 = lag(mktrf);
		if excntry ^= lag(excntry) then mktrf_lg1 = .;
	run;
	
	* Overlapping returns used to calculate correlation;
	data corr_data;
		set dsf3;
		ret_exc_3l = ret_exc + lag(ret_exc) + lag2(ret_exc);
		mkt_exc_3l = mktrf + lag(mktrf) + lag2(mktrf);
		if id ^= lag2(id) then do;
			ret_exc_3l = .;
			mkt_exc_3l = .;
		end;
		keep id eom zero_obs ret_exc_3l mkt_exc_3l;
	run;
	
	*Unique Month Ends;
	proc sql;
		create table month_ends as 
		select distinct eom
		from dsf3
		order by eom;
	quit;
	
	/* Stop timer */
	data _null_;
		dur = datetime() - &__prep_start;
		put 30*'-' / ' PREPARING DAILY DATA TOOK:' dur time13.2 / 30*'-';
	run;
%mend;

* MACRO: ROLL APPLY DAILY -------------------------
- Apply &__stats. functions to rolling windows of data.
- The idea is to iteratively apply the functions to &__n. different splits of the data.
  The output of each function is a stock id-eom pair plus the calculated characteristics
- The currently implemented &__stats. are:
	* rvol, rmax, skew, capm, capm_ext, ff3, hxz4, dimsonbeta, downbeta, zero_trades, turnover, dolvol, ami, prc_to_high, mktcorr, mktvol
- Arguments:
	* OUT: Output dataset in a long format with all the requested characteristics
	* ...;
%macro roll_apply_daily(out=, __n=, __min=, fcts=, __month_ends=, sfx =,__stats=);  /* Create stats over rolling __n months. stats in (rvol, rmax, skew, capm, capm_ext, ff3, hxz4, dimsonbeta, downbeta, zero_trades, turnover, dolvol, ami, prc_to_high, mktcorr, mktvol) */
	/* Start timer */
	%let __roll_start = %sysfunc(datetime());
	* Divide data into __n groups;
	proc sql;
		create table dates_apply as 
		select *, mod(monotonic(), &__n.) as grp
		from &__month_ends.;
	quit;
	* Helper: If first group, save &new. as &base. otherwise, append &new. to &base.;
	%macro save_or_append(base=, new=);
		%if &__grp. = 0 %then %do;
			data &base.; set &new.; run;
		%end;
		%else %do;
			proc append base=&base. data=&new.; run;
		%end;
	%mend;
	
	* Drop unneccesary columns for faster join;
	data __input; set dsf3; run;
	%if %sysfunc(find(&__stats., ff3)) = 0 %then %do;
		proc sql;
			alter table __input
			drop hml, smb_ff;
		quit;
	%end;
	%if %sysfunc(find(&__stats., hxz4)) = 0 %then %do;
		proc sql;
			alter table __input
			drop roe, inv, smb_hxz;
		quit;
	%end;
	%if %sysfunc(find(&__stats., turnover)) = 0 and %sysfunc(find(&__stats., ami)) = 0 and %sysfunc(find(&__stats., zero_trades)) = 0 and %sysfunc(find(&__stats., dolvol)) = 0%then %do;
		proc sql;
			alter table __input
			drop dolvol_d, shares, tvol;
		quit;
	%end;
	%if %sysfunc(find(&__stats., prc_to_high)) = 0 %then %do;
		proc sql;
			alter table __input
			drop prc_adj;
		quit;
	%end;
	
	* Apply __stats to each group;
	%do __grp=0 %to %eval(&__n. - 1); 
		* Prepare data;
		proc sql;
			create table calc_dates as
			select a.eom, b.eom as calc_date
			from dates_apply as a left join dates_apply(where=(grp = &__grp.)) as b
			on a.eom > intnx("month", b.eom, -&__n., "e") and a.eom <= b.eom;
		quit;
		
		* Not neccesary if mktcorr is the only stat;
		%if %nwords(&__stats.)>1 or %sysfunc(find(&__stats., mktcorr))=0 %then %do;
			proc sql;
				/* Used for volume variables */
				create table calc_data_raw as 
				select a.*, b.calc_date
				from __input as a left join calc_dates as b
				on a.eom = b.eom
				where not missing(b.calc_date)  
				order by a.id, b.calc_date;
	
				/* Used for return variables */
				create table calc_data_screen as 
				select *
				from calc_data_raw
				where not missing(ret_exc) and zero_obs < 10  /* We exclude stock-months with 10 or more zero returns */
				group by id, calc_date
				having count(ret_exc) >= &__min.;
			quit;	
		%end;
		
		* Return Volatility;
		%if %sysfunc(find(&__stats., rvol)) >= 1 %then %do;
			proc sql;
				create table __rvol as 
				select id, calc_date as eom, std(ret_exc) as rvol&sfx.
				from calc_data_screen
				group by id, calc_date
				having count(ret_exc) >= &__min.;
			quit;
			%save_or_append(base=op_rvol, new=__rvol);
		%end;
		
		* Maximum Return;
		%if %sysfunc(find(&__stats., rmax)) >= 1 %then %do;
			proc rank data= calc_data_screen out = __rmax1 descending;
				by id calc_date;
				var ret;
				ranks ret_rank;
			run;
			proc sql;
				create table __rmax2 as 
				select id, calc_date as eom, mean(ret) as rmax5&sfx., max(ret) as rmax1&sfx.
				from __rmax1
				where ret_rank<=5
				group by id, calc_date;
			quit;
			%save_or_append(base=op_rmax, new=__rmax2);
		%end;
		
		* Return Skewness;
		%if %sysfunc(find(&__stats., skew)) >= 1 %then %do;
			proc means data=calc_data_screen skewness noprint;
				by id calc_date;
				var ret_exc;
				output out = __skew1
					   skewness = rskew&sfx.;
			run;
			proc sql;
				create table __skew2 as
				select id, calc_date as eom, rskew&sfx.
				from __skew1
				where _freq_ >= &__min.;
			quit;
			%save_or_append(base=op_skew, new=__skew2);
		%end;
		
		* Price-to-high;
		%if %sysfunc(find(&__stats., prc_to_high)) >= 1 %then %do;
			proc sql;
				create table __prc_high as 
				select id, calc_date as eom, prc_adj / max(prc_adj) as prc_highprc&sfx.
				from calc_data_screen
				group by id, calc_date
				having date = max(date) and count(prc_adj) >= &__min.;
			quit;
			%save_or_append(base=op_prc_high, new=__prc_high);
		%end;
		
		* Amihud (2002);
		%if %sysfunc(find(&__stats., ami)) >= 1 %then %do;
			proc sql;
				create table __ami as 
				select id, calc_date as eom, mean(abs(ret) / dolvol_d) *1e6 as ami&sfx.
				from calc_data_screen
				group by id, calc_date
				having count(dolvol_d) >= &__min.;
			quit;
			%save_or_append(base=op_ami, new=__ami);
		%end;
		
		* CAPM regression (beta + ivol);
		%if %sysfunc(find(&__stats., capm)) >= 1 and %sysfunc(find(&__stats., capm_ext)) = 0 %then %do;
			proc reg data=calc_data_screen outest=__capm1 edf NOPRINT;
				by id calc_date;
				model ret_exc=mktrf;
			run;
			proc sql;
				create table __capm2 as 
				select id, calc_date as eom, mktrf as beta&sfx., sqrt(_rmse_**2 * _edf_ / (_edf_ + 1)) as ivol_capm&sfx.
				from __capm1
				where (_edf_ + 2) >= &__min.;
			quit;
			%save_or_append(base=op_capm, new=__capm2);
		%end;
		
		* CAPM regression extended (beta + ivol + iskew + coskewness);
		%if %sysfunc(find(&__stats., capm_ext)) >= 1 %then %do;
			proc reg data=calc_data_screen outest=__capm_ext1 edf NOPRINT;
				by id calc_date;
				model ret_exc=mktrf;
				output out=__capm_ext_res
					   residual=res;  /* Including the output statement increases the time by a factor of 3. It's neccesary to compute skewness */
			run;
			
			proc sql;
				create table __capm_ext2 as 
				select id, calc_date as eom, mktrf as beta&sfx., sqrt(_rmse_**2 * _edf_ / (_edf_ + 1)) as ivol_capm&sfx.
				from __capm_ext1
				where (_edf_ + 2) >= &__min.;
			quit;
			
			* Idiosyncratic skewness;
			proc means data=__capm_ext_res skewness noprint;
				by id calc_date;
				var res;
				output out = __capm_ext_skew(where=(_freq_ >= &__min.))
					   skewness = iskew_capm&sfx.;
			run;
			
			* Coskewness;
			proc sql;
				create table __capm_ext_coskew1 as 
				select id, calc_date, res, mktrf - mean(mktrf) as mktrf_dm
				from __capm_ext_res
				group by id, calc_date;
				
				create table  __capm_ext_coskew2 as 
				select id, calc_date, mean(res * mktrf_dm**2) / (sqrt(mean(res**2)) * mean(mktrf_dm**2) ) as coskew&sfx.
				from __capm_ext_coskew1
				group by id, calc_date
				having count(res) >= &__min.;
			quit;
			
			proc sql;
				create table __capm_ext3 as 
				select a.*, b.iskew_capm&sfx., c.coskew&sfx.
				from __capm_ext2 as a
				left join __capm_ext_skew as b on a.id=b.id and a.eom=b.calc_date
				left join __capm_ext_coskew2 as c on a.id=c.id and a.eom=c.calc_date;
			quit;
			
			%save_or_append(base=op_capm_ext, new=__capm_ext3);
		%end;
		
		* Fama and French (1993) 3 factor model;
		%if %sysfunc(find(&__stats., ff3)) >= 1 %then %do;
			proc reg data=calc_data_screen(where=(not missing(hml) and not missing(smb_ff))) outest=__ff31 edf NOPRINT;
				by id calc_date;
				model ret_exc=mktrf smb_ff hml;
				output out=__ff3_res
					   residual=res;  /* Including the output statement increases the time by a factor of 3. It's neccesary to compute skewness */
			run;
			
			proc sql;
				create table __ff32 as 
				select id, calc_date as eom, sqrt(_rmse_**2 * _edf_ / (_edf_ + 1)) as ivol_ff3&sfx.
				from __ff31
				where (_edf_ + 2) >= &__min.;
			quit;
			
			* Idiosyncratic skewness;
			proc means data=__ff3_res skewness noprint;
				by id calc_date;
				var res;
				output out = __ff3_skew(where=(_freq_ >= &__min.))
					   skewness = iskew_ff3&sfx.;
			run;
			
			proc sql;
				create table __ff33 as 
				select a.*, b.iskew_ff3&sfx.
				from __ff32 as a
				left join __ff3_skew as b on a.id=b.id and a.eom=b.calc_date;
			quit;
			%save_or_append(base=op_ff3, new=__ff33);
		%end;
		
		* Hou, Xue and Zhang (2015) 4 factor model;
		%if %sysfunc(find(&__stats., hxz4)) >= 1 %then %do;
			proc reg data=calc_data_screen(where=(not missing(roe) and not missing(inv) and not missing(smb_hxz))) outest=__hxz41 edf NOPRINT;
				by id calc_date;
				model ret_exc=mktrf smb_hxz roe inv;
				output out=__hxz4_res
					   residual=res;  /* Including the output statement increases the time by a factor of 3. It's neccesary to compute skewness */
			run;
			
			proc sql;
				create table __hxz42 as 
				select id, calc_date as eom, sqrt(_rmse_**2 * _edf_ / (_edf_ + 1)) as ivol_hxz4&sfx.
				from __hxz41
				where (_edf_ + 2) >= &__min.;
			quit;
			
			* Idiosyncratic skewness;
			proc means data=__hxz4_res skewness noprint;
				by id calc_date;
				var res;
				output out = __hxz4_skew(where=(_freq_ >= &__min.))
					   skewness = iskew_hxz4&sfx.;
			run;
			
			proc sql;
				create table __hxz43 as 
				select a.*, b.iskew_hxz4&sfx.
				from __hxz42 as a
				left join __hxz4_skew as b on a.id=b.id and a.eom=b.calc_date;
			quit;
			%save_or_append(base=op_hxz4, new=__hxz43);
		%end;
		
		* Dimson beta;
		%if %sysfunc(find(&__stats., dimsonbeta)) >= 1 %then %do;
			proc sql;
				create table __dimson1 as 
				select a.excntry, a.id, a.date, a.eom, a.ret_exc, a.mktrf, b.mktrf_lg1, b.mktrf_ld1
				from calc_data_screen as a left join mkt_lead_lag4 as b
				on a.excntry = b.excntry and a.date = b.date
				where not missing(b.mktrf_lg1) and not missing(b.mktrf_ld1);
				
				create table __dimson2 as 
				select *
				from __dimson1
				group by id, eom
				having count(*) >= &__min.; 
			quit;
			
			proc reg data=__dimson2 outest=__dimson3 edf NOPRINT;
				by id eom;
				model ret_exc=mktrf mktrf_lg1 mktrf_ld1;
			run;
			
			data __dimson4;
				set __dimson3;
				beta_dimson&sfx. = mktrf + mktrf_lg1 + mktrf_ld1;	
				keep id eom beta_dimson&sfx.;
			run;
			%save_or_append(base=op_dimson, new=__dimson4);
		%end;
		
		* Downside beta;
		%if %sysfunc(find(&__stats., downbeta)) >= 1 %then %do;
			proc reg data=calc_data_screen(where=(mktrf < 0)) outest=__downbeta1 edf NOPRINT;
				by id calc_date;
				model ret_exc=mktrf;
			run;
			proc sql;
				create table __downbeta2 as 
				select id, calc_date as eom, mktrf as betadown&sfx.
				from __downbeta1
				where (_edf_ + 2) >= (&__min. / 2);  /* Use convention that we require half as many obs for downside beta */
			quit;
			%save_or_append(base=op_downbeta, new=__downbeta2);
		%end;
		
		* Number of zero trades with turnover as tiebreaker;
		%if %sysfunc(find(&__stats., zero_trades)) >= 1 %then %do;
			proc sql;
				create table __zero_trades1 as
				select id, calc_date as eom, mean(tvol=0) * 21 as zero_trades, mean(tvol / (shares * 1e6)) as turnover
				from calc_data_raw
				where not missing(tvol)
				group by id, calc_date
				having count(tvol) >= &__min.
				order by eom;
			quit;
			proc rank data=__zero_trades1(where=(not missing(zero_trades) and not missing(turnover))) out = __zero_trades2 descending ties=mean f; /* f means that we use fractional ranks i.e. between 0 and 1*/ 
				by eom;
				var turnover;
				ranks rank_turnover;
			run;
			proc sql;
				create table __zero_trades3 as
				select id, eom, zero_trades + rank_turnover / 100  as zero_trades&sfx. /* Divide by 100 to ensure that turnover only acts as a tie breaker (1/365*21=0.0833)*/
				from __zero_trades2;
			quit;
			%save_or_append(base=op_zero_trades, new=__zero_trades3);
		%end; 
				
		* Turnover;
		%if %sysfunc(find(&__stats., turnover)) >= 1 %then %do;
			proc sql;
				create table __turnover1 as 
				select id, date, calc_date, tvol / (shares * 1e6) as turnover_d  
				from calc_data_raw;
				
				create table __turnover2 as 
				select id, calc_date as eom, mean(turnover_d) as turnover&sfx., std(turnover_d) / (calculated turnover&sfx.) as turnover_var&sfx. 
				from __turnover1
				group by id, calc_date
				having count(turnover_d) >= &__min.;
			quit;
			%save_or_append(base=op_turnover, new=__turnover2);
		%end;
		
		* Dollar Volume;
		%if %sysfunc(find(&__stats., dolvol)) >= 1 %then %do;
			proc sql;
				create table __dolvol as 
				select id, calc_date as eom, mean(dolvol_d) as dolvol&sfx., std(dolvol_d) / (calculated dolvol&sfx.) as dolvol_var&sfx.
				from calc_data_raw
				group by id, calc_date
				having count(dolvol_d) >= &__min.;
			quit;
			%save_or_append(base=op_dolvol, new=__dolvol);
		%end;
		
		* Correlation to Market;
		%if %sysfunc(find(&__stats., mktcorr)) >= 1 %then %do;
			proc sql;
				create table __corr_data1 as 
				select a.*, b.calc_date
				from corr_data as a left join calc_dates as b
				on a.eom = b.eom
				where not missing(b.calc_date) and not missing(ret_exc_3l) and zero_obs < 10
				order by a.id, b.calc_date;
				
				create table __corr_data2 as 
				select *
				from __corr_data1
				group by id, calc_date
				having count(ret_exc_3l) >= &__min. and count(mkt_exc_3l) >= &__min.;
			quit;
			proc corr data = __corr_data2 outp=__corr1 noprint nomiss ;
				by id calc_date;
				var ret_exc_3l mkt_exc_3l;
			run;
			proc sql;
				create table __corr2 as 
				select id, calc_date as eom, ret_exc_3l as corr&sfx.
				from __corr1
				where _type_='CORR' and _name_ = 'mkt_exc_3l';
			quit;
			%save_or_append(base=op_corr, new=__corr2);
		%end;
		
		* Market Volatility (separately for each stock);
		%if %sysfunc(find(&__stats., mktvol)) >= 1 %then %do;
			proc sql;
				create table __mktvol as 
				select id, calc_date as eom, std(mktrf) as __mktvol&sfx.
				from calc_data_screen
				group by id, calc_date
				having count(ret_exc) >= &__min.;
			quit;
			%save_or_append(base=op_mktvol, new=__mktvol);
		%end;
		
		* NAME;
		%if %sysfunc(find(&__stats., NAME)) >= 1 %then %do;
			
			%save_or_append(base=, new=);
		%end;
		
	%end;	
	* Make all observations into a dataset by transposing and appending; 
	proc sql noprint;
	    select memname into :op_datasets separated by " "
	    from dictionary.tables
	    where lowcase(libname)="work" and  prxmatch("/^op\_/i", memname) > 0;
	quit;
	
	* Initialize dataset to append to;
	data &out.;
		format id $char20. eom YYMMDDN8. stat $char20. value 16.8;
		stop;  
	run;
	%do k=1 %to %nwords(&op_datasets.);
		%let __dt = %scan(&op_datasets., &k., %str(' '));
		proc sort data=&__dt.; by id eom; run;
		proc transpose data=&__dt. out=__op(rename=(col1=value)) name=stat ;
			by id eom;
		run;
		proc append base=&out. data=__op force; run; 
		proc delete data=&__dt.; run;
	%end;
	/* Stop timer */
	data _null_;
		dur = datetime() - &__roll_start;
		put 30*'-' / ' DAILY ROLL APPLY TOOK:' dur time13.2 / 30*'-';
	run;
%mend;

/* MACRO: FINISH DAILY CHARS*/
%macro finish_daily_chars(out=);
	* Make bidask into a long format;
	proc transpose data=scratch.corwin_schultz out=bidask(rename=(col1=value)) name=stat;
		by id eom;
	run;
	* Combine all roll chars;
	data daily_chars1; set scratch.roll_21d; run;
	proc append base=daily_chars1 data=scratch.roll_126d; run; 
	proc append base=daily_chars1 data=scratch.roll_252d; run; 
	proc append base=daily_chars1 data=scratch.roll_1260d; run;
	proc append base=daily_chars1 data=bidask force; run;
	proc sort data=daily_chars1 nodup; by id eom; run;
	proc transpose data = daily_chars1 out= daily_chars2(drop=_name_);
		by id eom;
		id stat;
		var value;
	run;
	proc sql;
		create table daily_chars3 as 
		select *, corr_1260d * rvol_252d/__mktvol_252d as betabab_1260d, rmax5_21d / rvol_252d as rmax5_rvol_21d
		from daily_chars2;
		
		alter table daily_chars3
		drop __mktvol_252d;
	quit;
	proc sort data=daily_chars3 out=&out.; by id eom; run;
%mend;



