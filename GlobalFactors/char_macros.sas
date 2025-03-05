/* MACROS USING COMPOSITE DATA */
/* MACRO: MISPRICING_FACTORS 
- Based on the paper by Yuan and Stambaugh (2016)
- Currently, the distress probability anomaly is not implemented.
- I use fractional ranks i.e. ranks from 0 to 1. I think they use absolute ranks in the paper.
*/
%macro mispricing_factors(out=, data=, min_stks=, min_fcts=3);
	proc sql;
		create table chars1 as 
		select id, eom, excntry, chcsho_12m, eqnpo_12m, oaccruals_at, noa_at, at_gr1, ppeinv_gr1a, 
			o_score, ret_12_1, gp_at, niq_at
		from &data.
		where common=1 and primary_sec=1 and obs_main=1 and exch_main = 1 and not missing(ret_exc) and not missing(me)
		order by excntry, eom;
	quit;
	%let __vars = chcsho_12m eqnpo_12m oaccruals_at noa_at at_gr1 ppeinv_gr1a o_score ret_12_1 gp_at niq_at;
	%let __direction = -1 1 -1 -1 -1 -1 -1 1 1 1;
	%do i=1 %to 10;
		%let __v = %scan(&__vars., &i, %str(' '));
		%let __d = %scan(&__direction., &i, %str(' '));
		%if &__d. = 1 %then %do;
			%let __sort=;
		%end;
		%else %do;
			%let __sort=descending;
		%end;
		proc sql;
			create table __subset as 
			select *
			from chars1
			group by excntry, eom
			having count(&__v.) >= &min_stks.;
		quit;
		proc rank data=__subset out = __ranks(keep=excntry id eom rank_&__v.) &__sort. ties=mean f;
			by excntry eom;
			var &__v.;
			ranks rank_&__v.;
		run;
		proc sql;
			create table chars%eval(&i.+1) as
			select a.*, b.rank_&__v.
			from chars&i. as a left join __ranks as b
			on a.id=b.id and a.eom=b.eom;
		quit;
	%end;
	data &out.;
		set chars11;
		mispricing_perf = mean(rank_o_score, rank_ret_12_1, rank_gp_at, rank_niq_at);
		if missing(rank_o_score) + missing(rank_ret_12_1) + missing(rank_gp_at) + missing(rank_niq_at) > &min_fcts. then
			mispricing_perf = .;
		mispricing_mgmt = mean(rank_chcsho_12m, rank_eqnpo_12m, rank_oaccruals_at, rank_noa_at, rank_at_gr1, rank_ppeinv_gr1a);
		if missing(rank_chcsho_12m) + missing(rank_eqnpo_12m) + missing(rank_oaccruals_at) + missing(rank_noa_at) + missing(rank_at_gr1) + missing(rank_ppeinv_gr1a) > &min_fcts. then
			mispricing_mgmt = .;	
		keep id eom mispricing_perf mispricing_mgmt;
	run;
%mend;

* MACRO: QUALITY MINUS JUNK
- Based on the paper by Asness, Frazzini and Pedersen (2018)
- I deviate slightly from the original paper in the variable construction
- The most clear deviation is the way the growth variables are created.
;
%macro quality_minus_junk(out=, data=, min_stks=);
	/* Helper Macro */
	%macro z_ranks(out=, data=, var=, min=, sort=);
		proc sql;
			create table __subset as 
			select *
			from &data.
			group by excntry, eom
			having count(&var.) >= &min.;
		quit;
		proc rank data=__subset out = __ranks(keep=excntry id eom rank_&var.) &sort. ties=mean;
			by excntry eom;
			var &var.;
			ranks rank_&var.;
		run;
		proc sql;
			create table &out. as 
			select excntry, id, eom, (rank_&var. - mean(rank_&var.)) / std(rank_&var.) as z_&var.
			from __ranks
			where not missing(rank_&var.)
			group by excntry, eom;
		quit;
		proc delete data=__subset __ranks; run;
	%mend;
	proc sql;
		create table qmj1 as 
		select id, eom, excntry, coalesce(roeq_be_std*2, roe_be_std) as __evol, /* I multiply the quarterly measure by sqrt(4)=2 to reflect that quarterly measures are less volatile than the annual measure. Empirically, this seems to be a reasonable approximation although perhaps a slightly higher multiplied could be used e.g. 2.5*/
			gp_at, ni_be, ni_at, ocf_at, gp_sale, oaccruals_at, gpoa_ch5, roe_ch5, roa_ch5, cfoa_ch5, 
			gmar_ch5, betabab_1260d, debt_at, o_score, z_score
		from &data.
		where common=1 and primary_sec=1 and obs_main=1 and exch_main=1 and not missing(ret_exc) and not missing(me)
		order by excntry, eom;
	quit;
	%let z_vars    = gp_at ni_be ni_at ocf_at gp_sale oaccruals_at
	                 gpoa_ch5  roe_ch5 roa_ch5 cfoa_ch5 gmar_ch5
	                 betabab_1260d debt_at o_score z_score __evol;  
	%let direction = 1 1 1 1 1 -1
	                 1 1 1 1 1
	                 -1 -1 -1 1 -1;
	%do i=1 %to 16;
		%let __v = %scan(&z_vars., &i, %str(' '));
		%let __d = %scan(&direction., &i, %str(' '));
		%if &__d. = 1 %then %do;
			%let __sort=;
		%end;
		%else %do;
			%let __sort=descending;
		%end;
		%z_ranks(out=__z, data=qmj1, var = &__v., min=&min_stks., sort=&__sort.);
		proc sql;
			create table qmj%eval(&i.+1) as
			select a.*, b.z_&__v.
			from qmj&i. as a left join __z as b
			on a.id=b.id and a.eom=b.eom;
		quit;
		%if &i.>1 %then %do;
			proc delete data=qmj&i.; run;
		%end;
	%end;
	data qmj18;
		set qmj17;
		__prof = mean(z_gp_at, z_ni_be, z_ni_at, z_ocf_at, z_gp_sale, z_oaccruals_at);
		__growth = mean(z_gpoa_ch5, z_roe_ch5, z_roa_ch5, z_cfoa_ch5, z_gmar_ch5);
		__safety = mean(z_betabab_1260d, z_debt_at, z_o_score, z_z_score, z___evol);
		keep excntry id eom __prof __growth __safety;
	run;
	%z_ranks(out=__prof, data=qmj18, var = __prof, min=&min_stks., sort=);
	%z_ranks(out=__growth, data=qmj18, var = __growth, min=&min_stks., sort=);
	%z_ranks(out=__safety, data=qmj18, var = __safety, min=&min_stks., sort=);
	proc sql;
		create table qmj19 as 
		select a.excntry, a.id, a.eom, b.z___prof as qmj_prof, c.z___growth as qmj_growth, d.z___safety as qmj_safety
		from qmj18 as a 
		left join __prof as b on a.excntry=b.excntry and a.id=b.id and a.eom=b.eom
		left join __growth as c on a.excntry=c.excntry and a.id=c.id and a.eom=c.eom
		left join __safety as d on a.excntry=d.excntry and a.id=d.id and a.eom=d.eom;
	quit;
	/* QMJ SCORE! */
	data qmj20;
		set qmj19;
		__qmj = (qmj_prof + qmj_growth + qmj_safety) / 3; * Missing if any of subcomponents are missing;
	run;
	%z_ranks(out=__qmj, data=qmj20, var=__qmj, min = &min_stks., sort=);
	proc sql;
		create table &out. as 
		select a.excntry, a.id, a.eom, a.qmj_prof, a.qmj_growth, a.qmj_safety, b.z___qmj as qmj
		from qmj20 as a left join __qmj as b 
		on a.excntry=b.excntry and a.id=b.id and a.eom=b.eom;
	quit;
	proc delete data=qmj1 qmj17 qmj18 qmj19 qmj20; run;
%mend;

/* MACRO USING RETURN DATA -------------------------------------------------------*/
* MACRO: BIDASK_HL -------------------------
- Corwin-Schultz High-Low Bid-ask Estimator
- Heavily inspired by Shane Corwins code: http://sites.nd.edu/scorwin/files/2019/12/Sample-SAS-Program.pdf
- Primary change: I adjust prices for stock splits
- Arguments:
	* OUT: Output dataset containing estimates of average monthly bid-ask spread and return volatility
	* DATA: Input dataset with high and low prices
	* __min_obs: Minimum amount of daily observations required to compute monthly estimates;
%macro bidask_hl(out=, data=, __min_obs=);
	proc sql;
		create table __dsf1 as 
		select a.id, a.date, a.eom, a.bidask, a.tvol, 
			a.prc / a.adjfct as prc, a.prc_high / a.adjfct as prc_high, a.prc_low / a.adjfct as prc_low /* Adjust price for stocks splits! */
		from &data. as a left join scratch.market_returns_daily as b
		on a.excntry=b.excntry and a.date=b.date
		where not missing(b.mkt_vw_exc)  /* This ensures that we look at trading days */
		order by id, date;
	quit;
	
	* Cleaning data;
	data __dsf2(drop=prc_low_r prc_high_r);
		retain prc_low_r prc_high_r;
		set __dsf1;
		by id date eom;
		* Keep initial valeus;
		prc_low_in = prc_low;
		prc_high_in = prc_high;
		hlreset = 0;
		* Initial Screens;
		if bidask = 1 or prc_low=prc_high or prc_low<=0 or prc_high<=0 or tvol=0 then do; 
			prc_high = .;
			prc_low = .;
		end;
		/* Replace bad/missing price with previous day range */
		if first.id then do;
			prc_low_r = .;
			prc_high_r = .;
		end;
		* Reset retained high and low volume;
		if 0<prc_low<prc_high then do;
			prc_low_r=prc_low; 
			prc_high_r=prc_high;
		end;
		* Replace mising/bad high and low prices with retained values;
		else do;
			* Replace if within prior days range;
			if prc_low_r <= prc <= prc_high_r then do;
				prc_low = prc_low_r;
				prc_high = prc_high_r;
				hlreset = 1;
			end;
			* Replace if below prior days range;
			if prc < prc_low_r then do;
				prc_low = prc;
				prc_high = prc_high_r - (prc_low_r-prc);
				hlreset = 2;
			end;
			* Replace if above prior days range;
			if prc > prc_high_r then do;
				prc_low = prc_low_r + (prc - prc_high_r);
				prc_high = prc;
				hlreset = 3;
			end;
		end;
		/* Final data screen after H/L reset */
		if prc_low ^= 0 and prc_high/prc_low > 8 then do;
			prc_low = .;
			prc_high = .;
		end;
	run;
	
	/* Adjust for overnight returns */
	data __dsf3;
		set __dsf2;
		retadj = 0;
		prc_low_t = prc_low;
		prc_high_t = prc_high;
		prc_low_l1 = lag(prc_low);
		prc_high_l1 = lag(prc_high);
		prc_l1 = lag(prc);
		if id ^= lag(id) then do;
			prc_low_l1 = .;
			prc_high_l1 = .;
			prc_l1 = .;
		end;
		if prc_l1<prc_low and prc_l1>0 then do; * Adjust when prior close is below current low;
			prc_high_t=prc_high-(prc_low-prc_l1); 
			prc_low_t=prc_l1; 
			retadj=1;
	 	end;
		if prc_l1>prc_high and prc_l1>0 then do; * Adjust when prior close is above current high;
			prc_high_t=prc_l1; 
			prc_low_t=prc_low+(prc_l1-prc_high); 
			retadj=2;
		end;
	run;
	
	/* Calculate daily high/low bid-ask spread*/
	data __dsf4; 
		set __dsf3;
	 	pi=constant('PI');
	 	k2 = sqrt(8/pi);
	 	const = 3-2*sqrt(2);
	 	prc_high_2d=max(prc_high_t,prc_high_l1);
	 	prc_low_2d=min(prc_low_t,prc_low_l1);
	 	if prc_low_t>0 and prc_low_l1>0 then 
	 		beta = (log(prc_high_t/prc_low_t))**2+(log(prc_high_l1/prc_low_l1))**2;
	 	if prc_low_2d>0 then 
	 		gamma = (log(prc_high_2d/prc_low_2d))**2;
	 	alpha = (sqrt(2*beta)-sqrt(beta))/const - sqrt(gamma/const);
	 	* Calculate spread with missing set to zero;
	 	spread = 2*(exp(alpha)-1)/(1+exp(alpha));
	 	spread_0 = max(spread,0); * Set negative spread estimates to zero;
	 	if spread = . then 
	 		spread_0 = .;
	 	* Calculate daily volatillity;
	 	sigma = ((sqrt(beta/2)-sqrt(beta)))/(k2*const)+sqrt(gamma/(k2*k2*const));
	 	sigma_0 = max(sigma,0); * Set negative sigma estimates to zero;
	 	if sigma= . then 
	 		sigma_0 = .;
	run;
	
	/* Monthly bid-ask estimates */
	proc sql;
		create table &out. as 
		select id, eom, mean(spread_0) as bidaskhl_21d, mean(sigma_0) as rvolhl_21d
		from __dsf4
		group by id, eom
		having count(spread_0) > &__min_obs.;
	quit;
	
	proc delete data=__dsf1 __dsf2 __dsf3 __dsf4; run;
%mend;


* MACRO: SEASONALITY
- Caclulates annual and non-annual seasonality measures following Heston and Sadka (2008)
- Specifically, calculate the average return over annual and non-annual lags within the specified
  start and end dates
- Within a given year, the annual lag is lag11 and the non-annual lags are lag0-lag10. 
- For return predictability, the information should be used to form portfolios at the end of lag0
- Said differently, the seasonality variables should be lagged 1 period relative to returns
;
%macro seasonality(start_year=, end_year=);
	* Return over all lags;
	__all_ret = 0;
	__all_n = 0;
	%do i = %eval((&start_year.-1) * 12) %to %eval(&end_year. * 12 - 1);
		__all_ret = __all_ret + lag&i(ret_x);
		__all_n = __all_n + 1;
	%end;
	* Return over annual lags;
	__an_ret = 0;
	__an_n = 0;
	%do i = %eval(&start_year.) %to &end_year.;
		%let __al = %eval(&i. * 12 - 1);  
		__an_ret = __an_ret + lag&__al.(ret_x);
		__an_n = __an_n + 1;
	%end;
	* Return over non-annual lags;
	__na_ret = __all_ret - __an_ret;
	__na_n = __all_n - __an_n;
	* Create Variables;
	seas_&start_year._&end_year.an = __an_ret / __an_n;
	seas_&start_year._&end_year.na = __na_ret / __na_n;
	if count < %eval(&end_year. * 12) then do;
		seas_&start_year._&end_year.an = .;
		seas_&start_year._&end_year.na = .;	
	end;
	drop __all_ret __all_n __an_ret __an_n __na_ret __na_n;
%mend;

/* MACRO USING ACCOUNTING DATA -------------------------------------------------------*/
* Create Growth in Variable over horizon;
%macro var_growth(var_gr=, horizon=); /* Horizon is in months */
	%let name_gr = %sysfunc(tranwrd(&var_gr., _x, %str()));  /* Remove '_x' from var name */
	%let name_gr = &name_gr._gr%sysevalf(&horizon./12);      /* Add gr and horizon in years to name */ 
	&name_gr. = &var_gr./lag&horizon.(&var_gr.)-1;
	if count<=&horizon. or lag&horizon.(&var_gr.)<=0 then
			&name_gr. = .; 
%mend;

* Change in Variable over Horizon Scaled by Assets;
%macro chg_to_assets(var_gra=, horizon=); /* Horizon is in months */
	%let name_gra = %sysfunc(tranwrd(&var_gra., _x, %str()));  /* Remove '_x' from var name */
	%let name_gra = &name_gra._gr%sysevalf(&horizon./12);      /* Add gr and horizon in years to name */ 
	%let name_gra = &name_gra.a;                                /* Add 'a' in the end*/
	&name_gra. = (&var_gra.-lag&horizon.(&var_gra.))/at_x;
	if count<=&horizon. or at_x<=0 then
			&name_gra. = .; 
%mend;

* Ratio Change;
%macro chg_var1_to_var2(name=, var1=, var2=, horizon=);
	__x = &var1. / &var2.;
	if &var2. <= 0 then 
		__x=.;
	&name. = (__x - lag&horizon.(__x));
	if count <= horizon then 
		&name. = .;
	drop __x;
%mend;

* Change to expectations (Abarnell and Bushee, 1998);
%macro chg_to_exp(var_ce=);
	%let name_ce = %sysfunc(tranwrd(&var_ce., _x, %str()));  /* Remove '_x' from var name */
	%let name_ce = &name_ce._ce;
	__expect = (lag12(&var_ce.) + lag24(&var_ce.))/2;
	&name_ce. = &var_ce. / (__expect) - 1;
	if count <= 24 or __expect <= 0 then
		&name_ce. = .;
	drop __expect;
%mend;

* Standardized Unexpected Realization;
* Uses the specification in Jegadeesh and Livnat (2006);
%macro standardized_unexpected(var=, qtrs=, qtrs_min=);
	%let name = %sysfunc(tranwrd(&var., _x, %str()));  /* Remove '_x' from var name */
	%let name = &name._su;  
	__chg = &var. - lag12(&var.);
	__chg_mean = %apply_to_lastq(x = __chg, _qtrs = &qtrs., func = mean);
	__chg_std = %apply_to_lastq(x = __chg, _qtrs = &qtrs., func = std);
	__chg_n = %apply_to_lastq(x = not missing(__chg), _qtrs = &qtrs., func = sum);
	if __chg_n <= &qtrs_min. then do;
		__chg_mean = .;
		__chg_std = .;
	end;
	&name. = (&var. - (lag12(&var.) + lag3(__chg_mean) )) / lag3(__chg_std); /* This is the correct one*/
	if count <= %eval(12 + &qtrs.*3) then
		&name. = .;
	drop __chg __chg_mean __chg_std __chg_n;
%mend;

* Volatility of Quarterly Data;
%macro volq(name=, var=, qtrs=, qtrs_min=);
	__n = %apply_to_lastq(x = not missing(&var.), _qtrs = &qtrs., func = sum);
	&name. = %apply_to_lastq(x = &var., _qtrs = &qtrs., func = std);
	if count <= %eval((&qtrs.-1)*3) or __n < &qtrs_min. then
		&name. = .;
	drop __n;
%mend;

* Volatility of Annual Data;
%macro vola(name=, var=, yrs=, yrs_min=);
	__n = %apply_to_lasty(x = not missing(&var.), yrs = &yrs., func = sum);
	&name. = %apply_to_lasty(x = &var., yrs = &yrs., func = std);
	if count <= %eval((&yrs.-1)*12) or __n < &yrs_min. then
		&name. = .;
	drop __n;
%mend;

* Earnings Smoothness;
%macro earnings_variability(esm_h=);
	__roa = ni_x / lag12(at_x);
	__croa = ocf_x / lag12(at_x);
	__roa_n = %apply_to_lasty(x= not missing(__roa), yrs=&esm_h., func=sum); 
	__croa_n = %apply_to_lasty(x= not missing(__croa), yrs=&esm_h., func=sum);
	__roa_std = %apply_to_lasty(x=__roa, yrs=&esm_h., func=std);
	__croa_std = %apply_to_lasty(x=__croa, yrs=&esm_h., func=std);
	earnings_variability = __roa_std / __croa_std;
	if count <= %eval(&esm_h. * 12) or __croa_std <= 0 or __roa_n < &esm_h. or __croa_n < &esm_h. then
		earnings_variability = .;
	 drop __roa __croa  __roa_n __croa_n __roa_std __croa_std;
%mend;

/* Equity Duration: Forecast of Cash Distribution */
%macro equity_duration_cd(horizon=, r=, roe_mean=, roe_ar1=, g_mean=, g_ar1=);
	* Create Initial Variables;
	__roe0 = ni_x / lag12(be_x);
	__g0 = sale_x / lag12(sale_x) - 1;
	__be0 = be_x;
	if count <= 12 or lag12(be_x) <= 1 then __roe0 = .; /* Use 1 million to avoid bad estimates from a small denominator */
	if count <= 12 or lag12(sale_x) <= 1 then __g0 = .; /* Use 1 million to avoid bad estimates from a small denominator */
	* Forecast Cash Distributions;
	%let roe_c = &roe_mean.*(1 - &roe_ar1.);
	%let g_c = &g_mean.*(1 - &g_ar1.);
	%do i = 1 %to &horizon.;
		%let j = %eval(&i.-1);
		__roe&i. = &roe_c. + &roe_ar1. * __roe&j.;
		__g&i. = &g_c. + &g_ar1. * __g&j.;
		__be&i. = __be&j. * (1 + __g&i.);
		__cd&i. = __be&j. * (__roe&i. - __g&i.);
	%end;
	* Create Duration Helper Variables;
	ed_constant = &horizon. + (1 + &r.) / &r.;
	ed_cd_w = 0;
	ed_cd = 0;
	ed_err = 0;
	%do t = 1 %to &horizon.;
		ed_cd_w = ed_cd_w + &t. * __cd&t. / (1 + &r.)**&t.;
		ed_cd = ed_cd + __cd&t. / (1 + &r.)**&t.;
		if __be&t. < 0 then ed_err = 1;
	%end;
	drop __roe: __g: __be: __cd:; 
%mend;

* Pitroski (2000) Fundamental Score;
%macro pitroski_f(name=);
	__f_roa = ni_x / lag12(at_x);
	if count <= 12 or lag12(at_x) <= 0 then __f_roa = .;
	__f_croa = ocf_x / lag12(at_x);
	if count <= 12 or lag12(at_x) <= 0 then __f_croa = .;
	__f_droa = __f_roa - lag12(__f_roa);
	if count <= 12 then __f_droa = .;
	__f_acc = __f_croa - __f_roa;
	__f_lev = dltt / at_x - lag12(dltt / at_x);
	if count <= 12 or at_x <= 0 or lag12(at_x) <= 0 then __f_lev = .;
	__f_liq = ca_x / cl_x - lag12(ca_x / cl_x);
	if count <= 12 or cl_x <= 0 or lag12(cl_x) <= 0 then __f_liq = .;
	__f_eqis = eqis_x;
	__f_gm = gp_x / sale_x - lag12(gp_x / sale_x);
	if count <= 12 or sale_x <= 0 or lag12(sale_x) <= 0 then __f_gm = .;
	__f_aturn = sale_x / lag12(at_x) - lag12(sale_x) / lag24(at_x); 
	if count <= 24 or lag12(at_x) <= 0 or lag24(at_x) <= 0 then __f_aturn = .; 
	&name. = (__f_roa > 0) + (__f_croa > 0) + (__f_droa > 0) + (__f_acc > 0) +
			  (__f_lev < 0) + (__f_liq > 0) + (coalesce(__f_eqis, 0) = 0) +  /* Set __f_eqis to zero if missing. This greatly expands coverage and seems like a reasonable approximation */ 
			  (__f_gm > 0) + (__f_aturn > 0);
	* Only allow __f_eqis to be missing;		  
	if missing(__f_roa) or missing(__f_croa) or missing(__f_droa) or missing(__f_acc) or
	   missing(__f_lev) or missing(__f_liq) or missing(__f_gm) or missing(__f_aturn) then &name. = .;
	drop __f_:;
%mend;

* Ohlson (1980) O-score;
%macro ohlson_o(name=);
	* Create Helpers;
	__o_lat = log(at_x);
	__o_lev = debt_x / at_x;
	__o_wc = (ca_x - cl_x) / at_x;
	__o_roe = nix_x / at_x;
	if at_x <= 0 then do;
		__o_lat = .;
		__o_lev = .;
		__o_wc = .;
		__o_roe = .;
	end;
	__o_cacl = cl_x / ca_x;
	if ca_x <= 0 then __o_cacl = .;
	__o_ffo = (pi_x + dp) / lt;
	if lt <= 0 then __o_ffo = .;
	__o_neg_eq = lt > at_x;
	if missing(lt) or missing(at_x) then __o_neg_eq = .;
	__o_neg_earn = (nix_x < 0 and lag12(nix_x) < 0);
	if count <= 12 or missing(nix_x) or missing(lag12(nix_x)) then __o_neg_earn = .;
	__o_nich = (nix_x - lag12(nix_x)) / (abs(nix_x) + abs(lag12(nix_x)));
	if count <= 12 or (abs(nix_x) + abs(lag12(nix_x))) = 0 then __o_nich = .;
	* Create O-score;
	&name. = -1.32 - 0.407 * __o_lat + 6.03 * __o_lev - 1.43 * __o_wc
	          + 0.076 * __o_cacl - 1.72 * __o_neg_eq - 2.37 * __o_roe
	          - 1.83 * __o_ffo + 0.285 * __o_neg_earn - 0.52 * __o_nich;
%mend;

* Altman (1968) Z-score;
%macro altman_z(name=);
	* Create Helpers;
	__z_wc = (ca_x - cl_x) / at_x;
	__z_re = re / at_x;
	__z_eb = ebitda_x / at_x;
	__z_sa = sale_x / at_x;
	if at_x <= 0 then do;
		__z_wc = .;
		__z_re = .;
		__z_eb = .;
		__z_sa = .;
	end;
	__z_me = me_fiscal / lt;
	if lt <= 0 then	__z_me = .;
	* Create Temporary Z-score;
	&name. = 1.2 * __z_wc + 1.4 * __z_re + 3.3 * __z_eb + 0.6 * __z_me + 1.0 * __z_sa;
	drop __z:
%mend;

* Intrinsic ROE based value from Frankel and Lee (1998);
%macro intrinsic_value(name=, r=);
	__iv_po = div_x/nix_x;
	if nix_x <= 0 then
		__iv_po = div_x / (at_x * 0.06);
	__iv_roe = nix_x / ((be_x + lag12(be_x)) / 2);
	if count <= 12 or (be_x + lag12(be_x)) <= 0 then __iv_roe = .;
	__iv_be1 = (1 + (1 - __iv_po) * __iv_roe) * be_x;
	&name. = be_x + (__iv_roe - &r.) / (1 + &r.) * be_x 
					+ (__iv_roe - &r.) / ((1 + &r.) * &r.) * __iv_be1; 
	* If Intrinsic value is Non-Positive, set to missing;
	if &name. <= 0 then &name. = .;
	drop __iv:;
%mend;

*  Kaplan-Zingales Index;
%macro kz_index(name=);
	* Create Helper Variables;
	__kz_cf = (ni_x + dp) / lag12(ppent);
	if count <= 12 or lag12(ppent)<=0 then __kz_cf = .;
	__kz_q = (at_x + me_fiscal - be_x) / at_x;
	if at_x <= 0 then __kz_q = .;
	__kz_db = debt_x / (debt_x + seq_x);
	if (debt_x + seq_x) = 0 then __kz_db = .;
	__kz_dv = div_x / lag12(ppent);
	if count <= 12 or lag12(ppent)<=0 then __kz_dv = .;
	__kz_cs = che / lag12(ppent);
	if count <= 12 or lag12(ppent)<=0 then __kz_cs = .;
	* Create Variable;
	&name. = - 1.002 * __kz_cf + 0.283 * __kz_q + 3.139 * __kz_db
	         - 39.368 * __kz_dv - 1.315 * __kz_cs;
%mend;

/* Earnings Predicability/Persistence*/
* I scale net income by total assets to account for issuance activity.;
%macro earnings_persistence(out=, data=, __n=, __min=);
	%let __months = %eval(&__n. * 12);
	proc sort data=&data. out=__acc1; by gvkey curcd datadate; run;
	data __acc2;
		set __acc1;
		by gvkey curcd;
		retain count;
		if first.curcd then 
			count = 1;
		else
			count = count+1;
	run;
	data __acc3;
		set __acc2;
		__ni_at = ni_x / at_x;
		if at_x <= 0 then
			__ni_at = .;
		__ni_at_l1 = lag12(__ni_at);
		if count<=12 then
			__ni_at_l1 =.;	
	run;
	proc sql;
		create table __acc4 as 
		select gvkey, curcd, datadate, __ni_at, __ni_at_l1
		from __acc3
		where not missing(__ni_at) and not missing(__ni_at_l1);
	quit;
	
	proc sql;
		create table month_ends as 
		select distinct datadate
		from __acc4
		order by datadate;
	quit;
	
	* Divide data into __n groups;
	proc sql;
		create table dates_apply as 
		select *, mod(monotonic(), &__months.) as grp
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
	
	%do __grp=0 %to %eval(&__months. - 1); 
		%put ############### GROUP %eval(&__grp.+1) out of &__months. ###############; 
		* Prepare data;
		proc sql;
			create table calc_dates as
			select a.datadate, b.datadate as calc_date
			from dates_apply as a left join dates_apply(where=(grp = &__grp.)) as b
			on a.datadate > intnx("year", b.datadate, -&__n., "e") and a.datadate <= b.datadate and month(a.datadate) = month(b.datadate); /* month(*) ensures annual lags*/
		quit;
		
		proc sql;
			create table calc_data as 
			select a.*, b.calc_date
			from __acc4 as a left join calc_dates as b
			on a.datadate = b.datadate
			where not missing(b.calc_date)  
			group by a.gvkey, a.curcd, b.calc_date
			having count(*) >= &__min.
			order by a.gvkey, b.calc_date;
		quit;
		
		proc reg data=calc_data outest=__earn_pers1 edf NOPRINT;
			by gvkey curcd calc_date;
			model __ni_at=__ni_at_l1;
		run;
		proc sql;
			create table __earn_pers2 as 
			select gvkey, curcd, calc_date as datadate, __ni_at_l1 as ni_ar1, sqrt(_rmse_**2 * _edf_ / (_edf_ + 1)) as ni_ivol
			from __earn_pers1
			where (_edf_ + 2) >= &__min.;
		quit;
		%save_or_append(base=op_ep, new=__earn_pers2);
	%end;
	proc sort data=op_ep out=&out. nodup; by gvkey curcd datadate; run; 
	proc delete data= __acc1 __acc2 __acc3 __acc4  dates_apply calc_dates calc_data month_ends __earn_pers1 __earn_pers2 op_ep; run;
%mend;

/* MACRO - FIRM AGE */
%macro firm_age(data=, out=);
	* CRSP first observation;
	proc sql;
		create table crsp_age1 as 
		select permco, min(date) as crsp_first format=YYMMDDN8.
		from crsp.msf
		group by permco;
	quit;
	
	* Compustat accounting first observation;
	proc sql;
		create table comp_acc_age1 as
		select gvkey, datadate from comp.funda
		outer union corr
		select gvkey, datadate from comp.g_funda;
		
		create table comp_acc_age2 as
		select gvkey, min(datadate) as comp_acc_first format=YYMMDDN8.
		from comp_acc_age1
		group by gvkey;
		
		update comp_acc_age2
		set comp_acc_first = intnx('year', comp_acc_first, -1, 'e');  /* When submitting an annual report, the firm must have existed for at least 1 year*/
	quit; 
	
	* Compustat return first obs;
	proc sql;
		create table comp_ret_age1 as
		select gvkey, datadate from comp.secm
		outer union corr
		select gvkey, datadate from comp.g_secd where monthend=1;
		
		create table comp_ret_age2 as
		select gvkey, min(datadate) as comp_ret_first format=YYMMDDN8.
		from comp_ret_age1
		group by gvkey;
		
		update comp_ret_age2
		set comp_ret_first = intnx('year', comp_ret_first, -1, 'e');  /* When submitting an annual report, the firm must have existed for at least 1 year*/
	quit; 
	
	* Add to Dataset;
	proc sql;
		create table comb1 as
		select a.id, a.eom, min(b.crsp_first, c.comp_acc_first, d.comp_ret_first) as first_obs format=YYMMDDN8.
		from &data. as a 
		left join crsp_age1 as b 
		on a.permco=b.permco
		left join comp_acc_age2 as c
		on a.gvkey=c.gvkey
		left join comp_ret_age2 as d
		on a.gvkey=d.gvkey;
	
		create table comb2 as 
		select *, min(eom) as first_alt format=YYMMDDN8. /* A few (0.3% of all obs) north american observations don't have observations in comp.secm so this is a fall back option*/
		from comb1
		group by id; 
	
		create table comb3 as
		select *, intck ('month', min(first_obs, first_alt), eom) as age
		from comb2;
		
		alter table comb3
		drop first_obs, first_alt;
	quit;
	* Output;
	proc sort data=comb3 out=&out.; by id eom; run;
%mend;