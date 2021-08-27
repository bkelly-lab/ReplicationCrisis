/* Clean working environment */
proc delete data = _all_ ; run ; 

***************************************************************************
* Manual Inputs
*************************************************************************** ; 
* Assign scratch and project folder names;
%let scratch_folder = /scratch/cbs/tij; 
%let project_folder = ~/Global Data;

***************************************************************************
* Libraries and Functions
*************************************************************************** ; 
* Libraries;
libname scratch "&scratch_folder."; 
libname project "&project_folder.";

* Project macros;
%include "&project_folder./project_macros.sas";
%include "&project_folder./char_macros.sas";
%include "&project_folder./market_chars.sas";
%include "&project_folder./accounting_chars.sas";
%include "&project_folder./ind_identification.sas";

*****************************************************************************
* Create Return Data
**************************************************************************** ; 
%prepare_comp_sf(freq=both);
%clean_comp_msf(data=comp_msf); * Delete obvious data errors (work-in-progress);
%prepare_crsp_sf(freq=d);
%prepare_crsp_sf(freq=m);
%combine_crsp_comp_sf(out_msf=world_msf1, out_dsf=scratch.world_dsf, crsp_msf=crsp_msf, comp_msf=comp_msf, crsp_dsf=crsp_dsf, comp_dsf=comp_dsf);

*****************************************************************************
* Add Industry Codes 
***************************************************************************** ;
%crsp_industry(out=crsp_ind);
%comp_industry(out=comp_ind);

proc sql;
	create table world_msf2 as
	select a.*, b.gics as gics, coalesce(b.sic, c.sic) as sic, coalesce(b.naics, c.naics) as naics /* Prefer COMPUSTAT to CRSP */
	from world_msf1 as a
	left join comp_ind as b on a.gvkey=b.gvkey and a.eom=b.date
	left join crsp_ind as c on a.permco=c.permco and a.permno=c.permno and a.eom=c.date;
quit;

* Add a column 'ff49' with Fama-French industry classification;
%ff_ind_class(data=world_msf2, ff_grps=49, out=world_msf3); 

* Extract Information;
%crsp_return_cutoffs(data=world_msf3, out=scratch.crsp_return_cutoffs);
%nyse_size_cutoffs(data=world_msf3, out=scratch.nyse_cutoffs);

* Classify stocks into size groups;
proc sql;
	create table scratch.world_msf as
	select case 
			when missing(a.me) then ('')
			when a.me >= b.nyse_p80 then 'mega'
			when a.me >= b.nyse_p50 then 'large'
			when a.me >= b.nyse_p20 then 'small'
			when a.me >= b.nyse_p1 then 'micro'
			else 'nano'
		end as size_grp, a.*
	from world_msf3 as a left join scratch.nyse_cutoffs as b
	on a.eom=b.eom;
quit;

*****************************************************************************
* Market Returns
**************************************************************************** ; 
%market_returns(out = scratch.market_returns, data = scratch.world_msf, freq=m, wins = crsp, wins_crsp_data=scratch.crsp_return_cutoffs);
%market_returns(out = scratch.market_returns_daily, data = scratch.world_dsf, freq=d, wins = all);

*****************************************************************************
* Create Characteristics Based on Accounting Data
**************************************************************************** ;
%standardized_accounting_data(coverage='world', convert_to_usd=1, me_data = scratch.world_msf, include_helpers_vars=1, start_date='31DEC1949'd); 
%create_acc_chars(data=acc_std_ann, out=achars_world, lag_to_public=4, max_data_lag=18, __keep_vars=&char_vars., me_data=scratch.world_msf, suffix=);
%create_acc_chars(data=acc_std_qtr, out=qchars_world, lag_to_public=4, max_data_lag=18, __keep_vars=&char_vars., me_data=scratch.world_msf, suffix=_qitem);
%combine_ann_qtr_chars(out=scratch.acc_chars_world, ann_data=achars_world, qtr_data=qchars_world, __char_vars=&char_vars., q_suffix=_qitem);

*****************************************************************************
* Create Characteristics Based on Monthly Market Data
**************************************************************************** ;
%market_chars_monthly(out=scratch.market_chars_m, data = scratch.world_msf, market_ret = scratch.market_returns, local_currency=0); 

*****************************************************************************
* Combine Returns, Accounting and Monthly Market Data
**************************************************************************** ; 
proc sql;
	create table scratch.world_data_prelim as 
	select a.*, b.*, c.*
	from scratch.world_msf as a  
	left join scratch.market_chars_m as b
	on a.id=b.id and a.eom=b.eom
	left join scratch.acc_chars_world as c
	on a.gvkey=c.gvkey and a.eom=c.public_date;

	alter table scratch.world_data_prelim 
	drop div_tot, div_cash, div_spc, public_date;
quit;

*****************************************************************************
* Asset Pricing Factors
**************************************************************************** ; 
* Create monthly and daily factors from FF3 and HXZ4;
%ap_factors(out=scratch.ap_factors_daily, freq=d, sf=scratch.world_dsf, mchars=scratch.world_data_prelim, mkt=scratch.market_returns_daily, min_stocks_bp=10, min_stocks_pf=3);	
%ap_factors(out=scratch.ap_factors_monthly, freq=m, sf=scratch.world_msf, mchars=scratch.world_data_prelim, mkt=scratch.market_returns, min_stocks_bp=10, min_stocks_pf=3);	

*****************************************************************************
* Factor based on combined data
**************************************************************************** ;
%firm_age(data=scratch.world_msf, out=scratch.firm_age);
%mispricing_factors(out=scratch.mp_factors, data=scratch.world_data_prelim, min_stks=10, min_fcts=3);	
%market_beta(out=scratch.beta_60m, data=scratch.world_msf, fcts=scratch.ap_factors_monthly, __n=60, __min=36);
%residual_momentum(out=scratch.resmom_ff3, data=scratch.world_msf, fcts=scratch.ap_factors_monthly, type=ff3, __n =36, __min=24, incl=12 6, skip=1 1);

* Free up space;
proc datasets library=work kill nolist; quit;

*****************************************************************************
* Create Characteristics Based on Daily Market Data
**************************************************************************** ; 
%bidask_hl(out=scratch.corwin_schultz, data=scratch.world_dsf, __min_obs=10); 
%prepare_daily(data=scratch.world_dsf, fcts=scratch.ap_factors_daily); 
%roll_apply_daily(out=scratch.roll_21d, __n=1, __min=15, fcts=scratch.ap_factors_daily,  __month_ends=month_ends, sfx =_21d, 
				  __stats= rvol rmax skew capm_ext ff3 hxz4 dimsonbeta zero_trades);
%roll_apply_daily(out=scratch.roll_126d, __n=6, __min=60, fcts=scratch.ap_factors_daily,  __month_ends=month_ends, sfx =_126d, 
				  __stats= zero_trades turnover dolvol ami);
%roll_apply_daily(out=scratch.roll_252d, __n=12, __min=120, fcts=scratch.ap_factors_daily,  __month_ends=month_ends, sfx =_252d, 
				  __stats= rvol capm downbeta zero_trades prc_to_high mktvol);
%roll_apply_daily(out=scratch.roll_1260d, __n=60, __min=750, fcts=scratch.ap_factors_daily,  __month_ends=month_ends, sfx =_1260d, 
				  __stats= mktcorr);
%finish_daily_chars(out=scratch.market_chars_d);

* Free up space;
proc datasets library=work kill nolist; quit;

*****************************************************************************
* Combine all characteristics and build final dataset
**************************************************************************** ; 
proc sql;
	create table world_data3 as
	select a.*, b.beta_60m, b.ivol_capm_60m, c.resff3_12_1, d.resff3_6_1, e.mispricing_mgmt, e.mispricing_perf, f.*, g.age
	from scratch.world_data_prelim as a 
	left join scratch.beta_60m as b on a.id=b.id and a.eom=b.eom
	left join scratch.resmom_ff3_12_1 as c on a.id=c.id and a.eom=c.eom
	left join scratch.resmom_ff3_6_1 as d on a.id=d.id and a.eom=d.eom
	left join scratch.mp_factors as e on a.id=e.id and a.eom=e.eom
	left join scratch.market_chars_d as f on a.id=f.id and a.eom=f.eom
	left join scratch.firm_age as g on a.id=g.id and a.eom=g.eom;
quit;

* Add Quality minus Junk;
%quality_minus_junk(out=scratch.qmj, data=world_data3, min_stks=10);
proc sql;
	create table world_data4 as 
	select a.*, b.qmj, b.qmj_prof, b.qmj_growth, b.qmj_safety
	from world_data3 as a left join scratch.qmj as b
	on a.excntry=b.excntry and a.id=b.id and a.eom=b.eom;
quit;

* Reorder Variables;
data world_data5;
	retain id date eom source size_grp obs_main exch_main primary_sec gvkey iid permno permco excntry curcd fx 
		common comp_tpci crsp_shrcd comp_exchg crsp_exchcd gics sic naics ff49
		adjfct shares me me_company prc prc_local dolvol ret ret_local ret_exc ret_lag_dif ret_exc_lead1m
		market_equity enterprise_value book_equity assets sales net_income; 
	set world_data4;
run;

* Save combined data;
proc sort data=world_data5 out=scratch.world_data nodup; by id eom; run;

/* Delete Temporary Files?*/
%if 1=1 %then %do;
	proc delete data=
		scratch.beta_60m scratch.qmj scratch.resmom_ff3_12_1 scratch.resmom_ff3_6_1 scratch.mp_factors
		scratch.ap_factors_daily scratch.ap_factors_monthly
		scratch.corwin_schultz scratch.roll_21d scratch.roll_126d scratch.roll_252d scratch.roll_1260d
		scratch.world_data_prelim scratch.world_dsf scratch.world_msf 
		scratch.market_chars_m scratch.market_chars_d scratch.acc_chars_world
		scratch.firm_age; 
	run;
%end;

*****************************************************************************
* Create Output in .csv Format for Download
**************************************************************************** ; 
* Create Output Folder;
options dlcreatedir;
libname op "&scratch_folder./output"; 

* Small Files;
proc export data=scratch.market_returns_daily
    outfile="&scratch_folder./output/market_returns_daily.csv"   
    dbms=CSV
    replace;
run;
proc export data=scratch.market_returns
    outfile="&scratch_folder./output/market_returns.csv"   
    dbms=CSV
    replace;
run;
proc export data=scratch.nyse_cutoffs
    outfile="&scratch_folder./output/nyse_cutoffs.csv"   
    dbms=CSV
    replace;
run;
proc export data=scratch.crsp_return_cutoffs
    outfile="&scratch_folder./output/crsp_return_cutoffs.csv"   
    dbms=CSV
    replace;
run;

* Save main data as .csv files by country;
%if 1=1 %then %do;
	%save_main_data_csv(out=world, data=scratch.world_data, path=&scratch_folder./output, end_date='31DEC2020'd); /* CRSP data is only updated anually, so we keep this updating frequency for consistency. Should be incremented everytime there's an update to the CRSP database)*/
%end;