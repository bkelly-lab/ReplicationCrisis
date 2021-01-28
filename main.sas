***************************************************************************
*                                  Settings
*************************************************************************** ; 
proc delete data = _all_ ; run ; 

/* Assign Libraries */
libname scratch "/scratch/cbs/tij"; /* REMEMBER TO ENSURE THAT FOLDER IS NOT DELETED!!*/
libname project "~/Global Data";

/* Include Project Macros */
%include "~/Global Data/project_macros.sas";
%include "~/Global Data/char_macros.sas";
%include "~/Global Data/market_chars.sas";
%include "~/Global Data/accounting_chars.sas";

*****************************************************************************
* Create Return Data
**************************************************************************** ; 
/* Prepare Data */
%prepare_comp_sf(freq=both);
%prepare_crsp_sf(freq=d);
%prepare_crsp_sf(freq=m);
%combine_crsp_comp_sf(out_msf=scratch.world_msf, out_dsf=scratch.world_dsf, crsp_msf=crsp_msf, comp_msf=comp_msf, crsp_dsf=crsp_dsf, comp_dsf=comp_dsf);

/* Create Market Returns */
%market_returns(out = scratch.market_returns, data = scratch.world_msf, freq=m, wins = 0.1);
%market_returns(out = scratch.market_returns_daily, data = scratch.world_dsf, freq=d, wins = 0.1);

*****************************************************************************
* Create Characteristics Based on Accounting Data
**************************************************************************** ;
%standardized_accounting_data(coverage='world', convert_to_usd=1, me_data = scratch.world_msf, include_helpers_vars=1, start_date='31DEC1949'd); 
%create_acc_chars(data=acc_std_ann, out=achars_world, lag_to_public=4, max_data_lag=18, __keep_vars=&char_vars., me_data=scratch.world_msf, suffix=);
%create_acc_chars(data=acc_std_qtr, out=qchars_world, lag_to_public=4, max_data_lag=18, __keep_vars=&char_vars., me_data=scratch.world_msf, suffix=_qitem);
%combine_ann_qtr_chars(out=acc_chars_world, ann_data=achars_world, qtr_data=qchars_world, __char_vars=&char_vars., q_suffix=_qitem);

* Save Accounting Data;
data scratch.acc_chars_world; set acc_chars_world; run;

*****************************************************************************
* Create Characteristics Based on Monthly Market Data
**************************************************************************** ;
%market_chars_monthly(out=scratch.market_chars_m, data = scratch.world_msf, market_ret = scratch.market_returns, local_currency=0); 

*****************************************************************************
* Combine Returns, Accounting and Monthly Market Data
**************************************************************************** ; 
proc sql;
	create table world_data1 as 
	select a.*, b.*, c.*
	from scratch.world_msf as a  
	left join scratch.market_chars_m as b
	on a.id=b.id and a.eom=b.eom
	left join scratch.acc_chars_world as c
	on a.gvkey=c.gvkey and a.eom=c.public_date;

	alter table world_data1 
	drop div_tot, div_cash, div_spc, public_date;
quit;

/* Add Size Group Indicator based on NYSE breakpoints */
%nyse_size_groups(out=world_data2, data=world_data1);
data project.nyse_cutoff; set nyse_cutoff;

/* Save preliminary dataset */
data scratch.world_data_prelim; set world_data2; run; 

*****************************************************************************
* Asset Pricing Factors
**************************************************************************** ; 
* Create monthly and daily factors from FF3 and HXZ4;
%ap_factors(out=scratch.ap_factors_monthly, freq=m, sf=scratch.world_msf, mchars=scratch.world_data_prelim, mkt=scratch.market_returns, min_stocks_bp=10, min_stocks_pf=5);	
%ap_factors(out=scratch.ap_factors_daily, freq=d, sf=scratch.world_dsf, mchars=scratch.world_data_prelim, mkt=scratch.market_returns_daily, min_stocks_bp=10, min_stocks_pf=5);	

*****************************************************************************
* Factor based on combined data
**************************************************************************** ;
%mispricing_factors(out=scratch.mp_factors, data=scratch.world_data_prelim, min_stks=10, min_fcts=3);	
%market_beta(out=scratch.beta_60m, data=scratch.world_msf, fcts=scratch.ap_factors_monthly, __n=60, __min=36);
%residual_momentum(out=scratch.resmom_ff3, data=scratch.world_msf, fcts=scratch.ap_factors_monthly, type=ff3, __n =36, __min=24, incl=12 6, skip=1 1);

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

*****************************************************************************
* Combine all characteristics and build final dataset
**************************************************************************** ; 
proc sql;
	create table world_data3 as
	select a.*, b.beta_60m, b.ivol_capm_60m, c.resff3_12_1, d.resff3_6_1, e.mispricing_mgmt, e.mispricing_perf, f.*
	from scratch.world_data_prelim as a 
	left join scratch.beta_60m as b on a.id=b.id and a.eom=b.eom
	left join scratch.resmom_ff3_12_1 as c on a.id=c.id and a.eom=c.eom
	left join scratch.resmom_ff3_6_1 as d on a.id=d.id and a.eom=d.eom
	left join scratch.mp_factors as e on a.id=e.id and a.eom=e.eom
	left join scratch.market_chars_d as f on a.id=f.id and a.eom=f.eom;
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
	retain id date eom source size_grp obs_main primary_sec gvkey iid permno permco excntry curcd fx 
		common comp_tpci crsp_shrcd comp_exchg crsp_exchcd
		adjfct shares me me_company prc prc_local dolvol ret ret_local ret_exc ret_lag_dif ret_exc_lead1m
		market_equity enterprise_value book_equity assets sales net_income; 
	set world_data4;
run;

* Save combined data;
proc sort data=world_data5 out=scratch.world_data nodup; by id eom; run;

/* Save Market Returns as .csv */
proc export data=scratch.market_returns
    outfile="~/Global Data/market_returns.csv"   
    dbms=CSV
    replace;
run;

/* Save main data as .csv files by country */
%save_main_data_csv(out=world, data=scratch.world_data, path=/scratch/cbs/tij);
