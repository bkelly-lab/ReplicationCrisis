***************************************************************************
*                     Characteritics to Extract 
*************************************************************************** ; 
/* Pure Accounting Based Characteristics */
%let acc_chars=
	/* Accounting Based Size Measures */
	assets sales book_equity net_income enterprise_value
	
	/* 1yr Growth */
	at_gr1 ca_gr1 nca_gr1 lt_gr1 cl_gr1 ncl_gr1 be_gr1 pstk_gr1 debt_gr1 sale_gr1 cogs_gr1 sga_gr1 opex_gr1
	
	/* 3yr Growth */
	at_gr3 ca_gr3 nca_gr3 lt_gr3 cl_gr3 ncl_gr3 be_gr3 pstk_gr3 debt_gr3 sale_gr3 cogs_gr3 sga_gr3 opex_gr3
	
	/* 1yr Growth Scaled by Assets */
	cash_gr1a inv_gr1a rec_gr1a ppeg_gr1a lti_gr1a intan_gr1a debtst_gr1a ap_gr1a
	txp_gr1a debtlt_gr1a txditc_gr1a coa_gr1a col_gr1a cowc_gr1a ncoa_gr1a ncol_gr1a nncoa_gr1a
	oa_gr1a ol_gr1a noa_gr1a fna_gr1a fnl_gr1a nfna_gr1a gp_gr1a ebitda_gr1a ebit_gr1a 
	ope_gr1a ni_gr1a nix_gr1a dp_gr1a ocf_gr1a fcf_gr1a nwc_gr1a
	eqnetis_gr1a dltnetis_gr1a dstnetis_gr1a dbnetis_gr1a netis_gr1a fincf_gr1a eqnpo_gr1a
	tax_gr1a
	div_gr1a eqbb_gr1a eqis_gr1a eqpo_gr1a capx_gr1a
	
	/* 3yr Growth Scaled by Assets */
	cash_gr3a inv_gr3a rec_gr3a ppeg_gr3a lti_gr3a intan_gr3a debtst_gr3a ap_gr3a
	txp_gr3a debtlt_gr3a txditc_gr3a coa_gr3a col_gr3a cowc_gr3a ncoa_gr3a ncol_gr3a nncoa_gr3a 
	oa_gr3a ol_gr3a fna_gr3a fnl_gr3a nfna_gr3a gp_gr3a ebitda_gr3a ebit_gr3a 
	ope_gr3a ni_gr3a nix_gr3a dp_gr3a ocf_gr3a fcf_gr3a nwc_gr3a
	eqnetis_gr3a dltnetis_gr3a dstnetis_gr3a dbnetis_gr3a netis_gr3a fincf_gr3a eqnpo_gr3a
	tax_gr3a
	div_gr3a eqbb_gr3a eqis_gr3a eqpo_gr3a capx_gr3a
	
	/* Investment */
	capx_at rd_at  
	
 	/* Profitability */
 	gp_sale ebitda_sale ebit_sale pi_sale ni_sale nix_sale ocf_sale fcf_sale  /* Profit Margins */
 	gp_at ebitda_at ebit_at fi_at cop_at							/* Return on Assets */ 
 	ope_be ni_be nix_be ocf_be fcf_be								/* Return on Book Equity */ 
 	gp_bev ebitda_bev ebit_bev fi_bev cop_bev						/* Return on Invested Capital */  
 	gp_ppen ebitda_ppen fcf_ppen									/* Return on Physical Capital */
 	
 	/* Issuance */
 	fincf_at netis_at eqnetis_at eqis_at dbnetis_at dltnetis_at dstnetis_at
 	
 	/* Equity Payout */
 	eqnpo_at eqbb_at div_at
 	
 	/* Accruals */
 	oaccruals_at oaccruals_ni taccruals_at taccruals_ni noa_at
 	
 	/* Capitalization/Leverage Ratios */
	be_bev debt_bev cash_bev pstk_bev debtlt_bev debtst_bev
	debt_mev pstk_mev debtlt_mev debtst_mev
	
	/* Financial Soundness Ratios */
	int_debtlt int_debt cash_lt inv_act rec_act  			
	ebitda_debt debtst_debt cl_lt debtlt_debt profit_cl ocf_cl		
	ocf_debt lt_ppen debtlt_be fcf_ocf
	opex_at nwc_at
	
	/* Solvency Ratios */
 	debt_at debt_be ebit_int
 	
 	/* Liquidity Ratios */
 	cash_cl caliq_cl ca_cl
 	inv_days rec_days ap_days cash_conversion 
 	
 	/* Activity/Efficiency Ratio */
 	inv_turnover at_turnover rec_turnover ap_turnover	
 	
 	/* Non-Recurring Items */
 	spi_at xido_at nri_at
 	
	/* Miscalleneous */
	adv_sale staff_sale rd_sale div_ni sale_bev sale_be sale_nwc tax_pi
	
	/* Balance Sheet Fundamentals to Market Equity */
	be_me at_me cash_me
	
	/* Income Fundamentals to Market Equity */
	gp_me ebitda_me ebit_me ope_me ni_me nix_me sale_me ocf_me fcf_me cop_me
	rd_me
	
	/* Equity Payout/issuance to Market Equity */
	div_me eqbb_me eqis_me eqpo_me eqnpo_me eqnetis_me
	
	/* Debt Issuance to Market Enterprice Value */
	dltnetis_mev dstnetis_mev dbnetis_mev
	
	/* Firm Payout/issuance to Market Enterpice Value */
	netis_mev
	
	/* Balance Sheet Fundamentals to Market Enterprise Value */
	at_mev be_mev bev_mev ppen_mev cash_mev
	
	/* Income/CF Fundamentals to Market Enterprise Value */
	gp_mev ebitda_mev ebit_mev cop_mev sale_mev ocf_mev fcf_mev fincf_mev
	
	/* New Variables from HXZ */ 
	ni_inc8q ppeinv_gr1a lnoa_gr1a capx_gr1 capx_gr2 capx_gr3 sti_gr1a
	niq_at niq_at_chg1 niq_be niq_be_chg1 saleq_gr1 rd5_at
	dsale_dinv dsale_drec dgp_dsale dsale_dsga
	saleq_su niq_su debt_me netdebt_me capex_abn inv_gr1 be_gr1a
	op_at pi_nix op_atl1 gp_atl1 ope_bel1 cop_atl1
	at_be ocfq_saleq_std  
	aliq_at aliq_mat tangibility
	eq_dur f_score o_score z_score kz_index intrinsic_value ival_me
	sale_emp_gr1 emp_gr1 cash_at
	earnings_variability ni_ar1 ni_ivol
	
	/* New Variables not in HXZ */
	niq_saleq_std ni_emp sale_emp ni_at
	ocf_at ocf_at_chg1
	roeq_be_std roe_be_std
	gpoa_ch5 roe_ch5 roa_ch5 cfoa_ch5 gmar_ch5
;

%put ### In total %nwords(&acc_chars.) characteristics will be created ###;

**********************************************************************************************************************
*                                    MACRO - Add Helper Vars to Standardized Compustat Accounting 
*********************************************************************************************************************
Description: 
   The main functionality of this macro is to take the output of %compustat_accounting_data and add helper variables.
   These helper variables have two main functionalities:
	1. They are used to expand coverage of a given variable 
	2. They are used to create variables not directly available from the accounting statements
   All added variables have a suffix of '_x'.
;

%macro add_helper_vars(data=, out=);
	/* First ensure that the gap between two dates is always one month */
	proc sql;
		create table __comp_dates1 as
		select gvkey, curcd, min(datadate) as start_date, max(datadate) as end_date
		from &data.
		group by gvkey, curcd;
	quit;
	
	%expand(data=__comp_dates1, out=__comp_dates2, id_vars=gvkey, start_date=start_date, end_date=end_date, freq='month', new_date_name=datadate);
	
	proc sql;
		create table __helpers1 as 
		select a.gvkey, a.curcd, a.datadate, b.*, not missing(b.gvkey) as data_available
		from __comp_dates2 as a
		left join &data. as b on a.gvkey=b.gvkey and a.curcd=b.curcd and a.datadate=b.datadate;
	quit;
	
	proc sort nodupkey data=__helpers1; by gvkey curcd datadate; run;
	
	data __helpers2;
		set __helpers1;
		by gvkey curcd;
		retain count;
		if first.curcd then 
			count = 1;
		else
			count = count+1;
	run;
	
	/* Create Helper Variables */
	data &out.; 
		set __helpers2;
		by gvkey curcd;
		
		/* Require Certain Variables to Be Positive */
		array var_pos at sale revt dv che;
		do over var_pos;
			if var_pos<0 then
				var_pos = .;
		end;

		/* X Variables to Maximize Coverage */
		*Income Statement;
		sale_x			= coalesce(sale, revt); /* in NA sale has better coverage in Global revt has better coverage. They are the same though*/
		gp_x 			= coalesce(gp, sale_x-cogs); /*Gross Profit*/
		opex_x			= coalesce(xopr, cogs+xsga); /* Operating Expenses */
		ebitda_x 		= coalesce(ebitda, oibdp, sale_x-opex_x, gp_x-xsga); /*Operating Income Before Depreciation*/
		ebit_x 			= coalesce(ebit, oiadp, ebitda_x-dp); /*Operating Income Before Depreciation*/
		op_x            = ebitda_x + coalesce(xrd, 0);  /* Operating Profit ala Ball et al (2015)*/
		ope_x			= ebitda_x-xint; /* Operating Profit to Equity ala FF*/
		pi_x			= coalesce(pi, ebit_x-xint+coalesce(spi,0)+coalesce(nopi,0));  /* Interest Income is included in NOPI*/
		xido_x			= coalesce(xido, xi+coalesce(do, 0));
		ni_x			= coalesce(ib, ni-xido_x, pi_x - txt - coalesce(mii, 0)); 
		nix_x			= coalesce(ni, ni_x+coalesce(xido_x, 0), ni_x + xi + do);
		fi_x			= nix_x+xint;		/* Firm income i.e. return to equity and debt holders */  
		div_x			= coalesce(dvt, dv); /* See [1] */
		
		* Cash Flow Statement;
		eqbb_x			= sum(prstkc, purtshr);  /* Equity Buyback is mainly PRSTKC in NA and PURTSHR in GLOBAL. Using sum() means that any of the two inputs are allowed to be missing */	
		eqis_x			= sstk;  /* Equity Issuance is SSTK which is common+preferred Stocks.*/	
		eqnetis_x		= sum(eqis_x,-eqbb_x);  /* Net Equity Issuance. Using sum means that the variable will be computed as long as one of the inputs are non-missing */
		eqpo_x			= div_x+eqbb_x;  /* Net Equity Payout= Div+Buyback-Issuance*/
		eqnpo_x			= div_x-eqnetis_x;  /* Net Equity Payout= Div+Buyback-Issuance*/
		dltnetis_x		= coalesce(sum(dltis,-dltr), ltdch, dif12(dltt)); /* Net Long Term Debt issuance. GLOBAL firms only have LTDCH, NA firms only have dltis and DLTR. If cash flow items are missing. Approximate by the change in long term book debt */
		if missing(dltis) and missing(dltr) and missing(ltdch) and count<=12 then
			dltnetis_x	= .; 
		dstnetis_x		= coalesce(dlcch, dif12(dlc)); /* Prefer dlcch. If this is missing, approximate by the change in short term book debt */
		if missing(dlcch) and count<=12 then
			dstnetis_x	= .; 
		dbnetis_x		= sum(dstnetis_x, dltnetis_x);
		netis_x			= eqnetis_x+dbnetis_x;  /* I require that both equity and debt issuance are available */
		fincf_x			= coalesce(fincf, netis_x-dv+coalesce(fiao, 0)+coalesce(txbcof, 0)); /* https://wrds-web.wharton.upenn.edu/wrds/support/Data/_001Manuals%20and%20Overviews/_001Compustat/_001North%20America%20-%20Global%20-%20Bank/_000dataguide/index.cfm?_ga=2.179697184.565214677.1585224362-1543109641.1544636729 */
		
		* Balance Sheet Statement;
		debt_x			= sum(dltt, dlc); /* This greatly expands coverage */
		pstk_x			= coalesce(pstkrv, pstkl, pstk); /* Value of Preferred Stock*/
		seq_x			= coalesce(seq, ceq+coalesce(pstk_x, 0), at-lt);
		at_x            = coalesce(at, seq_x + dltt + coalesce(lct, 0) + coalesce(lo, 0) + coalesce(txditc, 0));  /* at is only available yearly in the beginning of the fundq data whereas seq and dltt is available on a quarterly basis from the beginning */  
		ca_x			= coalesce(act, rect+invt+che+aco); /* Current Assets */
		nca_x			= at_x-ca_x; /* Non-Current Assets */
		cl_x			= coalesce(lct, ap+dlc+txp+lco); /* Current Liabilities */
		ncl_x			= lt-cl_x;
		
		netdebt_x		= debt_x - coalesce(che, 0); /* Net Debt for calculating Enterprise Value */
		txditc_x		= coalesce(txditc, sum(txdb, itcb));
		be_x			= seq_x+coalesce(txditc_x, 0)-coalesce(pstk_x, 0); /* The last one is sort of controversial in the sense that it is not what FF does*/
		bev_x			= coalesce(icapt+coalesce(dlc, 0)-coalesce(che, 0), netdebt_x+seq_x+coalesce(mib, 0));
		
		coa_x			= ca_x - che;  /* Operating (non cash) current assets */
		col_x			= cl_x-coalesce(dlc, 0);  /* Operating Current Liabilities */
		cowc_x			= coa_x-col_x;  /* Current Operating Working Capital */
		ncoa_x			= at_x-ca_x-coalesce(ivao, 0); /* Non-Current Operating Assets */
		ncol_x			= lt-cl_x-dltt; /* Non-Current Operating Liabilities */
		nncoa_x			= ncoa_x-ncol_x; /* Net Non-Current Operatng Assets */
		fna_x			= coalesce(ivst,0)+coalesce(ivao,0); /* Financial Assets */
		fnl_x			= debt_x+coalesce(pstk_x,0); /* Financial Liabilities */
		nfna_x			= fna_x-fnl_x; /* Net Financial Assets */
		oa_x			= coa_x+ncoa_x; /* Operating Assets */
		ol_x			= col_x+ncol_x; /* Operating Liabilities */
		noa_x			= oa_x-ol_x; /* Net Operating Assets*/
		lnoa_x          = ppent+intan+ao-lo+dp;  /* Long-term NOA (from HXZ A.3.5)*/
		
		caliq_x			= coalesce(ca_x-invt, che+rect); /* Liquid current assets use for quick ratio*/
		nwc_x			= ca_x-cl_x;
		ppeinv_x        = ppegt + invt;  * Should be moved to create_chars as it is a helper variables;
		aliq_x          = che + 0.75 * coa_x + 0.5 * (at_x - ca_x - coalesce(intan, 0));  /* From Ortiz-Molina and Phillips (2014). Don't subtract gdwl since this is already included intangibles */
		
		array var_bs be_x bev_x;
		do over var_bs;
			if var_bs<=0 then
				var_bs	= .;
		end;
		
		/* Accruals + OCF/FCF + Cash Based Operating Profit*/
		oacc_x			= coalesce(ni_x-oancf, dif12(cowc_x)+dif12(nncoa_x));	/* Operating Accruals: Difference between Accounting Earnings and Operating Cash Flow. [2] */
		tacc_x			= oacc_x+dif12(nfna_x);      /* Total Accruals = Accural Earnings - Cash Earnings = Change in Non-Cash Assets - Change in Liabilities. */
		if count<=12 then do;
			oacc_x 		= .;
			tacc_x		= .;
		end;
		ocf_x           = coalesce(oancf, ni_x-oacc_x, ni_x + dp - coalesce(wcapt, 0)); 
		fcf_x			= ocf_x-capx; /* Note that this does not include funds from financing activities */
		cop_x			= ebitda_x + coalesce(xrd, 0) - oacc_x;  /* Cash Based Operating Profitability (Gerakos et al, 2016) add R&D while subtracting accruals */
		
		drop count;
	run;		
	
	proc delete data= __comp_dates1 __comp_dates2 __helpers1 __helpers2; run;
%mend add_helper_vars;

**********************************************************************************************************************
*                                    MACRO - Compustat Accounting Data Standardized 
*********************************************************************************************************************
Description: 
   The main functionality of this macro is to create accounting datasets which are 
   comparable across frequency (quarterly/annual) and geography (North America/Global).
   To make the data comparable across frequency, we modify the quarterly data by: 
   	- Quarterize year-to-date variables.  
   	- Take the sum over the last 4 quarters for income and cash flow variables
	- Change name to be consistent with the annual data
   To make the data comparable across geography, we modify the global data by:
 	- Create columns available in the North American dataset in the global dataset.
 	  If possible we infer the values from available columns. Otherwise we just set to missing.
 	- If specified, we change all data to USD.
;


%macro standardized_accounting_data(coverage=, convert_to_usd=, me_data=, include_helpers_vars=1, start_date='01JAN1950'd);  /* Coverage in ('na', 'global', 'world'), convert_to_usd in (0,1), include_helpers_vars in (0,1)*/
	/* Compustat Accounting Vars to Extract */
	%let avars_inc =
		sale revt gp ebitda oibdp ebit oiadp  pi ib ni mii
		cogs xsga xopr xrd xad xlr dp xi do xido xint spi nopi txt
		dvt
	;
	%let avars_cf =
		/* Operating */
		oancf ibc dpc xidoc capx ibc dpc wcapt
		
		/* Financing */
		fincf fiao txbcof ltdch dltis dltr dlcch purtshr prstkc sstk
		dv dvc
	;
	%let avars_bs =
		/* Assets */
		at act aco che invt rect ivao ivst ppent ppegt intan ao gdwl re
		
		/* Liabilities */
		lt lct dltt dlc txditc txdb itcb txp ap lco lo 
		seq ceq pstkrv pstkl pstk mib icapt 
	;
	
	* Variables in avars_other are not measured in currency units, and only available in annual data;
	%let avars_other = emp; 
	
	%let avars=	&avars_inc. &avars_cf. &avars_bs.; 	/* fdate and pdate. Unfortunately only available from 2007. RDQ is available further back for NA but should probably use 10K dates instead???*/	
	
	%put INCOME STATEMENT: %nwords(&avars_inc.) || CASH FLOW STATEMENT: %nwords(&avars_cf.) || BALANCE SHEET: %nwords(&avars_bs.) || OTHER: %nwords(&avars_other.);
	
	proc sql noprint;
		select distinct lowcase(name) into :qvars_q separated by ' '
		from dictionary.columns
		where libname='COMP' and memname in ('FUNDQ', 'G_FUNDQ') and memtype='DATA'
		and findw(lowcase("&avars."),substr(lowcase(name),1,length(name)-1))>0
		and name like "%nrstr(%%)q"; /* Quarterly names ending with q */
	quit;
	
	proc sql noprint;
		select distinct lowcase(name) into :qvars_y separated by ' '
		from dictionary.columns
		where libname='COMP' and memname in ('FUNDQ', 'G_FUNDQ') and memtype='DATA'
		and findw(lowcase("&avars."),substr(lowcase(name),1,length(name)-1))>0
		and name like "%nrstr(%%)y"; /* Quarterly names ending with y (ytd_variables)*/
	quit;
	
	%let qvars = &qvars_q. &qvars_y.;
	
	* In International but not in NA: dvtq dvty purtshr purtshry ltdch ltdchy    LOC?;
	
	%if &coverage. = 'global' or &coverage. = 'world' %then %do;
		%let aname = __gfunda;
		%let qname = __gfundq;
		%let compcond=indfmt in ('INDL', 'FS') and datafmt='HIST_STD' and popsrc='I' and consol='C';
		data g_funda1; 
			set comp.g_funda;
			where &compcond. and datadate>=&start_date.; 
			source = 'GLOBAL'; format source $char6.;
			
			* Variables Not Available in G_FUNDA with Replacement;
			ni 			= ib+coalesce(xi,0)+coalesce(do,0);   /*https://wrds-www.wharton.upenn.edu/pages/support/support-articles/compustat/global/ni-net-income-variable/?_ga=2.199276522.59544135.1581901991-1543109641.1544636729 */
			
			* Variables Not Available in G_FUNDA without Replacement;
			gp 			= .;
			pstkrv		= .; /*captured by pstk_x*/
			pstkl		= .; /*captured by pstk_x*/
			itcb 		= .; /*Only used as substitute for txditc (deffered tax and investment credit)*/
			xad			= .;
			txbcof		= .; /* Part of FINCF but will automatically be set to zero*/
			keep gvkey datadate indfmt curcd source &avars. &avars_other.;
		run;
		proc sql;
			create table &aname. as
			select *
			from g_funda1
			group by gvkey, datadate
			having count(*)=1 or (count(*)=2 and indfmt='INDL');  /* If the accouting report is available inb both an industrial and financial format, choose financial format (happens very rarely in the international data)*/ 
			
			alter table &aname.
			drop indfmt;
		quit;
		
		data g_fundq1;
			set comp.g_fundq;
			where &compcond. and datadate>=&start_date.;
			source = 'GLOBAL'; format source $char6.;
			
			* Variables Not Available in G_FUNDQ with Replacement;
			niq			= ibq+coalesce(xiq, 0); /*Discontinued Operations is not available in g_fundq*/
			ppegtq		= ppentq+dpactq;  /* See [3] */
			
			* Variables Not Available in G_FUNDQ without Replacement;
			icaptq		= .;
			niy			= .;
			txditcq		= .;
			txpq		= .; 
			xidoq		= .; 
			xidoy		= .;
			xrdq		= .;
			xrdy		= .;
			txbcofy		= .; /* Part of FINCF but will automatically be set to zero*/
			keep gvkey datadate indfmt fyr fyearq fqtr curcdq source &qvars.; 
		run;
		proc sql;
			create table &qname. as
			select *
			from g_fundq1
			group by gvkey, datadate
			having count(*)=1 or (count(*)=2 and indfmt='INDL');  /* If the accouting report is available inb both an industrial and financial format, choose financial format (happens very rarely in the international data)*/ 
			
			alter table &qname.
			drop indfmt;
		quit;
	%end;
	
	%if &coverage. = 'na' or &coverage. = 'world' %then %do;
		%let aname = __funda;
		%let qname = __fundq;
		%let compcond=indfmt='INDL' and datafmt='STD' and popsrc='D' and consol='C';
		data &aname.; 
			set comp.funda; 
			where &compcond. and datadate>=&start_date.;
			source = 'NA'; format source $char6.;
			keep gvkey datadate curcd source &avars. &avars_other.; 
		run;
		data &qname.; 
			set comp.fundq; 
			where &compcond. and datadate>=&start_date.;
			source = 'NA'; format source $char6.;
			keep gvkey datadate fyr fyearq fqtr curcdq source &qvars.; 
		run;
	%end;	
	%if &coverage. = 'world' %then %do;
		%let aname = __wfunda;
		%let qname = __wfundq;
		data &aname.;
			set __gfunda __funda;
		run;
		data &qname.;
			set __gfundq __fundq;
		run;
		/*proc delete data= __gfunda __gfundq __funda __fundq; run;*/
	%end;

	/* If &convert_to_usd=1 then convert everything to USD otherwise keep as local currency*/
	%if &convert_to_usd.=1 %then %do;
		%compustat_fx(out=fx);
		
		proc sql;
			create table __tempa as 
			select a.*, b.fx
			from &aname. as a left join fx as b
			on a.datadate=b.date and a.curcd=b.curcdd;
			
			create table __tempq as 
			select a.*, b.fx
			from &qname. as a left join fx as b
			on a.datadate=b.date and a.curcdq=b.curcdd;
		quit;
		
		data __compa1;
			set __tempa;
			array var &avars.;
			do over var;
				var = var*fx;
			end;
			curcd = 'USD';
			drop fx;
		run;
		
		data __compq1;
			set __tempq;
			array var &qvars.;
			do over var;
				var = var*fx;
			end;
			curcdq = 'USD';
			drop fx;
		run; 
		
		proc delete data=fx __tempa __tempq;
	%end;
	%else %do;
		*Rename Data;
		proc sql;
			create table __compa1 as select * from &aname.; 
			create table __compq1 as select * from &qname.;
		quit;
	%end;
	
	proc delete data= &aname. &qname.;
	 
	/* Change Quarterly Data to Be Comparable to Annual Data */
	%QUARTERIZE(inset=__compq1, outset=__compq2, idvar=gvkey fyr, fyear=fyearq, fqtr=fqtr); /* Quarterize the YTD flow accounting variables */
	proc sort data=__compq2 nodupkey; by gvkey fyr fyearq fqtr; run; /*THEIS: 0 Deleted in the US*/
	%macro ttm(var); (&var + lag1(&var) + lag2(&var) + lag3(&var)) %mend; /* Note that ttm will return missing if either of the lags are missing. This is the behavior we want. */

	%macro temp();
	/* Prepare quarterly data: if quarterly Compustat variable is missing, replace with quarterized version*/
	data __compq3; 
		set __compq2;
		by gvkey fyr fyearq fqtr;
		
		/* Replace missing &var.q with quarterized version &var.y_q*/
		%do i=1 %to %nwords(&qvars_y.); 
			%let var_ytd = %scan(&qvars_y., &i, %str(' '));
			%let var = %sysfunc(prxchange(s/y$//, 1, &var_ytd.));
			if missing(&var.q) then
				&var.q = &var.y_q;
			drop &var.y_q;
		%end;	
		
		/* Create Quarterly Variables to Keep */
		ni_qtr = ibq;
		sale_qtr = saleq;
		ocf_qtr = coalesce(oancfq, ibq + dpq - coalesce(wcaptq, 0));
	
		*Cumulate Income/CF Items Over 4 Quarters (This should be made automatic at some point;
		%let yrl_vars = cogsq xsgaq xintq dpq txtq xrdq dvq spiq saleq revtq cogsq 
			xoprq oibdpq oiadpq ibq niq xidoq nopiq miiq piq xiq
			xidocq capxq oancfq ibcq dpcq wcaptq
			prstkcq sstkq purtshrq
			dsq dltrq ltdchq dlcchq
			fincfq fiaoq txbcofq dvtq;
		
		%do i=1 %to %nwords(&yrl_vars.);
			%let var_yrl = %scan(&yrl_vars., &i, %str(' '));
			%let var_yrl_name = %sysfunc(prxchange(s/q$//, 1, &var_yrl.));  
			&var_yrl_name. = %ttm(&var_yrl.);
			if (gvkey^=lag3(gvkey) or fyr^=lag3(fyr) or curcdq^=lag3(curcdq) or %ttm(fqtr)^=10) then 
				&var_yrl_name. = .;
			if missing(&var_yrl_name.) and fqtr=4 then
				&var_yrl_name. = &var_yrl_name.y;  * If financial quarter is 4, the ytd variable is yearly;
			drop &var_yrl. &var_yrl_name.y;
		%end;
		
		* Rename All Other (Balance Sheet and CURCDQ) Items to Facilitate Merge (This should be made automatic at some point);
		%let bs_vars = seqq ceqq pstkq icaptq mibq gdwlq req 
			atq actq invtq rectq ppegtq ppentq aoq acoq intanq cheq ivaoq ivstq 
			ltq lctq dlttq dlcq txpq apq lcoq loq txditcq txdbq;
		%do i=1 %to %nwords(&bs_vars.);
			%let var_bs = %scan(&bs_vars., &i, %str(' '));
			%let var_bs_name = %sysfunc(prxchange(s/q$//, 1, &var_bs.));  
			rename &var_bs. = &var_bs_name.;
		%end;
		rename curcdq = curcd;
		
	run;
	%mend;
	%temp();
	
	/* Ensure One Obs pr. Datadate */
	proc sort data=__compq3 nodupkey; by gvkey datadate fyr; run;
	data __compq4; set __compq3; by gvkey datadate; if last.datadate; drop fyr fyearq fqtr; run; /*THEIS: US->1432 Observations are deleted in this step*/	
	
	/* Add empty quarterly variables to annual data */
	data __compa2;
		set __compa1;
		ni_qtr = .;
		sale_qtr = .;
		ocf_qtr = .;
	run;
	
	/* Add Market Equity at Fiscal End */
	proc sql;
		create table __me_data as 
		select distinct gvkey, eom, me_company as me_fiscal 
		from &me_data.
		where not missing(gvkey) and primary_sec=1 and not missing(me_company) and common=1 and obs_main=1 /* Notice, exch_main is not a requirement */
		group by gvkey, eom
		having me_company=max(me_company);
	quit; 
	
	proc sql;
		create table __compa3 as 
		select a.*, b.me_fiscal
		from __compa2 as a 
		left join __me_data as b
		on a.gvkey = b.gvkey and a.datadate = b.eom;
	quit;
	
	proc sql;
		create table __compq5 as 
		select a.*, b.me_fiscal
		from __compq4 as a 
		left join __me_data as b
		on a.gvkey = b.gvkey and a.datadate = b.eom;
	quit;
	
	/* Include Helper Variables */
	%if &include_helpers_vars.=1 %then %do;
		%let qdata = __compq6;
		%let adata = __compa4;
		%add_helper_vars(data = __compq5, out=&qdata.);
		%add_helper_vars(data = __compa3, out=&adata.);
		proc delete data=__compq5 __compa3; run;
	%end;
	%else %do;
		%let qdata = __compq5;
		%let adata = __compa3;
	%end;
	
	/* Output */
	proc sort data= &adata. out=acc_std_ann nodupkey; by gvkey datadate; run;
	proc sort data= &qdata. out=acc_std_qtr nodupkey; by gvkey datadate; run;

	proc delete data=__compq1 __compq2 __compq3 __compq4 __compa1 __compa2 &qdata. &adata.; run; 
%mend standardized_accounting_data;








**********************************************************************************************************************
*                  MACRO - Create Accounting Characteristics from Compustat Standardized Data 
*********************************************************************************************************************
Description: 
   The main functionality of this macro is to take the output from %standardized_accounting_data and create characteristics 
   that require accounting data.  
   !!! When we are satisfied with the data, we should really include labels for all accounting characteristics !!!
;
%macro create_acc_chars(data=, out=, lag_to_public=, max_data_lag=, __keep_vars=, me_data=, suffix=);
	/* Helper Macros */
	%macro mean_year(var); mean(&var, lag12(&var)) %mend; /* Note that this function will still return a value even if one of the lags are missing. To change this do (var+lag12(var))/2. Could also consider if we should take the average of all observable observations? ie var+lag1(var)+lag2(var)...+lag12(var)*/
	
	%macro apply_to_lastq(x=, _qtrs=, func=); /* The Macro Below is a Generic way of creating lag from current to &n*/
		%let mv = &func.(&x.; 
		%do _i=1 %to &_qtrs.-1;
			%let mv = &mv., lag%eval(&_i.*3)(&x.);
		%end;
		%let mv = &mv.);
		&mv.;
	%mend apply_to_lastq;
	
	%macro apply_to_lasty(x=, yrs=, func=); /* The Macro Below is a Generic way of creating ANNUAL lags from current to &n */
		%let mv = &func.(&x.; 
		%do _i=1 %to &yrs.-1;
			%let mv = &mv., lag%eval(&_i.*12)(&x.);
		%end;
		%let mv = &mv.);
		&mv.;
	%mend apply_to_lasty;
	
	/* Start Procedure */
	proc sort data=&data. out=__chars3; by gvkey curcd datadate; run; /* deleted _chars1 and __chars2 steps*/
	
	data __chars4;
		set __chars3;
		by gvkey curcd;
		retain count;
		if first.curcd then 
			count = 1;
		else
			count = count+1;
	run;
	
	/* Create Accounting Characteristics */
	data __chars5; 
		set __chars4;
		by gvkey curcd;
		
		/* Accounting Based Size Measures */
		assets = at_x;
		sales = sale_x;
		book_equity = be_x;
		net_income = ni_x;
	
		/* Growth Characteristics */
		%let growth_vars =
			at_x ca_x nca_x 									/* Assets - Aggregated */
			lt cl_x ncl_x  									/* Liabilities - Aggregated */
			be_x pstk_x debt_x 								/* Financing Book Values */
			sale_x cogs xsga opex_x  						/* Sales and Operating Costs */
			capx invt
		;	
		
		* 1yr Growth;
		%do i=1 %to %nwords(&growth_vars.);
			%let var_gr1 = %scan(&growth_vars., &i, %str(' '));
			%var_growth(var_gr=&var_gr1., horizon=12);
		%end;
		* 3yr Growth;
		%do i=1 %to %nwords(&growth_vars.);
			%let var_gr3 = %scan(&growth_vars., &i, %str(' '));
			%var_growth(var_gr=&var_gr3., horizon = 36);
		%end;
		
		/* Change Scaled by Asset Characteristics */
		%let ch_asset_vars = 
			che invt rect ppegt ivao ivst intan 				/* Assets - Individual Items */
			dlc ap txp dltt txditc 								/* Liabilities - Individual Items*/
			coa_x col_x cowc_x ncoa_x ncol_x nncoa_x 			/* Operating Assets/Liabilities */
			oa_x ol_x									/* Operating Assets/Liabilities */
			fna_x fnl_x nfna_x									/* Financial Assets/Liabilities */
			gp_x ebitda_x ebit_x ope_x ni_x nix_x dp 			/* Income Statement*/
			fincf_x ocf_x fcf_x nwc_x 							/* Aggreagted Cash Flow */
			eqnetis_x dltnetis_x dstnetis_x dbnetis_x netis_x	/* Financing Cash Flow */
			eqnpo_x
			txt													/* Tax Change */
			eqbb_x eqis_x div_x eqpo_x						/* Financing Cash Flow */
			capx be_x
		;
		* 1yr Change Scaled by Assets;
		%do i=1 %to %nwords(&ch_asset_vars.);
			%let var_gr1a = %scan(&ch_asset_vars., &i, %str(' '));
			%chg_to_assets(var_gra = &var_gr1a., horizon = 12);
		%end;
		
		* 3yr Change Scaled by Assets;
		%do i=1 %to %nwords(&ch_asset_vars.);
			%let var_gr3a = %scan(&ch_asset_vars., &i, %str(' '));
			%chg_to_assets(var_gra = &var_gr3a., horizon = 36);
		%end;
		
		/* Investment Measure */	
		capx_at			= capx/at_x;  
		rd_at			= xrd/at_x;
		
		
		/* Non-Recurring Items */
		spi_at			= spi/at_x;
		xido_at			= xido_x/at_x;
		nri_at			= (spi+xido_x)/at_x;					/* Non-Recurring Items */
		
		/*Profitability Ratios and Rates of Return*/
		* Profit Margins; 
		gp_sale			= gp_x/sale_x; 							/* Gross Profit Margin*/                                  
		ebitda_sale		= ebitda_x/sale_x; 						/* Operating Profit Margin before Depreciation */
		ebit_sale		= ebit_x/sale_x; 						/* Operating profit Margin after Depreciation */                                 
		pi_sale			= pi_x/sale_x;  						/* Pretax Profit Margin */                                         
		ni_sale			= ni_x/sale_x;							/* Net Profit Margin Before XI */
		nix_sale		= ni/sale_x;							/* Net Profit Margin */
		ocf_sale		= ocf_x/sale_x;  						/* Cash Flow Margin */       
		fcf_sale		= fcf_x/sale_x;
		
		* Return on Assets;
		gp_at			= gp_x/at_x;
		ebitda_at		= ebitda_x/at_x;
		ebit_at			= ebit_x/at_x; 	
		fi_at			= fi_x/at_x;
		cop_at			= cop_x/at_x;
		ni_at           = ni_x/at_x;
		
		* Return on Book Equity;
		ope_be			= ope_x/be_x; 													
		ni_be			= ni_x/be_x; 								
		nix_be			= nix_x/be_x;
		ocf_be			= ocf_x/be_x;
		fcf_be			= fcf_x/be_x;
		
		* Return on Invested Book Capital;
		gp_bev			= gp_x/bev_x;
		ebitda_bev		= ebitda_x/bev_x;
		ebit_bev		= ebit_x/bev_x; 					/* Pre tax Return on Book Enterprise Value */
		fi_bev 			= fi_x/bev_x; 						/* ROIC */
		cop_bev			= cop_x/bev_x;						/* Cash Based Operating Profit to Invested Capital */
		
		* Return on Physical Capital;
		gp_ppen			= gp_x/ppent;
		ebitda_ppen		= ebitda_x/ppent;
		fcf_ppen		= fcf_x/ppent;
		
		* Issuance Variables;
		fincf_at		= fincf_x / at_x;
		netis_at		= netis_x / at_x;
		eqnetis_at		= eqnetis_x / at_x;
		eqis_at			= eqis_x / at_x;
		dbnetis_at		= dbnetis_x / at_x;
		dltnetis_at		= dltnetis_x / at_x;
		dstnetis_at		= dstnetis_x / at_x;
		
		/* Equity Payout */
		eqnpo_at		= eqnpo_x / at_x;
		eqbb_at			= eqbb_x / at_x;
		div_at			= div_x / at_x;
		
		* Accruals;
		oaccruals_at 	= oacc_x/at_x;							/* Operating Accruals */
		oaccruals_ni 	= oacc_x/abs(nix_x);					/* Percent Operating Accruals */
		taccruals_at 	= tacc_x/at_x;							/* Total Accruals */
		taccruals_ni 	= tacc_x/abs(nix_x);					/* Percent Total Accruals */
		noa_at			= noa_x/lag12(at_x);					/* Net Operating Asset to Total Assets*/
		if count <= 12 or lag12(at_x) <= 0 then do;
			noa_at = .;
		end;
		
		/*Capitalization/Leverage Ratios Book*/
		be_bev 			= be_x/bev_x;   						/* Common Equity as % of Book Enterprise Value*/
		debt_bev 		= debt_x/bev_x;  						/* Total Debt as % of Book Enterprise Value*/
		cash_bev		= che/bev_x;							/* Cash and Short-Term Investments to Book Enterprise Value */
		pstk_bev		= pstk_x/bev_x;							/* Prefered Stock to Book Enterprise Value */
		debtlt_bev		= dltt/bev_x;    						/* Long-term debt as % of Book Enterprise Value */
		debtst_bev		= dlc/bev_x;							/* Short-term debt as % of Book Enterprise Value */
			
		/*Financial Soundness Ratios*/
		int_debt 		= xint/debt_x; 							/* Interest as % of average total debt*/
		int_debtlt 		= xint/dltt; 							/* Interest as % of average long-term debt*/
		ebitda_debt 	= ebitda_x/debt_x; 						/* Ebitda to total debt*/
		profit_cl		= ebitda_x/cl_x; 						/* Profit before D&A to current liabilities*/
		ocf_cl			= ocf_x/cl_x; 							/* Operating cash flow to current liabilities*/
		ocf_debt		= ocf_x/debt_x;							/* Operating cash flow to total debt*/
		cash_lt 		= che/lt; 								/* Cash balance to Total Liabilities*/
		inv_act 		= invt/act; 															/*inventory as % of current assets*/
		rec_act 		= rect/act; 															/*receivables as % of current assets*/
		debtst_debt 	= dlc/debt_x; 														/*short term term as % of total debt*/
		cl_lt			= cl_x/lt; 																/*current liabilities as % of total liabilities*/
		debtlt_debt		= dltt/debt_x; 																/*long-term debt as % of total liabilities*/
		lt_ppen			= lt/ppent; 															/*total liabilities to total tangible assets*/
		debtlt_be		= dltt/be_x; 																/*long-term debt to book equity*/
		opex_at			= opex_x/at_x;													/* Operating Leverage ala Novy-Marx (2011) */
		nwc_at			= nwc_x/at_x;
		if ocf_x>0 then 
			fcf_ocf 	= fcf_x/ocf_x;  													/*Free Cash Flow/Operating Cash Flow*/
		
		/*Solvency Ratios*/
		debt_at		= debt_x/at_x; 																/*Debt-to-assets*/
		debt_be		= debt_x/be_x; 													/*debt to shareholders' equity ratio*/	
		ebit_int	= ebit_x/xint; 							/*interest coverage ratio*/
		
		/*Liquidity Ratios*/
		inv_days 			= %mean_year(invt)/cogs * 365;		/* Days Inventory Outstanding */
		rec_days 			= %mean_year(rect)/sale_x * 365;	/* Days Sales Outstanding */
		ap_days 			= %mean_year(ap)/cogs * 365;		/* Days Accounts Payable Outstanding */
		if count<=12 then do;
			array var_liq inv_days rec_days ap_days;
			do over var_liq;
				var_liq=.;
			end;
		end;
		cash_conversion = inv_days + rec_days - ap_days; 		/* Cash Conversion Cycle*/
		if cash_conversion<0 then 
			cash_conversion =.;
		if cl_x>0 then do;
			cash_cl		= che/cl_x; 											/* Cash Ratio*/                                   
			caliq_cl	= caliq_x/cl_x;										/* Quick Ratio (acid test)*/
			ca_cl		= ca_x/cl_x;										/* Current Ratio*/
		end;
		
		/*Activity/Efficiency Ratios*/
		inv_turnover = cogs/%mean_year(invt);				/* Inventory Turnover */
		at_turnover = sale_x/%mean_year(at_x);				/* Asset Turnover */
		rec_turnover = sale_x/%mean_year(rect);			/* Receivables Turnover */
		ap_turnover = (cogs+dif12(invt))/%mean_year(ap);	/* Account Payables Turnover */
		if count<=12 then do;
			array var_turn inv_turnover at_turnover rec_turnover ap_turnover;
			do over var_turn;
				var_turn=.;
			end;
		end;
		
		/*Miscallenous Ratios*/
		adv_sale		= xad/sale_x; 												/*advertising as % of sales*/
		staff_sale		= xlr/sale_x; 														/*labor expense as % of sales*/
		sale_bev 		= sale_x/bev_x; 															/*sale per $ Book Enterprise Value*/
	    rd_sale			= xrd/sale_x; 
		sale_be 		= sale_x/be_x; 															/*sales per $ total stockholders' equity*/
		if coalesce(nix_x, ni_x)>0 then 
			div_ni		= div_x/nix_x; 												/*Dividend payout ratio. THEIS: I added ib as a possibility*/	
		if nwc_x>0 then 
			sale_nwc    = sale_x/nwc_x;														/*sales per $ working capital*/
		if pi_x>0 then 
			tax_pi		= txt/pi_x; 								/*effective tax rate*/
			
			
			
		/* NEW VARIABLES */
		cash_at = che / at_x;
		if at_x <= 0 then
			cash_at = .;
		* Employees based variables;
		ni_emp = ni_x / emp;
		if emp <= 0 then ni_emp = .;
		sale_emp = sale_x / emp;
		if emp <= 0 then sale_emp = .;
		sale_emp_gr1 = sale_emp / lag12(sale_emp) - 1; /* Labor force efficiency */
		if count <= 12 or lag12(sale_emp) <= 0 then sale_emp_gr1 = .;
		emp_gr1 = (emp - lag12(emp)) / (0.5 * emp + 0.5 * lag12(emp));
		if count <= 12 or emp_gr1 = 0 or (0.5 * emp + 0.5 * lag12(emp)) = 0 then emp_gr1 = .;
		
		* Number of Consecutive Earnings Increases;
		ni_inc = ni_x > lag12(ni_x);
		if missing(ni_x) or missing(lag12(ni_x)) then
			ni_inc = .;
		ni_inc8q = 0;	
		no_decrease = 1;
		%do q = 0 %to 7;
			%let ql = %sysevalf(&q.*3);
			if lag&ql.(ni_inc) = 1 and no_decrease = 1 then 
				ni_inc8q = ni_inc8q + 1;
			else 
				no_decrease = 0;
		%end;
		n_ni_inc = %apply_to_lastq(x = not missing(ni_inc), _qtrs = 8, func = sum);
		if missing(ni_inc) or n_ni_inc ^= 8 or count < 33 then 
			ni_inc8q = .;
		drop no_decrease n_ni_inc; 	
		
		* 1yr Change Scaled by Lagged Assets;
		%let ch_asset_lag_vars = 
			noa_x ppeinv_x
		;
		%do i=1 %to %nwords(&ch_asset_lag_vars.);
			%let var_gr1al = %scan(&ch_asset_lag_vars., &i, %str(' '));
			%let name_gr1al = %sysfunc(tranwrd(&var_gr1al, _x, %str()));  /* Remove '_x' from var name */
			&name_gr1al._gr1a = (&var_gr1al-lag12(&var_gr1al))/lag12(at_x);
			if count<=12 or lag12(at_x)<=0 then
				&name_gr1al._gr1a = .;
		%end;
		
		* 1yr Change Scaled by Average Assets;
		%let ch_asset_avg_vars = 
			lnoa_x
		;
		%do i=1 %to %nwords(&ch_asset_avg_vars.);
			%let var_gr1aa = %scan(&ch_asset_avg_vars., &i, %str(' '));
			%let name_gr1aa = %sysfunc(tranwrd(&var_gr1aa, _x, %str()));  /* Remove '_x' from var name */
			&name_gr1aa._gr1a = (&var_gr1aa-lag12(&var_gr1aa))/(at_x + lag12(at_x));
			if count<=12 or (at_x + lag12(at_x))<=0 then
				&name_gr1aa._gr1a = .;
		%end;
		
		* CAPEX growth over 2 years;
		%var_growth(var_gr=capx, horizon=24);
			
		* Quarterly Profitability Measures;
		saleq_gr1 = sale_qtr / lag12(sale_qtr) - 1;
		if count <= 12 or lag12(sale_qtr) < 0 then
			saleq_gr1 = .;
		niq_be = ni_qtr / lag3(be_x);
		if count <= 3 or lag3(be_x) < 0 then
			niq_be = .;
		niq_at = ni_qtr / lag3(at_x);
		if count <= 3 or lag3(at_x) < 0 then
			niq_at = .;
		niq_be_chg1 = niq_be - lag12(niq_be);
		niq_at_chg1 = niq_at - lag12(niq_at);
		if count <= 12 then do;
			niq_be_chg1 = .;
			niq_at_chg1 = .;
		end;
		
		* R&D capital-to-assets;
		rd5_at = (xrd + lag12(xrd)*0.8 + lag24(xrd)*0.6 + lag36(xrd)*0.4 + lag48(xrd)*0.2) / at_x;
		if count <= 48 or at_x <= 0 then
			rd5_at = .;
			
		* Abarbanell and Bushee (1998);
		%chg_to_exp(var_ce = sale_x);
		%chg_to_exp(var_ce = invt);
		%chg_to_exp(var_ce = rect);
		%chg_to_exp(var_ce = gp_x);
		%chg_to_exp(var_ce = xsga);
		
		dsale_dinv = sale_ce - invt_ce;
		dsale_drec = sale_ce - rect_ce;
		dgp_dsale  = gp_ce - sale_ce;
		dsale_dsga = sale_ce - xsga_ce;
		drop sale_ce invt_ce rect_ce gp_ce xsga_ce;
		
		* Earnings and Revenue 'Surpise';
		%standardized_unexpected(var=sale_qtr, qtrs = 8, qtrs_min = 6);
		%standardized_unexpected(var=ni_qtr, qtrs = 8, qtrs_min = 6);
			
		* Abnormal Corporate Investment;
		__capex_sale = capx / sale_x;
		if sale_x <= 0 then
			__capx_sale = .;
		capex_abn = __capex_sale / ((lag12(__capex_sale) + lag24(__capex_sale) + lag36(__capex_sale)) / 3) - 1;
		if count <= 36 then 
			capex_abn = .;
		drop __capex_sale;
		
		/* Profit scaled by lagged */
		op_atl1 = op_x / lag12(at_x);
		if count <= 12 or lag12(at_x) <= 0 then
			op_atl1 = .;
		gp_atl1 = gp_x / lag12(at_x);
		if count <= 12 or lag12(at_x) <= 0 then
			gp_atl1 = .;
		ope_bel1 = ope_x / lag12(be_x);
		if count <= 12 or lag12(be_x) <= 0 then
			ope_bel1 = .;
		cop_atl1 = cop_x / lag12(at_x);
		if count <= 12 or lag12(at_x) <= 0 then
			cop_atl1 = .;
			
		/* Profitability Measures*/
		pi_nix = pi_x / nix_x;
		if pi_x <= 0 or nix_x <= 0 then
			pi_nix = .;
		ocf_at = ocf_x / at_x;
		op_at = op_x / at_x;
		if at_x <= 0 then do;
			ocf_at = .;
			op_at = .;
		end;
		ocf_at_chg1 = ocf_at - lag12(ocf_at);
		if count <= 12 then 
			ocf_at_chg1 = .;
		
		/* Book Leverage */
		at_be = at_x / be_x;
		
			/* Volatility Quarterly Items */
		__ocfq_saleq = ocf_qtr / sale_qtr;
		__niq_saleq = ni_qtr / sale_qtr;
		if sale_qtr <= 0 then do;
			__ocfq_saleq = .;
			__niq_saleq = .;
		end;
		__roeq = ni_qtr / be_x;
		if be_x <= 0 then 
			__roeq = .;
		%volq(name = ocfq_saleq_std, var = __ocfq_saleq, qtrs = 16, qtrs_min = 8);
		%volq(name = niq_saleq_std, var = __niq_saleq, qtrs = 16, qtrs_min = 8);
		%volq(name = roeq_be_std, var = __roeq, qtrs = 20, qtrs_min = 12);
		drop __ocfq_saleq __niq_saleq __roeq;
		
		/* Volatility Annual Items*/
		__roe = ni_x / be_x;
		if be_x <= 0 then
			__roe = .;
		%vola(name = roe_be_std, var = __roe, yrs = 5, yrs_min = 5);
		drop __roe;
		
		/* Asset Tangibility */
		tangibility = (che + 0.715 * rect + 0.547 * invt + 0.535 * ppegt) / at_x;  
		
		* Earnings Smoothness;
		%earnings_variability(esm_h=5);
		
		* Asset Liquidity;
		aliq_at = aliq_x / lag12(at_x);
		if count <= 12 or lag12(at_x) <= 0 then aliq_at = .;
		
		* Equity Duration Helper Variables;
		%equity_duration_cd(horizon=10, r=0.12, roe_mean=0.12, roe_ar1=0.57, g_mean=0.06, g_ar1=0.24);
		
		* Pitroski F-Score;
		%pitroski_f(name = f_score);
		
		* Ohlson (1980) O-score;
		%ohlson_o(name = o_score);
		
		* Altman (1968) Z-score;
		%altman_z(name = z_score);
		 
		* Intrinsic ROE based value from Frankel and Lee (1998);
		%intrinsic_value(name = intrinsic_value, r=0.12);
		
		* Kaplan-Zingales Index;
		%kz_index(name=kz_index);
		
		* 5 year ratio change (For quality minus junk variables);
		%chg_var1_to_var2(name=gpoa_ch5, var1=gp_x, var2=at_x, horizon=60);
		%chg_var1_to_var2(name=roe_ch5, var1=ni_x, var2=be_x, horizon=60);
		%chg_var1_to_var2(name=roa_ch5, var1=ni_x, var2=at_x, horizon=60);
		%chg_var1_to_var2(name=cfoa_ch5, var1=ocf_x, var2=at_x, horizon=60);
		%chg_var1_to_var2(name=gmar_ch5, var1=gp_x, var2=sale_x, horizon=60);
		
		/* Delete Helper Variables */
		drop count;
	run;	
	
	/* Create earningspersistence */
	%earnings_persistence(out=earnings_pers, data=__chars5, __n=5, __min=5);
	
	proc sql;
		create table __chars6 as 
		select a.*, b.ni_ar1, b.ni_ivol
		from __chars5 as a left join earnings_pers as b
		on a.gvkey = b.gvkey and a.curcd=b.curcd and a.datadate=b.datadate;
	quit;
	
	/* Keep only dates with accounting data */
	data __chars7;
		set __chars6;
		where data_available=1;
	run;
	
	/* Expand by Public Availability */
	* Would be great to change start_date to filling_date or some derivative which was a function of fqtr;
	proc sort data=__chars7; by gvkey descending datadate; run;
	data __chars8;
		set __chars7;
		by gvkey;
		
		start_date = intnx('month', datadate, &lag_to_public.,'e'); format start_date YYMMDDN8.;
		next_start_date = lag(start_date);
		if first.gvkey then 
			next_start_date=.;
		end_date = min(intnx('month', next_start_date, -1, 'e'), intnx('month', datadate, &max_data_lag., 'e')); format end_date YYMMDDN8.;	
			
		drop next_start_date;	
	run;
	
	%expand(data=__chars8, out=__chars9, id_vars=gvkey, start_date=start_date, end_date=end_date, freq='month', new_date_name=public_date);
	
	/* Convert All Raw (non-scaled) Variables to USD [2]*/
	%compustat_fx(out=__fx);
	proc sql;
		create table __chars10 as 
		select a.*, b.fx 
		from __chars9 as a left join __fx as b
		on a.curcd=b.curcdd and a.public_date=b.date;
	quit;
	
	data __chars11;
		set __chars10;
		array var_raw 
			assets sales book_equity net_income;
		do over var_raw;
			var_raw = var_raw*fx;
		end;
		drop curcd;
	run;
	
	/* Create Ratios using both Accounting and Market Values */
	* Note that valuation ratios are created at the company level;
	proc sql;
		create table __me_data1 as 
		select distinct gvkey, eom, me_company /* Include id for join with daily std */
		from &me_data.
		where not missing(gvkey) and primary_sec=1 and not missing(me_company) and common=1 and obs_main=1 /* Notice, exch_main is not a requirement */
		group by gvkey, eom
		having me_company=max(me_company);
	quit;
	
	proc sql; 
		create table __chars12 as
		select a.*, b.me_company
		from __chars11 as a left join __me_data1 as b
		on a.gvkey=b.gvkey and a.public_date=b.eom;
	quit;
	
	proc sort data=__chars12 out=__chars13 nodupkey; by gvkey public_date; run; /*THEIS: Global-> No duplicates US-> 3464 duplicate observations where deleted*/

	data __chars14;
		set __chars13;
		/* Prepare Data */
		mev				= me_company+netdebt_x*fx;			  /* Enterprise Value (in Dollars) */
		mat             = at_x * fx - be_x * fx + me_company;  /* Market Asset Value */
		if mev <= 0 then mev = .;
		if me_company <= 0 then	me_company = .;
		if mat <= 0 then mat = .;
		
		/* Characteristics Scaled by Market Equity */
		%let me_vars = at_x be_x debt_x netdebt_x che sale_x gp_x ebitda_x ebit_x ope_x ni_x nix_x cop_x  
			ocf_x fcf_x div_x eqbb_x eqis_x eqpo_x eqnpo_x eqnetis_x
			xrd;
		%do i=1 %to %nwords(&me_vars.);
			%let var_me = %scan(&me_vars., &i, %str(' '));
			%let name_me = %sysfunc(tranwrd(&var_me., _x, %str()));  /* Remove '_x' from var name */
			&name_me._me = (&var_me.*fx)/me_company;
		%end;
		ival_me = (intrinsic_value*fx) / me_company;
		
		/* Characteristics Scaled by Market Enterprise Value */
		%let mev_vars = at_x bev_x ppent be_x che sale_x gp_x ebitda_x ebit_x ope_x ni_x nix_x cop_x ocf_x fcf_x
			debt_x pstk_x dltt dlc dltnetis_x dstnetis_x dbnetis_x netis_x fincf_x;
		%do i=1 %to %nwords(&mev_vars.);
			%let var_mev = %scan(&mev_vars., &i, %str(' '));
			%let name_mev = %sysfunc(tranwrd(&var_mev., _x, %str()));  /* Remove '_x' from var name */
			&name_mev._mev = (&var_mev.*fx)/mev;
		%end;
		
		/* Characteristics Scaled by Market Assets */
		aliq_mat = aliq_x * fx / lag12(mat);
		if gvkey ^= lag12(gvkey) then aliq_mat = .;
		
		/* Size Measure */
		enterprise_value = mev;
		
		/* Equity Duration */
		eq_dur = ed_cd_w * fx / me_company + ed_constant * (me_company - ed_cd * fx) / me_company;
		if ed_err = 1 or eq_dur <= 0 then eq_dur = .;
	run;
	
	/* Format Output */
	proc sql noprint;
		select name into :col_names separated by ' ' 
		from dictionary.columns
		where libname=upcase("work") and memname = upcase("__chars14");
	quit;
	
	
	data __chars15;
		set __chars14;
		%do i=1 %to %nwords(&col_names.);
			%let old_name = %scan(&col_names., &i, %str(' '));
			%let new_name = %sysfunc(prxchange(s/xrd/rd/, 1, &old_name.));  /* Replace XRD with RD */
			%let new_name = %sysfunc(prxchange(s/xsga/sga/, 1, &new_name.));  /* Replace XSGA with SGA */
			%let new_name = %sysfunc(prxchange(s/dlc/debtst/, 1, &new_name.));  /* Replace DLC with DEBTST */
			%let new_name = %sysfunc(prxchange(s/dltt/debtlt/, 1, &new_name.));  /* Replace DLTT with DEBTLT */
			%let new_name = %sysfunc(prxchange(s/oancf/ocf/, 1, &new_name.));  /* Replace OANCF with OCF */
			%let new_name = %sysfunc(prxchange(s/ppegt/ppeg/, 1, &new_name.));  /* Replace PPEGT with PPEG */
			%let new_name = %sysfunc(prxchange(s/ppent/ppen/, 1, &new_name.));  /* Replace PPENT with PPEN */
			%let new_name = %sysfunc(prxchange(s/che/cash/, 1, &new_name.));  /* Replace CHE with CASH */
			%let new_name = %sysfunc(prxchange(s/invt/inv/, 1, &new_name.));  /* Replace INVT with INV */
			%let new_name = %sysfunc(prxchange(s/rect/rec/, 1, &new_name.));  /* Replace RECT with REC */
			%let new_name = %sysfunc(prxchange(s/txt/tax/, 1, &new_name.));  /* Replace TXT with TAX */
			%let new_name = %sysfunc(prxchange(s/ivao/lti/, 1, &new_name.));  /* Replace IVAO with LTI */
			%let new_name = %sysfunc(prxchange(s/ivst/sti/, 1, &new_name.));  /* Replace IVST with STI */
			%let new_name = %sysfunc(prxchange(s/sale_qtr/saleq/, 1, &new_name.));  /* Replace SALE_QTR with SALEQ */
			%let new_name = %sysfunc(prxchange(s/ni_qtr/niq/, 1, &new_name.));  /* Replace SALE_QTR with SALEQ */
			%let new_name = %sysfunc(prxchange(s/ocf_qtr/ocfq/, 1, &new_name.));  /* Replace SALE_QTR with SALEQ */
			rename &old_name. = &new_name.;
		%end;
		
	run;
	
	* Reorder and Keep only Selected Columns;
	data __chars16;
		retain source gvkey datadate public_date assets sales book_equity net_income enterprise_value;
		set __chars15;
		keep source gvkey public_date datadate &__keep_vars.;
	run;

	* Add suffix if specified;
	%if %length(&suffix.)>0 %then %do;
		data __chars16;
			set __chars16;
			%do i=1 %to %nwords(&__keep_vars.);
				%let var_x = %scan(&__keep_vars., &i, %str(' '));
				rename &var_x. = &var_x.&suffix.;
			%end;
			rename datadate=datadate&suffix.;
		run;
	%end;
	
	proc sort nodupkey data=__chars16 out=&out.; by gvkey public_date; run;
	proc delete data= __chars3 __chars4 __chars5 __chars6 __chars7 __chars8 
	__chars9 __chars10 __chars11 __chars12 __chars13 __chars14 __chars15 __chars16 __me_data __me_data1 __fx earnings_pers; run;
%mend create_acc_chars;

/* Combine Characteristics from Annual and Quarterly Data */
%macro combine_ann_qtr_chars(out=, ann_data=, qtr_data=, __char_vars=, q_suffix=);
	proc sql;
		create table __acc_chars1 as 
		select a.*, b.*
		from &ann_data. as a left join &qtr_data. as b
		on a.gvkey=b.gvkey and a.public_date=b.public_date;
	quit;
	
	/* Substitute Annual Characteristic for Quarterly if Quarterly is more recent */
	data __acc_chars2;
		set __acc_chars1;
		%do i=1 %to %nwords(&__char_vars.);
			%let ann_var = %scan(&__char_vars., &i.);
			%let qtr_var = &ann_var.&q_suffix.;
			if missing(&ann_var.) or (not missing(&qtr_var.) and datadate&q_suffix. > datadate) then /* Didn't include the first part before! */ 
				&ann_var. = &qtr_var.;
			drop &qtr_var.;
		%end;
		drop datadate datadate&q_suffix.; /* We can no longer be sure which items accounting dates refer to */
	run;
	
	proc sort nodupkey data=__acc_chars2 out=&out; by gvkey public_date; run;
	
	proc delete data=__acc_chars1 __acc_chars2; run;
%mend combine_ann_qtr_chars;
