*************************************************************
*  HELPER MACROS
************************************************************ ; 
* Winsorize_own: Flexible version of WRDS %winsorize macro
	Only difference currently, is the explicit specification of perc_low and perc_high.
	This allows for winsorizing in each end and also to specify winsorization below 1%
;
%macro winsorize_own(inset=, outset=, sortvar=, vars=, perc_low=1, perc_high=99, trim=0);
	/* List of all variables */
	%let vars = %sysfunc(compbl(&vars));
	%let nvars = %nwords(&vars);
	
	/* Display Output */
	%put ### START.;
	
	/* Trimming / Winsorization Options */
	%if &trim=0 %then %put ### Winsorization; %else %put ### Trimming;
	%put ### Number of Variables:  &nvars;
	%put ### List   of Variables:  &vars;
	options nonotes;
	
	/* Ranking within &sortvar levels */
	%put ### Sorting... ;
	proc sort data=&inset; by &sortvar; run; 
	
	/* 2-tail winsorization/trimming */
	%let var2 = %sysfunc(tranwrd(&vars,%str( ),%str(__ )))__;
	%let var_p1 = %sysfunc(tranwrd(&vars,%str( ),%str(__&perc_low )))__&perc_low ;
	%let var_p2 = %sysfunc(tranwrd(&vars,%str( ),%str(__&perc_high )))__&perc_high ;
	
	/* Theis: Handle naming if winsorization < 1%. In this case, digits cause problems */
	%let var_p1 = %sysfunc(prxchange(s/\./_/, -1, &var_p1.));  /* Replace . with _ */
	%let var_p2 = %sysfunc(prxchange(s/\./_/, -1, &var_p2.));  /* Replace . with _ */
	
	/* Calculate upper and lower percentiles */
	proc univariate data=&inset noprint;
	  by &sortvar;
	  var &vars;
	  output out=_perc pctlpts=&perc_low &perc_high pctlpre=&var2;
	run;
	
	%if &trim=1 %then 
	  %let condition = %str(if myvars(i)>=perct2(i) or myvars(i)<=perct1(i) then myvars(i)=. );
	  %else %let condition = %str(myvars(i)=min(perct2(i),max(perct1(i),myvars(i))) );
	
	%if &trim=0 %then %put ### Winsorizing at &perc_low.% and &perc_high.%... ;
	%else %put ### Trimming at &perc_low.% and &perc_high.%... ;
	
	/* Save output with trimmed/winsorized variables */
	data &outset;
	merge &inset (in=a) _perc;
	by &sortvar;
	  if a;
	  array myvars {&nvars} &vars;
	  array perct1 {&nvars} &var_p1;
	  array perct2 {&nvars} &var_p2;
	  do i = 1 to &nvars;
	    if not missing(myvars(i)) then
	    do;
	      &condition;
	    end;
	  end;
	drop i &var_p1 &var_p2;
	run;
	
	/* House Cleaning */
	proc sql; drop table _perc; quit;
	options notes;
	
	%put ### DONE . ; %put ;

%mend winsorize_own;


/* Flexible version of WRDS populate function which can also do daily frequency */
%macro populate_own(inset=, outset=, datevar=, idvar=, datename=, forward_max=, period=); /* Period in ('day', 'month') */
	/* Start Macro*/
	%put ; %put ### START. Populating Data --> Note that duplicate idvar and datevar will be removed;

	/* nodupkey sort necessary */
	proc sort data=&inset. out=__temp nodupkey; by &idvar descending &datevar.; run;
	options nonotes;
	
	/* Populate Dates */
	/* FORWARD_MAX is the Regular Periodicity or the Forward Population Intervals */
	%let nid = %nwords(&idvar.);
	%let id2 = %scan(&idvar.,&nid.,%str( ));
	
	data &outset. ; format &datename. YYMMDDN8.; 
		set __temp;
		by &idvar.;
		&datename. =&datevar.;
		output;
		following = lag(&datename.); 
		forward_max = intnx('month', &datevar., &forward_max.,'e');
		if first.&id2 then
		 	following = .;
		
  		n = intck(&period.,&datename., min(following, forward_max));
  		do i=1 to n-1;
	   		&datename. = intnx(&period.,&datename. ,1,"E"); output;
	  	end;
	 	
		drop following forward_max n i;
	run;
	
	proc sort data=&outset. nodupkey; by &idvar. &datevar. &datename.; run;
	
	/* House Cleaning */
	proc sql; drop table __temp; quit;
	options notes;
	%put ### DONE . Dataset &OUTSET. with &period. Frequency Generated ; %put ;

%MEND populate_own;

/* Generic expand in a data set with a start date column and an end date column. */
%macro expand(data=, out=, id_vars=, start_date=, end_date=, freq='day', new_date_name=date); /*freq in ('day', 'month')*/
	data __expanded;
	   set &data.;
	   format &new_date_name. YYMMDDN8.;
	   do i = 0 to intck(&freq., &start_date., &end_date.);
	      &new_date_name. = intnx(&freq., &start_date., i, 'e');
	      output;
	   end;
	   drop &start_date. &end_date. i;
	run;
	
	proc sort data=__expanded out=&out nodupkey; by &id_vars. &new_date_name.; run;
	proc delete data=__expanded; run;
%mend expand;

/* USD to Foreign FX Conversion Rate from Compustat*/
%macro compustat_fx(out=);
	data usd_curcdd; 
		curcdd='USD';
		datadate=input(put(19500101,8.),yymmdd8.);
		fx=1;
		format datadate yymmddn8.;
	run;  /* comp.exrt_dly only starts in 1982 and since we convert to USD we know that the fx for USD is 1 */
	
	proc sql; 
		create table __fx1 as
		select distinct a.tocurd as  curcdd  , a.datadate,  b.exratd/a.exratd as fx /*fx is quoted as x/USD so to go from x to USD do x*fx*/
		from comp.exrt_dly a , comp.exrt_dly b
		where a.fromcurd = 'GBP' and b.tocurd = 'USD' /*b.exratd is always from GBP to USD, a.exratd is from GBP to currency X*/
		and a.fromcurd = b.fromcurd and a.datadate = b.datadate;
	quit;
	
	data __fx2; set __fx1 usd_curcdd; run; 
	
	proc sort data = __fx2;  by curcdd descending datadate; run ; 
	
	/* Carry forward fx observations in case gaps*/
	data __fx3; format date YYMMDDN8.; 
		set __fx2;
		by curcdd;
		date = datadate;
		output;
		following = lag(date); 
		if first.curcdd then
		 	following = date+1;
  		n = following-date;
  		do i=1 to n-1;
	   		date = date+1; output;
	  	end;
	 	
		drop datadate following n i;
	run;
	
	proc sort data=__fx3 out=&out nodupkey; by curcdd date; run;
	
	proc delete data=usd_curcdd __fx1 __fx2 __fx3; run;
%mend compustat_fx; 


**********************************************************************************************************************
*  MACRO - Add Primary Security from Compustat *
**********************************************************************************************************************
The macro expects a data set with gvkey and iid. It then returns the same dataset with a column "primary_sec" which is 1 when a security is the primary security in the US, Canada or internationally.	
Importantly, a given company (gvkey) can potentially have up to 3 primary securities although it happens very rarely. In the full Compustat monthly dataset, of all gvkey-eom pairs we have
	0 primary securities:  1.82%
  	1 primary securities: 95.10%
  	2 primary securities:  3.08%
  	3 primary securities:  0.01%
;
%macro add_primary_sec(data=, out=, date_var=);
	proc sql;
		create table __prihistrow as 
		select gvkey, itemvalue as prihistrow, effdate, thrudate
		from comp.g_sec_history where item = 'PRIHISTROW';
	quit;
	
	proc sql;
		create table __prihistusa as 
		select gvkey, itemvalue as prihistusa, effdate, thrudate
		from comp.sec_history where item = 'PRIHISTUSA';
	quit;
	
	proc sql;
		create table __prihistcan as 
		select gvkey, itemvalue as prihistcan, effdate, thrudate
		from comp.sec_history where item = 'PRIHISTCAN';
	quit;
	
	proc sql;
		create table __header as 
		select gvkey, prirow, priusa, prican from comp.company
		outer union corr
		select gvkey, prirow, priusa, prican from comp.g_company;
	quit;
	
	proc sort data=__header nodupkey; by gvkey; run; /* Only one duplicate (gvkey=254381)*/
	
	proc sql;
		create table __data1 as 
		select distinct a.*, coalesce(b.prihistrow, e.prirow) as prihistrow, coalesce(c.prihistusa, e.priusa)  as prihistusa, coalesce(d.prihistcan, e.prican) as prihistcan /* Prefer historical primary sec indicator, if this is not available, use header. Distinct avoid around 400 dup observations due to gvkey=213098 having overlapping prihistrow in g_sec_history */
		from &data. as a 
		left join __prihistrow as b
			on a.gvkey=b.gvkey and a.&date_var.>=b.effdate and (a.&date_var.<=b.thrudate or missing(b.thrudate))
		left join __prihistusa as c
			on a.gvkey=c.gvkey and a.&date_var.>=c.effdate and (a.&date_var.<=c.thrudate or missing(c.thrudate))
		left join __prihistcan as d
			on a.gvkey=d.gvkey and a.&date_var.>=d.effdate and (a.&date_var.<=d.thrudate or missing(d.thrudate))
		left join __header as e
			on a.gvkey=e.gvkey;
			
		create table __data2 as 
		select *, (not missing(iid) and (iid=prihistrow or iid=prihistusa or iid=prihistcan)) as primary_sec /* If the security id is identified as primary in either USA, CANADA or Rest of the World it is deemed a primary security */
		from __data1;
	quit;
	
	data &out.; set __data2(drop = prihistrow prihistusa prihistcan);
	
	proc delete data=__prihistrow __prihistusa __prihistcan __header __data1 __data2; run;
%mend add_primary_sec;

/* MACRO - COMPUSTAT EXCHANGES */*
	The macro returns a daily and monthly dataset with summary statistics of trading for each exchg.
 	The macro take two arguments:
		- wins_perc determines the winsorization percentile to applied to daily/monthly dollar volume within an exchange. 
		  This is implemented to avoid outliers but will downward bias the states dollar volumes since the disproportionally censors large firms 
		- min_dolvol_share i an argument in [0,1] that determines the minimum share of dollar volume within a country an exchange has to have to be called a main_exchange.
		  Bessembinder et al (2020) uses 0.02. However, this will exclude small cap exchanges such as NYSE American and TSX Venture Exchange. I therefore prefer to use 0 and therefore include all ordinary exchanges as "main exchanges".
;

%macro comp_exchanges(wins_perc=1, min_dolvol_share=0); 
	/* Exchange Classification */
	/* What about 153? . Could consider excluding 111 since its the same securities traded on 110. This is just due to a rule in Thailand limiting the ownership of foreign investors in Thailand https://thaishares.com/nvdr/*/
	%let special_exchanges =
			(0, 
			1, 
			2, 
			3, 
			4, 
			13, 
			19, 
			20, 
			127, 
			150, 		/* AIAF Mercado De Renta Fija --> Spanish exchange for trading debt securities https://practiceguides.chambers.com/practice-guides/capital-markets-debt-2019/spain/1-debt-marketsexchanges */
			157, 
			229, 
			263, 
			269, 
			281, 
			283, 
			290, 
			320, 
			326, 
			341, 
			342, 
			347, 
			348, 
			349,		/* BATS Chi-X Europe --> Trades stocks from various european exchanges. Should we keep it?*/
			352)		/* CHI-X Australia --> Only Trades securities listed on ASX (exchg=106). Should we keep it?*/
	;

	/* Determine Country of Exchange (Note that we assume that this is constant through time) */
	proc sql;
		create table __ex_country1 as
		select distinct exchg, excntry from comp.g_security
		outer union corr
		select distinct exchg, excntry from comp.security;
		
		create table __ex_country2 as
		select distinct exchg, 
			case 
				when count(excntry)>1 then 'multi_national' /*, calculated count > 1 as multi_national*/
				else excntry
			end as ex_country
		from __ex_country1
		where not missing(excntry) and not missing(exchg)
		group by exchg;
		
		create table __ex_country3 as
		select a.*, b.exchgdesc
		from __ex_country2 as a left join comp.r_ex_codes as b
		on a.exchg=b.exchgcd;
	quit;
	
	/* FX Table*/
	%compustat_fx(out=__fx); 
	
	proc sql;
		/* Dollar Volume from Security Daily */
		create table __sec_daily1 as
		select gvkey, iid, datadate, tpci, exchg, curcdd, prccd/qunit as prccd, cshtrd
		from comp.g_secd /*(firstobs=1 obs=10000000)*/
		where not missing(prccd)
		outer union corr
		select gvkey, iid, datadate, tpci, exchg, curcdd, prccd, cshtrd
		from comp.secd /*(firstobs=1 obs=10000000)*/
		where not missing(prccd);
		
		create table __sec_daily2 as
		select a.gvkey, a.iid, a.tpci, a.exchg, a.datadate, intnx('month', datadate,0,'E') as eom format=YYMMDDN8., a.prccd*b.fx*a.cshtrd/1e6 as dolvol
		from __sec_daily1 as a 
		left join __fx as b
			on a.datadate=b.date and a.curcdd=b.curcdd;
		
		/* Monthly Dollar Volume - Combine SECM and SEC_DAILY*/
		create table __secm1 as
		select a.gvkey, a.iid, a.tpci, a.exchg, intnx('month', a.datadate,0,'E') as eom format=YYMMDDN8.,  
			a.prccm*b.fx*a.cshtrm/1e6 as dolvol 
		from comp.secm as a left join __fx as b
		on a.datadate=b.date and a.curcdm=b.curcdd
		where not missing(a.prccm);
		
		create table __sec_monthly1 as 
		select gvkey, iid, tpci, exchg, eom, sum(dolvol) as dolvol
		from __sec_daily2 
		group by gvkey, iid, eom, tpci, exchg;
		
		create table __sec_monthly2 as 
		select *, 'd' as freq from __sec_monthly1
		outer union corr
		select *, 'm' as freq from __secm1;  
	quit;
	
	/* When duplicate observations, prefer SECM. Inspection shows that SECM is more accurate than SECD */
	proc sort data=__sec_monthly2; by gvkey iid eom descending freq; run; 
	data __sec_monthly3;
		set __sec_monthly2;
		by gvkey iid eom; 
		if first.eom; 
	run;
	
	%macro exchange_summary(data=, out=, date_var=);
		/* Winsorize Dollar Volume to Account for Outliers */
		%winsorize(inset=&data., outset=__sec1, sortvar=exchg &date_var., vars=dolvol, perc1=&wins_perc.); 
		
		/* Add Common Stocks Indicator as a Helper Variable*/
		data __sec2; 
			set __sec1; 
			if tpci='0' then cs=1; else cs=0;  
		run; 
		
		/* Create Exchange Statistics Monthly */
		proc sql;
			create table __exchanges1 as 
			select exchg, &date_var., count(*) as n, sum(cs) as n_cs, sum(dolvol) as dolvol, sum(dolvol*cs) as dolvol_cs
			from __sec2
			where not missing(exchg)
			group by exchg, &date_var.;
			
			create table __exchanges2 as 
			select a.*, b.ex_country, b.exchgdesc
			from __exchanges1 as a 
			left join __ex_country3 as b
				on a.exchg=b.exchg;
				
			/* Classify Exchange */
			create table __exchanges3 as 
			select *, 
				case 
					when exchg in &special_exchanges. or ex_country='multi_national' then 'special'
					else 'ordinary'  /* I use case when if we want to extend the number of categories in the future */
				end as exch_type
			from __exchanges2;
			
			/* Share of Common Stock Volume within a Country's Ordinary Exchanges*/
			create table __exchanges4 as
			select *, dolvol_cs/sum(dolvol_cs) as dolvol_share
			from __exchanges3
			group by ex_country, exch_type, &date_var.;
			
			update __exchanges4
			set dolvol_share = .
			where exch_type ^= 'ordinary';
		quit;
		
		/* Output */
		proc sort data=__exchanges4 out=&out nodupkey; by exchg &date_var.; run;
		
		proc delete data=__sec1 __sec2 __exchanges1 __exchanges2 __exchanges3 __exchanges4; run;
	%mend;
	
	%exchange_summary(data=__sec_monthly3, out=__exchanges_monthly1, date_var=eom);
	%exchange_summary(data=__sec_daily2, out=exchanges_daily, date_var=datadate);
	
	/* Add Main Exchange Indicator to the Monthly Exchange File */
	proc sql;
		create table __exchanges_monthly2 as 
		select *, mean(missing(dolvol_share)) as dolvol_miss_frac /* CSHTRD is missing in the early years of many countries. We therefore tag all exchange as the main exchanges if there is no dollar volume available for the country-eom pair*/
		from __exchanges_monthly1
		group by ex_country, eom; 	
		
		create table exchanges_monthly as 
		select exchg, exchgdesc, exch_type, ex_country, eom, n, n_cs, dolvol, dolvol_cs, dolvol_share,
			case 
				when exch_type = 'ordinary' and (dolvol_share>=&min_dolvol_share. or dolvol_miss_frac=1) and n_cs>=10 then 1 /* The Dollar Requirement from Bessembinder is 2% but that removes a lot of small cap exchanges*/
				else 0
			end as main_exchange
		from __exchanges_monthly2
		order by exchg, eom;
	quit;
	
	proc delete data= __ex_country1 __ex_country2 __ex_country3 __fx __sec_daily1 __sec_daily2
		__secm1 __sec_monthly1 __sec_monthly2 __sec_monthly3
		__exchanges_monthly1 __exchanges_monthly2 __exchanges_monthly3; run;
%mend comp_exchanges;

**********************************************************************************************************************
*                                    US - Data From CRSP
********************************************************************************************************************* ; 
%macro prepare_crsp_sf(freq=m); /* p(eriodicity) in ('d', 'm') for daily and monthly. Returns crsp_msf if p=m and crsp_dsf if p=d*/
	/* CRSP with Company Information*/
	proc sql;
		create table __crsp_sf1 as
		select a.permno, a.permco, a.date, (a.prc < 0) as bidask, abs(a.prc) as prc, a.shrout/1000 as shrout, calculated prc * calculated shrout as me,
		   a.ret, a.retx, a.cfacshr, a.vol, 
		   case when a.prc > 0 and a.askhi > 0 then a.askhi else . end as prc_high,  /* Highest price when prc is not the bid-ask average: https://wrds-web.wharton.upenn.edu/wrds/query_forms/variable_documentation.cfm?vendorCode=CRSP&libraryCode=crspa&fileCode=dsf&id=askhi*/
		   case when a.prc > 0 and a.bidlo > 0 then a.bidlo else . end as prc_low,    /* Lowest price when prc is not the bid-ask average: https://wrds-web.wharton.upenn.edu/wrds/query_forms/variable_documentation.cfm?vendorCode=CRSP&libraryCode=crspa&fileCode=dsf&id=bidlo */
		   b.shrcd, b.exchcd, c.gvkey, c.liid as iid, c.linkprim in ('P', 'C') as primary_sec /*http://www.crsp.org/products/documentation/crspccmlink-security-link-history*/
		from crsp.&freq.sf as a 
		left join crsp.&freq.senames as b
		   on a.permno=b.permno and a.date>=namedt and a.date<=b.nameendt
		left join crsp.ccmxpf_lnkhist as c
		   on a.permno=c.lpermno and (a.date>=c.linkdt or missing(c.linkdt)) and 
		   (a.date<=c.linkenddt or missing(c.linkenddt)) and c.linktype in ('LC', 'LU', 'LS');
	quit;
	
	/* Adjust trading volume following Gao and Ritter (2010)*/
	proc sql;
		update __crsp_sf1
		set vol = 
			case 
				when date < '01FEB2001'd then vol / 2
				when date <= '31DEC2001'd then vol / 1.8
				when date < '31DEC2003'd then vol / 1.6
				else vol
			end
		where exchcd = 3;
	quit;
	
	/* Add dividend and dollar volume */
	proc sort data=__crsp_sf1; by permno date; run;
	
	data __crsp_sf2;
		set __crsp_sf1;
		by permno;
		dolvol = abs(prc) * vol;
		div_tot = (ret-retx)*lag(prc)*(cfacshr/lag(cfacshr)); /* The CFACSHR part is to put it on the pr share basis of the current date */
		if first.permno then
			div_tot=.;
	run;
	
	/* Incorporate Delisting Returns */
	proc sql;
		create table __crsp_sf3 as
		select a.*, b.dlret, b.dlstcd
		from __crsp_sf2 as a left join crsp.&freq.sedelist as b
		on a.permno=b.permno and year(a.date)=year(b.dlstdt) and month(a.date)=month(b.dlstdt);
	quit;
	
	data __crsp_sf3; 
		set __crsp_sf3; 
		if missing(dlret) and (dlstcd=500 or (520<=dlstcd<=584)) then dlret=-0.3; /*If delisting is missing and is for performance related reasons. Set to -30%. This is relevant to 155 observations only*/
		if missing(ret) and not missing(dlret) then ret=0;
		ret= (1+ret)*(1+coalesce(dlret, 0))-1; /*If missing set to zero*/
		drop dlret dlstcd;
	run;
	
	* Excess Return;
	%if &freq.=d %then %let scale=21;
	%if &freq.=m %then %let scale=1;
	
	proc sql;
		create table __crsp_sf4 as 
		select a.*, a.ret-coalesce(b.t30ret, c.rf)/&scale. as ret_exc /* I prefer crsp.mcti but FF has monthly updates */
		from __crsp_sf3 as a 
		left join crsp.mcti as b 
			on year(a.date)=year(b.caldt) and month(a.date)=month(b.caldt)
		left join ff.factors_monthly as c 
			on year(a.date)=year(c.date) and month(a.date)=month(c.date);
	quit;
	
	* Company Market Equity;
	proc sql;
		create table __crsp_sf5 as 
		select *, sum(me) as me_company
		from __crsp_sf4
		group by permco, date;
	quit;
	
	* Create Primary Sec for Stocks with misisng GVKEY (primarily neccesary prior to 1950);
	proc sql;
		create table __crsp_sf6 as
		select *, max(dolvol) as max_dolvol
		from __crsp_sf5
		group by permco, date;
		
		update __crsp_sf6		/*For CRSP data ending in 2019. 618.458 out of 630.910 with missing(gvkey) or missing(iid) are updated from 0 to 1 */
		set primary_sec=1
		where (missing(gvkey) or missing(iid)) and dolvol=max_dolvol;  /* If no GVKEY is available, choose security with the highest dollar volume as the primary security in that month */
		
		alter table __crsp_sf6
		drop max_dolvol;
	quit;
	
	* Make volume comparable across daily and monthly set:  https://wrds-web.wharton.upenn.edu/wrds/query_forms/variable_documentation.cfm?vendorCode=CRSP&libraryCode=crspa&fileCode=dsf&id=vol;
	%if &freq.=m %then %do;
		proc sql;
			update __crsp_sf6
			set vol = vol*100,
			    dolvol = dolvol*100;
		quit;
	%end;
	
	proc sort nodupkey data=__crsp_sf6; by permno date; run; /*In monthly file: Two duplicates 15075-20180131 and 86812-20190731 In daily file 13 obs*/
	data crsp_&freq.sf; set __crsp_sf6;
	
	*proc delete data= __crsp_sf1 __crsp_sf2 __crsp_sf3 __crsp_sf4 __crsp_sf5; 
%mend prepare_crsp_sf;

**********************************************************************************************************************
*                                    World - Data From Compustat
********************************************************************************************************************* ; 
%macro prepare_comp_sf(freq=); /* freq in (d, m,both) */
	/* SECD has a lot of missing CSHOC. Therefore we use information from Accounting Statements. This is not a problem for g_secd*/
	%let comp_cond = indfmt='INDL' and datafmt='STD' and popsrc='D' and consol='C';
	proc sql; 
		create table __firm_shares1 as 
		select gvkey, datadate, cshoq as csho_fund, ajexq as ajex_fund 
		from comp.fundq where &comp_cond. and not missing(cshoq) and not missing(ajexq) 
		outer union corr
		select gvkey, datadate, csho as csho_fund, ajex as ajex_fund 
		from comp.funda where &comp_cond. and not missing(csho) and not missing(ajex);
	quit;
	%populate_own(inset=__firm_shares1, outset=__firm_shares2, datevar=datadate, idvar=gvkey, datename=ddate, forward_max=12, period = 'day');

	proc sql;
		create table __comp_dsf_na as
		select a.gvkey, a.iid, a.datadate, a.tpci, a.exchg, a.prcstd, a.curcdd, a.prccd as prc_local, a.ajexdi,
			case when a.prcstd^=5 then a.prchd else . end as prc_high_lcl,  /* Highest price when prc is not the bid-ask average: https://wrds-web.wharton.upenn.edu/wrds/query_forms/variable_documentation.cfm?vendorCode=COMP&libraryCode=compd&fileCode=secd&id=prchd */
			case when a.prcstd^=5 then a.prcld else . end as prc_low_lcl,   /* Lowest price when prc is not the bid-ask average: https://wrds-web.wharton.upenn.edu/wrds/support/Data/_001Manuals%20and%20Overviews/_001Compustat/_001North%20America%20-%20Global%20-%20Bank/_000dataguide/?file_name=Data/_001Manuals%20and%20Overviews/_001Compustat/_001North%20America%20-%20Global%20-%20Bank/_000dataguide */
			cshtrd, coalesce(a.cshoc/1e6, b.csho_fund*b.ajex_fund/a.ajexdi) as cshoc,  /* Prefer cshoc but if missing choose shares outstanding from accounting statement adjusted for issuance activity*/
		   	(a.prccd/a.ajexdi*a.trfd) as ri_local,
		   	a.curcddv, a.div, a.divd, a.divsp /* Dividend Variables */
		from comp.secd as a left join __firm_shares2 as b
		on a.gvkey=b.gvkey and a.datadate=b.ddate;
		
		/* Adjust trading volume of NASDAQ stocks following Gao and Ritter (2010)*/
		update __comp_dsf_na
		set cshtrd = 
			case 
				when datadate < '01FEB2001'd then cshtrd / 2
				when datadate <= '31DEC2001'd then cshtrd / 1.8
				when datadate < '31DEC2003'd then cshtrd / 1.6
				else cshtrd
			end
		where exchg = 14;
		
		create table __comp_dsf_global as
		select gvkey, iid, datadate, tpci, exchg, prcstd, curcdd, 
			prccd/qunit as prc_local, ajexdi, cshoc/1e6 as cshoc,
			case when prcstd^=5 then prchd/qunit else . end as prc_high_lcl,  /* Highest price when prc is not the bid-ask average: https://wrds-web.wharton.upenn.edu/wrds/query_forms/variable_documentation.cfm?vendorCode=COMP&libraryCode=compd&fileCode=secd&id=prchd */
			case when prcstd^=5 then prcld/qunit else . end as prc_low_lcl,   /* Lowest price when prc is not the bid-ask average: https://wrds-web.wharton.upenn.edu/wrds/support/Data/_001Manuals%20and%20Overviews/_001Compustat/_001North%20America%20-%20Global%20-%20Bank/_000dataguide/?file_name=Data/_001Manuals%20and%20Overviews/_001Compustat/_001North%20America%20-%20Global%20-%20Bank/_000dataguide */
			cshtrd, ((calculated prc_local)/ajexdi*trfd) as ri_local,
			curcddv, div, divd, divsp
		from comp.g_secd;
		
		create table __comp_dsf1 as 
		select * from __comp_dsf_na 
		outer union corr
		select * from __comp_dsf_global;
	quit;
	
	/* Add FX */
	%compustat_fx(out=fx); 
	proc sql;
		create table __comp_dsf2 as 
		select a.*, b.fx as fx, c.fx as fx_div
		from __comp_dsf1 as a
		left join fx as b
			on a.curcdd=b.curcdd and a.datadate=b.date
		left join fx as c
			on a.curcddv=c.curcdd and a.datadate=c.date;
	quit;
	
	data __comp_dsf3; 
		set __comp_dsf2; 
		/* Price Adjustment */
		prc = prc_local*fx;
		prc_high = prc_high_lcl*fx;
		prc_low = prc_low_lcl*fx;
		me = prc*cshoc;
		dolvol = cshtrd*prc;
		ri = ri_local*fx;
	
		/* Dividend Adjustment (set to zero if missing)*/
		div_tot = coalesce(div, 0)*fx_div;
		div_cash = coalesce(divd, 0)*fx_div;
		div_spc = coalesce(divsp, 0)*fx_div;
		
		drop div divd divsp fx_div curcddv prc_high_lcl prc_low_lcl;
	run; 
	
	* Create Daily, Monthly or Both Datasets;
	%if &freq. = m or &freq. = d %then %let iter_max = 1;
	%if &freq. = both %then %let iter_max = 2;
	
	%do iter=1 %to &iter_max.;
		%if &freq. = m or &freq. = d %then %let freq_use = &freq.;  /* Neccesary because of the case where both daily AND monthly datasets are created */
		%if &freq. = both and &iter. = 1 %then %let freq_use = d;
		%if &freq. = both and &iter. = 2 %then %let freq_use = m;
		
		%if &freq_use.=m %then %do;
			proc sql;
				create table __comp_msf1 as 
				select *, intnx('month', datadate,0,'E') as eom format=YYMMDDN8., max(max(prc_high/ajexdi), max(prc/ajexdi))*ajexdi as prc_highm, min(min(prc_low/ajexdi), min(prc/ajexdi)) * ajexdi as prc_lowm,
					sum(div_tot/ajexdi)*ajexdi as div_totm, sum(div_cash/ajexdi)*ajexdi as div_cashm, sum(div_spc/ajexdi)*ajexdi as div_spcm,
					sum(cshtrd/ajexdi)*ajexdi as cshtrm, sum(dolvol) as dolvolm
				from __comp_dsf3
				group by gvkey, iid, calculated eom;
				
				create table __comp_msf2 as 
				select *
				from __comp_msf1(drop= cshtrd div_tot div_cash div_spc dolvol prc_high prc_low)
				where not missing(prc_local) and not missing(curcdd) and prcstd in (3, 4, 10) /* This is rather important when choosing last.eom later. Otherwise we sometimes have an end of month observations which is a dividend distribution rather than a trading day */
				order by gvkey, iid, eom, datadate;
			quit;
			
			data __comp_msf2; 
				set __comp_msf2; 
				rename 
					div_totm=div_tot 
					div_cashm=div_cash 
					div_spcm=div_spc
					dolvolm=dolvol
					prc_highm=prc_high
					prc_lowm=prc_low;
			run;
			
			/* Choose Last observation in Month */
			data __comp_msf3;
				set __comp_msf2;
				by gvkey iid eom;
				if last.eom;
			run;
			
			/* Add Information from SECM */
			proc sql;
				create table __comp_secm1 as 
				select a.gvkey, a.iid, a.datadate, intnx('month', a.datadate,0,'E') as eom format=YYMMDDN8.,
					a.tpci, a.exchg, a.curcdm as curcdd, a.prccm as prc_local, a.prchm as prc_high, a.prclm as prc_low, a.ajexm as ajexdi, 
					coalesce(a.cshom/1e6, a.csfsm/1e3, a.cshoq, b.csho_fund*b.ajex_fund/a.ajexm) as cshoc, /* Notive again that I impute with shares from accounting statements in case it is missing*/
					a.dvpsxm, a.cshtrm, a.curcddvm,
					a.prccm/a.ajexm*a.trfm as ri_local,  /*ri_local = local return index [1]*/
					c.fx as fx, d.fx as fx_div
				from comp.secm as a 
				left join __firm_shares2 as b
					on a.gvkey=b.gvkey and a.datadate=b.ddate
				left join fx as c
					on a.curcdm=c.curcdd and a.datadate=c.date
				left join fx as d
					on a.curcddvm=d.curcdd and a.datadate=d.date;
					
				update __comp_secm1
				set cshtrm = 
					case 
						when datadate < '01FEB2001'd then cshtrm / 2
						when datadate <= '31DEC2001'd then cshtrm / 1.8
						when datadate < '31DEC2003'd then cshtrm / 1.6
						else cshtrm
					end
				where exchg = 14;
			quit;
			
			data __comp_secm2; 
				set __comp_secm1; 
				/* Price Adjustment */
				if curcdd='USD' then fx=1; 
				prc = prc_local*fx;
				prc_high = prc_high*fx;
				prc_low = prc_low*fx;
				me = prc*cshoc;
				dolvol = cshtrm*prc;
				ri = ri_local*fx;
			
				/* Dividend Adjustment*/
				if curcddvm='USD' then fx_div=1;
				div_tot = dvpsxm*fx_div;
				div_cash = .;  /* Not available in SECM*/
				div_spc = .;  /* Not available in SECM*/
				
				drop dvpsxm fx_div curcddvm;
			run;
			
			%let common_vars=gvkey, iid, datadate, eom, tpci, exchg, curcdd, prc_local, prc_high, prc_low,
				ajexdi, cshoc, ri_local, fx, prc, me, cshtrm, dolvol, ri, div_tot, div_cash, div_spc;
			proc sql;
				create table __comp_msf4 as 
				select &common_vars., prcstd, 'secd' as source from __comp_msf3
				union
				select &common_vars., 10 as prcstd, 'secm' as source from __comp_secm2;  /* This ensures that all observations are kept in the __returns step. Erroneous prcstd codes have been screen out */
				
				create table __comp_msf5 as
				select *
				from __comp_msf4
				group by gvkey, iid, eom
				having count(*)=1 | (count(*)=2 and source='secd'); /* If a security has an observation in both SECD and SECM. Prefer the observation from SECD*/
			quit; 
			
			proc sort nodupkey data=__comp_msf5(drop=source) out=__comp_msf6; by gvkey iid eom; run; /* DUPLICATES should always be 0!*/
			proc delete data=__comp_msf1 __comp_msf2 __comp_msf3 __comp_msf4 __comp_msf5 __comp_secm1 __comp_secm2; run;
			
			%let base=__comp_msf6;
			%let period = 'month';
			%let out = comp_msf;
		%end;
		%if &freq_use.=d %then %do;
			%let base=__comp_dsf3;
			%let period = 'day';
			%let out = comp_dsf;
		%end;
		
		/* Compute Returns */
		proc sort nodupkey data= &base. out=__comp_sf1; by gvkey iid datadate; run; /* Very important to know if there are any duplicates!!*/
 
		data __returns;
			set __comp_sf1(where = (not missing(ri) and prcstd in (3, 4, 10))); /* The screen is important, see [1] */
			by gvkey iid;
			ret = ri/lag(ri)-1;
			ret_local = ri_local/lag(ri_local)-1;
			ret_lag_dif = intck(&period., lag(datadate), datadate);
			if first.iid then do;  
				ret=.;
				ret_local=.;
				ret_lag_dif=.;
			end;
			/* Handle situations where currency code changes */
			if first.iid=0 and curcdd^=lag(curcdd) then do;
				ret_local = ret;
			end;
			keep gvkey iid datadate ret ret_local ret_lag_dif;
		run;

		/* Handling Delisting */
		data __sec_info; set comp.security comp.g_security; run; /* Combine SECURITY and G_SECURITY*/
		
		data __delist1;
			set __returns(where=(not missing(ret_local) and ret_local^=0)); /* Take delisting date to be last day of trading with non missing and non zero return [2]*/
			by gvkey iid datadate;
			if last.iid;
		run;
		
		proc sql;
			create table __delist2 as 
			select a.gvkey, a.iid, a.datadate, b.secstat, b.dlrsni
			from __delist1 as a left join __sec_info as b
			on a.gvkey=b.gvkey and a.iid=b.iid;
			
			create table __delist3 as 
			select gvkey, iid, datadate as date_delist,
				case when dlrsni in ('02', '03') then -0.3 else 0 end as dlret  
			from __delist2
			where secstat='I';
		quit;
		
		* Incorporate Delisting Return;
		proc sql;
			create table __comp_sf2 as
			select a.*, b.ret, b.ret_local, b.ret_lag_dif, c.date_delist, c.dlret
			from &base as a 
			left join __returns as b
				on a.gvkey=b.gvkey and a.iid=b.iid and a.datadate=b.datadate
			left join __delist3 as c
				on a.gvkey=c.gvkey and a.iid=c.iid;
		quit;
		
		data __comp_sf3;
			set __comp_sf2;
			where datadate<=date_delist or missing(date_delist); /* In a sample of 104,377 this removes 1,434 obs*/
			if datadate=date_delist then do;
				ret = (1+ret)*(1+dlret)-1;
				ret_local = (1+ret_local)*(1+dlret)-1;
			end;
			drop ri ri_local date_delist dlret;
		run;
		
		/* Excess Return */
		%if &freq_use.=d %then %let scale=21;
		%if &freq_use.=m %then %let scale=1;
		
		proc sql;
			create table __comp_sf4 as 
			select a.*, a.ret-coalesce(b.t30ret, c.rf)/&scale. as ret_exc /* I prefer crsp.mcti but FF has monthly updates */
			from __comp_sf3 as a 
			left join crsp.mcti as b 
				on year(a.datadate)=year(b.caldt) and month(a.datadate)=month(b.caldt)
			left join ff.factors_monthly as c 
				on year(a.datadate)=year(c.date) and month(a.datadate)=month(c.date);
		quit;
		
		/* Add Exchange Information*/
		data __excntry;
			set comp.security comp.g_security;
		run;
		
		proc sql;
			create table __comp_sf5 as
			select a.*, b.excntry
			from __comp_sf4 as a left join __excntry as b
			on a.gvkey=b.gvkey and a.iid=b.iid;
		quit;
		
		/* Add Primary Security Indicator?*/
		%add_primary_sec(data=__comp_sf5, out=__comp_sf6, date_var=datadate);
		
		/* Output */
		proc sort nodupkey data=__comp_sf6 out=&out.; by gvkey iid datadate; run;
		
		/*proc delete data=&base.; run;*/
	%end;
	
	
	/* proc freq data=comp_dsf; tables ret_day_dif; run; *Check: 97.2% of non missing ret_day_dif are <=3. 98.8% are <=4; 99.75% are <=10. This might be a reasonable general cutoff for settings returns to 0*/
	*proc delete data=__firm_shares1 __firm_shares2 fx 
		__comp_dsf_na __comp_dsf_global __comp_dsf1 __comp_dsf2 __comp_dsf3 __comp_sf4 __comp_sf5
		__returns __sec_info __delist1 __delist2 __delist3
		__comp_sf1 __comp_sf2 __comp_sf3 __comp_sf4 __comp_sf5 __comp_sf6 __excntry &base.; *run;
%mend prepare_comp_sf;

/* COMBINE CRSP AND COMPUSTAT MONTHLY WITH CRSP PREFERENCE*/
%macro combine_crsp_comp_sf(out_msf=, out_dsf=, crsp_msf=, comp_msf=, crsp_dsf=, comp_dsf=);
	/* Monthly Files */
	proc sql;
		create table __msf_world1 as
		select cats('crsp_',permno) as id length=20, permno, permco, gvkey, iid, 'USA' as excntry length=3, (shrcd in (10, 11, 12)) as common, bidask,
			shrcd as crsp_shrcd, exchcd as crsp_exchcd, '' as comp_tpci, . as comp_exchg, primary_sec,
			'USD' as curcd, 1 as fx, date, intnx('month',date,0,'E') as eom format=YYMMDDN8., 
		   	cfacshr as adjfct, shrout as shares, me, me_company, prc, prc as prc_local, prc_high, prc_low, dolvol, vol as tvol, 
		   	ret, ret as ret_local, ret_exc, 1 as ret_lag_dif, div_tot, . as div_cash, . as div_spc, 'CRSP' as source length=9 
		from &crsp_msf.
		outer union corr
		select cats('comp_',gvkey,'_',iid) as id length=20, . as permno, . as permco, gvkey, iid, excntry, (tpci='0') as common, (prcstd = 4) as bidask,
			. as crsp_shrcd, . as crsp_exchcd, tpci as comp_tpci, exchg as comp_exchg, primary_sec,
		   	curcdd as curcd, fx, datadate as date, eom, 
		   	ajexdi as adjfct, cshoc as shares, me, me as me_company, prc, prc_local, prc_high, prc_low, dolvol, cshtrm as tvol,
		   	ret_local, ret, ret_exc, ret_lag_dif, div_tot, div_cash, div_spc, 'COMPUSTAT' as source  
		from &comp_msf.;
	quit;	
	
	/* Add Excess Return over Month t+1*/
	proc sort data=__msf_world1	; by id descending eom; run;
	data __msf_world2; 
		set __msf_world1;
		ret_exc_lead1m = lag(ret_exc);
		if lag(id)^=id or lag(ret_lag_dif)^=1 then
			ret_exc_lead1m = .;
	run;
	
	/* Daily Files */
	proc sql;
		create table __dsf_world1 as
		select cats('crsp_',permno) as id length=20, 'USA' as excntry length=3, (shrcd in (10, 11, 12)) as common, bidask,
			primary_sec, 'USD' as curcd, 1 as fx, date, intnx('month',date,0,'E') as eom format=YYMMDDN8., 
		   	cfacshr as adjfct, shrout as shares, me, dolvol, vol as tvol, prc, prc_high, prc_low,
		   	ret as ret_local, ret, ret_exc, 1 as ret_lag_dif 
		from &crsp_dsf.
		outer union corr
		select cats('comp_',gvkey,'_',iid) as id length=20, excntry, (tpci='0') as common, (prcstd = 4) as bidask,
			primary_sec, curcdd as curcd, fx, datadate as date, intnx('month',datadate,0,'E') as eom format=YYMMDDN8., 
		   	ajexdi as adjfct, cshoc as shares, me, dolvol, cshtrd as tvol, prc, prc_high, prc_low,
		   	ret_local, ret, ret_exc, ret_lag_dif  
		from &comp_dsf.;
	quit;	
	
	/* Choose the main observation based on monthly data */
	* If multiple observations for the same GVKEY-IID, Then choose CRSP as the main observation;
	proc sql;
		create table __obs_main as 
		select id, gvkey, iid, eom, (count(gvkey) in (0, 1) or (count(gvkey)=2 and source ='CRSP')) as obs_main
		from __msf_world2
		group by gvkey, iid, eom;
		
		create table __msf_world3 as 
		select a.*, b.obs_main
		from __msf_world2 as a left join __obs_main as b
		on a.id = b.id and a.eom = b.eom;
		
		create table __dsf_world2 as 
		select a.*, b.obs_main
		from __dsf_world1 as a left join __obs_main as b
		on a.id = b.id and a.eom = b.eom;
	quit;
	
	proc sort data=__msf_world3 out=&out_msf. nodupkey; by id eom; run;
	proc sort data=__dsf_world2 out=&out_dsf. nodupkey; by id date; run;
	
	proc delete data= __msf_world1 __msf_world2 __msf_world3 __dsf_world1 __dsf_world2; run; 
%mend combine_crsp_comp_sf;

* MACRO: MARKET RETURNS;
%macro market_returns(out=, data=, freq=m, wins=0.1);
	%if &freq.=d %then %do;
		%let dt_col = date;
		%let max_date_lag = 14;
	%end;
	%if &freq.=m %then %do;
		%let dt_col = eom;
		%let max_date_lag = 1;
	%end;
	/* Create Index Data */
	proc sql;
		create table __common_stocks1 as
		select distinct id, date, eom, excntry, obs_main, primary_sec, common, ret_lag_dif, me, dolvol, ret, ret_local, ret_exc
		from &data.
		order by id, &dt_col.;
	quit;
	
	data __common_stocks2;
		set __common_stocks1;
		by id;
		me_lag1 = lag(me);
		dolvol_lag1 = lag(dolvol);
		if first.id then do;
			me_lag1 = .;
			dolvol_lag1 = .;
		end;
	run;
	
	%let wins_high = %sysevalf(100-&wins.);
	%winsorize_own(inset=__common_stocks2, outset=__common_stocks3, sortvar=eom, vars=me_lag1 dolvol_lag1 ret ret_local ret_exc, perc_low=&wins., perc_high=&wins_high.); /* Notice I do winsorization by eom. Alternatively, we could do it by (excntry, date) but this could result in outliers when a country have few stocks */
	
	proc sql;
		create table mkt1 as
		select excntry, &dt_col., 
			count(*) as stocks, 
			sum(me_lag1) as me_lag1, 
			sum(dolvol_lag1) as dolvol_lag1,
			sum(ret_local*me_lag1)/(calculated me_lag1) as mkt_vw_lcl,
			mean(ret_local) as mkt_ew_lcl,
			sum(ret*me_lag1)/(calculated me_lag1) as mkt_vw,
			mean(ret) as mkt_ew,
			sum(ret_exc*me_lag1)/(calculated me_lag1) as mkt_vw_exc,
			mean(ret_exc) as mkt_ew_exc
		from __common_stocks3
		where obs_main = 1 and primary_sec = 1 and common = 1 and ret_lag_dif <= &max_date_lag. and not missing(me_lag1) and not missing(ret_local) 
		group by excntry, &dt_col.;
	quit;
	%if &freq.=m %then %do;
		data &out.; set mkt1; run;
	%end;
	%if &freq.=d %then %do;
		proc sql;
			create table &out. as
			select *
			from mkt1
			group by excntry, year(date), month(date)
			having stocks / max(stocks) >= 0.25; /* With less than 25% of stocks trading, it's likely that the date is not an official trading date */
		quit;
	%end;
%mend;

**********************************************************************************************************************
*  MACRO - Add size group classification based on NYSE stocks                                                        *
**********************************************************************************************************************
- Classify each stocks into one of five size groups based on their end of month market cap relative to NYSE breakpoints;
%macro nyse_size_groups(out=, data=);
	proc sort data=&data.(where=(
		(crsp_exchcd=1 or comp_exchg=11) and obs_main = 1 and primary_sec = 1 and common = 1 and ret_lag_dif = 1 and not missing(me) and not missing(ret_exc)
	) ) out=nyse_stocks; by eom; run;
	
	proc means data=nyse_stocks noprint;
		by eom;
		var me;
		output out=nyse_cutoff(drop=_type_ _freq_) N=n P1=nyse_p1 p20 = nyse_p20 P50=nyse_p50 p80 = nyse_p80;
	run;
	
	proc sql;
		create table &out. as
		select case 
				when a.me >= b.nyse_p80 then 'mega'
				when a.me >= b.nyse_p50 then 'large'
				when a.me >= b.nyse_p20 then 'small'
				when a.me >= b.nyse_p1 then 'micro'
				else 'nano'
			end as size_grp, a.*
		from &data. as a left join nyse_cutoff as b
		on a.eom=b.eom;
	quit;
%mend;

* MACRO: AP_FACTORS_DAILY
- This macro creates the factors from the 3-factors model of Fama and French (1993)
  as well as the factors from the 4-factor of Hou, Xue and Zhang (2015).
  Factors other than market and small minus big, are created using an unconditional double sort on sort and the underlying characteristics
  following the methodology of Fama and French (1993). Breakpoints are based on all non-microcap stocks within a country.
  Arguements 
  	- out: Name of output dataset
  	- freq: In (d, m) i.e. either daily or monthly
  	- sf: Dataset of &freq. stocks returns
  	- mchars: Dataset of characteristics with monthly frequency
  	- mkt: dataset with market returns. This is also used to identify trading days.
  	- min_stocks_bp: Minimum number of stocks used to create breakpoints. 
  	- min_stocks_pf: Minimum number of stocks available in the beginning of a month, to create a valid portfolio.
;
%macro ap_factors(out=, freq=, sf=, mchars=, mkt=, min_stocks_bp=, min_stocks_pf=);
	%if &freq.=d %then %do;
		/* Daily return data */
		proc sql;
			create table world_sf1 as 
			select excntry, id, date, eom, ret_exc
			from &sf.
			where ret_lag_dif <= 5 and not missing(ret_exc); * Impose a maximum lag of 5 days between return calculation;
		quit;
		%winsorize_own(inset=world_sf1, outset=world_sf2, sortvar=eom, vars=ret_exc, perc_low=0.1, perc_high=99.9); /* Winsorize Returns at 0.1% (Note I winsorize by month) */
		%let __date_col = date;
	%end;
	%if &freq.=m %then %do;
		/* Monthly return data */
		proc sql;
			create table world_sf1 as 
			select excntry, id, eom, ret_exc
			from &sf.
			where ret_lag_dif = 1 and not missing(ret_exc); 
		quit;
		%winsorize_own(inset=world_sf1, outset=world_sf2, sortvar=eom, vars=ret_exc, perc_low=0.1, perc_high=99.9); /* Winsorize Returns at 0.1% */
		%let __date_col = eom;
	%end;
	
	/* Sorting Variables */
	proc sql;
		create table base1 as 
		select id, eom, size_grp, excntry, me, market_equity, be_me, at_gr1, niq_be,
			source, obs_main, common, comp_exchg, crsp_exchcd, primary_sec, ret_lag_dif
		from &mchars.;
	quit;
	
	%winsorize_own(inset=base1, outset=base2, sortvar=eom, vars=me, perc_low=0.1, perc_high=99.9); /* Winsorize market equity at 0.1% */
	
	proc sort data=base2; by id eom; run;
	
	%macro temp();
	/* Lag variables used at portfolio rebalacing */
	%let cols_lag = comp_exchg crsp_exchcd obs_main common primary_sec excntry size_grp me be_me at_gr1 niq_be;
	data base3; 
		set base2;
		by id eom;
		%do i=1 %to %nwords(&cols_lag.); 
			%let col = %scan(&cols_lag., &i, %str(' '));
			&col._l = lag(&col.);
			if id ^= lag(id) or source ^= lag(source) or intck("month", lag(eom), eom)^=1 then
				&col._l = .;
			drop &col.;
		%end;
	run;
	%mend;
	%temp();
	
	/* Screens */
	proc sql;
		create table base4 as
		select *,  case 
					when missing(size_grp_l) then ''
					when size_grp_l in ('large', 'mega') then 'big'
					else 'small'
				end as size_pf
		from base3
		where obs_main_l = 1 and common_l = 1 and primary_sec_l = 1 and ret_lag_dif = 1 and not missing(me_l)
		order by excntry_l, size_grp_l, eom;
	quit;
	
	/* Factors: Three-by-two sort on var and size Fama-French Style*/
	%macro sort_ff_style(out=, char=, freq=, min_stocks_bp=, min_stocks_pf=);
		* Breakpoints (based on NYSE stocks in the US and non-microcap stocks outside of the US);
		proc sql;
			create table bp_stocks as
			select *
			from base4
			where ((size_grp_l in ('small', 'large', 'mega') and excntry_l ^= 'USA') or ((crsp_exchcd_l=1 or comp_exchg_l=11) and excntry_l = 'USA')) and not missing(&char._l)
			order by eom, excntry_l;
		quit;
		
		proc means data=bp_stocks noprint;
			by eom excntry_l;
			var &char._l;
			output out=bps(drop=_type_ _freq_) N=n P30 = bp_p30 P70=bp_p70;
		run;
		
		* Create weights by end of month;
		proc sql;
			create table weights1 as 
			select a.excntry_l, a.id, a.eom, a.size_pf, a.me_l,
				case 
					when a.&char._l >= b.bp_p70 then 'high'
					when a.&char._l >= b.bp_p30 then 'mid'
					else 'low'
				end as char_pf
			from base4 as a left join bps as b
			on a.excntry_l = b.excntry_l and a.eom = b.eom
			where b.n >= &min_stocks_bp. and not missing(a.&char._l) and size_pf^='';
			
			create table weights2 as 
			select *, me_l / sum(me_l) as w
			from weights1
			group by excntry_l, size_pf, char_pf, eom
			having count(*) >= &min_stocks_pf.;
		quit;
		
	
		* Match with return data;
		proc sql;
			create table returns as 
			select a.*, b.w, b.size_pf, b.char_pf
			from world_sf2 as a inner join weights2 as b
			on a.id = b.id and a.eom=b.eom and a.excntry=b.excntry_l;
		quit;
		
		* Create portfolio returns;
		proc sql;
			create table pfs1 as 
			select "&char." as characteristic, excntry, size_pf, char_pf, &__date_col., sum(ret_exc*w) as ret_exc
			from returns
			group by excntry, size_pf, char_pf, &__date_col.;
		quit;
		
		proc sort data=pfs1 out = pfs2; by characteristic excntry &__date_col.; run;
		proc transpose data=pfs2 delimiter=_ out=pfs3(drop=_name_);
			by characteristic excntry &__date_col.;
			var ret_exc;
			id size_pf char_pf;
		run;
		
		data &out.;
			set pfs3;
			lms = (small_high + big_high) / 2 - (small_low + big_low) / 2;
			smb = (small_high + small_mid + small_low) / 3 - (big_high + big_mid + big_low) / 3;
			keep characteristic excntry &__date_col. lms smb;
		run;
	%mend;
	
	/* Create Individual Factors */
	%sort_ff_style(out=book_to_market, char=be_me, min_stocks_bp = &min_stocks_bp., min_stocks_pf = &min_stocks_pf.);
	%sort_ff_style(out=asset_growth, char=at_gr1, min_stocks_bp = &min_stocks_bp., min_stocks_pf = &min_stocks_pf.);
	%sort_ff_style(out=roeq, char=niq_be, min_stocks_bp = &min_stocks_bp., min_stocks_pf = &min_stocks_pf.);
	
	/* Fama and French (1993) */
	data ff;
		set book_to_market;
		rename lms = hml;
		rename smb = smb_ff;
	run;
	
	/* Hou, Xue and Zhang (2015) */
	data hxz1;
		set asset_growth roeq;
	run;
	
	proc transpose data=hxz1 out=hxz2;
		by characteristic excntry &__date_col.;
		var lms smb;
	run;
	
	proc sort data=hxz2 out = hxz3; by excntry &__date_col.; run;
	proc transpose data=hxz3 delimiter=_ out=hxz4(drop=_name_);
		by excntry &__date_col.;
		var col1;
		id characteristic _name_;
	run;
	
	data hxz;
		set hxz4;
		rename niq_be_lms = roe;
		smb_hxz = (at_gr1_smb + niq_be_smb) / 2;
		inv = -at_gr1_lms;
	run;
	
	/* Factor Dataset */
	proc sql;
		create table &out. as 
		select a.excntry, a.&__date_col., a.mkt_vw_exc as mktrf, b.hml, b.smb_ff, c.roe, c.inv, c.smb_hxz
		from &mkt. as a 
		left join ff as b on a.excntry = b.excntry and a.&__date_col. = b.&__date_col.
		left join hxz as c on a.excntry = c.excntry and a.&__date_col. = c.&__date_col.;
	quit;
%mend;

* MACRO: SAVE_MAIN_DATA_CSV
- The macro saves the main data as separate .csv files by country. 
- By main data we mean data for common stocks that are the primary security of the underlying firm with non-missing return and lagged market equity 
  Arguments
	- out: Name of the output Zip file (will be saved in &path.)
	- data: should be the path to the main sas dataset
	- path: path where data is stored. Should be a scratch directory
;
%macro save_main_data_csv(out=, data=, path=);
	* Lagged me data;
	data main_data1;
		set &data.;
		me_lag1 = lag(me);
		if id ^= lag(id) or intck("month", lag(eom), eom)^=1 then
			me_lag1 = .;
	run;
	
	* Reorder Variables;
	data main_data2;
		retain id date eom source size_grp obs_main primary_sec gvkey iid permno permco excntry curcd fx 
			common comp_tpci crsp_shrcd comp_exchg crsp_exchcd
			adjfct shares me me_lag1; 
		set main_data1;
	run;
	
	* Screen;
	proc sql;
		create table main_data3 as
		select * 
		from main_data2
		where primary_sec = 1 and common = 1 and obs_main = 1 and not missing(me_lag1) and ret_lag_dif = 1 and not missing(ret_exc) and eom <= '31DEC2019'd;
	quit;
	
	proc sql noprint;
		select distinct lowcase(excntry) into :countries separated by ' '
		from main_data3;
	quit;
	
	/* Create country .csv files */
	option nonotes;
	%do i=1 %to %nwords(&countries.);
		%let __c = %scan(&countries., &i., %str(' '));
		%put ################ "&path./&__c..csv" ########################;
		proc export data=main_data3(where=(excntry = upcase("&__c.")))
		    outfile="&path./&__c..csv"   
		    dbms=CSV
		    replace;
		run;
	%end;
	option notes;
	
	* Zip file for easier download;
	ods package (newzip) open nopf;
	%do i=1 %to %nwords(&countries.);
		%let __c = %scan(&countries., &i., %str(' '));
		ods package (newzip) add file="&path./&__c..csv";
	%end;
	ods package (newzip) publish archive 
		properties (
			archive_name="&out..zip" 
			archive_path= "&path."
		);
	ods package(newzip) close;
	
	/* Delete intermidiate .csv files */
	%macro file_delete(file);
	  %let rc= %sysfunc(filename(fref,&file));
	  %let rc= %sysfunc(fdelete(&fref));
	%mend;
	%do i=1 %to %nwords(&countries.);
		%let __c = %scan(&countries., &i., %str(' '));
		%file_delete(file=&path./&__c..csv);
	%end;
	proc delete data = main_data1 main_data2 main_data3; run;
%mend;




/* Footnotes */*
[1]: This screen ensures that the return is always computed between two days with a price. The ret_day_dif lets the user check if it's within a reasonable range. 
	 Compustat have some observations whre prc and curcdd is missing and div and curcddiv is not missing. In other words dividend ex-date occuring outside trading days 
	 should not be used to compute returns. Further, the screen on prcstd ensures that returns are computed with non-stale prices. 
	 Specifically prcstd in (3, 4, 10) ensures that the price is taken from observable market data. The main excluded prcstd is 5, which is "No prices available, last actual price was carried forward".
	 This is prevalent in the G_SECD where it accounts for 14.5% of the observations. 
[2]: Compustat also include a security inactivation date called 'dldtei'. In a subsample of 300 inactive securities,
	 The difference in months between the date I take to be the delisting date and dldtei is 0 for 67% and 1 for 14%.
	 The rest of the observations are scattered from -432 to 317. I think this validates my choice but is is not completely clear.
;