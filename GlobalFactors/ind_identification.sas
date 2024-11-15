* MACRO: FF_IND_CLASS
	Add variable matching 4-digit SIC identifiers to Fama-French industry identifiers
	   Arguments:
	   	data: name of input dataset that includes 4-digit SIC codes under name 'sic'
	   	ff_grps: number of industry portfolios for Fama-French identifiers
	   	OUT: name of output dataset;
%macro ff_ind_class(data=, ff_grps=, out=);
	%if &ff_grps = 38 %then %do;
		proc sql;
			/* French identifies "Other" as "almost nothing", so no firms are identified as "other" 
			https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/Data_Library/det_38_ind_port.html*/
			create table &out. as
			select *, 
				case
					when 100 <= sic <= 999 then 1
					when 1000 <= sic <= 1299 then 2 
					when 1300 <= sic <= 1399 then 3
					when 1400 <= sic <= 1499 then 4
					when 1500 <= sic <= 1799 then 5
					when 2000 <= sic <= 2099 then 6
					when 2100 <= sic <= 2199 then 7
					when 2200 <= sic <= 2299 then 8
					when 2300 <= sic <= 2399 then 9
					when 2400 <= sic <= 2499 then 10
					when 2500 <= sic <= 2599 then 11
					when 2600 <= sic <= 2661 then 12
					when 2700 <= sic <= 2799 then 13
					when 2800 <= sic <= 2899 then 14
					when 2900 <= sic <= 2999 then 15
					when 3000 <= sic <= 3099 then 16
					when 3100 <= sic <= 3199 then 17
					when 3200 <= sic <= 3299 then 18
					when 3300 <= sic <= 3399 then 19
					when 3400 <= sic <= 3499 then 20
					when 3500 <= sic <= 3599 then 21
					when 3600 <= sic <= 3699 then 22
					when 3700 <= sic <= 3799 then 23
					when 3800 <= sic <= 3879 then 24
					when 3900 <= sic <= 3999 then 25
					when 4000 <= sic <= 4799 then 26
					when 4800 <= sic <= 4829 then 27
					when 4830 <= sic <= 4899 then 28
					when 4900 <= sic <= 4949 then 29
					when 4950 <= sic <= 4959 then 30
					when 4960 <= sic <= 4969 then 31
					when 4970 <= sic <= 4979 then 32
					when 5000 <= sic <= 5199 then 33
					when 5200 <= sic <= 5999 then 34
					when 6000 <= sic <= 6999 then 35
					when 7000 <= sic <= 8999 then 36 
					when 9000 <= sic <= 9999 then 37
					else .
				end as ff38
			from &data.;
		run;
		%end;
		
	%else %if &ff_grps. = 49 %then %do;
		proc sql;
			create table &out. as
			select *, 	
				case
					when sic = 2048 or 100 <= sic <= 299 or 700 <= sic <= 799 or 910 <= sic <= 919 then 1
					when sic in (2095, 2098, 2099) or 2000 <= sic <= 2046 or 2050 <= sic <= 2063 or 
						2070 <= sic <= 2079 or 2090 <= sic <= 2092 then 2
					when sic in (2086, 2087, 2096, 2097) or 2064 <= sic <= 2068 then 3
					when sic = 2080 or 2082 <= sic <= 2085 then 4
					when 2100 <= sic <= 2199 then 5
					when sic in (3732, 3930, 3931) or 920 <= sic <= 999 or 3650 <= sic <= 3652 or 3940 <= sic <= 3949 then 6
					when sic in (7840, 7841, 7900, 7910, 7911, 7980) or 7800 <= sic <= 7833 or 7920 <= sic <= 7933 or 
						7940 <= sic <= 7949 or 7990 <= sic <= 7999 then 7
					when sic in (2770, 2771) or 2700 <= sic <= 2749 or 2780 <= sic <= 2799 then 8
					when sic in (2047, 2391, 2392, 3160, 3161, 3229, 3260, 3262, 3263, 3269, 3230, 3231, 3750, 3751, 3800, 3860, 
						3861, 3910, 3911, 3914, 3915, 3991, 3995) or 2510 <= sic <= 2519 or 2590 <= sic <= 2599 or 
						2840 <= sic <= 2844 or 3170 <= sic <= 3172 or 3190 <= sic <= 3199 or 3630 <= sic <= 3639 or 
						3870 <= sic <= 3873 or 3960 <= sic <= 3962 then 9
					when sic in (3020, 3021, 3130, 3131, 3150, 3151) or 2300 <= sic <= 2390 or 3100 <= sic <= 3111 or 
						 3140 <= sic <= 3149 or 3963 <= sic <= 3965 then 10
					when 8000 <= sic <= 8099 then 11
					when sic in (3693, 3850, 3851) or 3840 <= sic <= 3849 then 12
					when sic in (2830, 2831) or 2833 <= sic <= 2836 then 13
					when 2800 <= sic <= 2829 or 2850 <= sic <= 2879 or 2890 <= sic <= 2899 then 14
					when sic in (3031, 3041) or 3050 <= sic <= 3053 or 3060 <= sic <= 3099 then 15
					when 2200 <= sic <= 2284 or 2290 <= sic <= 2295 or 2297 <= sic <= 2299 or 2393 <= sic <= 2395 or 
						2397 <= sic <= 2399 then 16
					when sic in (2660, 2661, 3200, 3210, 3211, 3240, 3241, 3261, 3264, 3280, 3281, 3446, 3996) or 
						800 <= sic <= 899 or 2400 <= sic <= 2439 or 2450 <= sic <= 2459 or	2490 <= sic <= 2499 or 
						2950 <= sic <= 2952 or 3250 <= sic <= 3259 or 3270 <= sic <= 3275 or 3290 <= sic <= 3293 or 
						3295 <= sic <= 3299 or 3420 <= sic <= 3429 or 3430 <= sic <= 3433 or 3440 <= sic <= 3442 or 
						3448 <= sic <= 3452 or 3490 <= sic <= 3499 then 17
					when 1500 <= sic <= 1511 or 1520 <= sic <= 1549 or 1600 <= sic <= 1799 then 18
					when sic = 3300 or 3310 <= sic <= 3317 or 3320 <= sic <= 3325 or 3330 <= sic <= 3341 or 3350 <= sic <= 3357
						or 3360 <= sic <= 3379 or 3390 <= sic <= 3399 then 19
					when sic in (3400, 3443, 3444) or 3460 <= sic <= 3479 then 20
					when sic in (3538, 3585, 3586) or 3510 <= sic <= 3536 or 3540 <= sic <= 3569 or 3580 <= sic <= 3582 or 
						3589 <= sic <= 3599 then 21
					when sic in (3600, 3620, 3621, 3648, 3649, 3660, 3699) or 3610 <= sic <= 3613 or 3623 <= sic <= 3629 or 
						3640 <= sic <= 3646 or 3690 <= sic <= 3692 then 22
					when sic in (2296, 2396, 3010, 3011, 3537, 3647, 3694, 3700, 3710, 3711, 3799) or 3713 <= sic <= 3716 or 
						3790 <= sic <= 3792 then 23
					when sic in (3720, 3721, 3728, 3729) or 3723 <= sic <= 3725 then 24
					when sic in (3730, 3731) or 3740 <= sic <= 3743 then 25
					when sic = 3795 or 3760 <= sic <= 3769 or 3480 <= sic <= 3489 then 26
					when 1040 <= sic <= 1049 then 27
					when 1000 <= sic <= 1039 or 1050 <= sic <= 1119 or 1400 <= sic <= 1499 then 28
					when 1200 <= sic <= 1299 then 29
					when sic in (1300, 1389) or 1310 <= sic <= 1339 or 1370 <= sic <= 1382 or 2900 <= sic <= 2912 or 
						2990 <= sic <= 2999 then 30
					when sic in (4900, 4910, 4911, 4939) or 4920 <= sic <= 4925 or 4930 <= sic <= 4932 or 4940 <= sic <= 4942 then 31
					when sic in (4800, 4899) or 4810 <= sic <= 4813 or 4820 <= sic <= 4822 or 4830 <= sic <= 4841 or 
						4880 <= sic <= 4892 then 32
					when sic in (7020, 7021, 7200, 7230, 7231, 7240, 7241, 7250, 7251, 7395, 7500, 7600, 7620, 7622, 7623, 7640, 
						7641) or 7030 <= sic <= 7033 or 7210 <= sic <= 7212 or 7214 <= sic <= 7217 or 7219 <= sic <= 7221 or 
						7260 <= sic <= 7299 or 7520 <= sic <= 7549 or 7629 <= sic <= 7631 or 7690 <= sic <= 7699 or 
						8100 <= sic <= 8499 or 8600 <= sic <= 8699 or 8800 <= sic <= 8899 or 7510 <= sic <= 7515 then 33
					when sic in (3993, 7218, 7300, 7374, 7396, 7397, 7399, 7519, 8700, 8720, 8721) or 2750 <= sic <= 2759 or 
						7310 <= sic <= 7342 or 7349 <= sic <= 7353 or 7359 <= sic <= 7369 or 7376 <= sic <= 7385 or 
						7389 <= sic <= 7394 or 8710 <= sic <= 8713 or 8730 <= sic <= 8734 or 8740 <= sic <= 8748 or 
						8900 <= sic <= 8911 or 8920 <= sic <= 8999 or 4220 <= sic <= 4229 then 34
					when sic = 3695 or 3570 <= sic <= 3579 or 3680 <= sic <= 3689 then 35
					when sic = 7375 or 7370 <= sic <= 7373 then 36
					when sic in (3622, 3810, 3812) or 3661 <= sic <= 3666 or 3669 <= sic <= 3679 then 37
					when sic = 3811 or 3820 <= sic <= 3827 or 3829 <= sic <= 3839 then 38
					when sic in (2760, 2761) or 2520 <= sic <= 2549 or 2600 <= sic <= 2639 or 2670 <= sic <= 2699 or 
						3950 <= sic <= 3955 then 39
					when sic in (3220, 3221) or 2440 <= sic <= 2449 or 2640 <= sic <= 2659 or 3410 <= sic <= 3412 then 40
					when sic in (4100. 4130, 4131, 4150, 4151, 4230, 4231, 4780, 4789) or 4000 <= sic <= 4013 or 4040 <= sic <= 4049 
						or 4110 <= sic <= 4121 or 4140 <= sic <= 4142 or 4170 <= sic <= 4173 or 4190 <= sic <= 4200 or 
						4210 <= sic <= 4219 or 4240 <= sic <= 4249 or 4400 <= sic <= 4700 or 4710 <= sic <= 4712 or 
						4720 <= sic <= 4749 or 4782 <= sic <= 4785 then 41
					when sic in (5000, 5099, 5100) or 5010 <= sic <= 5015 or 5020 <= sic <= 5023 or 5030 <= sic <= 5060 or 
						5063 <= sic <= 5065 or 5070 <= sic <= 5078 or 5080 <= sic <= 5088 or 5090 <= sic <= 5094 or 
						5110 <= sic <= 5113 or 5120 <= sic <= 5122 or 5130 <= sic <= 5172 or 5180 <= sic <= 5182 or 
						5190 <= sic <= 5199 then 42
					when sic in (5200, 5250, 5251, 5260, 5261, 5270, 5271, 5300, 5310, 5311, 5320, 5330, 5331, 5334, 5900, 5999) or
						5210 <= sic <= 5231 or 5340 <= sic <= 5349 or 5390 <= sic <= 5400 or 5410 <= sic <= 5412 or 
						5420 <= sic <= 5469 or 5490 <= sic <= 5500 or 5510 <= sic <= 5579 or 5590 <= sic <= 5700 or 
						5710 <= sic <= 5722 or 5730 <= sic <= 5736 or 5750 <= sic <= 5799 or 5910 <= sic <= 5912 or 
						5920 <= sic <= 5932 or 5940 <= sic <= 5990 or 5992 <= sic <= 5995 then 43 
					when sic in (7000, 7213) or 5800 <= sic <= 5829 or 5890 <= sic <= 5899 or 7010 <= sic <= 7019 or 
						7040 <= sic <= 7049 then 44
					when sic = 6000 or 6010 <= sic <= 6036 or 6040 <= sic <= 6062 or 6080 <= sic <= 6082 or 6090 <= sic <= 6100 
						or 6110 <= sic <= 6113 or 6120 <= sic <= 6179 or 6190 <= sic <= 6199 then 45
					when sic in (6300, 6350, 6351, 6360, 6361) or 6310 <= sic <= 6331 or 6370 <= sic <= 6379 or 6390 <= sic <= 6411 
						then 46
					when sic in (6500, 6510, 6540, 6541, 6610, 6611) or 6512 <= sic <= 6515 or 6517 <= sic <= 6532 or 
						6550 <= sic <= 6553 or 6590 <= sic <= 6599 then 47
					when sic in (6700, 6798, 6799) or 6200 <= sic <= 6299 or 6710 <= sic <= 6726 or 6730 <= sic <= 6733 or 
						6740 <= sic <= 6779 or 6790 <= sic <= 6795 then 48
					when sic in (4970, 4971, 4990, 4991) or 4950 <= sic <= 4961 then 49
					else .
				end as ff49
				from &data.;
			run;
			%end;
%mend;

* MACRO: CRSP_INDUSTRY
	Create daily historical SIC and NAICS industry identifiers dataset from CRSP data 
	   Arguments:
	   	out: name of output dataset;
%macro crsp_industry(out=);
	/* Pull distinct date ranges and identifiers from CRSP datasets */
	proc sql;
		create table permno0 as
		select distinct permno, permco, namedt, nameendt, siccd as sic, input(naics, 6.0) as naics/* NAICS codes can't have leading zeroes so this should be okay*/
		from crsp.dsenames
		order by permno, namedt, nameendt;
	run;
	
	/* Alter missing industry identifiers */
	data permno1;
		set permno0;
		if missing(sic) then sic = -999;
		if sic = 0 then sic = -999;
		if missing(naics) then naics = -999;
	run;
	
	/* Find date distance for date ranges */
	data permno2; 
		set permno1;
		permno_diff = intck('day', namedt, nameendt, 'd');
	run;

	proc sort data = permno2;
		by permno namedt nameendt;
	run;
	
	/* Create new rows between valid dates */
	data permno3;
		set permno2;
		output;
		n = 0;
		if permno_diff > 0 then do;
			do until(n = permno_diff);
				namedt = intnx('day', namedt, 1);
				n + 1;
				output;
			end;
		end;
		drop nameendt permno_diff n;
	run;
	
	/* Get ready for output */
	data permno4;
		set permno3;
		if sic = -999 then sic = .;
		if naics = -999 then naics = .;
		date = namedt;
		drop namedt;
		format date yymmddn8.;
	run;
		
	proc sort data= permno4 out= &out. nodup; by permno date; run;
	
	proc delete data = permno0 permno1 permno2 permno3 permno4; run;
%mend; 

* MACRO: COMP_SIC_NAICS 
	Create a daily historical SIC and NAICS industry identifiers dataset using NA and global annual reports
	   Arguments: 
	   	OUT: name of output dataset;
%macro comp_sic_naics(OUT =, ff_num =);
	proc sql;
		/* Retrieve NA identifiers */
		create table comp1 as
		select distinct gvkey, datadate, sich as sic, naicsh as naics
		from COMP.FUNDA;
	run;
	
	/* Fix error of gvkey code 175650 */
	data comp2;
		set comp1;
		if gvkey = "175650" and datadate = '31DEC2005'd and missing(naics) then delete;
	run;
		
	proc sql;
		/* Retrieve global identifiers */
		create table comp3 as
		select distinct gvkey, datadate, sich as sic, naicsh as naics
		from COMP.G_FUNDA;
		
		/* Join global and NA data */
		create table comp4 as
		select a.gvkey as gvkeya, a.datadate as datea, a.sic as sica, a.naics as naicsa, 
			b.gvkey as gvkeyb, b.datadate as dateb, b.sic as sicb, b.naics as naicsb
		from comp2 as a full join comp3 as b
		on a.gvkey = b.gvkey and a.datadate = b.datadate;
	run;
	
	/* Coalesce NA and global */
	data comp5;
		set comp4;
		gvkey = put(coalesce(gvkeya, gvkeyb), $z6.);
		date  = coalesce(datea, dateb);
		sic   = coalesce(sica, sicb);
		naics = coalesce(naicsa, naicsb);
		format date yymmddn8.;
		drop gvkeya gvkeyb datea dateb sica sicb naicsa naicsb;
	run;
	
	/* Sort descending*/
	proc sort data = comp5;
		by gvkey descending date;
	run;
	
	/* Add valid date to in order to extend to daily observation */
	data comp6;
		set comp5;
		by gvkey;
		valid_to = intnx('day', lag(date), -1);
		if FIRST.gvkey then do;
			valid_to = date;
		end;
		format valid_to yymmddn8.;
	run;
	
	/* Re-sort */
	proc sort data = comp6;
		by gvkey date valid_to;
	run;
	
	/* Find date distance for date ranges */
	data comp7; 
		set comp6;
		comp_diff = intck('day', date, valid_to, 'd');
	run;

	proc sort data = comp7;
		by gvkey date valid_to;
	run;
	
	/* Create new rows between valid dates */
	data comp8;
		set comp7;
		output;
		n = 0;
		if comp_diff > 0 and comp_diff ne . then do;
			do until(n = comp_diff);
				date = intnx('day', date, 1);
				n + 1;
				output;
			end;
		end;
		drop valid_to comp_diff n;
	run;

	proc sort data= comp8 out= &out. nodup; by gvkey date; run;
	
	proc delete data = comp1 comp2 comp3 comp4 comp5 comp6 comp7 comp8; run;
%mend;

* MACRO: COMP_HGICS
	 Create a daily historical gics dataset from COMPUSTAT, either from the NA or global dataset 
	   Arguments:
	   	lib: COMPUSTAT library from which to pull historical gics data (CO_HGICS if NA, G_CO_HGICS if global)
		OUT: name of output dataset;
%macro COMP_HGICS(lib =, out =);
	/* Pull historical gics data */
	proc sql;
		create table gic1 as
		select distinct gvkey, indfrom, indthru, gsubind as gics
		from comp.&lib.
		where not missing(gvkey);
	run;
	
	proc sort data = gic1;
		by gvkey indfrom;
	run;
	
	/* Alter missing gics */
	data gic2;
		set gic1;
		by gvkey;
		if missing(gics) then gics = -999;
	run;
	
	/* Adjust indthru */
	data gic3;
		set gic2;
		by gvkey indfrom indthru;
		if LAST.gvkey and indthru = . then indthru = today();
	run;

	/* Estimate difference between indfrom and indthru */
	data gic4;
		set gic3;
		gic_diff = intck('days', indfrom, indthru);
	run;
	
	proc sort data = gic4;
		by gvkey indfrom indthru;
	run;

	/* Add rows to create daily data */
	data gic5;
		set gic4;
		by gvkey;
		output;
		n = 0;
		if gic_diff > 0 and gic_diff ne . then do;
			do until(n = gic_diff);
				indfrom = intnx('day', indfrom, 1);
				n + 1;
				output;
			end;
		end;
	run;
	
	data gic6;
		set gic5;
		date = indfrom;
		format date yymmddn8.;
		drop indfrom indthru gic_diff n;
	run;
	
	proc sort data= gic6 out=&OUT nodup; by gvkey date; run;
		
	proc delete data = gic1 gic2 gic3 gic4 gic5 gic6; run;
%mend COMP_HGICS;

/* MACRO: HGICS JOIN
	Join NA and global daily historical gics data from COMPUSTAT 
	   Argument: 
	   	OUT: name of output dataset */
%macro HGICS_JOIN(out=);
	/* Construct NA and global historical gics data */
	%comp_hgics(lib = co_hgic, OUT = na_hgics);
	%comp_hgics(lib = g_co_hgic, OUT = g_hgics);
	
	proc sql;
		create table gjoin1 as
		select a.gvkey as na_gvkey, a.gics as na_gics, a.date as na_date, b.gvkey as g_gvkey, b.gics 
			as g_gics, b.date as g_date
		from na_hgics as a full join g_hgics as b 
		on a.gvkey = b.gvkey and a.date = b.date;
	
	/* Coalesce NA and global */
	data gjoin2;
		set gjoin1;
		gvkey = put(coalesce(na_gvkey, g_gvkey), $z6.);
		date  = coalesce(na_date, g_date);
		gics  = coalesce(na_gics, g_gics);
		format date yymmddn8.;
		drop na_gvkey na_date na_gics g_gvkey g_date g_gics;
	run;
	
	proc sort data = gjoin2 out= &out nodup; by gvkey date; run;
	
	proc delete data = na_hgics g_hgics gjoin1 gjoin2; run;
%mend;

* MACRO: COMP_INDUSTRY
	Join SIC and NAICS industry identifiers to GICS identifiers constructed from COMPUSTAT data
	   Arguments:
	   	OUT: name of output dataset;
%macro comp_industry(out=);
	/* Construct datasets */
	%hgics_join(out=comp_gics);
	%comp_sic_naics(out=comp_other);
	
	/* Join datasets */
	data join1;
		merge comp_gics comp_other;
		by gvkey date;
	run;
	
	proc sort data = join1 nodupkey;
		by gvkey date;
	run;
	
	/* Check for gaps in coverage */
	data join2;
		set join1;
		by gvkey date;
		lagdate = lag(date);
		date_1  = intnx('day', date, -1);
		gap     = 0;
		format lagdate yymmddn8. date_1 yymmddn8.;
	run;
	
	data join3;
		set join2;
		by gvkey date;
		if not FIRST.gvkey and lagdate ne date_1 then gap = 1;
	run;
	
	/* Create rows for gaps in coverage with all indicators as missing */
	proc sql;
		create table gap1 as
		select *
		from join3
		where gap = 1;
	run;
	
	/* Size of gap */
	data gap2;
		set gap1;
		diff = intck('days', lagdate, date);
	run;
	
	/* Add rows to create daily data */
	data gap3;
		set gap2;
		by gvkey date;
		output;
		n = 0;
		if gap = 1 then do;
			do until(n = diff - 1);
				date      = intnx('day', date, -1);
				gics      = .;
				sic       = .;
				naics     = .;
				n + 1;
				output;
			end;
		end;
		drop lagdate date_1 gap diff n; 
	run;
	
	proc sort data = gap3;
		by gvkey date;
	run;
	
	/* Join added rows to original daily data */
	data joined1;
		merge join1 gap3;
		by gvkey date;
	run;
	
	proc sort data = joined1 out= &out. nodup; by gvkey date; run;	
	
	proc delete data = comp_gics comp_other join1 join2 join3 gap1 gap2 gap3 joined1; run;
%mend;

