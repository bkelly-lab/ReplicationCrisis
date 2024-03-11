# CHANGELOG.md
This change log keeps track of changes to the underlying data set. In brackets, we highlight versions of importance. The version with _factor data set_ is the basis of the factor portfolios we upload at [https://jkpfactors.com/](https://jkpfactors.com/). The version with _paper data set_ is the basis of [Jensen, Kelly and Pedersen (2023)](https://onlinelibrary.wiley.com/doi/full/10.1111/jofi.13249).

## 11-03-2024 [Factor data set]
__Changes__:
- Added 2023 data
- Updated the country classification according the latest MSCI market classification

## 03-03-2023 [Factor data set]
__Changes__:
- Added 2022 data
- Added 'me' (market equity) and 'ret' (total return) and removed 'source_crsp' from daily return files

__Impact__:
- Replication rate: 83.2% 

## 30-06-2022 [Paper data set] 

__Changes__:
- Changed name of "Skewness" cluster to "Short-Term Reversal"

__Impact__:
- Replication rate: 82.4% 

## 08-02-2022 

__Changes__:
- Fix error in the construction of intrinsic_value. Previously, we failed to scale intrinsic_value by market equity as done in Frankel and Lee (1998). We call the new characteristic ival_me and keep intrinsic_value in the data set. The alpha of the new factor based on ival_me is significantly different from zero, while the factor based on intrinsic_value is insignificant.

__Impact__:
- Replication rate: 82.4% (added 2020 data)


## 16-11-2021 

__Changes__:
- Changed return cutoffs to depend on all stocks, instead of only stocks from CRSP.
- Added monthly and daily returns to the output folder. 
- Changed the 'source' (character) column to 'source_crsp' (integer),. source_crsp is 1 if CRSP is the return data source.
- Changed the 'id' column from character to integer. For stocks from CRSP, the id is just their permno. For stocks from Compustat, the first digits is 1 if the stocks is traded on a US exchange, 2 if it's traded on a Canadian exchange, and 3 otherwise. The next two digits are the IID from Compustat, and the remaining six digits are the gvkey.  
- Adapted the primary_sec column such that all observations from CRSP have primary_sec=1. 
- Previously, we treated a zero return as a missing observation. Now, we have removed this screen, such that a zero return is treated like any other return. 
- Previously, we winsorized daily returns, market equity, and dollar volume, before creating charactersitics based on daily stock market data. Now, we have removed this winsorization, and daily characteristics are based on the raw data. 
- Added the option to create daily factor return in the portfolios.R code.
- Added the option to create industry returns in the portfolios.R code.

__Impact__:
- Replication rate: 83.2%

## 27-08-2021 

__Changes__:
- Fixed a bug regarding how daily delisting returns from CRSP is incorporated.
- Added indfmt='FS' to the international accounting data. 

__Impact__:
- Replication rate: 83.2%

## 14-06-2021 

__Changes__:

- We changed the winsorization scheme. First, we removed the 0.01%/99.9% winsorization of market equity in all countries. Second, we removed the winsorization of returns from the CRSP database. For Compustat returns, we set returns above (below) the 99.9% (0.01%) of CRSP returns in the same month, to that level. In other words, we base our winsorization of Compustat data on CRSP data from the same month. 
- We made several changes to the code for easier usability. Notably, the updated `main.sas` file returns a zip folder called "output" in the scratch folder, which contains all data neccesary to re-produce the results in the paper.  

__Impact__:

- Replication rate: 83.2%
- The revisions impacted all factors slightly, but the overall results are qualitatively very similar. 

## 02-19-2021 

__Changes__:

- Previously we did not exclude securities that are only traded over the counter. In the new version of the data set, we include an indicator column "exch_main" to exclude non-standard exchanges. In the US, the main exchanges are AMEX, NASDAQ and NYSE. Outside of the US, we exclude over the counter exchanges, stock connect exchanges in China and cross-country exchanges such as BATS Chi-X Europe. The documentation includes a full list of the excluded exchanges.  
- Included SIC, NAICS and GICS industry codes.

__Impact__:

- Replication rate: 84.0%. 
- Excluding non-standard exchanges mainly affected the US. By December 2019, the number of stocks in the US dropped from 5,256 to 4,102 (-22%) after adding the new 'exch_main' screen. The excluded securities are mainly tiny stocks traded over the counter, so the aggregate market cap only dropped by 2%. The change also mostly affected post 2000 data, because over the counter observations in Compustat are very rare before this point in time.    
- The change had a small effect outside of the US, because of our 'primary_sec' screen. It's very rare for Compustat to identify a security traded on a non-standard exchange as the primary security of a firm. 
- Because the changes mainly affected tiny stocks, our results did not change much. Across the 153 factors in the US, Developed and Emerging regions, the change in posterior monthly alpha ranged from -0.06% to +0.07.

## 02-15-2021

__Changes__:

- A bug caused _ivol_ff3_21d_, _iskew_ff3_21d_, _ivol_hxz4_21d_ and _iskew_hxz4_21d_ to require 17 (ff3) and 18 (hxz) observations for a valid estimate. Consistent with our original intent, we now require at least 15 observations for a valid estimate.    

__Impact__:

- Replication Rate: 84.0%. 
- The changes had a negligible effect on the affected factors.

## 02-01-2021 

__Changes__:

- Fixed a small bug in the bidask_hl() macro.
- When creating asset pricing factors (FF and HXZ), we previously required at least 5 stocks in a sub-portfolio (e.g. small stocks with high BM) for the observation to be valid. This led to missing observation in the 1950's for small stocks with low bm. We lowered this requirement to at least 3 stocks. Furthermore, when creating asset pricing factors, we changed the breakpoints to be based on NYSE stocks in the US instead of non-microcap stocks. Outside of the US, breakpoints are still based on non-microcap stocks.
  
__Impact__:

- Replication Rate: 84.0% 
- The _bidaskhl_21d_ factor changed slightly but is still significantly negative in all regions. The US factor IR changed from -0.11 to -0.09.
- The change in asset pricing factor generally didn't affect the results much.

## 01-25-2021
__Changes__:

- Changed residual momentum characteristics (resff3_12_1 & resff3_6_1) to be scaled with the standard deviation of residuals consistent with Blitz, Huij and Mertens (2011). 
- Fixed error in creating _qmj_prof_. The issue was that the _oaccruals_at_ used the value instead of the z-score of ranks. This effectively meant that accruals didn't impact the profitability score. 
- Fixed error for annual seasonality characteristics (factor names starting with seas_ and ending with _an). There was a bug in the screening procedure which meant that the characteristic for one stock could use information from an unrelated stock. 
- Rounding issues when converting a .csv file to an excel file, caused the zero_trades_* variables to not have any decimals which made the turnover tie-breaker ineffective.
- Standardized unexpected earnings (niq_su) and sales (saleq_su) is computed as the actual value minus the expected value (standardized by the standard deviation of this change). Before, the expected value was computed as the mean yearly change over the last 8 quarters added to the last quarterly value. Now the expected value is the same mean yearly change, but added to the quarterly value 4 quarters ago consistent with Jegadeesh and Livnat (2006).
  
__Impact__:

- Replication Rate: 84.0%
- The change to the residual momentum variables, made them slightly weaker. As an example, the monthly OLS information ratio of the US resff3_12_1 factor dropped from 0.33 to 0.28. 
- The _qmj_prof_ change made _qmj_prof_ and _qmj_ slightly stronger. As an example, the monthly OLS information ratio of the US _qmj_prof_ factor increased from 0.16 to 0.22.  
- The seasonality fix didn't have a large qualitative impact for the US factors, but did have a large positive effect outside of the US. As an example, the OLS IR of the developed market _seas_11_15an_ factor changed from -0.06 to 0.11.   
- The zero_trades where missing in the developed market because of too few non-missing observations. The developed market zero trades factors are generally strong and the IR ranges from 0.07 to 0.20. Similarly, The Emerging market zero trades factors where slightly negative before. After, the factors are strong with IRs ranging from 0.14 to 0.17. The US market zero trades factors improved slightly. The IR of zero_trades_21d has the most notable increase from 0.05 to 0.09.   
- The standardized unexpected sales (saleq_su) variable went from a significant IR of 0.12 to an insignificant IR of 0.05. This explains the drop in the replication rate. On the other hand, niq_su increased from 0.11 to 0.19.

## 01-15-2021 
__Changes__:

  - Base data set used in the first online version of Jensen, Kelly and Pedersen (2021).
  
__Impact__:

- Replication Rate: 84.9%
