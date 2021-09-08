## Overview
This repository contains code that is used for the paper [Is There a Replication Crisis in Finance?](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3774514) by Jensen, Kelly and Pedersen (2021). The code used to create the underlying dataset can be found at [https://github.com/bkelly-lab/GlobalFactor](https://github.com/bkelly-lab/GlobalFactor). Please cite Jensen, Kelly and Pedersen (2021) if using the code or data. 

## How To Run the Code

1. Start with factor returns in each country. The files you need to download are `hml.csv`, `cmp.csv` and `market_returns.csv` using one of the following methods.
	1. Download the factor returns used in the paper [here](https://www.dropbox.com/sh/wcrjok1qyxtrasi/AABZ90GDCUvIzDzijt8Qoo3ha?dl=0). 
	1. Download the latest version of the factor returns [here](https://www.dropbox.com/sh/ep40dynf2w3lck4/AACYXMDxbGOv21lVK7OC4fq7a?dl=0). 
	1. Generate the factor returns from scratch by following the steps in https://github.com/bkelly-lab/ReplicationCrisis/tree/master/GlobalFactors](https://github.com/bkelly-lab/ReplicationCrisis/tree/master/GlobalFactors) as the output from `portfolios.R`. 
2. Copy the code from this repository to a local folder. 
3. Open `main.R` in the programming language "R".
4. Ensure that the current working directory is the folder from 2. To check this, write `getwd()` in the console. To change the working directory use `setwd()`.
5. Run `main.R`.

## Outputs
1. The consol prints key numbers used in the paper as well as the paper tables in latex format.
2. If `save_figures=TRUE` (default), the folder in `output_path` will contain figures of the same format used in the paper. 

## Optional Settings 
1. `portfolio_path` is the folder with the portfolio data from step 1 (default: current working directory/Factors).
2. `object_path` is a folder where R objects can be saved for faster                iterations (default: current working directory/Objects).
3. `output_path` is a folder where figures can be saved (default: current working directory/Figures).
4. `save_figures` should be `TRUE` if you wish to save figures, otherwise `FALSE` (default: TRUE).
5. `settings` controls settings for the analysis, including the start and end date,           the portfolio weighting scheme, the cluster settings, the empirical Bayes settings, and the             tangency portfolio settings (default: the settings in the paper).  

## Notes

The code is divded into 4 separate R scripts. `0 - Functions.R` contains the project functions, `1 - Prepare Data.R` prepares the data, `2 - Determine Clusters.R` finds statistical clusters based on return data, `3 - Analysis.R` analyzes the data and `4 - Output.R` generates tables and figures based on the analysis. Everything is sourced from `main.R` which also contains user-defined control variables.  
