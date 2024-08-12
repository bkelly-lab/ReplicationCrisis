## Overview
This repository contains the code used for the paper [Is There a Replication Crisis in Finance?](https://onlinelibrary.wiley.com/doi/10.1111/jofi.13249) by Jensen, Kelly and Pedersen (2023). Please cite this paper if you are using the code or data:
```
@article{JensenKellyPedersen2023,
	author = {Jensen, Theis Ingerslev and Kelly, Bryan and Pedersen, Lasse Heje},
	title = {Is There a Replication Crisis in Finance?},
	journal = {The Journal of Finance},
	volume = {78},
	number = {5},
	pages = {2465-2518},
	year = {2023}
}
```
Follow this [link](https://www.dropbox.com/sh/61j1v0sieq9z210/AACdJ68fs5_eT_eJMunwMBWia?dl=0) for a detailed documentation of the data sets.

The code consists of the following two self-contained components:

- [GlobalFactors](https://github.com/bkelly-lab/ReplicationCrisis/tree/master/GlobalFactors) is a folder with code that creates data sets of global stock returns, firm characteristics, and global long-short factors. __Note that the data can be downloaded without running the code__. The global stock return and firm characteristics can be downloaded from WRDS ([link](https://wrds-www.wharton.upenn.edu/pages/get-data/contributed-data-forms/global-factor-data/)). The long-short factors factor returns used in the paper ([here](https://www.dropbox.com/sh/wcrjok1qyxtrasi/AABZ90GDCUvIzDzijt8Qoo3ha?dl=0)) and the latest version of the factor returns (see weblink below). In addition, we keep a folder with the latest versions of the factor returns and additional data such as the underlying portfolios, market returns, and industry returns ([link](https://www.dropbox.com/sh/xq278bryrj0qf9s/AABUTvTGok91kakyL07LKyQoa?dl=0)).

- [Analysis](https://github.com/bkelly-lab/ReplicationCrisis/tree/master/Analysis) is a folder that contains the analysis in the paper, including all figures and tables. This folder takes the global factors as input (either the ones that can be downloaded or the ones that you construct yourself). 

See also the website [https://JKPfactors.com/](https://JKPfactors.com/), where the most recent long-short factors can be downloaded using a simple drop-down menu. 

