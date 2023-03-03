## Overview
This repository contains the code used for the paper [Is There a Replication Crisis in Finance?](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3774514) by Jensen, Kelly and Pedersen (2022). Please cite this paper if you are using the code or data:
```
@article{JensenKellyPedersen2022,
   author = {Jensen, Theis Ingerslev and Kelly, Bryan T and Pedersen, Lasse Heje},
   journal = {Journal of Finance, Forthcoming},
   title = {Is There A Replication Crisis In Finance?},
   year = {2022}
}
```
Follow this [link](https://www.dropbox.com/sh/61j1v0sieq9z210/AACdJ68fs5_eT_eJMunwMBWia?dl=0) for a detailed documentation of the data sets.

The code consists of the following two self-contained components:

- [GlobalFactors](https://github.com/bkelly-lab/ReplicationCrisis/tree/master/GlobalFactors) is a folder with code that creates data sets of global stock returns, stock characteristics, and global long-short factors. Note that the long-short factors can also be downloaded directly, both the factor returns used in the paper ([here](https://www.dropbox.com/sh/wcrjok1qyxtrasi/AABZ90GDCUvIzDzijt8Qoo3ha?dl=0)) and the latest version of the factor returns (see weblink below). In addition, we keep a folder with the latest versions of the factor returns and additional data such as the underlying portfolios, market returns, and industry returns ([link](https://www.dropbox.com/sh/zvrnbfg6u8ugo8o/AABugE3vXglTg-tr32Wb9-hHa?dl=0)).

- [Analysis](https://github.com/bkelly-lab/ReplicationCrisis/tree/master/Analysis) is a folder that contains the analysis in the paper, including all figures and tables. This folder takes the global factors as input (either the ones that can be downloaded or the ones that you construct yourself). 

See also the website [https://JKPfactors.com/](https://JKPfactors.com/), where the most recent long-short factors can be downloaded using a simple drop-down menu. 

