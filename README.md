# HiGHmedUCCScores
Scoring functions of the HiGHmed use case Cardiology

HiGHmedUCCScores is an R Packages which was created to allow the calculation of two different types of heart failure scores, to calculate the survival prospects of heart failure patients. 
The scoring functions were selected based on the availability of their input data items in the minimal dataset of the [HiGHmed Use Case Cardiology](https://www.highmed.org/highmed-use-case-cardiology). 

The used scoring systems are the *MAGGIC Score* and the *Barcelona BioHF Score*. 


## MAGGIC Score

The [MAGGIC Score](https://academic.oup.com/eurheartj/article/34/19/1404/422939?login=true) from 2013 is based on a lookup table, that assigns a numeric value to each 
value expression of a set of categorical and numeric clinical parameters, which are finally summed up and a survival probability in a time span of 1 and 3 years is deducted. 

An [online tool](https://www.mdcalc.com/maggic-risk-calculator-heart-failure#why-use) to calculate the score for single patients is available. 

The algorithm allows no NA handling, if one or more input values of the algorithm is missing, the score cannot be calculated for the respective patient. 

## Barcelona BioHF Score

The Barcelona Bio HF Score was released in two different versions:  [ V1 (2014) ](https://pubmed.ncbi.nlm.nih.gov/24454874/) and [ V2 (2017) ](https://onlinelibrary.wiley.com/doi/full/10.1002/ejhf.949).

Only for the newer version there is still an [online calculator](http://ww2.bcnbiohfcalculator.org/web/en/disclaimer) for the single patient use available. 

In comparison to the MAGGIC Score, the Barcelone Bio HF allows for certain optional numeric values to be missing. 
It also uses a different algorithmic approach than the MAGGIG score: A regression formula, which can also be adjusted (by which coefficients to be used) based on what data is available for calculation. 
The details are described in the [supplements of the BCN BioHF publication](https://pubmed.ncbi.nlm.nih.gov/24454874/), 
in particular in the [accopanying word document](https://journals.plos.org/plosone/article/file?type=supplementary&id=info:doi/10.1371/journal.pone.0085466.s002). 

## R Implementation

The three scoring systems (MAGGIC, BCN BioHF V1, BCN BioHF V2) were transcribed into R based formulas. The functions can be used on single values,
but also by using `dplyr` on entire dataframes. 

Since the package was created using `Roxygen` they can be installed using `devtools` and each function has an R-style manual page.

## Installation

Unpack or clone the package into a directory of your choice.

Within R, one needs to install the R library `devtools` : 

`install.packages("devtools")`

Then change into the respective directory:

`setwd("~/dir/of/your/choice/HiGHmedUCCScores")`

Install by

`devtools::install()`

After successful installation one can check the manual pages: 

`?HiGHmedUCCScores::calc_bcn_biohf_v1`

`?HiGHmedUCCScores::calc_bcn_biohf_v2`

`?HiGHmedUCCScores::calc_maggic_score`


