clear all
capture log close

use "https://github.com/scunning1975/causal-inference-class/raw/master/hansen_dwi"

gen dui = 0 
	replace dui = 1 if bac1 >= 0.08
	
rename bac1 bac1_orig
gen bac1 = bac1 - 0.08

rddensity bac1, c(0.08) plot

* balance tests

reg male dui##c.bac1 if bac1_orig >= 0.03 & bac1_orig <= 0.13, robust
reg white dui##c.bac1 if bac1_orig >= 0.03 & bac1_orig <= 0.13, robust
reg age dui##c.bac1 if bac1_orig >= 0.03 & bac1_orig <= 0.13, robust
reg acc dui##c.bac1 if bac1_orig >= 0.03 & bac1_orig <= 0.13, robust

reg male dui##c.bac1 if bac1_orig >= 0.055 & bac1_orig <= 0.105, robust
reg white dui##c.bac1 if bac1_orig >= 0.055 & bac1_orig <= 0.105, robust
reg age dui##c.bac1 if bac1_orig >= 0.055 & bac1_orig <= 0.105, robust
reg acc dui##c.bac1 if bac1_orig >= 0.055 & bac1_orig <= 0.105, robust

* non-parametric visualisation of avg recidivism

// ssc install cmogram, replace
cmogram recidivism bac1_orig if bac1_orig >= 0.003 & bac1_orig <= 0.13, cut(0.08) scatter lfit line(0.08)

cmogram recidivism bac1_orig if bac1_orig >= 0.003 & bac1_orig <= 0.13, cut(0.08) scatter qfitci line(0.08)

* main results

reg recidivism dui##c.bac1 if bac1_orig >= 0.003 & bac1_orig <= 0.13, robust
reg recidivism male white age acc dui##c.bac1 if bac1_orig >= 0.055 & bac1_orig <= 0.105, robust

* local polynomial regression with kernel weights and optimal bandwidth selection

// ssc install rdrobust, replace

rdrobust recidivism bac1, p(0) c(0.0) kernel(triangular) bwselect(mserd) all
rdrobust recidivism bac1_orig, p(0) c(0.08) kernel(triangular) bwselect(mserd) all

rdrobust recidivism bac1, p(1) c(0.0) kernel(triangular) bwselect(mserd) all

rdrobust recidivism bac1, p(2) c(0.0) kernel(triangular) bwselect(mserd) all

* optimal bandwidths graphs

rdplot recidivism bac1_orig if bac1_orig >= 0.03 & bac1_orig <= 0.13, p(0) c(0.08) kernel(triangular) bwselect(mserd)

rdplot recidivism bac1_orig if bac1_orig >= 0.03 & bac1_orig <= 0.13, p(3) c(0.08) kernel(triangular) bwselect(msetwo)

* donut hole regression

preserve
drop if bac1_orig >= 0.079 & bac1_orig <= 0.081

reg recidivism male white age acc dui##c.bac1 if bac1_orig >= 0.055 & bac1_orig <= 0.105, robust

restore












