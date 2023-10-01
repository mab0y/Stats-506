clear

cd C:\Users\mab0y\OneDrive\文档\Stats-506\PS-03
import sasxport5 DEMO_D.XPT
save DEMO_D
clear
import sasxport5 VIX_D.XPT
merge 1:1 seqn using DEMO_D, keep(match)
save MERGED
count

gen viq220_1 = .
replace viq220_1 = 0 if viq220 == 2
replace viq220_1 = 1 if viq220 == 1
// https://www.statalist.org/forums/forum/general-stata-discussion/general/1367450-rounding-down-every-output-to-integral-numbers
gen ridageyr_1 = floor(ridageyr/10)*10
// https://stackoverflow.com/questions/26857269/how-to-change-string-value-label
gen ridageyr_1_class = "Not Available"
foreach i of numlist 0/9 {
	local start = 10*`i'
	local end = 10*`i'+9
	replace ridageyr_1_class = "`start'-`end'" if ridageyr_1 == `i'*10
}
preserve
collapse (mean) viq220_1, by(ridageyr_1_class)
list
restore

gen mexican_american_dummy = (ridreth1 == 1)
gen other_hispanic_dummy = (ridreth1 == 2)
gen non_hispanic_white_dummy = (ridreth1 == 3)
gen non_hispanic_black_dummy = (ridreth1 == 4)

gen riagendr_1 = (riagendr==1)
label define sex 0 "female" 1 "male"
label values riagendr_1 sex

logit viq220_1 ridageyr, or
estimates store model1

logit viq220_1 ridageyr mexican_american_dummy other_hispanic_dummy non_hispanic_white_dummy non_hispanic_black_dummy riagendr_1, or
estimates store model2

logit viq220_1 ridageyr mexican_american_dummy other_hispanic_dummy non_hispanic_white_dummy non_hispanic_black_dummy riagendr_1 indfmpir, or
estimates store model3

// https://stats.oarc.ucla.edu/stata/faq/how-can-i-use-estout-to-make-regression-tables-that-look-like-those-in-journal-articles/
estout model1 model2 model3, cells(b(star fmt(3)) se(par fmt(2))) eform stats(N r2_p aic,fmt(0 3 3))

// https://www.ssc.wisc.edu/sscc/pubs/sfs/sfs-prtest.htm
prtest viq220_1, by(riagendr_1)