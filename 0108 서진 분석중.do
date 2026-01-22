* =========================
* 0. Setup & Load data
* =========================
cd "/Users/kimseojin/Desktop/3학기/미충족의료노동/"
clear all
set more off

use "Analysis_Ready_Final copy 2.dta", clear

describe workhour prob_300 weight year




* =========================
* 1. Analysis sample
* =========================
keep if is_paidworker == 1
drop if missing(workhour) | workhour==0
drop if year == 2018 | year == 2020

* =========================
* 2. Period indicator
* =========================
drop period
*drop period_lbl
gen period = (year >= 2019) if !missing(year)
*label define period_lbl 0 "Pre (2016-2017)" 1 "Post (2019)"
label values period period_lbl

tab year period

* =========================
* 3. Treatment intensity deciles
* =========================
drop decile
xtile decile = prob_300, nq(10)
label variable decile "Prob_300 decile (1=low,10=high)"
tab decile

* =========================
* 4. Visual check (means by decile x period)
* =========================
preserve
    collapse (mean) workhour_mean=workhour [pw=weight], by(decile period)

    list, sepby(period)

    twoway ///
      (connected workhour_mean decile if period==0) ///
      (connected workhour_mean decile if period==1), ///
      legend(order(1 "Pre (2016-2017)" 2 "Post (2019)")) ///
      xtitle("Treatment probability deciles (1=Low, 10=High)") ///
      ytitle("Weekly working hours") ///
      title("First stage: Work hours by exposure intensity") ///
      xlabel(1(1)10)
restore




* =========================
* 5. Survey design (KNHANES)
* =========================
svyset psu [pweight=weight], strata(kstrata) vce(linearized) singleunit(centered)
svydescribe




* =========================
* 6-1. Descriptive stats (overall)
* =========================
svy: mean workhour
svy: mean women has_chronic is_seoul is_privins is_recipient
svy: mean MO1_1 MH1_1


* =========================
* 6-2. Descriptive stats by decile
* =========================
svy: mean workhour, over(decile)
svy: mean women has_chronic incm5 educ is_seoul, over(decile)


* =========================
* 6-3. Descriptive stats by period
* =========================
svy: mean workhour, over(period)
svy: mean women has_chronic incm5 educ is_seoul, over(period)






* =========================
* 7-1. First stage (continuous exposure)
* =========================
svy: regress workhour c.prob_300##i.period

*estat svytest c.prob_300#1.period

* =========================
* 7-1. First stage (continuous exposure)
* =========================
*svy: regress workhour c.prob_300##i.period
*estat svytest c.prob_300
*estat svytest c.prob_300#1.period

* =========================
* 7-3. First stage with controls
* =========================
svy: regress workhour i.decile##i.period ///
    i.agegroup i.women i.incm5 i.educ i.has_chronic i.is_seoul i.is_privins

	
	
	
	
	
* =========================
* 8. Outcome: Unmet medical need
* =========================

* =========================
* Unmet medical need (binary)
* =========================
capture drop unmet
gen unmet = .
replace unmet = 1 if M_2_yr == 1
replace unmet = 0 if M_2_yr == 2
label define unmet_lbl 0 "No unmet need" 1 "Has unmet need"
label values unmet unmet_lbl



* =========================
* Unmet medical need reason
* =========================
capture drop unmet_reason
gen unmet_reason = M_2_rs
replace unmet_reason = . if inlist(M_2_rs, 88, 99)

* =========================
* Reason-specific outcomes
* =========================
capture drop unmet_time unmet_cost
gen unmet_time = (M_2_rs == 1) if M_2_yr == 1
gen unmet_cost = (M_2_rs == 3) if M_2_yr == 1




* =========================
* 9-1. Main regression (decile x period)
* =========================
svy: logit unmet i.decile##i.period ///
    i.agegroup i.women i.incm5 i.educ ///
    i.has_chronic i.is_seoul i.is_privins

	
** reason specific
svy: logit unmet_time i.decile##i.period ///
    i.agegroup i.women i.incm5 i.educ ///
    i.has_chronic i.is_seoul i.is_privins ///
    if unmet == 1

svy: logit unmet_cost i.decile##i.period ///
    i.agegroup i.women i.incm5 i.educ ///
    i.has_chronic i.is_seoul i.is_privins ///
    if unmet == 1

	
	
* =========================
* 10-1. Pre-trend / placebo test (Pre period only)
* =========================
preserve
    keep if period == 0

    svy: logit unmet i.decile ///
        i.agegroup i.women i.incm5 i.educ ///
        i.has_chronic i.is_seoul i.is_privins

restore


* =========================
* 10-2. Continuous exposure DID
* =========================
svy: logit unmet c.prob_300##i.period ///
    i.agegroup i.women i.incm5 i.educ ///
    i.has_chronic i.is_seoul i.is_privins

	
	
	
* =========================
* 10-4. Marginal effects
* =========================
svy: logit unmet i.decile##i.period ///
    i.agegroup i.women i.incm5 i.educ ///
    i.has_chronic i.is_seoul i.is_privins

margins decile#period
marginsplot, ///
    x(decile) by(period) ///
    title("Predicted probability of unmet medical need") ///
    ytitle("Predicted probability") ///
    xtitle("Labor market risk exposure decile") ///
    scheme(s1color)

	
* =========================
* 10-5. Mechanism check (adjust for workhour)
* =========================
svy: logit unmet i.decile##i.period ///
    c.workhour ///
    i.agegroup i.women i.incm5 i.educ ///
    i.has_chronic i.is_seoul i.is_privins

	
	* =========================
* 10-6. Heterogeneity: low-income workers
* =========================
svy: logit unmet i.decile##i.period ///
    i.agegroup i.women i.educ ///
    i.has_chronic i.is_seoul i.is_privins ///
    if incm5 <= 2

	
	
* =========================
* 10-7. Healthcare utilization outcomes
* =========================
svy: regress MO1_1 i.decile##i.period ///
    i.agegroup i.women i.incm5 i.educ ///
    i.has_chronic i.is_seoul i.is_privins

svy: regress MH1_1 i.decile##i.period ///
    i.agegroup i.women i.incm5 i.educ ///
    i.has_chronic i.is_seoul i.is_privins
