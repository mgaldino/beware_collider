** Analyses: "Protecting the Vote? Peacekeeping Presence And The Risk Of Electoral Violence"
** Authors: Fjelde and Smidt
** Last modified: 26.02.2021



** Preamble
clear
set more off
version 13.0
set max_memory .

** IMPORTANT: Install packages
*ssc install outreg2
*ssc install cem
*ssc install blindschemes
*net install collin, from(https://stats.idre.ucla.edu/stat/stata/ado/analysis/) replace

** IMPORTANT: Create two folders "Tables" and "Figures" in your working directory, where you saved the replication data
** IMPORTANT: Change working director path
capture cd "YOUR DIRECTORY"


** Log
log using "replication_main.log", replace


** Load dataset of analysis
use "replicationData.dta", clear

** Only keep countries and elections where are UN PKO is present
** Only keep years for which we have GEOPKO data
keep if PKO_presNoPol==1
keep if year>=1994 & year<=2017


** Drop period around election month that is longer than 13 months
drop if collapsed_time_si_el > 6 & collapsed_time_to_el > 6 

** Create a new election_id that is the same for consecutive elections
egen election_id_new = group(collapsed_election_id_spell)

** Make data panel: By admin units - election spells - months
egen adminUnitElection_id = concat(unique_id   election_id_new)
destring adminUnitElection_id, replace


** Create treatment variable for matching
capture gen un_military_base_du = un_military_base2
capture replace un_military_base_du = 1 if un_military_base_du>0


** Create variables for violence trends
** Note: Positive means a previous DECREASE in violence; Negative means an INCREASE in electoral violence
xtset adminUnitElection_id date
capture drop veco_viol_trend*
gen veco_viol_trend1 = (L1.veco_viol) - (L2.veco_viol+L3.veco_viol)/2
gen veco_viol_trend2 = (L1.veco_viol+L2.veco_viol)/2 - (L3.veco_viol)

** Create variables for robustness tests
capture drop ecav_event_pre_du  
gen ecav_event_pre_du = 0 if ecav_event_pre!=.
replace ecav_event_pre_du = 1 if ecav_event_pre>0 & ecav_event_pre_du !=.

capture drop ecav_event_post_du 
gen ecav_event_post_du = 0 if ecav_event_post!=.
replace ecav_event_post_du = 1 if ecav_event_post>0 & ecav_event_post_du !=.

capture drop ecav_anti_gov_du
gen ecav_anti_gov_du = ecav_anti_gov
replace ecav_anti_gov_du = 1 if ecav_anti_gov>0 & ecav_anti_gov!= .

capture drop ecav_pro_gov_du
gen ecav_pro_gov_du = ecav_pro_gov
replace ecav_pro_gov_du = 1 if ecav_pro_gov>0 & ecav_pro_gov!= .

** Add controls 
global controls "viol_stateBased_preDep viol_stateBased_preDep_trend viol_OneSided_preDep viol_OneSided_preDep_trend pop_gpw_sum un_military_base_du_spL"
global controls_cem "viol_stateBased_preDep viol_stateBased_preDep_trend viol_OneSided_preDep viol_OneSided_preDep_trend pop_gpw_sum roadDensity imr_mean bdist1 capdist un_military_base_du_spL" ///  un_military_base_du2Lag1_std


** Create sample of estimation with controls
set seed 1234
logit veco_viol_du un_military_base_du $controls_cem
capture drop sample
gen sample = 1 if e(sample)==1

** Get number of events of electoral violence in DECO
tab veco_viol_du if sample==1
tab veco_viol if sample==1
di 47 + (2*9) + (3*3) + (5*2) + 21 + 25
sum veco_viol_du if sample==1

** Get number of events of electoral violence and contention in ECAV
tab ecav_event_du if sample==1
tab ecav_event if sample==1
di 68 + (2*27) + (3*9) + (4*2) + (5*3) + (6*4) + (8*1) + (10*2) + (11*2) + (13*2) + 20 + 22 + 27 + 28


** Standardize control variables in the sample
foreach var of varlist $controls_cem {
	egen `var'_std = std(`var') if sample==1
}

global controls_cem_std "viol_stateBased_preDep_std viol_stateBased_preDep_trend_std viol_OneSided_preDep_std viol_OneSided_preDep_trend_std pop_gpw_sum_std roadDensity_std imr_mean_std bdist1_std capdist_std un_military_base_du_spL_std" ///un_military_base_du2Lag1_std 

** Add labels
do "replication_labels.do"


** Set to panel data
xtset adminUnitElection_id




************************************************
** Check which elections are in the sample *****
************************************************
tab date collapsed_election_id_spell if collapsed_election_id_spell!=. & country=="Burundi" // 1 election
tab date collapsed_election_id_spell if collapsed_election_id_spell!=. & country=="Central African Republic" // 4 elections
tab date collapsed_election_id_spell if collapsed_election_id_spell!=. & country=="Chad" // 1 elections but only five observation as PKO left before end of electoral period
tab date collapsed_election_id_spell if collapsed_election_id_spell!=. & country=="Côte d'Ivoire" // 4 elections
tab date collapsed_election_id_spell if collapsed_election_id_spell!=. & country=="Democratic Republic of the Congo" // 3 elections with longer pre-election period for 2006 (two-round election)
tab date collapsed_election_id_spell if collapsed_election_id_spell!=. & country=="Liberia" // 5 elections with shorter election period in 1997
tab date collapsed_election_id_spell if collapsed_election_id_spell!=. & country=="Mali" // 2 elections
tab date collapsed_election_id_spell if collapsed_election_id_spell!=. & country=="Mozambique" // 1 election with shorter election period due to leaving of PKO
tab date collapsed_election_id_spell if collapsed_election_id_spell!=. & country=="Sierra Leone" // 1 election
tab date collapsed_election_id_spell if collapsed_election_id_spell!=. & country=="Sudan" // 2 elections 





****************************************
********** OLS Fixed effects ***********
****************************************
xtreg veco_viol_du un_military_base2, fe vce(cl adminUnitElection_id)
local Rsq: display %9.3f e(r2)
local N: display %9.0f e(N)
local N_g: display %9.0f e(N_g)
capture drop estimate
capture drop low
capture drop high
tempname b
matrix `b' = e(b)
gen estimate = `b'[1,1] in 1
tempname V
matrix `V' = e(V)
gen low =  `b'[1,1] - 1.96*sqrt(`V'[1,1]) in 1
gen high = `b'[1,1] + 1.96*sqrt(`V'[1,1]) in 1
outreg2 using ".\Tables\table1.tex", replace ///
keep(un_military_base2) ///
ctitle("Model 1") label dec(3) alpha(0.001, 0.01, 0.05, 0.1) symbol(***,**, *, +) ///
noni nor2 noobs addtext(R-squared, `Rsq', N, `N', FE for admin. units, "YES", FE for months, "NO" ) 


xtreg veco_viol_du un_military_base2 $controls, fe vce(cl adminUnitElection_id)
local Rsq: display %9.3f e(r2)
local N: display %9.0f e(N)
local N_g: display %9.0f e(N_g)

tempname b
matrix `b' = e(b)
replace estimate = `b'[1,1] in 2
tempname V
matrix `V' = e(V)
replace low =  `b'[1,1] - 1.96*sqrt(`V'[1,1]) in 2
replace high = `b'[1,1] + 1.96*sqrt(`V'[1,1]) in 2
outreg2 using ".\Tables\table1.tex", append ///
keep(un_military_base2 $controls) ///
ctitle("Model 2") label dec(3) alpha(0.001, 0.01, 0.05, 0.1) symbol(***,**, *, +)  ///
noni nor2 noobs addtext(R-squared, `Rsq', N, `N' , FE for admin. units, "YES", FE for months, "NO") 



xtreg veco_viol_du un_military_base2 $controls i.date, fe vce(cl adminUnitElection_id)
local Rsq: display %9.3f e(r2)
local N: display %9.0f e(N)
local N_g: display %9.0f e(N_g)
tempname b
matrix `b' = e(b)
replace estimate = `b'[1,1] in 3
tempname V
matrix `V' = e(V)
replace low =  `b'[1,1] - 1.96*sqrt(`V'[1,1]) in 3
replace high = `b'[1,1] + 1.96*sqrt(`V'[1,1]) in 3
outreg2 using ".\Tables\table1.tex", append ///
keep(un_military_base2  $controls) ///
ctitle("Model 3") label dec(3) alpha(0.001, 0.01, 0.051, 0.1) symbol(***,**, *, +)  ///
noni nor2 noobs addtext(R-squared, `Rsq', N, `N' , FE for admin. units, "YES", FE for months, "YES") 


*************************
*** Time trends *********
*************************

xtreg veco_viol_du un_military_base2 $controls veco_viol_trend1 if veco_viol_trend1!=., fe vce(cl adminUnitElection_id)
local Rsq: display %9.3f e(r2)
local N: display %9.0f e(N)
local N_g: display %9.0f e(N_g)
tempname b
matrix `b' = e(b)
replace estimate = `b'[1,1] in 4
tempname V
matrix `V' = e(V)
replace low =  `b'[1,1] - 1.96*sqrt(`V'[1,1]) in 4
replace high = `b'[1,1] + 1.96*sqrt(`V'[1,1]) in 4
outreg2 using ".\Tables\table1.tex", append ///
keep(un_military_base2  $controls veco_viol_trend1) ///
ctitle("Model 4") label dec(3) alpha(0.001, 0.01, 0.05, 0.1) symbol(***,**, *, +)  ///
noni nor2 noobs addtext(R-squared, `Rsq', N, `N' , FE for admin. units, "YES", FE for months, "NO") 


xtreg veco_viol_du un_military_base2 $controls veco_viol_trend2 if veco_viol_trend2!=., fe vce(cl adminUnitElection_id)
local Rsq: display %9.3f e(r2)
local N: display %9.0f e(N)
local N_g: display %9.0f e(N_g)
tempname b
matrix `b' = e(b)
replace estimate = `b'[1,1] in 5
tempname V
matrix `V' = e(V)
replace low =  `b'[1,1] - 1.96*sqrt(`V'[1,1]) in 5
replace high = `b'[1,1] + 1.96*sqrt(`V'[1,1]) in 5
outreg2 using ".\Tables\table1.tex", append ///
keep(un_military_base2 $controls veco_viol_trend2) ///
ctitle("Model 5") label dec(3) alpha(0.001, 0.01, 0.05, 0.1) symbol(***,**, *, +)  ///
noni nor2 noobs addtext(R-squared, `Rsq', N, `N' , FE for admin. units, "YES", FE for months, "NO") 

** Estimated effects
list low estimate high in 1/5



***************************************
******* Matching **********************
***************************************

** Make CEM weights
capture drop imb_temp*
capture drop cem_temp*
imb $controls_cem_std if sample==1, tr(un_military_base_du)
matrix imb = r(imbal)
matrix list imb
matrix imb2 = imb[1..10,2]
svmat double imb2, names(imb_temp)

cem $controls_cem_std if sample==1, tr(un_military_base_du) autocuts(fd)
matrix imb = r(imbal)
matrix imb2 = imb[1..10,2]
svmat double imb2, names(cem_temp)

tab veco_viol_du if cem_weights!=0 


** Make propensity scores 
drop if sample!=1
capture drop match*
capture drop ps_temp
teffects psmatch (veco_viol_du)(un_military_base_du $controls_cem_std ) , vce(robust) atet nn(1) gen(match)
tebalance summarize
matrix imb = r(table)
matrix list imb
matrix imb2 = imb[1..10,2]
svmat double imb2, names(ps_temp)

capture drop ob
gen ob = _n //store the observation numbers for future use
save "temp_fulldata", replace // save the complete data set

preserve 
keep if un_military_base_du==1 // keep just the treated group
keep match1 // keep just the match1 variable (the observation numbers of their matches)
bysort match1: gen _weight = _N // count how many times each control observation is a match
by match1: keep if _n==1 // keep just one row per control observation
rename match1 ob //rename for merging purposes

merge 1:m ob using "temp_fulldata" // merge back into the full data
replace _weight = 1 if un_military_base_du==1 // set weight to 1 for treated observations
drop _merge

tab un_military_base_du2Lag1 veco_viol_du if _weight==1 & sam==1
tab un_military_base_du2Lag1 veco_viol_du if _weight>1 & sam==1

tab un_military_base_du2Lag1 un_military_base_du if _weight==1 & sam==1
tab un_military_base_du2Lag1 un_military_base_du if sam==1

tab un_military_base_du2 un_military_base_du2Lag1 if sam==1
tab un_military_base_du2 un_military_base_du2Lag1 if _weight!=. & sam==1

** Post-propensity score matching
logit veco_viol_du un_military_base_du $controls_cem_std adminArea un_military_base_du2Lag1 [fweight=_weight], vce(robust)
vif, uncentered
collin un_military_base_du $controls_cem_std adminArea un_military_base_du2Lag1 
gen sam = e(sample)
estat ic
mat es_ic = r(S)
local AIC: display %4.1f es_ic[1,5]
local BIC: display %4.1f es_ic[1,6]
local LL: display %4.1f es_ic[1,3]

outreg2 using ".\Tables\table2.tex", replace ///
keep(un_military_base_du $controls_cem_std adminArea un_military_base_du2Lag1 ) ///
ctitle("Model 6") label dec(3) alpha(0.001, 0.01, 0.05, 0.1) symbol(***,**, *, +) ///
noni nor2 addtext(Log-Likelihood, `LL' ,BIC, `BIC', AIC, `AIC') 


logit veco_viol_du un_military_base2 $controls_cem_std adminArea un_military_base_du2Lag1  [fweight=_weight], vce(robust)
estat ic
mat es_ic = r(S)
local AIC: display %4.1f es_ic[1,5]
local BIC: display %4.1f es_ic[1,6]
local LL: display %4.1f es_ic[1,3]

outreg2 using ".\Tables\table2.tex", append ///
keep(un_military_base2 $controls_cem_std adminArea un_military_base_du2Lag1 ) ///
ctitle("Model 7") label dec(3) alpha(0.001, 0.01, 0.05, 0.1) symbol(***,**, *, +) ///
noni nor2 addtext(Log-Likelihood, `LL' ,BIC, `BIC', AIC, `AIC') 

restore

* Final balance stats plot
set scheme plotplainblind
capture drop y1 y2 y3
gen y1 = 11-_n in 1/10
gen y2 = y1-0.2
gen y3 = y2-0.2
twoway scatter y1 imb_temp1, msize(1.5) || scatter y2 ps_temp, msize(1.5) || scatter y3 cem_temp1, msize(1.5) ///
ylabel(1 "UN PKO bases (spatial lag)" 2 "Distance to capital" 3 "Distance to border" 4 "Infant mortality"  5 "Road density " 6 "Population" 7 "One-sided violence"  8 "One-sided viol. trend" 9 "State-based viol. trend" 10 "State-based violence", angle(0) labsize(4)) ///
xline(0) legend(size(4) title("Difference in means:", size(4)) label(1 "Before matching") label(2 "After PS matching") label(3 "After CEM") ) xscale(range(-1 2)) xlabel(-1(1)2, labsize(4))
graph export "./Figures/BalanceStats.png", replace


** Post CEM-Matching
logit veco_viol_du un_military_base_du $controls_cem_std adminArea un_military_base_du2Lag1  [iweight=cem_weights], vce(cl adminUnitElection_id)
estat ic
mat es_ic = r(S)
local AIC: display %4.1f es_ic[1,5]
local BIC: display %4.1f es_ic[1,6]
local LL: display %4.1f es_ic[1,3]

outreg2 using ".\Tables\table2.tex", append ///
keep(un_military_base_du $controls_cem_std adminArea un_military_base_du2Lag1) ///
ctitle("Model 8") label dec(3) alpha(0.001, 0.01, 0.05, 0.1) symbol(***,**, *, +) ///
noni nor2 addtext( Log-Likelihood, `LL' ,BIC, `BIC', AIC, `AIC') 


logit veco_viol_du un_military_base2 $controls_cem_std adminArea un_military_base_du2Lag1 [iweight=cem_weights], vce(cl adminUnitElection_id)
estat ic
mat es_ic = r(S)
local AIC: display %4.1f es_ic[1,5]
local BIC: display %4.1f es_ic[1,6]
local LL: display %4.1f es_ic[1,3]

outreg2 using ".\Tables\table2.tex", append ///
keep(un_military_base2 $controls_cem_std adminArea un_military_base_du2Lag1) ///
ctitle("Model 9") label dec(3) alpha(0.001, 0.01, 0.05, 0.1) symbol(***,**, *, +) ///
noni nor2 addtext(Log-Likelihood, `LL' ,BIC, `BIC', AIC, `AIC') 



***********************************************
**** Additional tests: Hypotheses 2 and 3 *****
***********************************************


********************************************************
*** Hypothesis 2: TIMING OF ELECTION VIOLENCE **********
********************************************************

** Pre-election violence

xtreg veco_viol_pre_du un_military_base2, fe vce(cl adminUnitElection_id)
capture drop sample_pre
gen sample_pre = e(sample)
local Rsq: display %9.3f e(r2)
local N: display %9.0f e(N)
local N_g: display %9.0f e(N_g)
capture drop estimate
capture drop low
capture drop high
tempname b
matrix `b' = e(b)
gen estimate = `b'[1,1] in 1
tempname V
matrix `V' = e(V)
gen low =  `b'[1,1] - 1.54*sqrt(`V'[1,1]) in 1
gen high = `b'[1,1] + 1.54*sqrt(`V'[1,1]) in 1
outreg2 using ".\Tables\app_table_G_pre.tex", replace ///
keep(un_military_base2) ///
ctitle("Model G1") label dec(3) alpha(0.001, 0.01, 0.05, 0.1) symbol(***,**, *, +) ///
noni nor2 noobs addtext(R-squared, `Rsq', N, `N', FE for admin. units, "YES", FE for months, "NO" ) 

xtreg veco_viol_pre_du un_military_base2 $controls, fe vce(cl adminUnitElection_id)
tempname b
matrix `b' = e(b)
replace estimate = `b'[1,1] in 2
tempname V
matrix `V' = e(V)
replace low =  `b'[1,1] - 1.54*sqrt(`V'[1,1]) in 2
replace high = `b'[1,1] + 1.54*sqrt(`V'[1,1]) in 2
outreg2 using ".\Tables\app_table_G_pre.tex", append ///
keep(un_military_base2 $controls) ///
ctitle("Model G2") label dec(3) alpha(0.001, 0.01, 0.05, 0.1) symbol(***,**, *, +) ///
noni nor2 noobs addtext(R-squared, `Rsq', N, `N', FE for admin. units, "YES", FE for months, "NO" ) 

xtreg veco_viol_pre_du un_military_base2 $controls i.date, fe vce(cl adminUnitElection_id)
local Rsq: display %9.3f e(r2)
local N: display %9.0f e(N)
local N_g: display %9.0f e(N_g)
tempname b
matrix `b' = e(b)
replace estimate = `b'[1,1] in 3
tempname V
matrix `V' = e(V)
replace low =  `b'[1,1] - 1.54*sqrt(`V'[1,1]) in 3
replace high = `b'[1,1] + 1.54*sqrt(`V'[1,1]) in 3
outreg2 using ".\Tables\app_table_G_pre.tex", append ///
keep(un_military_base2 $controls) ///
ctitle("Model G3") label dec(3) alpha(0.001, 0.01, 0.05, 0.1) symbol(***,**, *, +) ///
noni nor2 noobs addtext(R-squared, `Rsq', N, `N', FE for admin. units, "YES", FE for months, "YES" ) 

xtreg veco_viol_pre_du un_military_base2 $controls veco_viol_trend1, fe vce(cl adminUnitElection_id)
local Rsq: display %9.3f e(r2)
local N: display %9.0f e(N)
local N_g: display %9.0f e(N_g)
tempname b
matrix `b' = e(b)
replace estimate = `b'[1,1] in 4
tempname V
matrix `V' = e(V)
replace low =  `b'[1,1] - 1.54*sqrt(`V'[1,1]) in 4
replace high = `b'[1,1] + 1.54*sqrt(`V'[1,1]) in 4
outreg2 using ".\Tables\app_table_G_pre.tex", append ///
keep(un_military_base2 $controls) ///
ctitle("Model G4") label dec(3) alpha(0.001, 0.01, 0.05, 0.1) symbol(***,**, *, +) ///
noni nor2 noobs addtext(R-squared, `Rsq', N, `N', FE for admin. units, "YES", FE for months, "NO" ) 

xtreg veco_viol_pre_du un_military_base2 $controls veco_viol_trend2, fe vce(cl adminUnitElection_id)
local Rsq: display %9.3f e(r2)
local N: display %9.0f e(N)
local N_g: display %9.0f e(N_g)
tempname b
matrix `b' = e(b)
replace estimate = `b'[1,1] in 5
tempname V
matrix `V' = e(V)
replace low =  `b'[1,1] - 1.54*sqrt(`V'[1,1]) in 5
replace high = `b'[1,1] + 1.54*sqrt(`V'[1,1]) in 5
outreg2 using ".\Tables\app_table_G_pre.tex", append ///
keep(un_military_base2 $controls) ///
ctitle("Model G5") label dec(3) alpha(0.001, 0.01, 0.05, 0.1) symbol(***,**, *, +) ///
noni nor2 noobs addtext(R-squared, `Rsq', N, `N', FE for admin. units, "YES", FE for months, "NO" ) 

** Post-election violence
xtreg veco_viol_post_du un_military_base2, fe vce(cl adminUnitElection_id)
capture drop sample_post
gen sample_post = e(sample)
local Rsq: display %9.3f e(r2)
local N: display %9.0f e(N)
local N_g: display %9.0f e(N_g)
tempname b
matrix `b' = e(b)
replace estimate = `b'[1,1] in 6
tempname V
matrix `V' = e(V)
replace low =  `b'[1,1] - 1.54*sqrt(`V'[1,1]) in 6
replace high = `b'[1,1] + 1.54*sqrt(`V'[1,1]) in 6
outreg2 using ".\Tables\app_table_G_post.tex", replace ///
keep(un_military_base2) ///
ctitle("Model G6") label dec(3) alpha(0.001, 0.01, 0.05, 0.1) symbol(***,**, *, +) ///
noni nor2 noobs addtext(R-squared, `Rsq', N, `N', FE for admin. units, "YES", FE for months, "NO" ) 

xtreg veco_viol_post_du un_military_base2 $controls, fe vce(cl adminUnitElection_id)
local Rsq: display %9.3f e(r2)
local N: display %9.0f e(N)
local N_g: display %9.0f e(N_g)
tempname b
matrix `b' = e(b)
replace estimate = `b'[1,1] in 7
tempname V
matrix `V' = e(V)
replace low =  `b'[1,1] - 1.54*sqrt(`V'[1,1]) in 7
replace high = `b'[1,1] + 1.54*sqrt(`V'[1,1]) in 7
outreg2 using ".\Tables\app_table_G_post.tex", append ///
keep(un_military_base2 $controls) ///
ctitle("Model G7") label dec(3) alpha(0.001, 0.01, 0.05, 0.1) symbol(***,**, *, +) ///
noni nor2 noobs addtext(R-squared, `Rsq', N, `N', FE for admin. units, "YES", FE for months, "NO" ) 

xtreg veco_viol_post_du un_military_base2 $controls i.date, fe vce(cl adminUnitElection_id)
local Rsq: display %9.3f e(r2)
local N: display %9.0f e(N)
local N_g: display %9.0f e(N_g)
tempname b
matrix `b' = e(b)
replace estimate = `b'[1,1] in 8
tempname V
matrix `V' = e(V)
replace low =  `b'[1,1] - 1.54*sqrt(`V'[1,1]) in 8
replace high = `b'[1,1] + 1.54*sqrt(`V'[1,1]) in 8
outreg2 using ".\Tables\app_table_G_post.tex", append ///
keep(un_military_base2 $controls) ///
ctitle("Model G8") label dec(3) alpha(0.001, 0.01, 0.05, 0.1) symbol(***,**, *, +) ///
noni nor2 noobs addtext(R-squared, `Rsq', N, `N', FE for admin. units, "YES", FE for months, "YES" ) 

xtreg veco_viol_post_du un_military_base2 $controls veco_viol_trend1, fe vce(cl adminUnitElection_id)
local Rsq: display %9.3f e(r2)
local N: display %9.0f e(N)
local N_g: display %9.0f e(N_g)
tempname b
matrix `b' = e(b)
replace estimate = `b'[1,1] in 9
tempname V
matrix `V' = e(V)
replace low =  `b'[1,1] - 1.54*sqrt(`V'[1,1]) in 9
replace high = `b'[1,1] + 1.54*sqrt(`V'[1,1]) in 9
outreg2 using ".\Tables\app_table_G_post.tex", append ///
keep(un_military_base2 $controls) ///
ctitle("Model G9") label dec(3) alpha(0.001, 0.01, 0.05, 0.1) symbol(***,**, *, +) ///
noni nor2 noobs addtext(R-squared, `Rsq', N, `N', FE for admin. units, "YES", FE for months, "NO" ) 

xtreg veco_viol_post_du un_military_base2 $controls veco_viol_trend2, fe vce(cl adminUnitElection_id)
local Rsq: display %9.3f e(r2)
local N: display %9.0f e(N)
local N_g: display %9.0f e(N_g)
tempname b
matrix `b' = e(b)
replace estimate = `b'[1,1] in 10
tempname V
matrix `V' = e(V)
replace low =  `b'[1,1] - 1.54*sqrt(`V'[1,1]) in 10
replace high = `b'[1,1] + 1.54*sqrt(`V'[1,1]) in 10
outreg2 using ".\Tables\app_table_G_post.tex", append ///
keep(un_military_base2 $controls) ///
ctitle("Model G10") label dec(3) alpha(0.001, 0.01, 0.05, 0.1) symbol(***,**, *, +) ///
noni nor2 noobs addtext(R-squared, `Rsq', N, `N', FE for admin. units, "YES", FE for months, "NO" ) 


** Make graph to illustrate the results for the pre- vs. post-election violence analyses

set scheme plotplainblind
capture drop y1
capture drop y2
gen y1 = _n in 1/5
gen y2 = _n+1 in 6/10
twoway rcap low high y1 if y1<=11, horizontal lc(gray) lwidth(0.2) || scatter y1 estimate if y1<=11, msymbol("Oh") msize(2) || ///
	   rcap low high y2 if y2<=11, horizontal lc(gray) lwidth(0.2) || scatter y2 estimate if y2<=11, msymbol("Dh") msize(2)  ///
ylabel( 1 "Time trends 2" 2 "Time trends 1" 3 "Monthly FE" 4 "Controls" 5 "No controls"    7 "Time trends 2" 8 "Time trends 1" 9 "Monthly FE" 10 "Controls" 11 "No controls" , angle(0)  labsize(4)) ///
xline(0) legend(size(4) title("Effects of UN military on:", size(4) ) order(4 2 1) label(4 "Pre-election")  label(2 "Post-election") label(1 "90% Confidence Interval") )  yscale(range(1 11)) xscale(range(-0.01 0.005)) xlabel(-0.01(0.005)0.005, labsize(4))
graph export "./Figures/Effects_prepost.png", replace



********************************************************
*** Hypothesis 2: ACTORS IN ELECTION VIOLENCE **********
********************************************************

capture drop veco_anti_gov_viol2_du
gen veco_anti_gov_viol2_du = veco_anti_gov_viol2
replace veco_anti_gov_viol2_du = 1 if veco_anti_gov_viol2>0 & veco_anti_gov_viol2!=.

capture drop veco_pro_gov_viol2_du
gen veco_pro_gov_viol2_du = veco_pro_gov_viol2
replace veco_pro_gov_viol2_du = 1 if veco_pro_gov_viol2>0 & veco_pro_gov_viol2!=.


** Anti government violence

xtreg veco_anti_gov_viol2_du  un_military_base2 , fe vce(cl adminUnitElection_id)
local Rsq: display %9.3f e(r2)
local N: display %9.0f e(N)
local N_g: display %9.0f e(N_g)
tempname b
matrix `b' = e(b)
replace estimate = `b'[1,1] in 11
tempname V
matrix `V' = e(V)
replace low =  `b'[1,1] - 1.54*sqrt(`V'[1,1]) in 11
replace high = `b'[1,1] + 1.54*sqrt(`V'[1,1]) in 11
outreg2 using ".\Tables\app_table_H_anti.tex", replace ///
keep(un_military_base2) ///
ctitle("Model H1") label dec(3) alpha(0.001, 0.01, 0.05, 0.1) symbol(***,**, *, +) ///
noni nor2 noobs addtext(R-squared, `Rsq', N, `N', FE for admin. units, "YES", FE for months, "NO" ) 

xtreg veco_anti_gov_viol2_du  un_military_base2 $controls , fe vce(cl adminUnitElection_id)
local Rsq: display %9.3f e(r2)
local N: display %9.0f e(N)
local N_g: display %9.0f e(N_g)
tempname b
matrix `b' = e(b)
replace estimate = `b'[1,1] in 12
tempname V
matrix `V' = e(V)
replace low =  `b'[1,1] - 1.54*sqrt(`V'[1,1]) in 12
replace high = `b'[1,1] + 1.54*sqrt(`V'[1,1]) in 12
outreg2 using ".\Tables\app_table_H_anti.tex",append ///
keep(un_military_base2 $controls) ///
ctitle("Model H2") label dec(3) alpha(0.001, 0.01, 0.05, 0.1) symbol(***,**, *, +) ///
noni nor2 noobs addtext(R-squared, `Rsq', N, `N', FE for admin. units, "YES", FE for months, "NO" ) 

xtreg veco_anti_gov_viol2_du  un_military_base2 $controls i.date, fe vce(cl adminUnitElection_id)
local Rsq: display %9.3f e(r2)
local N: display %9.0f e(N)
local N_g: display %9.0f e(N_g)
tempname b
matrix `b' = e(b)
replace estimate = `b'[1,1] in 13
tempname V
matrix `V' = e(V)
replace low =  `b'[1,1] - 1.54*sqrt(`V'[1,1]) in 13
replace high = `b'[1,1] + 1.54*sqrt(`V'[1,1]) in 13
outreg2 using ".\Tables\app_table_H_anti.tex",append ///
keep(un_military_base2 $controls) ///
ctitle("Model H3") label dec(3) alpha(0.001, 0.01, 0.05, 0.1) symbol(***,**, *, +) ///
noni nor2 noobs addtext(R-squared, `Rsq', N, `N', FE for admin. units, "YES", FE for months, "YES" ) 

xtreg veco_anti_gov_viol2_du  un_military_base2 $controls veco_viol_trend1, fe vce(cl adminUnitElection_id)
tempname b
matrix `b' = e(b)
replace estimate = `b'[1,1] in 14
tempname V
matrix `V' = e(V)
replace low =  `b'[1,1] - 1.54*sqrt(`V'[1,1]) in 14
replace high = `b'[1,1] + 1.54*sqrt(`V'[1,1]) in 14
outreg2 using ".\Tables\app_table_H_anti.tex",append ///
keep(un_military_base2 $controls) ///
ctitle("Model H4") label dec(3) alpha(0.001, 0.01, 0.05, 0.1) symbol(***,**, *, +) ///
noni nor2 noobs addtext(R-squared, `Rsq', N, `N', FE for admin. units, "YES", FE for months, "NO" ) 

xtreg veco_anti_gov_viol2_du  un_military_base2 $controls veco_viol_trend2, fe vce(cl adminUnitElection_id)
local Rsq: display %9.3f e(r2)
local N: display %9.0f e(N)
local N_g: display %9.0f e(N_g)
tempname b
matrix `b' = e(b)
replace estimate = `b'[1,1] in 15
tempname V
matrix `V' = e(V)
replace low =  `b'[1,1] - 1.54*sqrt(`V'[1,1]) in 15
replace high = `b'[1,1] + 1.54*sqrt(`V'[1,1]) in 15
outreg2 using ".\Tables\app_table_H_anti.tex", append ///
keep(un_military_base2 $controls) ///
ctitle("Model H5") label dec(3) alpha(0.001, 0.01, 0.05, 0.1) symbol(***,**, *, +) ///
noni nor2 noobs addtext(R-squared, `Rsq', N, `N', FE for admin. units, "YES", FE for months, "NO" ) 



** Pro government violence

xtreg veco_pro_gov_viol2_du  un_military_base2 , fe vce(cl adminUnitElection_id)
local Rsq: display %9.3f e(r2)
local N: display %9.0f e(N)
local N_g: display %9.0f e(N_g)
tempname b
matrix `b' = e(b)
replace estimate = `b'[1,1] in 16
tempname V
matrix `V' = e(V)
replace low =  `b'[1,1] - 1.54*sqrt(`V'[1,1]) in 16
replace high = `b'[1,1] + 1.54*sqrt(`V'[1,1]) in 16
outreg2 using ".\Tables\app_table_H_pro.tex", replace ///
keep(un_military_base2) ///
ctitle("Model H6") label dec(3) alpha(0.001, 0.01, 0.05, 0.1) symbol(***,**, *, +) ///
noni nor2 noobs addtext(R-squared, `Rsq', N, `N', FE for admin. units, "YES", FE for months, "NO" ) 


xtreg veco_pro_gov_viol2_du  un_military_base2 $controls, fe vce(cl adminUnitElection_id)
local Rsq: display %9.3f e(r2)
local N: display %9.0f e(N)
local N_g: display %9.0f e(N_g)
tempname b
matrix `b' = e(b)
replace estimate = `b'[1,1] in 17
tempname V
matrix `V' = e(V)
replace low =  `b'[1,1] - 1.54*sqrt(`V'[1,1]) in 17
replace high = `b'[1,1] + 1.54*sqrt(`V'[1,1]) in 17
outreg2 using ".\Tables\app_table_H_pro.tex", append  ///
keep(un_military_base2 $controls) ///
ctitle("Model H7") label dec(3) alpha(0.001, 0.01, 0.05, 0.1) symbol(***,**, *, +) ///
noni nor2 noobs addtext(R-squared, `Rsq', N, `N', FE for admin. units, "YES", FE for months, "NO" ) 


xtreg veco_pro_gov_viol2_du  un_military_base2 $controls i.date, fe vce(cl adminUnitElection_id)
local Rsq: display %9.3f e(r2)
local N: display %9.0f e(N)
local N_g: display %9.0f e(N_g)
tempname b
matrix `b' = e(b)
replace estimate = `b'[1,1] in 18
tempname V
matrix `V' = e(V)
replace low =  `b'[1,1] - 1.54*sqrt(`V'[1,1]) in 18
replace high = `b'[1,1] + 1.54*sqrt(`V'[1,1]) in 18
outreg2 using ".\Tables\app_table_H_pro.tex", append  ///
keep(un_military_base2 $controls) ///
ctitle("Model H8") label dec(3) alpha(0.001, 0.01, 0.05, 0.1) symbol(***,**, *, +) ///
noni nor2 noobs addtext(R-squared, `Rsq', N, `N', FE for admin. units, "YES", FE for months, "YES" ) 


xtreg veco_pro_gov_viol2_du  un_military_base2 $controls veco_viol_trend1, fe vce(cl adminUnitElection_id)
local Rsq: display %9.3f e(r2)
local N: display %9.0f e(N)
local N_g: display %9.0f e(N_g)
tempname b
matrix `b' = e(b)
replace estimate = `b'[1,1] in 19
tempname V
matrix `V' = e(V)
replace low =  `b'[1,1] - 1.54*sqrt(`V'[1,1]) in 19
replace high = `b'[1,1] + 1.54*sqrt(`V'[1,1]) in 19
outreg2 using ".\Tables\app_table_H_pro.tex", append  ///
keep(un_military_base2 $controls) ///
ctitle("Model H9") label dec(3) alpha(0.001, 0.01, 0.05, 0.1) symbol(***,**, *, +) ///
noni nor2 noobs addtext(R-squared, `Rsq', N, `N', FE for admin. units, "YES", FE for months, "NO" ) 


xtreg veco_pro_gov_viol2_du  un_military_base2 $controls veco_viol_trend2, fe vce(cl adminUnitElection_id)
local Rsq: display %9.3f e(r2)
local N: display %9.0f e(N)
local N_g: display %9.0f e(N_g)
tempname b
matrix `b' = e(b)
replace estimate = `b'[1,1] in 20
tempname V
matrix `V' = e(V)
replace low =  `b'[1,1] - 1.54*sqrt(`V'[1,1]) in 20
replace high = `b'[1,1] + 1.54*sqrt(`V'[1,1]) in 20
outreg2 using ".\Tables\app_table_H_pro.tex", append  ///
keep(un_military_base2 $controls) ///
ctitle("Model H10") label dec(3) alpha(0.001, 0.01, 0.05, 0.1) symbol(***,**, *, +) ///
noni nor2 noobs addtext(R-squared, `Rsq', N, `N', FE for admin. units, "YES", FE for months, "NO" ) 


** Make graph to illustrate the results for the pre- vs. post-election violence analyses

set scheme plotplainblind
capture drop y1
capture drop y2
gen y1 = _n in 11/15
gen y2 = _n+1 in 16/20
twoway rcap low high y1, horizontal lc(gray) lwidth(0.2) || scatter y1 estimate, msymbol(Oh) msize(2)  mcolor(black) || ///
	   rcap low high y2, horizontal lc(gray) lwidth(0.2) || scatter y2 estimate, msymbol(Dh) msize(2)  mcolor(eltblue) ///
ylabel( 11 "Time trends 2" 12 "Time trends 1" 13 "Monthly FE" 14 "Controls" 15 "No controls"    17 "Time trends 2" 18 "Time trends 1" 19 "Monthly FE" 20 "Controls" 21 "No controls" , angle(0)  labsize(4)) ///
xline(0) legend(size(4) title("Effects of UN military on:", size(4) ) order(4 2 1) label(2 "Anti-gov. electoral viol.")  label(4 "Pro-gov. electoral viol.") label(1 "90% Confidence Interval") )  yscale(range(11 21)) xscale(range(-0.01 0.005)) xlabel(-0.01(0.005)0.005, labsize(4))
graph export "./Figures/Effects_oppgov.png", replace

log close
exit




***********************************************
**** Appendix *********************************
***********************************************

** Appendix A: Summary statistics

estpost summarize veco_viol_du veco_viol_pre_du veco_viol_post_du veco_anti_gov_viol2_du veco_pro_gov_viol2_du un_military_base2 un_military_base_du $controls_cem $controls_cem_std veco_viol_trend1 veco_viol_trend2 adminArea if sample==1, detail
esttab . using ".\Tables\app_table_A_summaryStats.tex", cells("mean(fmt(3)) sd min max count") nonumber label replace //title("Table A1. Summary Statistics") 


** Appendix B: Inclusion of variable for exclusion of politically relevant ethnic groups
xtreg veco_viol_du un_military_base2 excluded_mean, fe vce(cl adminUnitElection_id)
local Rsq: display %9.3f e(r2)
local N: display %9.0f e(N)
local N_g: display %9.0f e(N_g)
outreg2 using ".\Tables\app_table_B_FEwithExcluded.tex", replace ///
keep(un_military_base2 excluded_mean) ///
ctitle("Model B6") label dec(3) alpha(0.001, 0.01, 0.05, 0.1) symbol(***,**, *, +) ///
noni nor2 noobs addtext(R-squared, `Rsq', N, `N', FE for admin. units, "YES", FE for months, "NO" ) 

xtreg veco_viol_du un_military_base2 $controls excluded_mean, fe vce(cl adminUnitElection_id)
local Rsq: display %9.3f e(r2)
local N: display %9.0f e(N)
local N_g: display %9.0f e(N_g)
outreg2 using ".\Tables\app_table_B_FEwithExcluded.tex", append ///
keep(un_military_base2 $controls excluded_mean ) ///
ctitle("Model B7") label dec(3) alpha(0.001, 0.01, 0.05, 0.1) symbol(***,**, *, +)  ///
noni nor2 noobs addtext(R-squared, `Rsq', N, `N' , FE for admin. units, "YES", FE for months, "NO") 

xtreg veco_viol_du un_military_base2 $controls excluded_mean i.date, fe vce(cl adminUnitElection_id)
local Rsq: display %9.3f e(r2)
local N: display %9.0f e(N)
local N_g: display %9.0f e(N_g)
outreg2 using ".\Tables\app_table_B_FEwithExcluded.tex", append ///
keep(un_military_base2  $controls excluded_mean) ///
ctitle("Model B8") label dec(3) alpha(0.001, 0.01, 0.051, 0.1) symbol(***,**, *, +)  ///
noni nor2 noobs addtext(R-squared, `Rsq', N, `N' , FE for admin. units, "YES", FE for months, "YES") 

xtreg veco_viol_du un_military_base2 $controls excluded_mean veco_viol_trend1 if veco_viol_trend1!=., fe vce(cl adminUnitElection_id)
local Rsq: display %9.3f e(r2)
local N: display %9.0f e(N)
local N_g: display %9.0f e(N_g)
outreg2 using ".\Tables\app_table_B_FEwithExcluded.tex", append ///
keep(un_military_base2  $controls excluded_mean veco_viol_trend1) ///
ctitle("Model B9") label dec(3) alpha(0.001, 0.01, 0.05, 0.1) symbol(***,**, *, +)  ///
noni nor2 noobs addtext(R-squared, `Rsq', N, `N' , FE for admin. units, "YES", FE for months, "NO") 

xtreg veco_viol_du un_military_base2 $controls excluded_mean veco_viol_trend2 if veco_viol_trend2!=., fe vce(cl adminUnitElection_id)
local Rsq: display %9.3f e(r2)
local N: display %9.0f e(N)
local N_g: display %9.0f e(N_g)
outreg2 using ".\Tables\app_table_B_FEwithExcluded.tex", append ///
keep(un_military_base2 $controls excluded_mean veco_viol_trend2) ///
ctitle("Model B10") label dec(3) alpha(0.001, 0.01, 0.05, 0.1) symbol(***,**, *, +)  ///
noni nor2 noobs addtext(R-squared, `Rsq', N, `N' , FE for admin. units, "YES", FE for months, "NO") 



** Appendix B: IMPUTED Inclusion of variable for exclusion of politically relevant ethnic groups
set seed 9999
mi set wide
capture mi register imputed excluded_mean
capture mi impute regress excluded_mean $controls, add(20)
mi estimate, dots post: xtreg veco_viol_du un_military_base2 excluded_mean, fe vce(cl adminUnitElection_id)
local Rsq: display %9.3f e(r2)
local N: display %9.0f e(N)
local N_g: display %9.0f e(N_g)
outreg2 using ".\Tables\app_table_B_IMPU_FEwithExcluded.tex", replace ///
keep(un_military_base2 excluded_mean) ///
ctitle("Model B1") label dec(3) alpha(0.001, 0.01, 0.05, 0.1) symbol(***,**, *, +) ///
noni nor2 noobs addtext(R-squared, `Rsq', N, `N', FE for admin. units, "YES", FE for months, "NO" ) 

mi estimate, dots post: xtreg veco_viol_du un_military_base2 $controls excluded_mean, fe vce(cl adminUnitElection_id)
local Rsq: display %9.3f e(r2)
local N: display %9.0f e(N)
local N_g: display %9.0f e(N_g)
outreg2 using ".\Tables\app_table_B_IMPU_FEwithExcluded.tex", append ///
keep(un_military_base2 $controls excluded_mean) ///
ctitle("Model B2") label dec(3) alpha(0.001, 0.01, 0.05, 0.1) symbol(***,**, *, +)  ///
noni nor2 noobs addtext(R-squared, `Rsq', N, `N' , FE for admin. units, "YES", FE for months, "NO") 

mi estimate,  post noisily: xtreg veco_viol_du un_military_base2 $controls excluded_mean i.date, fe vce(cl adminUnitElection_id)
local Rsq: display %9.3f e(r2)
local N: display %9.0f e(N)
local N_g: display %9.0f e(N_g)
outreg2 using ".\Tables\app_table_B_IMPU_FEwithExcluded.tex", append ///
keep(un_military_base2  $controls excluded_mean) ///
ctitle("Model B3") label dec(3) alpha(0.001, 0.01, 0.051, 0.1) symbol(***,**, *, +)  ///
noni nor2 noobs addtext(R-squared, `Rsq', N, `N' , FE for admin. units, "YES", FE for months, "YES") 

mi estimate, dots post: xtreg veco_viol_du un_military_base2 $controls excluded_mean veco_viol_trend1 if veco_viol_trend1!=., fe vce(cl adminUnitElection_id)
local Rsq: display %9.3f e(r2)
local N: display %9.0f e(N)
local N_g: display %9.0f e(N_g)
outreg2 using ".\Tables\app_table_B_IMPU_FEwithExcluded.tex", append ///
keep(un_military_base2  $controls excluded_mean veco_viol_trend1) ///
ctitle("Model B4") label dec(3) alpha(0.001, 0.01, 0.05, 0.1) symbol(***,**, *, +)  ///
noni nor2 noobs addtext(R-squared, `Rsq', N, `N' , FE for admin. units, "YES", FE for months, "NO") 

mi estimate, dots post: xtreg veco_viol_du un_military_base2 $controls excluded_mean veco_viol_trend2 if veco_viol_trend2!=., fe vce(cl adminUnitElection_id)
local Rsq: display %9.3f e(r2)
local N: display %9.0f e(N)
local N_g: display %9.0f e(N_g)
outreg2 using ".\Tables\app_table_B_IMPU_FEwithExcluded.tex", append ///
keep(un_military_base2 $controls excluded_mean veco_viol_trend2) ///
ctitle("Model B5") label dec(3) alpha(0.001, 0.01, 0.05, 0.1) symbol(***,**, *, +)  ///
noni nor2 noobs addtext(R-squared, `Rsq', N, `N' , FE for admin. units, "YES", FE for months, "NO") 


** Appendix B: Matching and post-matching analyses with variables for excluded groups

* New set of matching variables with excluded groups
capture mi unset
global controls_cem2 "viol_stateBased_preDep viol_stateBased_preDep_trend viol_OneSided_preDep viol_OneSided_preDep_trend pop_gpw_sum roadDensity imr_mean bdist1 capdist un_military_base_du_spL excluded_mean"
quietly: logit veco_viol_du un_military_base_du $controls_cem2  un_military_base_du2Lag1  excluded_mean, vce(cl adminUnitElection_id)
capture drop sample_imp
gen sample_imp = e(sample)

* Standardize new set of matching vars
foreach var of varlist $controls_cem2 {
	capture drop `var'_std
	egen `var'_std = std(`var') if sample_imp==1
}

global controls_cem_std2 "viol_stateBased_preDep_std viol_stateBased_preDep_trend_std viol_OneSided_preDep_std viol_OneSided_preDep_trend_std pop_gpw_sum_std roadDensity_std imr_mean_std bdist1_std capdist_std un_military_base_du_spL_std"
do "replication_labels.do"


* Make Propensity scores 
preserve 
keep if sample_imp==1
capture drop match*
teffects psmatch (veco_viol_du)(un_military_base_du $controls_cem_std2  ) , vce(robust) atet nn(1) gen(match) 
tebalance summarize

capture drop ob
gen ob = _n //store the observation numbers for future use
save "temp_fulldata", replace // save the complete data set

keep if un_military_base_du==1 // keep just the treated group
keep match1 // keep just the match1 variable (the observation numbers of their matches)
bysort match1: gen _weight = _N // count how many times each control observation is a match
by match1: keep if _n==1 // keep just one row per control observation
rename match1 ob //rename for merging purposes

merge 1:m ob using "temp_fulldata" // merge back into the full data
replace _weight = 1 if un_military_base_du==1 // set weight to 1 for treated observations
drop _merge

* Post-propensity score matching analyses
logit veco_viol_du un_military_base_du $controls_cem_std2 excluded_mean_std un_military_base_du2Lag1  [fweight=_weight], vce(robust)
gen sam = e(sample)
estat ic
mat es_ic = r(S)
local AIC: display %4.1f es_ic[1,5]
local BIC: display %4.1f es_ic[1,6]
local LL: display %4.1f es_ic[1,3]

outreg2 using ".\Tables\app_table_B_withExcluded.tex", replace ///
keep(un_military_base_du $controls_cem_std2 un_military_base_du2Lag1 excluded_mean_std ) ///
ctitle("Model B11") label dec(3) alpha(0.001, 0.01, 0.05, 0.1) symbol(***,**, *, +) ///
noni nor2 addtext(Log-Likelihood, `LL' ,BIC, `BIC', AIC, `AIC') 

logit veco_viol_du un_military_base2 $controls_cem_std2 excluded_mean_std un_military_base_du2Lag1  [fweight=_weight], vce(robust)
estat ic
mat es_ic = r(S)
local AIC: display %4.1f es_ic[1,5]
local BIC: display %4.1f es_ic[1,6]
local LL: display %4.1f es_ic[1,3]

outreg2 using ".\Tables\app_table_B_withExcluded.tex", append ///
keep(un_military_base2 $controls_cem_std2 un_military_base_du2Lag1 excluded_mean_std ) ///
ctitle("Model B12") label dec(3) alpha(0.001, 0.01, 0.05, 0.1) symbol(***,**, *, +) ///
noni nor2 addtext(Log-Likelihood, `LL' ,BIC, `BIC', AIC, `AIC') 

restore

* Make CEM weights
imb $controls_cem_std2 if sample_imp==1, tr(un_military_base_du)
cem $controls_cem_std2 if sample_imp==1, tr(un_military_base_du) autocuts(fd)
tab veco_viol_du if cem_weights!=0 

* Post CEM-analyses
logit veco_viol_du un_military_base_du $controls_cem_std2 un_military_base_du2Lag1 excluded_mean_std [iweight=cem_weights], vce(cl adminUnitElection_id)
estat ic
mat es_ic = r(S)
local AIC: display %4.1f es_ic[1,5]
local BIC: display %4.1f es_ic[1,6]
local LL: display %4.1f es_ic[1,3]

outreg2 using ".\Tables\app_table_B_withExcluded.tex", append ///
keep(un_military_base_du $controls_cem_std2  un_military_base_du2Lag1 excluded_mean_std ) ///
ctitle("Model B13") label dec(3) alpha(0.001, 0.01, 0.05, 0.1) symbol(***,**, *, +) ///
noni nor2 addtext( Log-Likelihood, `LL' ,BIC, `BIC', AIC, `AIC') 

logit veco_viol_du un_military_base2 $controls_cem_std2  un_military_base_du2Lag1 excluded_mean_std  [iweight=cem_weights], vce(cl adminUnitElection_id)
estat ic
mat es_ic = r(S)
local AIC: display %4.1f es_ic[1,5]
local BIC: display %4.1f es_ic[1,6]
local LL: display %4.1f es_ic[1,3]

outreg2 using ".\Tables\app_table_B_withExcluded.tex", append ///
keep(un_military_base2 $controls_cem_std2  un_military_base_du2Lag1 excluded_mean_std  ) ///
ctitle("Model B14") label dec(3) alpha(0.001, 0.01, 0.05, 0.1) symbol(***,**, *, +) ///
noni nor2 addtext(Log-Likelihood, `LL' ,BIC, `BIC', AIC, `AIC') 




** Appendix C: ECAV dataset
capture drop excluded_mean_*
capture mi unset
xtset adminUnitElection_id date
capture drop ecav_trend* 
sort adminUnitElection_id date
gen ecav_trend1 = (L1.ecav_event) - (L2.ecav_event+L3.ecav_event)/2
gen ecav_trend2 = (L1.ecav_event+L2.ecav_event)/2 - (L3.ecav_event)

do "replication_labels.do"

xtreg ecav_event_du un_military_base2, fe vce(cl adminUnitElection_id)
local Rsq: display %9.3f e(r2)
local N: display %9.0f e(N)
local N_g: display %9.0f e(N_g)
outreg2 using ".\Tables\app_table_C_ECAV.tex", replace ///
keep(un_military_base2) ///
ctitle("Model C1") label dec(3) alpha(0.001, 0.01, 0.05, 0.1) symbol(***,**, *, +) ///
noni nor2 noobs addtext(R-squared, `Rsq', N, `N', FE for admin. units, "YES", FE for months, "NO" ) 

xtreg ecav_event_du un_military_base2 $controls, fe vce(cl adminUnitElection_id)
local Rsq: display %9.3f e(r2)
local N: display %9.0f e(N)
local N_g: display %9.0f e(N_g)
outreg2 using ".\Tables\app_table_C_ECAV.tex", append ///
keep(un_military_base2 $controls) ///
ctitle("Model C2") label dec(3) alpha(0.001, 0.01, 0.05, 0.1) symbol(***,**, *, +) ///
noni nor2 noobs addtext(R-squared, `Rsq', N, `N', FE for admin. units, "YES", FE for months, "NO" ) 

xtreg ecav_event_du un_military_base2 $controls i.date, fe vce(cl adminUnitElection_id)
local Rsq: display %9.3f e(r2)
local N: display %9.0f e(N)
local N_g: display %9.0f e(N_g)
outreg2 using ".\Tables\app_table_C_ECAV.tex", append ///
keep(un_military_base2 $controls) ///
ctitle("Model C3") label dec(3) alpha(0.001, 0.01, 0.05, 0.1) symbol(***,**, *, +) ///
noni nor2 noobs addtext(R-squared, `Rsq', N, `N', FE for admin. units, "YES", FE for months, "NO" ) 

xtreg ecav_event_du un_military_base2 $controls ecav_trend1 , fe vce(cl adminUnitElection_id)
local Rsq: display %9.3f e(r2)
local N: display %9.0f e(N)
local N_g: display %9.0f e(N_g)
outreg2 using ".\Tables\app_table_C_ECAV.tex", append ///
keep(un_military_base2 $controls ecav_trend1) ///
ctitle("Model C4") label dec(3) alpha(0.001, 0.01, 0.05, 0.1) symbol(***,**, *, +) ///
noni nor2 noobs addtext(R-squared, `Rsq', N, `N', FE for admin. units, "YES", FE for months, "NO" ) 

xtreg ecav_event_du un_military_base2 $controls ecav_trend2 , fe vce(cl adminUnitElection_id)
local Rsq: display %9.3f e(r2)
local N: display %9.0f e(N)
local N_g: display %9.0f e(N_g)
outreg2 using ".\Tables\app_table_C_ECAV.tex", append ///
keep(un_military_base2 $controls ecav_trend2) ///
ctitle("Model C5") label dec(3) alpha(0.001, 0.01, 0.05, 0.1) symbol(***,**, *, +) ///
noni nor2 noobs addtext(R-squared, `Rsq', N, `N', FE for admin. units, "YES", FE for months, "NO" ) 



* Analyses with ecav after matching
quietly: logit ecav_event_du un_military_base_du $controls_cem, vce(cl adminUnitElection_id)
capture drop sample_ecav
gen sample_ecav = e(sample)

* Standardize new set of matching vars
foreach var of varlist $controls_cem {
	capture drop `var'_std
	egen `var'_std = std(`var') if sample_ecav==1
}

global controls_cem_std "viol_stateBased_preDep_std viol_stateBased_preDep_trend_std viol_OneSided_preDep_std pop_gpw_sum_std roadDensity_std imr_mean_std bdist1_std capdist_std un_military_base_du_spL_std"
do "replication_labels.do"

* Make Propensity scores 
preserve 
keep if sample_ecav==1
capture drop match*
teffects psmatch (ecav_event_du)(un_military_base_du $controls_cem_std) , vce(robust) atet nn(1) gen(match) 
tebalance summarize

capture drop ob
gen ob = _n //store the observation numbers for future use
save "temp_fulldata", replace // save the complete data set

keep if un_military_base_du==1 // keep just the treated group
keep match1 // keep just the match1 variable (the observation numbers of their matches)
bysort match1: gen _weight = _N // count how many times each control observation is a match
by match1: keep if _n==1 // keep just one row per control observation
rename match1 ob //rename for merging purposes

merge 1:m ob using "temp_fulldata" // merge back into the full data
replace _weight = 1 if un_military_base_du==1 // set weight to 1 for treated observations
drop _merge


* Post-propensity score matching analyses
logit ecav_event_du un_military_base_du $controls_cem_std adminArea un_military_base_du2Lag1 [fweight=_weight], vce(robust)
estat ic
mat es_ic = r(S)
local AIC: display %4.1f es_ic[1,5]
local BIC: display %4.1f es_ic[1,6]
local LL: display %4.1f es_ic[1,3]

outreg2 using ".\Tables\app_table_C_Match_ECAV.tex", replace ///
keep(un_military_base_du $controls_cem_std adminArea un_military_base_du2Lag1 ) ///
ctitle("Model C6") label dec(3) alpha(0.001, 0.01, 0.05, 0.1) symbol(***,**, *, +) ///
noni nor2 addtext(Log-Likelihood, `LL' ,BIC, `BIC', AIC, `AIC') 


logit ecav_event_du un_military_base2 $controls_cem_std un_military_base_du2Lag1 adminArea  [fweight=_weight], vce(robust)
estat ic
mat es_ic = r(S)
local AIC: display %4.1f es_ic[1,5]
local BIC: display %4.1f es_ic[1,6]
local LL: display %4.1f es_ic[1,3]

outreg2 using ".\Tables\app_table_C_Match_ECAV.tex", append ///
keep(un_military_base2 $controls_cem_std adminArea un_military_base_du2Lag1 ) ///
ctitle("Model C7") label dec(3) alpha(0.001, 0.01, 0.05, 0.1) symbol(***,**, *, +) ///
noni nor2 addtext(Log-Likelihood, `LL' ,BIC, `BIC', AIC, `AIC') 

restore

* Make CEM weights
imb $controls_cem_std if sample_ecav==1, tr(un_military_base_du)
cem $controls_cem_std if sample_ecav==1, tr(un_military_base_du) autocuts(fd)
tab ecav_event_du if cem_weights!=0 

* Post CEM-analyses
logit ecav_event_du un_military_base_du $controls_cem_std adminArea un_military_base_du2Lag1 [iweight=cem_weights], vce(cl adminUnitElection_id)
estat ic
mat es_ic = r(S)
local AIC: display %4.1f es_ic[1,5]
local BIC: display %4.1f es_ic[1,6]
local LL: display %4.1f es_ic[1,3]

outreg2 using ".\Tables\app_table_C_Match_ECAV.tex", append ///
keep(un_military_base_du $controls_cem_std adminArea un_military_base_du2Lag1 ) ///
ctitle("Model C8") label dec(3) alpha(0.001, 0.01, 0.05, 0.1) symbol(***,**, *, +) ///
noni nor2 addtext( Log-Likelihood, `LL' ,BIC, `BIC', AIC, `AIC') 

logit ecav_event_du un_military_base2 $controls_cem_std adminArea un_military_base_du2Lag1 [iweight=cem_weights], vce(cl adminUnitElection_id)
estat ic
mat es_ic = r(S)
local AIC: display %4.1f es_ic[1,5]
local BIC: display %4.1f es_ic[1,6]
local LL: display %4.1f es_ic[1,3]

outreg2 using ".\Tables\app_table_C_Match_ECAV.tex", append ///
keep(un_military_base2 $controls_cem_std adminArea un_military_base_du2Lag1 ) ///
ctitle("Model C9") label dec(3) alpha(0.001, 0.01, 0.05, 0.1) symbol(***,**, *, +) ///
noni nor2 addtext(Log-Likelihood, `LL' ,BIC, `BIC', AIC, `AIC') 




** Appendix D: Spatial error models with fixed effects (in balanced panels)

* see R file _analysis_spatialModels_apr20.R




** Appendix E: Targeting of third party peacemakers and PKOs with violence as control
tab PAR_pkoFatalities if sample==1

xtreg veco_viol_du un_military_base2 PAR_pkoFatalities_yr, fe vce(cl adminUnitElection_id)
capture drop sample_App_E
gen sample_App_E = e(sample)
local Rsq: display %9.3f e(r2)
local N: display %9.0f e(N)
local N_g: display %9.0f e(N_g)

outreg2 using ".\Tables\app_table_E_PAR.tex", replace ///
keep(un_military_base2 PAR_pkoFatalities_yr) ///
ctitle("Model E1") label dec(3) alpha(0.001, 0.01, 0.05, 0.1) symbol(***,**, *, +) ///
noni nor2 noobs addtext(R-squared, `Rsq', N, `N', FE for admin. units, "YES", FE for months, "NO" ) 


xtreg veco_viol_du un_military_base2 $controls PAR_pkoFatalities_yr, fe vce(cl adminUnitElection_id)
local Rsq: display %9.3f e(r2)
local N: display %9.0f e(N)
local N_g: display %9.0f e(N_g)

outreg2 using ".\Tables\app_table_E_PAR.tex", append ///
keep(un_military_base2 $controls PAR_pkoFatalities_yr)  ///
ctitle("Model E2") label dec(3) alpha(0.001, 0.01, 0.05, 0.1) symbol(***,**, *, +)  ///
noni nor2 noobs addtext(R-squared, `Rsq', N, `N' , FE for admin. units, "YES", FE for months, "NO") 


xtreg veco_viol_du un_military_base2 $controls PAR_pkoFatalities_yr i.date, fe vce(cl adminUnitElection_id)
local Rsq: display %9.3f e(r2)
local N: display %9.0f e(N)
local N_g: display %9.0f e(N_g)
 
outreg2 using ".\Tables\app_table_E_PAR.tex", append ///
keep(un_military_base2  $controls PAR_pkoFatalities_yr) ///
ctitle("Model E3") label dec(3) alpha(0.001, 0.01, 0.051, 0.1) symbol(***,**, *, +)  ///
noni nor2 noobs addtext(R-squared, `Rsq', N, `N' , FE for admin. units, "YES", FE for months, "YES") 

*** Time trends 
xtreg veco_viol_du un_military_base2 $controls veco_viol_trend1 PAR_pkoFatalities_yr  if veco_viol_trend1!=., fe vce(cl adminUnitElection_id)
local Rsq: display %9.3f e(r2)
local N: display %9.0f e(N)
local N_g: display %9.0f e(N_g)
 
outreg2 using ".\Tables\app_table_E_PAR.tex", append ///
keep(un_military_base2  $controls PAR_pkoFatalities_yr veco_viol_trend1) ///
ctitle("Model E4") label dec(3) alpha(0.001, 0.01, 0.05, 0.1) symbol(***,**, *, +)  ///
noni nor2 noobs addtext(R-squared, `Rsq', N, `N' , FE for admin. units, "YES", FE for months, "NO") 


xtreg veco_viol_du un_military_base2 $controls veco_viol_trend2 PAR_pkoFatalities_yr if veco_viol_trend2!=., fe vce(cl adminUnitElection_id)
local Rsq: display %9.3f e(r2)
local N: display %9.0f e(N)
local N_g: display %9.0f e(N_g)
 
outreg2 using ".\Tables\app_table_E_PAR.tex", append ///
keep(un_military_base2 $controls PAR_pkoFatalities_yr veco_viol_trend2) ///
ctitle("Model E5") label dec(3) alpha(0.001, 0.01, 0.05, 0.1) symbol(***,**, *, +)  ///
noni nor2 noobs addtext(R-squared, `Rsq', N, `N' , FE for admin. units, "YES", FE for months, "NO") 


** Post-matching

** Coarsened Exact Matching
* cannot be estimated

** Propensity scores Matching
preserve 
drop if sample_App_E!=1
capture drop match*
capture drop ps_temp
teffects psmatch (veco_viol_du)(un_military_base_du $controls_cem_std ) , vce(robust) atet nn(1) gen(match) 
tebalance summarize
matrix imb = r(table)
matrix list imb
matrix imb2 = imb[1..9,2]
svmat double imb2, names(ps_temp)

* Make weights
capture drop ob
gen ob = _n //store the observation numbers for future use
save "temp_fulldata", replace // save the complete data set

keep if un_military_base_du==1 // keep just the treated group
keep match1 // keep just the match1 variable (the observation numbers of their matches)
bysort match1: gen _weight = _N // count how many times each control observation is a match
by match1: keep if _n==1 // keep just one row per control observation
rename match1 ob //rename for merging purposes

merge 1:m ob using "temp_fulldata" // merge back into the full data
replace _weight = 1 if un_military_base_du==1 // set weight to 1 for treated observations
drop _merge


** Post-propensity score matching
logit veco_viol_du un_military_base_du $controls_cem_std PAR_pkoFatalities_yr adminArea un_military_base_du2Lag1  [fweight=_weight], vce(robust)
gen sam = e(sample)
estat ic
mat es_ic = r(S)
local AIC: display %4.1f es_ic[1,5]
local BIC: display %4.1f es_ic[1,6]
local LL: display %4.1f es_ic[1,3]


outreg2 using ".\Tables\app_table_E_PAR_MATCH.tex", replace ///
keep(un_military_base_du $controls_cem_std PAR_pkoFatalities_yr adminArea un_military_base_du2Lag1 ) ///
ctitle("Model E6") label dec(3) alpha(0.001, 0.01, 0.05, 0.1) symbol(***,**, *, +) ///
noni nor2 addtext(Log-Likelihood, `LL' ,BIC, `BIC', AIC, `AIC') 


logit veco_viol_du un_military_base2 $controls_cem_std  PAR_pkoFatalities_yr adminArea un_military_base_du2Lag1  [fweight=_weight], vce(robust)
estat ic
mat es_ic = r(S)
local AIC: display %4.1f es_ic[1,5]
local BIC: display %4.1f es_ic[1,6]
local LL: display %4.1f es_ic[1,3]


outreg2 using ".\Tables\app_table_E_PAR_MATCH.tex", append ///
keep(un_military_base2 $controls_cem_std PAR_pkoFatalities_yr adminArea un_military_base_du2Lag1  ) ///
ctitle("Model E7") label dec(3) alpha(0.001, 0.01, 0.05, 0.1) symbol(***,**, *, +) ///
noni nor2 addtext(Log-Likelihood, `LL' ,BIC, `BIC', AIC, `AIC') 

restore



**** Appendix F: Excluding DR Congo 2010, Mali 2013, Sudan 2010 2015, CAR 2016 AND Country-specific effects
tab country if PAR_pkoFatalities>0 & PAR_pkoFatalities!=.
tab country if PAR_pkoFatalities_yr>0 & PAR_pkoFatalities_yr!=.

preserve
drop if country=="Democratic Republic of the Congo" & year>=2009
drop if country=="Mali" & year>=2012
drop if country=="Central African Republic" & year>=2015
drop if country=="Sudan" & year>=2009

xtreg veco_viol_du un_military_base2, fe vce(cl adminUnitElection_id)
local Rsq: display %9.3f e(r2)
local N: display %9.0f e(N)
local N_g: display %9.0f e(N_g)
outreg2 using ".\Tables\app_table_F_country.tex", replace ///
keep(un_military_base2) ///
ctitle("Model F1") label dec(3) alpha(0.001, 0.01, 0.05, 0.1) symbol(***,**, *, +) ///
noni nor2 noobs addtext(R-squared, `Rsq', N, `N', FE for admin. units, "YES", FE for months, "NO" ) 

xtreg veco_viol_du un_military_base2 $controls, fe vce(cl adminUnitElection_id)
local Rsq: display %9.3f e(r2)
local N: display %9.0f e(N)
local N_g: display %9.0f e(N_g)
outreg2 using ".\Tables\app_table_F_country.tex", append ///
keep(un_military_base2 $controls) ///
ctitle("Model F2") label dec(3) alpha(0.001, 0.01, 0.05, 0.1) symbol(***,**, *, +)  ///
noni nor2 noobs addtext(R-squared, `Rsq', N, `N' , FE for admin. units, "YES", FE for months, "NO") 

xtreg veco_viol_du un_military_base2 $controls i.date, fe vce(cl adminUnitElection_id)
local Rsq: display %9.3f e(r2)
local N: display %9.0f e(N)
local N_g: display %9.0f e(N_g)
outreg2 using ".\Tables\app_table_F_country.tex", append ///
keep(un_military_base2  $controls) ///
ctitle("Model F3") label dec(3) alpha(0.001, 0.01, 0.051, 0.1) symbol(***,**, *, +)  ///
noni nor2 noobs addtext(R-squared, `Rsq', N, `N' , FE for admin. units, "YES", FE for months, "YES") 

xtreg veco_viol_du un_military_base2 $controls veco_viol_trend1 if veco_viol_trend1!=., fe vce(cl adminUnitElection_id)
local Rsq: display %9.3f e(r2)
local N: display %9.0f e(N)
local N_g: display %9.0f e(N_g)
outreg2 using ".\Tables\app_table_F_country.tex", append ///
keep(un_military_base2  $controls veco_viol_trend1) ///
ctitle("Model F4") label dec(3) alpha(0.001, 0.01, 0.05, 0.1) symbol(***,**, *, +)  ///
noni nor2 noobs addtext(R-squared, `Rsq', N, `N' , FE for admin. units, "YES", FE for months, "NO") 

xtreg veco_viol_du un_military_base2 $controls veco_viol_trend2 if veco_viol_trend2!=., fe vce(cl adminUnitElection_id)
local Rsq: display %9.3f e(r2)
local N: display %9.0f e(N)
local N_g: display %9.0f e(N_g)
outreg2 using ".\Tables\app_table_F_country.tex", append ///
keep(un_military_base2 $controls veco_viol_trend2) ///
ctitle("Model F5") label dec(3) alpha(0.001, 0.01, 0.05, 0.1) symbol(***,**, *, +)  ///
noni nor2 noobs addtext(R-squared, `Rsq', N, `N' , FE for admin. units, "YES", FE for months, "NO") 

restore

preserve
keep if (country=="Democratic Republic of the Congo" & year>=2009) | ///
(country=="Mali" & year>=2012) | ///
(country=="Central African Republic" & year>=2015) | ///
(country=="Sudan" & year>=2009)

xtreg veco_viol_du un_military_base2, fe vce(cl adminUnitElection_id)
local Rsq: display %9.3f e(r2)
local N: display %9.0f e(N)
local N_g: display %9.0f e(N_g)
outreg2 using ".\Tables\app_table_F_Withcountry.tex", replace ///
keep(un_military_base2) ///
ctitle("Model F6") label dec(3) alpha(0.001, 0.01, 0.05, 0.1) symbol(***,**, *, +) ///
noni nor2 noobs addtext(R-squared, `Rsq', N, `N', FE for admin. units, "YES", FE for months, "NO" ) 

xtreg veco_viol_du un_military_base2 $controls, fe vce(cl adminUnitElection_id)
local Rsq: display %9.3f e(r2)
local N: display %9.0f e(N)
local N_g: display %9.0f e(N_g)
outreg2 using ".\Tables\app_table_F_Withcountry.tex", append ///
keep(un_military_base2 $controls) ///
ctitle("Model F7") label dec(3) alpha(0.001, 0.01, 0.05, 0.1) symbol(***,**, *, +)  ///
noni nor2 noobs addtext(R-squared, `Rsq', N, `N' , FE for admin. units, "YES", FE for months, "NO") 

xtreg veco_viol_du un_military_base2 $controls i.date, fe vce(cl adminUnitElection_id)
local Rsq: display %9.3f e(r2)
local N: display %9.0f e(N)
local N_g: display %9.0f e(N_g)
outreg2 using ".\Tables\app_table_F_Withcountry.tex", append ///
keep(un_military_base2  $controls) ///
ctitle("Model F8") label dec(3) alpha(0.001, 0.01, 0.051, 0.1) symbol(***,**, *, +)  ///
noni nor2 noobs addtext(R-squared, `Rsq', N, `N' , FE for admin. units, "YES", FE for months, "YES") 

xtreg veco_viol_du un_military_base2 $controls veco_viol_trend1 if veco_viol_trend1!=., fe vce(cl adminUnitElection_id)
local Rsq: display %9.3f e(r2)
local N: display %9.0f e(N)
local N_g: display %9.0f e(N_g)
outreg2 using ".\Tables\app_table_F_Withcountry.tex", append ///
keep(un_military_base2  $controls veco_viol_trend1) ///
ctitle("Model F9") label dec(3) alpha(0.001, 0.01, 0.05, 0.1) symbol(***,**, *, +)  ///
noni nor2 noobs addtext(R-squared, `Rsq', N, `N' , FE for admin. units, "YES", FE for months, "NO") 

xtreg veco_viol_du un_military_base2 $controls veco_viol_trend2 if veco_viol_trend2!=., fe vce(cl adminUnitElection_id)
local Rsq: display %9.3f e(r2)
local N: display %9.0f e(N)
local N_g: display %9.0f e(N_g)
outreg2 using ".\Tables\app_table_F_Withcountry.tex", append ///
keep(un_military_base2 $controls veco_viol_trend2) ///
ctitle("Model F10") label dec(3) alpha(0.001, 0.01, 0.05, 0.1) symbol(***,**, *, +)  ///
noni nor2 noobs addtext(R-squared, `Rsq', N, `N' , FE for admin. units, "YES", FE for months, "NO") 

restore







*****Appendix I: Robustness test for Hypothesis 2 with ECAV data



** Pre-election violence (ECAV)

xtreg ecav_event_pre_du un_military_base2, fe vce(cl adminUnitElection_id)
capture drop sample_pre
gen sample_pre = e(sample)
local Rsq: display %9.3f e(r2)
local N: display %9.0f e(N)
local N_g: display %9.0f e(N_g)
capture drop estimate
capture drop low
capture drop high
tempname b
matrix `b' = e(b)
gen estimate = `b'[1,1] in 1
tempname V
matrix `V' = e(V)
gen low =  `b'[1,1] - 1.54*sqrt(`V'[1,1]) in 1
gen high = `b'[1,1] + 1.54*sqrt(`V'[1,1]) in 1
outreg2 using ".\Tables\app_table_I_ecav_pre.tex", replace ///
keep(un_military_base2) ///
ctitle("Model I1") label dec(3) alpha(0.001, 0.01, 0.05, 0.1) symbol(***,**, *, +) ///
noni nor2 noobs addtext(R-squared, `Rsq', N, `N', FE for admin. units, "YES", FE for months, "NO" ) 

xtreg ecav_event_pre_du un_military_base2 $controls, fe vce(cl adminUnitElection_id)
local Rsq: display %9.3f e(r2)
local N: display %9.0f e(N)
local N_g: display %9.0f e(N_g)
tempname b
matrix `b' = e(b)
replace estimate = `b'[1,1] in 2
tempname V
matrix `V' = e(V)
replace low =  `b'[1,1] - 1.54*sqrt(`V'[1,1]) in 2
replace high = `b'[1,1] + 1.54*sqrt(`V'[1,1]) in 2
outreg2 using ".\Tables\app_table_I_ecav_pre.tex", append ///
keep(un_military_base2 $controls) ///
ctitle("Model I2") label dec(3) alpha(0.001, 0.01, 0.05, 0.1) symbol(***,**, *, +) ///
noni nor2 noobs addtext(R-squared, `Rsq', N, `N', FE for admin. units, "YES", FE for months, "NO" ) 

xtreg ecav_event_pre_du un_military_base2 $controls i.date, fe vce(cl adminUnitElection_id)
local Rsq: display %9.3f e(r2)
local N: display %9.0f e(N)
local N_g: display %9.0f e(N_g)
tempname b
matrix `b' = e(b)
replace estimate = `b'[1,1] in 3
tempname V
matrix `V' = e(V)
replace low =  `b'[1,1] - 1.54*sqrt(`V'[1,1]) in 3
replace high = `b'[1,1] + 1.54*sqrt(`V'[1,1]) in 3
outreg2 using ".\Tables\app_table_I_ecav_pre.tex", append ///
keep(un_military_base2 $controls) ///
ctitle("Model I3") label dec(3) alpha(0.001, 0.01, 0.05, 0.1) symbol(***,**, *, +) ///
noni nor2 noobs addtext(R-squared, `Rsq', N, `N', FE for admin. units, "YES", FE for months, "YES" ) 

xtreg ecav_event_pre_du un_military_base2 $controls ecav_trend1, fe vce(cl adminUnitElection_id)
local Rsq: display %9.3f e(r2)
local N: display %9.0f e(N)
local N_g: display %9.0f e(N_g)
tempname b
matrix `b' = e(b)
replace estimate = `b'[1,1] in 4
tempname V
matrix `V' = e(V)
replace low =  `b'[1,1] - 1.54*sqrt(`V'[1,1]) in 4
replace high = `b'[1,1] + 1.54*sqrt(`V'[1,1]) in 4
outreg2 using ".\Tables\app_table_I_ecav_pre.tex", append ///
keep(un_military_base2 $controls) ///
ctitle("Model I4") label dec(3) alpha(0.001, 0.01, 0.05, 0.1) symbol(***,**, *, +) ///
noni nor2 noobs addtext(R-squared, `Rsq', N, `N', FE for admin. units, "YES", FE for months, "NO" ) 

xtreg ecav_event_pre_du un_military_base2 $controls ecav_trend2, fe vce(cl adminUnitElection_id)
local Rsq: display %9.3f e(r2)
local N: display %9.0f e(N)
local N_g: display %9.0f e(N_g)
tempname b
matrix `b' = e(b)
replace estimate = `b'[1,1] in 5
tempname V
matrix `V' = e(V)
replace low =  `b'[1,1] - 1.54*sqrt(`V'[1,1]) in 5
replace high = `b'[1,1] + 1.54*sqrt(`V'[1,1]) in 5
outreg2 using ".\Tables\app_table_I_ecav_pre.tex", append ///
keep(un_military_base2 $controls) ///
ctitle("Model I5") label dec(3) alpha(0.001, 0.01, 0.05, 0.1) symbol(***,**, *, +) ///
noni nor2 noobs addtext(R-squared, `Rsq', N, `N', FE for admin. units, "YES", FE for months, "NO" ) 



** Post-election violence (ECAV)

xtreg ecav_event_post_du un_military_base2 , fe vce(cl adminUnitElection_id)
capture drop sample_post
gen sample_post = e(sample)
local Rsq: display %9.3f e(r2)
local N: display %9.0f e(N)
local N_g: display %9.0f e(N_g)
tempname b
matrix `b' = e(b)
replace estimate = `b'[1,1] in 6
tempname V
matrix `V' = e(V)
replace low =  `b'[1,1] - 1.54*sqrt(`V'[1,1]) in 6
replace high = `b'[1,1] + 1.54*sqrt(`V'[1,1]) in 6
outreg2 using ".\Tables\app_table_I_ecav_post.tex", replace ///
keep(un_military_base2 $controls) ///
ctitle("Model I6") label dec(3) alpha(0.001, 0.01, 0.05, 0.1) symbol(***,**, *, +) ///
noni nor2 noobs addtext(R-squared, `Rsq', N, `N', FE for admin. units, "YES", FE for months, "NO" ) 

xtreg ecav_event_post_du un_military_base2 $controls, fe vce(cl adminUnitElection_id)
local Rsq: display %9.3f e(r2)
local N: display %9.0f e(N)
local N_g: display %9.0f e(N_g)
tempname b
matrix `b' = e(b)
replace estimate = `b'[1,1] in 7
tempname V
matrix `V' = e(V)
replace low =  `b'[1,1] - 1.54*sqrt(`V'[1,1]) in 7
replace high = `b'[1,1] + 1.54*sqrt(`V'[1,1]) in 7
outreg2 using ".\Tables\app_table_I_ecav_post.tex", append ///
keep(un_military_base2) ///
ctitle("Model I7" $controls) label dec(3) alpha(0.001, 0.01, 0.05, 0.1) symbol(***,**, *, +) ///
noni nor2 noobs addtext(R-squared, `Rsq', N, `N', FE for admin. units, "YES", FE for months, "NO" ) 

xtreg ecav_event_post_du un_military_base2 $controls i.date, fe vce(cl adminUnitElection_id)
local Rsq: display %9.3f e(r2)
local N: display %9.0f e(N)
local N_g: display %9.0f e(N_g)
tempname b
matrix `b' = e(b)
replace estimate = `b'[1,1] in 8
tempname V
matrix `V' = e(V)
replace low =  `b'[1,1] - 1.54*sqrt(`V'[1,1]) in 8
replace high = `b'[1,1] + 1.54*sqrt(`V'[1,1]) in 8
outreg2 using ".\Tables\app_table_I_ecav_post.tex", append ///
keep(un_military_base2 $controls) ///
ctitle("Model I8") label dec(3) alpha(0.001, 0.01, 0.05, 0.1) symbol(***,**, *, +) ///
noni nor2 noobs addtext(R-squared, `Rsq', N, `N', FE for admin. units, "YES", FE for months, "YES" ) 

xtreg ecav_event_post_du un_military_base2 $controls ecav_trend1, fe vce(cl adminUnitElection_id)
local Rsq: display %9.3f e(r2)
local N: display %9.0f e(N)
local N_g: display %9.0f e(N_g)
tempname b
matrix `b' = e(b)
replace estimate = `b'[1,1] in 9
tempname V
matrix `V' = e(V)
replace low =  `b'[1,1] - 1.54*sqrt(`V'[1,1]) in 9
replace high = `b'[1,1] + 1.54*sqrt(`V'[1,1]) in 9
outreg2 using ".\Tables\app_table_I_ecav_post.tex", append ///
keep(un_military_base2 $controls) ///
ctitle("Model I9") label dec(3) alpha(0.001, 0.01, 0.05, 0.1) symbol(***,**, *, +) ///
noni nor2 noobs addtext(R-squared, `Rsq', N, `N', FE for admin. units, "YES", FE for months, "NO" ) 

xtreg ecav_event_post_du un_military_base2 $controls ecav_trend2, fe vce(cl adminUnitElection_id)
local Rsq: display %9.3f e(r2)
local N: display %9.0f e(N)
local N_g: display %9.0f e(N_g)
tempname b
matrix `b' = e(b)
replace estimate = `b'[1,1] in 10
tempname V
matrix `V' = e(V)
replace low =  `b'[1,1] - 1.54*sqrt(`V'[1,1]) in 10
replace high = `b'[1,1] + 1.54*sqrt(`V'[1,1]) in 10
outreg2 using ".\Tables\app_table_I_ecav_post.tex", append ///
keep(un_military_base2 $controls) ///
ctitle("Model I10") label dec(3) alpha(0.001, 0.01, 0.05, 0.1) symbol(***,**, *, +) ///
noni nor2 noobs addtext(R-squared, `Rsq', N, `N', FE for admin. units, "YES", FE for months, "NO" ) 



** Appendix J: Robustness test for Hypothesis 3 with ECAV data

** Anti-government violence (ECAV)

xtreg ecav_anti_gov_du un_military_base2 , fe vce(cl adminUnitElection_id)
local Rsq: display %9.3f e(r2)
local N: display %9.0f e(N)
local N_g: display %9.0f e(N_g)
tempname b
matrix `b' = e(b)
replace estimate = `b'[1,1] in 11
tempname V
matrix `V' = e(V)
replace low =  `b'[1,1] - 1.54*sqrt(`V'[1,1]) in 11
replace high = `b'[1,1] + 1.54*sqrt(`V'[1,1]) in 11
outreg2 using ".\Tables\app_table_J_ecav_anti.tex", replace ///
keep(un_military_base2) ///
ctitle("Model J1") label dec(3) alpha(0.001, 0.01, 0.05, 0.1) symbol(***,**, *, +) ///
noni nor2 noobs addtext(R-squared, `Rsq', N, `N', FE for admin. units, "YES", FE for months, "NO" ) 

xtreg ecav_anti_gov_du un_military_base2 $controls, fe vce(cl adminUnitElection_id)
local Rsq: display %9.3f e(r2)
local N: display %9.0f e(N)
local N_g: display %9.0f e(N_g)
tempname b
matrix `b' = e(b)
replace estimate = `b'[1,1] in 12
tempname V
matrix `V' = e(V)
replace low =  `b'[1,1] - 1.54*sqrt(`V'[1,1]) in 12
replace high = `b'[1,1] + 1.54*sqrt(`V'[1,1]) in 12
outreg2 using ".\Tables\app_table_J_ecav_anti.tex", append ///
keep(un_military_base2 $controls) ///
ctitle("Model J2") label dec(3) alpha(0.001, 0.01, 0.05, 0.1) symbol(***,**, *, +) ///
noni nor2 noobs addtext(R-squared, `Rsq', N, `N', FE for admin. units, "YES", FE for months, "NO" ) 


xtreg ecav_anti_gov_du un_military_base2 $controls i.date, fe vce(cl adminUnitElection_id)
local Rsq: display %9.3f e(r2)
local N: display %9.0f e(N)
local N_g: display %9.0f e(N_g)
tempname b
matrix `b' = e(b)
replace estimate = `b'[1,1] in 13
tempname V
matrix `V' = e(V)
replace low =  `b'[1,1] - 1.54*sqrt(`V'[1,1]) in 13
replace high = `b'[1,1] + 1.54*sqrt(`V'[1,1]) in 13
outreg2 using ".\Tables\app_table_J_ecav_anti.tex", append ///
keep(un_military_base2 $controls) ///
ctitle("Model J3") label dec(3) alpha(0.001, 0.01, 0.05, 0.1) symbol(***,**, *, +) ///
noni nor2 noobs addtext(R-squared, `Rsq', N, `N', FE for admin. units, "YES", FE for months, "YES" ) 


xtreg ecav_anti_gov_du un_military_base2 $controls ecav_trend1, fe vce(cl adminUnitElection_id)
local Rsq: display %9.3f e(r2)
local N: display %9.0f e(N)
local N_g: display %9.0f e(N_g)
tempname b
matrix `b' = e(b)
replace estimate = `b'[1,1] in 14
tempname V
matrix `V' = e(V)
replace low =  `b'[1,1] - 1.54*sqrt(`V'[1,1]) in 14
replace high = `b'[1,1] + 1.54*sqrt(`V'[1,1]) in 14
outreg2 using ".\Tables\app_table_J_ecav_anti.tex", append ///
keep(un_military_base2 $controls) ///
ctitle("Model J4") label dec(3) alpha(0.001, 0.01, 0.05, 0.1) symbol(***,**, *, +) ///
noni nor2 noobs addtext(R-squared, `Rsq', N, `N', FE for admin. units, "YES", FE for months, "NO" ) 


xtreg ecav_anti_gov_du un_military_base2 $controls ecav_trend2, fe vce(cl adminUnitElection_id)
local Rsq: display %9.3f e(r2)
local N: display %9.0f e(N)
local N_g: display %9.0f e(N_g)
tempname b
matrix `b' = e(b)
replace estimate = `b'[1,1] in 15
tempname V
matrix `V' = e(V)
replace low =  `b'[1,1] - 1.54*sqrt(`V'[1,1]) in 15
replace high = `b'[1,1] + 1.54*sqrt(`V'[1,1]) in 15
outreg2 using ".\Tables\app_table_J_ecav_anti.tex", append ///
keep(un_military_base2 $controls) ///
ctitle("Model J5") label dec(3) alpha(0.001, 0.01, 0.05, 0.1) symbol(***,**, *, +) ///
noni nor2 noobs addtext(R-squared, `Rsq', N, `N', FE for admin. units, "YES", FE for months, "NO" ) 



** Pro government violence (ECAV)

xtreg ecav_pro_gov_du un_military_base2 , fe vce(cl adminUnitElection_id)
local Rsq: display %9.3f e(r2)
local N: display %9.0f e(N)
local N_g: display %9.0f e(N_g)
tempname b
matrix `b' = e(b)
replace estimate = `b'[1,1] in 16
tempname V
matrix `V' = e(V)
replace low =  `b'[1,1] - 1.54*sqrt(`V'[1,1]) in 16
replace high = `b'[1,1] + 1.54*sqrt(`V'[1,1]) in 16
outreg2 using ".\Tables\app_table_J_ecav_pro.tex", replace ///
keep(un_military_base2) ///
ctitle("Model J6") label dec(3) alpha(0.001, 0.01, 0.05, 0.1) symbol(***,**, *, +) ///
noni nor2 noobs addtext(R-squared, `Rsq', N, `N', FE for admin. units, "YES", FE for months, "NO" ) 

xtreg ecav_pro_gov_du un_military_base2 $controls, fe vce(cl adminUnitElection_id)
local Rsq: display %9.3f e(r2)
local N: display %9.0f e(N)
local N_g: display %9.0f e(N_g)
tempname b
matrix `b' = e(b)
replace estimate = `b'[1,1] in 17
tempname V
matrix `V' = e(V)
replace low =  `b'[1,1] - 1.54*sqrt(`V'[1,1]) in 17
replace high = `b'[1,1] + 1.54*sqrt(`V'[1,1]) in 17
outreg2 using ".\Tables\app_table_J_ecav_pro.tex", append ///
keep(un_military_base2 $controls) ///
ctitle("Model J7") label dec(3) alpha(0.001, 0.01, 0.05, 0.1) symbol(***,**, *, +) ///
noni nor2 noobs addtext(R-squared, `Rsq', N, `N', FE for admin. units, "YES", FE for months, "NO" ) 

xtreg ecav_pro_gov_du un_military_base2 $controls i.date, fe vce(cl adminUnitElection_id)
local Rsq: display %9.3f e(r2)
local N: display %9.0f e(N)
local N_g: display %9.0f e(N_g)
tempname b
matrix `b' = e(b)
replace estimate = `b'[1,1] in 18
tempname V
matrix `V' = e(V)
replace low =  `b'[1,1] - 1.54*sqrt(`V'[1,1]) in 18
replace high = `b'[1,1] + 1.54*sqrt(`V'[1,1]) in 18
outreg2 using ".\Tables\app_table_J_ecav_pro.tex", append ///
keep(un_military_base2 $controls) ///
ctitle("Model J8") label dec(3) alpha(0.001, 0.01, 0.05, 0.1) symbol(***,**, *, +) ///
noni nor2 noobs addtext(R-squared, `Rsq', N, `N', FE for admin. units, "YES", FE for months, "YES" ) 

xtreg ecav_pro_gov_du un_military_base2 $controls ecav_trend1, fe vce(cl adminUnitElection_id)
local Rsq: display %9.3f e(r2)
local N: display %9.0f e(N)
local N_g: display %9.0f e(N_g)
tempname b
matrix `b' = e(b)
replace estimate = `b'[1,1] in 19
tempname V
matrix `V' = e(V)
replace low =  `b'[1,1] - 1.54*sqrt(`V'[1,1]) in 19
replace high = `b'[1,1] + 1.54*sqrt(`V'[1,1]) in 19
outreg2 using ".\Tables\app_table_J_ecav_pro.tex", append ///
keep(un_military_base2 $controls) ///
ctitle("Model J9") label dec(3) alpha(0.001, 0.01, 0.05, 0.1) symbol(***,**, *, +) ///
noni nor2 noobs addtext(R-squared, `Rsq', N, `N', FE for admin. units, "YES", FE for months, "NO" ) 

xtreg ecav_pro_gov_du un_military_base2 $controls ecav_trend2, fe vce(cl adminUnitElection_id)
local Rsq: display %9.3f e(r2)
local N: display %9.0f e(N)
local N_g: display %9.0f e(N_g)
tempname b
matrix `b' = e(b)
replace estimate = `b'[1,1] in 20
tempname V
matrix `V' = e(V)
replace low =  `b'[1,1] - 1.54*sqrt(`V'[1,1]) in 20
replace high = `b'[1,1] + 1.54*sqrt(`V'[1,1]) in 20
outreg2 using ".\Tables\app_table_J_ecav_pro.tex", append ///
keep(un_military_base2 $controls) ///
ctitle("Model J10") label dec(3) alpha(0.001, 0.01, 0.05, 0.1) symbol(***,**, *, +) ///
noni nor2 noobs addtext(R-squared, `Rsq', N, `N', FE for admin. units, "YES", FE for months, "NO" ) 








** Test requested by R3: Multinomial models of pro- and anti-government violence
xtset adminUnitElection_id

capture drop veco_anti_pro_viol
gen veco_anti_pro_viol = 0
replace veco_anti_pro_viol = 1 if veco_anti_gov_viol2 >0 & veco_anti_gov_viol2!=. // anti-gov
replace veco_anti_pro_viol = 2 if veco_pro_gov_viol2  >0 & veco_pro_gov_viol2!=. & veco_anti_pro_viol!=1 // pro-gov
replace veco_anti_pro_viol = 3 if veco_pro_gov_viol2  >0 & veco_anti_gov_viol2>0 & veco_pro_gov_viol2!=. & veco_anti_gov_viol2!=. // anti-gov
tab veco_anti_pro_viol 

capture drop ecav_anti_pro_viol
gen ecav_anti_pro_viol = 0
replace ecav_anti_pro_viol = 1 if ecav_anti_gov >0 & ecav_anti_gov !=. // anti-gov
replace ecav_anti_pro_viol = 2 if ecav_pro_gov  >0 & ecav_pro_gov  !=. & ecav_anti_pro!=1 // pro-gov
replace ecav_anti_pro_viol = 3 if ecav_pro_gov  >0 & ecav_anti_gov >0  & ecav_pro_gov !=. & ecav_anti_gov!=. // anti-gov
tab ecav_anti_pro_viol


* Random effects multinomial models (NOT CONVERGED)

///gsem (1.veco_anti_pro_viol <- un_military_base_du $controls RI1[adminUnitElection_id]) ///
   ///  (2.veco_anti_pro_viol <- un_military_base_du $controls RI2[adminUnitElection_id]), mlogit startvalues(fixedonly) tol(0.1) ltol(0.1)

///gsem (1.ecav_anti_pro_viol <- un_military_base_du $controls_cem_std adminArea un_military_base_du2Lag1 RI1[adminUnitElection_id]) ///
   ///  (2.ecav_anti_pro_viol <- un_military_base_du $controls_cem_std adminArea un_military_base_du2Lag1 RI2[adminUnitElection_id]), mlogit startvalues(fixedonly)

	 
* Fixed effects multinomial models (NOT CONVERGED)

/// femlogit veco_anti_pro_viol un_military_base_du $controls, baseoutcome(0) robust

/// femlogit ecav_anti_pro_viol un_military_base_du $controls, baseoutcome(0) robust


******* Matching and then mlogit (without FE or RE)

** Make CEM weights

set seed 1234
quietly mlogit veco_anti_pro_viol  un_military_base_du $controls_cem
capture drop sample
gen sample = 1 if e(sample)==1

global controls_cem_std "viol_stateBased_preDep_std viol_stateBased_preDep_trend_std viol_OneSided_preDep_std viol_OneSided_preDep_trend_std pop_gpw_sum_std roadDensity_std imr_mean_std bdist1_std capdist_std un_military_base_du_spL_std" 

capture drop imb_temp*
capture drop cem_temp*
imb $controls_cem_std if sample==1, tr(un_military_base_du)
matrix imb = r(imbal)
matrix list imb
matrix imb2 = imb[1..10,2]
svmat double imb2, names(imb_temp)

cem $controls_cem_std if sample==1, tr(un_military_base_du) autocuts(fd)
matrix imb = r(imbal)
matrix imb2 = imb[1..10,2]
svmat double imb2, names(cem_temp)

tab veco_viol_du if cem_weights!=0 

** Make propensity scores 
drop if sample!=1
capture drop match*
capture drop ob
capture drop ps_temp1
teffects psmatch (veco_viol_du)(un_military_base_du $controls_cem_std ) , vce(robust) atet nn(1) gen(match)
tebalance summarize
matrix imb = r(table)
matrix list imb
matrix imb2 = imb[1..10,2]
svmat double imb2, names(ps_temp)

capture drop ob
gen ob = _n //store the observation numbers for future use
save "temp_fulldata", replace // save the complete data set

** Estimation of post-matching models (NOT CONVERGED)
preserve 
keep if un_military_base_du==1 // keep just the treated group
keep match1 // keep just the match1 variable (the observation numbers of their matches)
bysort match1: gen _weight = _N // count how many times each control observation is a match
by match1: keep if _n==1 // keep just one row per control observation
rename match1 ob //rename for merging purposes

merge 1:m ob using "temp_fulldata" // merge back into the full data
replace _weight = 1 if un_military_base_du==1 // set weight to 1 for treated observations
drop _merge

** Post-propensity score matching
/*mlogit veco_anti_pro_viol un_military_base_du $controls_cem_std adminArea un_military_base_du2Lag1 [fweight=_weight], vce(robust)
gen sam = e(sample)
estat ic
mat es_ic = r(S)
local AIC: display %4.1f es_ic[1,5]
local BIC: display %4.1f es_ic[1,6]
local LL: display %4.1f es_ic[1,3]

outreg2 using ".\Tables\app_table_K_multinom.tex", replace ///
keep(un_military_base_du $controls_cem_std un_military_base_du_spL adminArea) ///
ctitle("Model K1") label dec(3) alpha(0.001, 0.01, 0.05, 0.1) symbol(***,**, *, +) ///
noni nor2 addtext(Log-Likelihood, `LL' ,BIC, `BIC', AIC, `AIC') 


mlogit veco_anti_pro_viol un_military_base2 $controls_cem_std  adminArea un_military_base_du2Lag1  [fweight=_weight], vce(robust)
estat ic
mat es_ic = r(S)
local AIC: display %4.1f es_ic[1,5]
local BIC: display %4.1f es_ic[1,6]
local LL: display %4.1f es_ic[1,3]

outreg2 using ".\Tables\app_table_K_multinom.tex", append ///
keep(un_military_base2 $controls_cem_std un_military_base_du_spL adminArea) ///
ctitle("Model K2") label dec(3) alpha(0.001, 0.01, 0.05, 0.1) symbol(***,**, *, +) ///
noni nor2 addtext(Log-Likelihood, `LL' ,BIC, `BIC', AIC, `AIC') 

restore
*/

** Post CEM-Matching
/*mlogit veco_anti_pro_viol un_military_base_du $controls_cem_std adminArea un_military_base_du2Lag1  [iweight=cem_weights], vce(cl adminUnitElection_id)
estat ic
mat es_ic = r(S)
local AIC: display %4.1f es_ic[1,5]
local BIC: display %4.1f es_ic[1,6]
local LL: display %4.1f es_ic[1,3]

outreg2 using ".\Tables\app_table_H_multinom.tex", append ///
keep(un_military_base_du $controls_cem_std un_military_base_du_spL adminArea) ///
ctitle("Model 8") label dec(3) alpha(0.001, 0.01, 0.05, 0.1) symbol(***,**, *, +) ///
noni nor2 addtext( Log-Likelihood, `LL' ,BIC, `BIC', AIC, `AIC') 
*/

* Does not converge
/*mlogit veco_anti_pro_viol un_military_base2 $controls_cem_std adminArea un_military_base_du2Lag1 [iweight=cem_weights], vce(cl adminUnitElection_id)
estat ic
mat es_ic = r(S)
local AIC: display %4.1f es_ic[1,5]
local BIC: display %4.1f es_ic[1,6]
local LL: display %4.1f es_ic[1,3]

outreg2 using ".\Tables\app_table_H_multinom.tex", append ///
keep(un_military_base2 $controls_cem_std un_military_base_du_spL adminArea) ///
ctitle("Model 9") label dec(3) alpha(0.001, 0.01, 0.05, 0.1) symbol(***,**, *, +) ///
noni nor2 addtext(Log-Likelihood, `LL' ,BIC, `BIC', AIC, `AIC') 
*/


