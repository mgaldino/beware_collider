
********************************************************************************
****************************** FIG 1 (a) ***************************************
********************************************************************************
use database, clear

bys s_id: egen n_female=sum(female==1)
bys s_id: egen n_male=sum(female==0)
gen total=n_female+n_male

replace n_female=n_female/total

graph twoway (scatter n_female s_id, legend(off) ///
schem(s1color) lc(gs3) mc(gs3) connect(l) msymbol(T) xlabel(1 "1999" 2 ///
"2000" 3 "2001" 4 "2002" 5 "2003" 6 "2004" 7 "2005" 8 "2006" 9 "2007" 10 ///
"2008" 11 "2009" 12 "2010" 13 "2011" 14 "2012" 15 "2013" 16 "2014" 17 ///
"2015" 18 "2016" 19 "2017", angle(45))  ylabel(.15(.05).4, ///
format(%9.0g) labsize(small) labgap(1)) yline(.15(.05).4, ///
lc(gs10) lw(vvthin)) ytitle(Female MPs, height(7)) xtitle("", ///
height(7)) xline(8, lc(gs3)) xline(11, lp(dash) ///
lc(gs3)) xline(1 4 7 13 17, lp(dash) lc(gs10)) sort)

********************************************************************************
****************************** FIG 1 (b) ***************************************
********************************************************************************
use database, clear

preserve

	drop if party=="PAN"

	gen party_n=0

	replace party_n=1 if party=="BE"
	replace party_n=2 if party=="PEV"
	replace party_n=3 if party=="PCP"
	replace party_n=4 if party=="PS"
	replace party_n=5 if party=="PSD"
	replace party_n=6 if party=="CDS-PP"

	gen female_a=(female==1) if legislature>10
	gen female_b=(female==1) if legislature<11

	graph bar female_b female_a, ///
	over(party_n, relabel(1 "BE" 2 "PEV" 3 "PCP" 4 "PS" 5 "PSD" 6 "CDS")) ///
	schem(s1color)  bar(1, col(gs4)) bar(2, fc(gs16) lc(gs3)) ///
	legend(label(1 "Pre-quota") label(2 "Post-quota") region(lstyle(none))) ///
	ylabel(0(.15).6, format(%9.0g) labsize(small) labgap(1)) yline(0(.15).6, ///	
	lc(gs14) lw(vvthin))  ///
	ytitle(Female MPs, height(7))
	
restore

********************************************************************************
****************************** FIG 2 (a) ***************************************
********************************************************************************
use database, clear

xtset mpid s_id
gen zero=0
label var zero "2006"

xtivreg2 count female_session_1 female_session_2 female_session_3 ///
female_session_4 female_session_5 female_session_6 female_session_7 zero ///
female_session_9 female_session_10 female_session_11 ///
female_session_12 female_session_13 female_session_14 female_session_15 ///
female_session_16 female_session_17 female_session_18 female_session_19 ///
session* seniority senioritysqr leaders commission, fe r cluster(mpid)

coefplot, omitted drop(session* seniority senioritysqr leaders commission) ///
schem(s1color) lc(gs3) mc(gs3) xline(11) xline(8, ///
lp(dash)) vertical ylabel(-20(10)20) yline(-20(10)20, lc(gs14) lw(vthin)) ///
xlabel(1 "1999" 2 "2000" 3 "2001" 4 "2002" 5 "2003" 6 "2004" 7 "2005" 8 ///
"2006" 9 "2007" 10 "2008" 11 "2009" 12 "2010" 13 "2011" 14 "2012" 15 "2013" ///
16 "2014" 17 "2015" 18 "2016" 19 "2017", angle(45)) yline(0, lw(medium)) ///
ci(95) ciopts(recast(rline) lp(dash) lc(gs3)) recast(connected) /// 
lpattern(line) ytitle(Speeches, height(7)) xtitle(, height(7))

********************************************************************************
****************************** FIG 2 (b) ***************************************
********************************************************************************
use database_prime, clear

xtset mpid s_id
gen zero=0
label var zero "2006"

xtivreg2 count female_session_1 female_session_2 female_session_3 ///
female_session_4 female_session_5 female_session_6 female_session_7 zero ///
female_session_9 female_session_10 female_session_11 ///
female_session_12 female_session_13 female_session_14 female_session_15 ///
female_session_16 female_session_17 female_session_18 female_session_19 ///
session* seniority senioritysqr leaders commission, fe r cluster(mpid)

coefplot, omitted drop(session* seniority senioritysqr leaders commission) ///
schem(s1color) lc(gs3) mc(gs3) xline(11) xline(8, ///
lp(dash)) vertical ylabel(-20(10)20) yline(-20(10)20, lc(gs14) lw(vthin)) ///
xlabel(1 "1999" 2 "2000" 3 "2001" 4 "2002" 5 "2003" 6 "2004" 7 "2005" 8 ///
"2006" 9 "2007" 10 "2008" 11 "2009" 12 "2010" 13 "2011" 14 "2012" 15 "2013" ///
16 "2014" 17 "2015" 18 "2016" 19 "2017", angle(45)) yline(0, lw(medium)) ///
ci(95) ciopts(recast(rline) lp(dash) lc(gs3)) recast(connected) ///
lpattern(line) ytitle(Speeches, height(7)) xtitle(, height(7))

********************************************************************************
****************************** FIG 3 (a) ***************************************
********************************************************************************
use database, clear

keep if female==1

* quota women

gen new=f_after
replace new=0 if before_and_after==1

* quota women with different experiences

gen noexp=(seniority==1)
gen new_noexp=new*noexp

gen noexp2=(seniority==2)
gen new_noexp2=new*noexp2

gen noexp3=(seniority==3)
gen new_noexp3=new*noexp3

gen nonquota=(new==0)
gen ref_nonquota=nonquota*timedummy

xtset mpid s_id

ivreg2 count ref_nonquota new_noexp new_noexp2 new_noexp3 session* partido* ///
commission seniority senioritysqr leaders, r partial(session* partido* ///
commission seniority senioritysqr leaders) cluster(mpid)
est store m5

coefplot m5, keep(ref_nonquota new_noexp new_noexp2 new_noexp3) msy(S) ///
mcol(gs3) vertical ylabel(-5(5)20, format(%9.0g) labsize(small) ///
labgap(1)) yline(-5(5)20, lc(gs14) lw(vthin)) legend(off) ytitle(Speeches, ///
height(7)) xlabel(, format(%9.0g) labsize(small) labgap(1)) schem(s1color) ///
ciopts(recast(rcap) lc(gs3)) yline(0) ///
coeflabels(ref_nonquota="Pre-quota x Reform" new_noexp="Quota x Sen=1" ///
new_noexp2="Quota x Sen=2" new_noexp3="Quota x Sen=3")

********************************************************************************
****************************** FIG 3 (b) ***************************************
********************************************************************************
use database, clear

keep if female==0

* quota women

gen new=m_after
replace new=0 if before_and_after==1
replace new=0 if seniority>3

* quota women with different experiences

gen noexp=(seniority==1)
gen new_noexp=new*noexp

gen noexp2=(seniority==2)
gen new_noexp2=new*noexp2

gen noexp3=(seniority==3)
gen new_noexp3=new*noexp3

gen nonquota=(new==0)
gen ref_nonquota=nonquota*timedummy

gen nonquota_noexp=nonquota*noexp

xtset mpid s_id

ivreg2 count ref_nonquota new_noexp new_noexp2 new_noexp3 session* partido* ///
commission seniority senioritysqr leaders, r partial(session* partido* ///
commission seniority senioritysqr leaders) cluster(mpid)
est store m5

coefplot m5, keep(ref_nonquota new_noexp new_noexp2 new_noexp3) msy(S) ///
mcol(gs3) vertical ylabel(-20(5)5, format(%9.0g) labsize(small) ///
labgap(1)) yline(-20(5)5, lc(gs14) lw(vthin)) legend(off) ytitle(Speeches, ///
height(7)) xlabel(, format(%9.0g) labsize(small) labgap(1)) schem(s1color) ///
ciopts(recast(rcap) lc(gs3)) yline(0) ///
coeflabels(ref_nonquota="Pre-quota x Reform" new_noexp="Quota x Sen=1" ///
new_noexp2="Quota x Sen=2" new_noexp3="Quota x Sen=3")

********************************************************************************
********************************************************************************
