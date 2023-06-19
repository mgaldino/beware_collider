
********************************************************************************
****************************** FIG A1 (a) **************************************
********************************************************************************
use database, clear

xtset mpid s_id
gen zero=0
label var zero "2006"

xtivreg2 sum_tokens female_session_1 female_session_2 female_session_3 ///
female_session_4 female_session_5 female_session_6 female_session_7 zero ///
female_session_9 female_session_10 female_session_11 ///
female_session_12 female_session_13 female_session_14 female_session_15 ///
female_session_16 female_session_17 female_session_18 female_session_19 ///
session* seniority senioritysqr leaders commission, fe r cluster(mpid)

coefplot, omitted drop(session* seniority senioritysqr leaders commission) ///
schem(s1color) lc(gs3) mc(gs3) xline(11) xline(8, ///
lp(dash)) vertical ylabel(-15(5)10) yline(-15(5)10, lc(gs14) lw(vthin)) ///
xlabel(1 "1999" 2 "2000" 3 "2001" 4 "2002" 5 "2003" 6 "2004" 7 "2005" 8 ///
"2006" 9 "2007" 10 "2008" 11 "2009" 12 "2010" 13 "2011" 14 "2012" 15 "2013" ///
16 "2014" 17 "2015" 18 "2016" 19 "2017", angle(45)) yline(0, lw(medium)) ///
ci(95) ciopts(recast(rline) lp(dash) lc(gs3)) recast(connected) ///
lpattern(line) ytitle(Words, height(7)) xtitle(, height(7))

********************************************************************************
****************************** FIG A1 (b) **************************************
********************************************************************************
use database_prime, clear

xtset mpid s_id
gen zero=0
label var zero "2006"

xtivreg2 sum_tokens female_session_1 female_session_2 female_session_3 ///
female_session_4 female_session_5 female_session_6 female_session_7 zero ///
female_session_9 female_session_10 female_session_11 ///
female_session_12 female_session_13 female_session_14 female_session_15 ///
female_session_16 female_session_17 female_session_18 female_session_19 ///
session* seniority senioritysqr leaders commission, fe r cluster(mpid)

coefplot, omitted drop(session* seniority senioritysqr leaders commission) ///
schem(s1color) lc(gs3) mc(gs3) xline(11) xline(8, ///
lp(dash)) vertical ylabel(-15(5)10) yline(-15(5)10, lc(gs14) lw(vthin)) ///
xlabel(1 "1999" 2 "2000" 3 "2001" 4 "2002" 5 "2003" 6 "2004" 7 "2005" 8 ///
"2006" 9 "2007" 10 "2008" 11 "2009" 12 "2010" 13 "2011" 14 "2012" 15 "2013" ///
16 "2014" 17 "2015" 18 "2016" 19 "2017", angle(45)) yline(0, lw(medium)) ///
ci(95) ciopts(recast(rline) lp(dash) lc(gs3)) recast(connected) ///
lpattern(line) ytitle(Words, height(7)) xtitle(, height(7))

********************************************************************************
****************************** FIG A2 (a) **************************************
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

gen nonquota_noexp=nonquota*noexp

gen experienced=(seniority!=1)
gen ref_experienced=timedummy*experienced

xtset mpid s_id

ivreg2 sum_tokens ref_nonquota new_noexp new_noexp2 new_noexp3 session* ///
partido* commission seniority senioritysqr leaders, r partial(session* ///
partido* commission seniority senioritysqr leaders) cluster(mpid)
est store m5

coefplot m5, keep(ref_nonquota new_noexp new_noexp2 new_noexp3) msy(S) ///
mcol(gs3) vertical ylabel(-5(5)15, format(%9.0g) labsize(small) ///
labgap(1)) yline(-5(5)15, lc(gs14) lw(vthin)) legend(off) ytitle(Words, ///
height(7)) xlabel(, format(%9.0g) labsize(small) labgap(1)) schem(s1color) ///
ciopts(recast(rcap) lc(gs3)) yline(0) ///
coeflabels(ref_nonquota="Pre-quota x Reform" new_noexp="Quota x 1st Leg" ///
new_noexp2="Quota x 2nd Leg" new_noexp3="Quota x 3rd Leg")

********************************************************************************
****************************** FIG A2 (b) **************************************
********************************************************************************
use database, clear

keep if female==0

* quota men

gen new=m_after
replace new=0 if before_and_after==1

* quota men with different experiences

gen noexp=(seniority==1)
gen new_noexp=new*noexp

gen noexp2=(seniority==2)
gen new_noexp2=new*noexp2

gen noexp3=(seniority==3)
gen new_noexp3=new*noexp3

gen nonquota=(new==0)
gen ref_nonquota=nonquota*timedummy

gen nonquota_noexp=nonquota*noexp

gen experienced=(seniority!=1)
gen ref_experienced=timedummy*experienced

xtset mpid s_id

drop session10 session19

ivreg2 sum_tokens ref_nonquota new_noexp new_noexp2 new_noexp3 session* ///
partido* commission seniority senioritysqr leaders, r partial(session* ///
partido* commission seniority senioritysqr leaders) cluster(mpid)
est store m5

coefplot m5, keep(ref_nonquota new_noexp new_noexp2 new_noexp3) msy(S) ///
mcol(gs3) vertical ylabel(-10(2.5)2.5, format(%9.0g) labsize(small) ///
labgap(1)) yline(-10(2.5)2.5, lc(gs14) lw(vthin)) legend(off) ytitle(Words, ///
height(7)) xlabel(, format(%9.0g) labsize(small) labgap(1)) schem(s1color) ///
ciopts(recast(rcap) lc(gs3)) yline(0) ///
coeflabels(ref_nonquota="Pre-quota x Reform" new_noexp="Quota x 1st Leg" ///
new_noexp2="Quota x 2nd Leg" new_noexp3="Quota x 3rd Leg")

********************************************************************************
****************************** FIG C1 (a) **************************************
********************************************************************************
use database, clear

bysort female legislature: egen mean_speeches=mean(count)

graph twoway (scatter mean_speeches legislature if female==1, legend(off) ///
schem(s1color) lc(gs3) mc(gs3) connect(l) msymbol(T) ///
ytitle(Speeches, height(7)) xtitle("", height(7)) ///
xlabel(8 "2002" 9 "2005" 10 "2009" 11 "2011" 12 "2013" 13 "2015") ///
ylabel(12(2)20, format(%9.0g) labsize(small) labgap(1)) yline(12(1)20, ///
lc(gs10) lw(vvthin)) xline(10) sort) ///
(scatter mean_speeches legislature if female==0, lc(gs3) mc(gs3) connect(l) ///
msymbol(S) sort)

********************************************************************************
****************************** FIG C1 (b) **************************************
********************************************************************************
use database, clear

bys female legislature: egen mean_tokens=mean(sum_tokens)

graph twoway (scatter mean_tokens legislature if female==1, legend(off) ///
schem(s1color) lc(gs3) mc(gs3) connect(l) msymbol(T) ///
ytitle(Words, height(7)) xtitle("", height(7)) ///
xlabel(8 "2002" 9 "2005" 10 "2009" 11 "2011" 12 "2013" 13 "2015") ///
ylabel(6(2)12, format(%9.0g) labsize(small) labgap(1)) yline(6(1)12, ///
lc(gs10) lw(vvthin)) xline(10) sort) ///
(scatter mean_tokens legislature if female==0, lc(gs3) mc(gs3) connect(l) ///
msymbol(S) sort)

********************************************************************************
****************************** FIG C2 (a) **************************************
********************************************************************************
use database_general, clear

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
****************************** FIG C2 (b) **************************************
********************************************************************************
use database_general, clear

xtset mpid s_id
gen zero=0
label var zero "2006"

xtivreg2 sum_tokens female_session_1 female_session_2 female_session_3 ///
female_session_4 female_session_5 female_session_6 female_session_7 zero ///
female_session_9 female_session_10 female_session_11 ///
female_session_12 female_session_13 female_session_14 female_session_15 ///
female_session_16 female_session_17 female_session_18 female_session_19 ///
session* seniority senioritysqr leaders commission, fe r cluster(mpid)

coefplot, omitted drop(session* seniority senioritysqr leaders commission) ///
schem(s1color) lc(gs3) mc(gs3) xline(11) xline(8, ///
lp(dash)) vertical ylabel(-10(5)15) yline(-10(5)15, lc(gs14) lw(vthin)) ///
xlabel(1 "1999" 2 "2000" 3 "2001" 4 "2002" 5 "2003" 6 "2004" 7 "2005" 8 ///
"2006" 9 "2007" 10 "2008" 11 "2009" 12 "2010" 13 "2011" 14 "2012" 15 "2013" ///
16 "2014" 17 "2015" 18 "2016" 19 "2017", angle(45)) yline(0, lw(medium)) ///
ci(95) ciopts(recast(rline) lp(dash) lc(gs3)) recast(connected) ///
lpattern(line) ytitle(Words, height(7)) xtitle(, height(7))

********************************************************************************
********************************************************************************
