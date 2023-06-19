
********************************************************************************
******************************* TABLE 1 ****************************************
********************************************************************************
use database, clear

xtset mpid s_id
xtivreg2 count treatment session* commission leaders seniority ///
senioritysqr, fe r partial(session*) cluster(mpid)

preserve

	use database_general, clear

	xtset mpid s_id
	xtivreg2 count treatment session* commission leaders seniority ///
	senioritysqr, fe r partial(session*) cluster(mpid)

restore

preserve

	use database_prime, clear

	xtset mpid s_id
	xtivreg2 count treatment session* commission leaders seniority ///
	senioritysqr, fe r partial(session*) cluster(mpid)

restore

********************************************************************************
******************************* TABLE 2 ****************************************
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

ivreg2 count ref_nonquota new session* partido* commission ///
seniority senioritysqr leaders, r partial(session* partido*) cluster(mpid)

ivreg2 count ref_nonquota new new_noexp session* partido* commission ///
seniority senioritysqr leaders, r partial(session* partido*) cluster(mpid)

ivreg2 count ref_nonquota new new_noexp new_noexp2 session* partido* ///
commission seniority senioritysqr leaders, r partial(session* partido*) ///
cluster(mpid)

ivreg2 count ref_nonquota nonquota_noexp new new_noexp new_noexp2 session* ///
partido* commission seniority senioritysqr leaders, r partial(session* ///
partido*) cluster(mpid)

********************************************************************************
********************************************************************************
