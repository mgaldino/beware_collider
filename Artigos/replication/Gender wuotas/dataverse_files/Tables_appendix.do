
********************************************************************************
******************************* TABLE A1 ***************************************
********************************************************************************
use database, clear

xtset mpid s_id
xtivreg2 sum_tokens treatment session* commission leaders seniority ///
senioritysqr, fe r partial(session*) cluster(mpid)

preserve

	use database_general, clear

	xtset mpid s_id
	xtivreg2 sum_tokens treatment session* commission leaders seniority ///
	senioritysqr, fe r partial(session*) cluster(mpid)

restore

preserve

	use database_prime, clear

	xtset mpid s_id
	xtivreg2 sum_tokens treatment session* commission leaders seniority ///
	senioritysqr, fe r partial(session*) cluster(mpid)

restore

********************************************************************************
******************************* TABLE A2 ***************************************
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

ivreg2 sum_tokens ref_nonquota new session* partido* commission ///
seniority senioritysqr leaders, r partial(session* partido*) cluster(mpid)

ivreg2 sum_tokens ref_nonquota new new_noexp session* partido* commission ///
seniority senioritysqr leaders, r partial(session* partido*) cluster(mpid)

ivreg2 sum_tokens ref_nonquota new new_noexp new_noexp2 session* partido* ///
commission seniority senioritysqr leaders, r partial(session* partido*) ///
cluster(mpid)

ivreg2 sum_tokens ref_nonquota nonquota_noexp new new_noexp new_noexp2 ///
session* partido* commission seniority senioritysqr leaders, r ///
partial(session* partido*) cluster(mpid)

********************************************************************************
******************************* TABLE B1 ***************************************
********************************************************************************
use database, clear

gen ano =.
replace ano = 1999 if legislature == 8
replace ano = 2002 if legislature == 9
replace ano = 2005 if legislature == 10
replace ano = 2009 if legislature == 11
replace ano = 2011 if legislature == 12
replace ano = 2015 if legislature == 13

replace party = "CDS" if party == "CDS-PP"
replace party = "PCP" if party == "PEV"

gen name = ustrlower(ustrregexra(ustrnormalize( shortname, "nfd" ) , "\p{Mark}", ""))
gen fname = regexs(0) if regexm(name, "([a-zA-Z]+)")
replace fname = "angela" if fname == "ngela"
replace fname = "vania" if fname == "v"
replace fname = "ines" if fname == "in"
gen lname = regexs(0) if regexm(name, "([a-zA-Z]*$)")
replace lname = "simoes" if lname == "es"
replace lname = "proa" if lname == "a"
replace lname = "rego" if lname == "go"
replace lname = "epifanio" if lname == "nio"
replace lname = "prudencio" if lname == "ncio"


egen id = concat(party ano fname lname)

replace id = "BE2009pedrofilipesoares" if id == "BE2009pedrosoares" & name == "pedro filipe soares"
replace id = "BE2011pedrofilipesoares" if id == "BE2011pedrosoares" & name == "pedro filipe soares"
replace id = "BE2015pedrofilipesoares" if id == "BE2015pedrosoares" & name == "pedro filipe soares"

replace id = "PS2002mariaromao" if id == "PS2002mariasantos" & name == "maria santos" & constituency == "Lisboa"

replace id = "PS2005mariaalmeidasantos" if id == "PS2005mariasantos" & name == "maria antonia de almeida santos" 
replace id = "PS2009mariaalmeidasantos" if id == "PS2009mariasantos" & name == "maria antonia de almeida santos" 
replace id = "PS2011mariaalmeidasantos" if id == "PS2011mariasantos" & name == "maria antonia de almeida santos" 
replace id = "PS2015mariaalmeidasantos" if id == "PS2015mariasantos" & name == "maria antonia de almeida santos" 

replace id = "PSD1999fernandosantospereira" if id == "PSD1999fernandopereira" & name == "fernando santos pereira" 
replace id = "PSD2002fernandosantospereira" if id == "PSD2002fernandopereira" & name == "fernando santos pereira" 

replace id = "PSD2002carlosalbertogoncalves" if id == "PSD2002carlosgoncalves" & name == "carlos alberto goncalves" 
replace id = "PSD2005carlosalbertogoncalves" if id == "PSD2005carlosgoncalves" & name == "carlos alberto goncalves" 
replace id = "PSD2009carlosalbertogoncalves" if id == "PSD2009carlosgoncalves" & name == "carlos alberto goncalves" 
replace id = "PSD2011carlosalbertogoncalves" if id == "PSD2011carlosgoncalves" & name == "carlos alberto goncalves" 
replace id = "PSD2015carlosalbertogoncalves" if id == "PSD2015carlosgoncalves" & name == "carlos alberto goncalves"

merge m:1 id using background

sort mpid l_id

foreach v of varlist datanascimento nívelinstrução númerodecargospolíticosqueo{
	by mpid: replace `v' = `v'[_n-1] if `v'==.
}

gsort mpid -l_id

foreach v of varlist datanascimento nívelinstrução númerodecargospolíticosqueo{
	by mpid: replace `v' = `v'[_n-1] if `v'==.
}

drop if _merge==2
drop _merge

rename idadedosdeputados age
replace age = ano - datanascimento

gen education = .
replace education = 1 if inrange(nívelinstrução,.,7)
replace education = 2 if inrange(nívelinstrução,8,10)
replace education = 3 if inrange(nívelinstrução,11,.)

gen experience = (númerodecargospolíticosqueo!=0) if númerodecargospolíticosqueo<.

sort mpid (l_id)
bys mpid (l_id): replace age = age[1]

estpost summarize count sum_tokens female seniority commission leaders ///
experience education age

********************************************************************************
******************************* TABLE B2 ***************************************
********************************************************************************

* all legislators, before

reg count female if legislature<11, r 
reg sum_tokens female if legislature<11, r
reg seniority female if legislature<11, r
reg commission female if legislature<11, r 
reg leaders female if legislature<11, r 
reg experience female if legislature<11, r
reg education female if legislature<11, r
reg age female if legislature<11, r

* all legislators, after

reg count female if legislature>10, r 
reg sum_tokens female if legislature>10, r 
reg seniority female if legislature>10, r 
reg commission female if legislature>10, r 
reg leaders female if legislature>10, r 
reg experience female if legislature>10, r
reg education female if legislature>10, r
reg age female if legislature>10, r

* before and after legislators, before

reg count female if legislature<11 & before_and_after==1, r
reg sum_tokens female if legislature<11 & before_and_after==1, r
reg seniority female if legislature<11 & before_and_after==1, r
reg commission female if legislature<11 & before_and_after==1, r
reg leaders female if legislature<11 & before_and_after==1, r
reg experience female if legislature<11 & before_and_after==1, r
reg education female if legislature<11 & before_and_after==1, r
reg age female if legislature<11 & before_and_after==1, r

* before and after legislators, after

reg count female if legislature>10 & before_and_after==1, r
reg sum_tokens female if legislature>10 & before_and_after==1, r
reg seniority female if legislature>10 & before_and_after==1, r
reg commission female if legislature>10 & before_and_after==1, r
reg leaders female if legislature>10 & before_and_after==1, r
reg experience female if legislature>10 & before_and_after==1, r
reg education female if legislature>10 & before_and_after==1, r
reg age female if legislature>10 & before_and_after==1, r

********************************************************************************
******************************* TABLE B7 ***************************************
********************************************************************************

* displaced men, pre-reform

preserve

	replace m_before=0 if m_before_and_after==1
	replace m_after=0 if m_before_and_after==1

	drop if m_after==1

	keep if female==0
	keep if legislature<11

	reg count m_before, r 
	reg sum_tokens m_before, r 
	reg seniority m_before , r 
	reg commission m_before, r 
	reg leaders m_before , r
	reg experience m_before , r 
	reg education m_before , r
	reg age m_before, r 

restore

* post-quota women, post-reform

preserve

	replace f_before=0 if f_before_and_after==1
	replace f_after=0 if f_before_and_after==1

	drop if f_before==1

	keep if female==1
	keep if legislature>10

	reg count f_after, r 
	reg sum_tokens f_after, r 
	reg seniority f_after , r 
	reg commission f_after, r 
	reg leaders f_after , r 
	reg experience f_after , r 
	reg education f_after , r 
	reg age f_after, r 

restore

********************************************************************************
******************************* TABLE B4 ***************************************
********************************************************************************
use database, clear

xtset mpid s_id

gen quota_female=female*timedummy

reghdfe count quota_female female commission leaders seniority ///
senioritysqr, absorb(s_id party) cluster(mpid)

preserve

	use database_general, clear

	xtset mpid s_id
	gen quota_female=female*timedummy

	reghdfe count quota_female female commission leaders seniority ///
	senioritysqr, absorb(s_id party) cluster(mpid)

restore

preserve

	use database_prime, clear

	xtset mpid s_id
	gen quota_female=female*timedummy

	reghdfe count quota_female female commission leaders seniority ///
	senioritysqr, absorb(s_id party) cluster(mpid)

restore

********************************************************************************
******************************* TABLE B5 ***************************************
********************************************************************************
use database, clear

xtset mpid s_id

gen quota_female=female*timedummy

reghdfe sum_tokens quota_female female commission leaders seniority ///
senioritysqr, absorb(s_id party) cluster(mpid)

preserve

	use database_general, clear

	xtset mpid s_id
	gen quota_female=female*timedummy

	reghdfe sum_tokens quota_female female commission leaders seniority ///
	senioritysqr, absorb(s_id party) cluster(mpid)

restore

preserve

	use database_prime, clear

	xtset mpid s_id
	gen quota_female=female*timedummy

	reghdfe sum_tokens quota_female female commission leaders seniority ///
	senioritysqr, absorb(s_id party) cluster(mpid)

restore

********************************************************************************
******************************* TABLE B6 ***************************************
********************************************************************************
use database, clear

xtset mpid s_id

xtivreg2 sum_tokens female_session_11 female_session_12 female_session_13 ///
female_session_14 female_session_15 female_session_16 female_session_17 ///
female_session_18 female_session_19 session* commission /// 
leaders seniority senioritysqr, fe r partial(session* commission  ///
seniority leaders senioritysqr) cluster(mpid)

xtivreg2 count female_session_11 female_session_12 female_session_13 ///
female_session_14 female_session_15 female_session_16 female_session_17 ///
female_session_18 female_session_19 session* commission /// 
leaders seniority senioritysqr, fe r partial(session* commission  ///
seniority leaders senioritysqr) cluster(mpid)

preserve

	use database_prime, clear

	xtset mpid s_id

	xtivreg2 sum_tokens female_session_11 female_session_12 female_session_13 ///
	female_session_14 female_session_15 female_session_16 female_session_17 ///
	female_session_18 female_session_19 female session* commission /// 
	leaders seniority senioritysqr, fe r partial(session* commission  ///
	seniority leaders senioritysqr) cluster(mpid)

	xtivreg2 count female_session_11 female_session_12 female_session_13 ///
	female_session_14 female_session_15 female_session_16 female_session_17 ///
	female_session_18 female_session_19 female session* commission /// 
	leaders seniority senioritysqr, fe r partial(session* commission  ///
	seniority leaders senioritysqr) cluster(mpid)

restore
	
********************************************************************************
******************************* TABLE B8 ***************************************
********************************************************************************
use database, clear

keep if female==0

* quota men

gen new=m_after
replace new=0 if before_and_after==1
replace new=0 if seniority>3

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
******************************* TABLE B9 ***************************************
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

ivreg2 sum_tokens ref_nonquota new session* partido* commission ///
seniority senioritysqr leaders, r partial(session* partido*) cluster(mpid)

ivreg2 sum_tokens ref_nonquota new new_noexp session* partido* commission ///
seniority senioritysqr leaders, r partial(session* partido*) cluster(mpid)

ivreg2 sum_tokens ref_nonquota new new_noexp new_noexp2 session* partido* ///
commission seniority senioritysqr leaders, r partial(session* partido*) ///
cluster(mpid)

ivreg2 sum_tokens ref_nonquota nonquota_noexp new new_noexp new_noexp2 ///
session* partido* commission seniority senioritysqr leaders, r ///
partial(session* partido*) cluster(mpid)

********************************************************************************
******************************* TABLE B10 **************************************
********************************************************************************
use database, clear

preserve

	keep if seniority==1
	xtset mpid s_id

	ivreg2 count treatment female session* partido* commission ///
	leaders, r partial(session* partido*) cluster(mpid)

restore

preserve

	keep if seniority==2
	xtset mpid s_id

	ivreg2 count treatment female session* partido* commission leaders, r  ///
	partial(session* partido*) cluster(mpid)

restore

preserve

	keep if seniority==3
	xtset mpid s_id

	ivreg2 count treatment female session* partido* commission leaders,  ///
	r partial(session* partido*) cluster(mpid)

restore

preserve

	use database_prime, clear

	keep if seniority==1
	xtset mpid s_id

	ivreg2 count treatment female session* partido* commission ///
	leaders, r partial(session* partido*) cluster(mpid)

restore

preserve

	use database_prime, clear

	keep if seniority==2
	xtset mpid s_id

	ivreg2 count treatment female session* partido* commission leaders,  ///
	r partial(session* partido*) cluster(mpid)

restore

preserve

	use database_prime, clear

	keep if seniority==3
	xtset mpid s_id

	ivreg2 count treatment female session* partido* commission leaders,  ///
	r partial(session* partido*) cluster(mpid)

restore

********************************************************************************
********************************************************************************
