import delimited "N:\durable\projects\Perline\Eating disorders data\data_moba_mother_eatingdisorders.csv", encoding(ISO-8859-2) clear

destring barn_nr birth_yr, replace ignore("NA" 0)
destring aa1475 aa1476 aa1477 aa1478 aa1480 aa1482 aa1484 aa1486 aa1479 aa1481 aa1483 aa1485 aa1487 aa1488 nn285 nn286 nn287 nn288 nn289 nn290 nn291 nn292 nn293 nn367 nn294 nn295 nn296 nn297 nn298 nn299 nn300 nn301, replace ignore("NA")

tab1 aa1475 aa1476 aa1477 aa1478 aa1480 aa1482 aa1484 aa1486 aa1479 aa1481 aa1483 aa1485 aa1487 aa1488


*følte seg tykk
nn289 nn290 
*Gjelder dette fremdeles?
nn292 
*overspising siste året?
nn293 nn367 
nn294 nn295
*kontrollere kropsvekt 
nn296 nn297 nn298 nn299 nn300 
*selvfølelse
nn301
 
gen nn289p = 0 if nn285 == 0
replace nn289p = nn289 if nn289 != .
replace nn289p = 0 if nn292 == 0
gen nn290p = 0 if nn285 == 0
replace nn290p = nn290 if nn290 != .
replace nn290p = 0 if nn292 == 0
tab nn289p nn290p, m

tab nn293 nn367, m
tab nn294 nn295, m

tab1 aa1475 aa1476 aa1477 aa1478 aa1480 aa1482 aa1484 aa1486 aa1479 aa1481 aa1483 aa1485 aa1487 aa1488
pwcorr aa1475 aa1476 aa1477 aa1478 aa1480 aa1482 aa1484 aa1486 aa1479 aa1481 aa1483 aa1485 aa1487 aa1488

tab1 nn289p nn290p nn293 nn367 nn294 nn295 nn296 nn297 nn298 nn299 nn300 nn301
pwcorr nn289p nn290p nn293 nn367 nn294 nn295 nn296 nn297 nn298 nn299 nn300 nn301


irt grm aa1475 aa1476 aa1477 aa1478 aa1480 aa1482 aa1484 aa1486 aa1479 aa1481 aa1483 aa1485 aa1487 aa1488, intmethod(ghermite)
matrix b_ED1_GRM = e(b)
estimates save "N:\durable\projects\Perline\Eating disorders data\ED1_GRM", replace
*irt grm aa1475 aa1476 aa1477 aa1478 aa1480 aa1482 aa1484 aa1486 aa1479 aa1481 aa1483 aa1485 aa1487 aa1488, intmethod(mvaghermite) from(b_ED1_GRM)

irt nrm aa1475 aa1476 aa1477 aa1478 aa1480 aa1482 aa1484 aa1486 aa1479 aa1481 aa1483 aa1485 aa1487 aa1488, intmethod(ghermite) from(b_ED1_GRM, skip)
matrix b_ED1_NRM = e(b)
estimates save "N:\durable\projects\Perline\Eating disorders data\ED1_NRM", replace

irt grm nn289p nn290p nn293 nn367 nn294 nn295 nn296 nn297 nn298 nn299 nn300 nn301,  intmethod(ghermite)
matrix b_ED8_GRM = e(b)
estimates save "N:\durable\projects\Perline\Eating disorders data\ED8_GRM", replace

irt nrm nn289p nn290p nn293 nn367 nn294 nn295 nn296 nn297 nn298 nn299 nn300 nn301,  intmethod(ghermite) from(b_ED8_GRM, skip)
matrix b_ED8_NRM = e(b)
estimates save "N:\durable\projects\Perline\Eating disorders data\ED8_NRM", replace

estimates use "N:\durable\projects\Perline\Eating disorders data\ED1_GRM"
predict ED1_GRM, latent se(ED1_GRM_se)
estimates use "N:\durable\projects\Perline\Eating disorders data\ED1_NRM"
predict ED1_NRM, latent se(ED1_NRM_se)
estimates use "N:\durable\projects\Perline\Eating disorders data\ED8_GRM"
predict ED8_GRM, latent se(ED8_GRM_se)
estimates use "N:\durable\projects\Perline\Eating disorders data\ED8_NRM"
predict ED8_NRM, latent se(ED8_NRM_se)


*Variable including those with non response (i.e. S.E. == 1)
generate ED1_GRM_NR = ED1_GRM
generate ED1_NRM_NR = ED1_NRM
generate ED8_NRM_NR = ED8_NRM
generate ED8_GRM_NR = ED8_GRM
generate ED1_GRM_NR_se = ED1_GRM_se
generate ED1_NRM_NR_se = ED1_NRM_se
generate ED8_NRM_NR_se = ED8_NRM_se
generate ED8_GRM_NR_se = ED8_GRM_se
*Setting S.E. == 1 to missing (non response)
replace ED1_GRM = . if  ED1_GRM_se == 1
replace ED1_NRM = . if  ED1_NRM_se == 1
replace ED8_GRM = . if  ED8_GRM_se == 1
replace ED8_NRM = . if  ED8_NRM_se == 1
replace ED1_GRM_se = . if  ED1_GRM_se == 1
replace ED1_NRM_se = . if  ED1_NRM_se == 1
replace ED8_GRM_se = . if  ED8_GRM_se == 1
replace ED8_NRM_se = . if  ED8_NRM_se == 1

save "N:\durable\projects\Perline\Eating disorders data\data_moba_mother_eatingdisorders_scores.dta", replace

summ ED1_GRM_se ED1_NRM_se ED8_GRM_se ED8_NRM_se, d
twoway (scatter ED1_GRM_se ED1_NRM_se)
twoway (scatter ED8_GRM_se ED8_NRM_se)

twoway (scatter ED1_GRM_se ED1_GRM)
twoway (scatter ED1_NRM_se ED1_NRM)
twoway (scatter ED8_GRM_se ED8_GRM)
twoway (scatter ED8_NRM_se ED8_NRM)

histogram ED1_NRM_se
histogram ED8_NRM_se

cor ED1_GRM ED1_NRM ED8_GRM ED8_NRM
reg ED8_NRM c.ED1_NRM
predict ED1_NRM_res8, residual
twoway (scatter ED1_NRM_res8 ED8_NRM)
reg ED8_NRM c.ED1_NRM#c.ED1_NRM_se
predict ED1_NRM_res8_se, residual
twoway (scatter ED1_NRM_res8_se ED8_NRM)

reg ED8_NRM c.ED1_NRM#c.ED1_NRM_se#c.ED8_NRM_se



