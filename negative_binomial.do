***************************************
*** ECONOMETRICS OF INSURANCE *********
*** Mai-Anh Dang | TSE

import delimited "C:\Users\utilisateur\iCloudDrive\TSE-EEE\Insurance\Insurance project\Data\merged_final_insurance.csv"

encode pol_coverage, gen(polcoverage)
encode pol_pay_freq, gen(pol_payfreq)
encode pol_payd, gen(polpayd)
encode pol_usage, gen(polusage)
encode drv_drv2, gen(drv2)
encode drv_sex1 , gen(sex1)
encode drv_sex2 , gen(sex2)
encode vh_fuel, gen(vhfuel)
encode vh_type, gen(vhtype)

* negbi without offset
nbreg claim_nb polpayd polusage pol_duration drv_age1 drv_age2 ///
sex1 sex2 drv_age_lic1 drv_age_lic2 vh_age  vhfuel vh_cyl vh_din ///
vh_sale_begin vh_sale_end vh_speed vhtype vh_weight, nolog

* negbi with duration as offeset
gen logduration = log(pol_duration)

nbreg claim_nb polpayd polusage drv_age1 drv_age2 sex1 sex2 drv_age_lic1 ///
drv_age_lic2 vh_age  vhfuel vh_cyl vh_din vh_sale_begin vh_sale_end vh_speed /// 
vhtype vh_weight, nolog offset(logduration)

* negbi with duration_sit as offeset
gen logdurationsit = log(pol_sit_duration)

nbreg claim_nb polpayd polusage drv_age1 drv_age2 sex1 sex2 drv_age_lic1 /// 
drv_age_lic2 vh_age  vhfuel vh_cyl vh_din vh_sale_begin vh_sale_end vh_speed /// 
vhtype vh_weight, nolog offset(logdurationsit)
