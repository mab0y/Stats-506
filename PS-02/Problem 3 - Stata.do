clear

import delimited cars.csv
rename (dimensionsheight dimensionslength dimensionswidth engineinformationdriveline engineinformationenginestatistic engineinformationenginetype engineinformationhybrid engineinformationnumberofforward engineinformationtransmission fuelinformationcitympg fuelinformationfueltype fuelinformationhighwaympg identificationclassification identificationid identificationmake identificationmodelyear identificationyear v18) (height length width driveline horsepower engine_type hybrid forward_gears transmission city_mpg fuel_type highway_mpg classification id make model_year year torque)

keep if fuel_type == "Gasoline"

reg highway_mpg c.horsepower c.torque c.height c.length c.width i.year

gen horsepower_torque = horsepower*torque
reg highway_mpg c.horsepower##c.torque c.height  c.length c.width i.year


summarize torque
local mean_value = r(mean)
local sd_value = r(sd)
local mean_plus_sd = `mean_value' + `sd_value'
local mean_minus_sd = `mean_value' - `sd_value'


margins,  at(horsepower=(200(200)600) torque=(`mean_minus_sd' `mean_value' `mean_plus_sd') year=2011)
marginsplot, xlabel(horsepower) ylabel(highway_mpg)




