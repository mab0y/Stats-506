use "C:\Users\mab0y\Downloads\public2022.dta"

display _N

gen B3_binary=(B3>=3)

svyset CaseID [pw=weight_pop]

svy:logit c.B3_binary i.ND2 i.B7_b i.GH1 i.ppeducat i.race_5cat