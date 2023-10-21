library(haven)
dat <- read_dta("C:/Users/mab0y/Downloads/public2022.dta")

dat$B3_binary <- as.numeric(dat$B3>=3)
dat$ND2 <- as.factor(dat$ND2)
dat$B7_b <- as.factor(dat$B7_b)
dat$GH1 <- as.factor(dat$GH1)
dat$ppeducat <- as.factor(dat$ppeducat)
dat$race_5cat <- as.factor(dat$race_5cat)

library(survey)
survey_design <- svydesign(id = ~ CaseID, weight = ~ weight_pop, data = dat)

# https://stats.stackexchange.com/questions/504209/understanding-the-deviance-and-pseudo-r2-from-a-glm
residual_model<-svyglm(B3_binary ~ ND2 + B7_b + GH1 + ppeducat + race_5cat, design=survey_design,family = "quasibinomial")

null_model <- svyglm(B3_binary ~ 1, design = survey_design, family = binomial())

pseudo_R2 <- 1 - (deviance(residual_model) / deviance(null_model))

