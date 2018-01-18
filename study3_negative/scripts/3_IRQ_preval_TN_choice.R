### 3 - IRQ_preval_TN choice analyses: affiliation repsonse
setwd("")
source('doBootstrap.R')

library(nlme)
library(plyr)

###--- Setup ---###
# load data
d <- read.csv('IRQ_preval_TN_choice_proc.csv')

# demographics
mean(d$age) # 34.29
sd(d$age) 
table(d$gender) 
table(d$ethnicity)

# as.numeric response
d$response <- as.character(d$response)
d$response[d$response == "person"] = 1
d$response[d$response == "no_person"] = 0
d$response <- as.numeric(d$response)


###--- Analyses: Image ratings ---###
with(d, mean(task_mrate)) # 4.31
with(d, sd(task_mrate)) # 1.18

# test ratings against all 1s
x <- rep(1,200)
with(d, t.test(d$task_mrate, x))
# bootstrap CI
boot_task_mrate <- doBoot(d$task_mrate, x, whichTest = 'difference, unpaired', numberOfIterations = 1000)
# 3.307875 [ 3.143494 , 3.467844 ]


###--- Analyses: Response data ---###
###--- response by task_mrate
glm_resp_task_mrate <- with(d, glm(response ~ task_mrate, family=binomial(logit)))
summary(glm_resp_task_mrate)


###--- response by irq_total
glm_resp_irq_total <- with(d, glm(response ~ irq_total, family=binomial(logit)))
summary(glm_resp_irq_total)
# bootstrap CI
boot_resp_irq_total <- doBootLogRegression(d, response ~ irq_total, mixedEffects = FALSE, numberOfIterations = 1000)
#Coefficient : irq_total 0.1098191 [ 0.03518635 , 0.1796551 ]


###--- response by irq subscales
# irq_tn
glm_resp_irq_tn_total <- with(d, glm(response ~ irq_tn_total, family=binomial(logit)))
summary(glm_resp_irq_tn_total)
# bootstrap CI
boot_resp_irq_tn <- doBootLogRegression(d, response ~ irq_tn_total, mixedEffects = FALSE, numberOfIterations = 1000)
#Coefficient : irq_tn_total 0.05287996 [ -4.460426e-05 , 0.1087748 ]


# irq_tp
glm_resp_irq_tp_total <- with(d, glm(response ~ irq_tp_total, family=binomial(logit)))
summary(glm_resp_irq_tp_total)
# bootstrap CI
boot_resp_irq_tp <- doBootLogRegression(d, response ~ irq_tp_total, mixedEffects = FALSE, numberOfIterations = 1000)
#Coefficient : irq_tp_total 0.06070319 [ 0.006967067 , 0.1168908 ]

# TP/TN subscale comparison
glm_resp_irq_tn_tp <- with(d, glm(response ~ irq_tn_total + irq_tp_total, family=binomial(logit)))
summary(glm_resp_irq_tn_tp)
anova(glm_resp_irq_tn_total, glm_resp_irq_tn_tp, test="Chisq")


# irq_en
glm_resp_irq_en_total <- with(d, glm(response ~ irq_en_total, family=binomial(logit)))
summary(glm_resp_irq_en_total)
# bootstrap CI
boot_resp_irq_en <- doBootLogRegression(d, response ~ irq_en_total, mixedEffects = FALSE, numberOfIterations = 1000)
#Coefficient : irq_en_total 0.1426286 [ 0.06836985 , 0.2166911 ]

# TN/EN subscale comparison
glm_resp_irq_tn_en <- with(d, glm(response ~ irq_tn_total + irq_en_total, family=binomial(logit)))
summary(glm_resp_irq_tn_en)
anova(glm_resp_irq_tn_total, glm_resp_irq_tn_en, test="Chisq")


# irq_ep
glm_resp_irq_ep_total <- with(d, glm(response ~ irq_ep_total, family=binomial(logit)))
summary(glm_resp_irq_ep_total)
# bootstrap CI
boot_resp_irq_ep <- doBootLogRegression(d, response ~ irq_ep_total, mixedEffects = FALSE, numberOfIterations = 1000)
#Coefficient : irq_ep_total 0.07537387 [ 0.01194544 , 0.150224 ]

# TN/EP subscale comparison
glm_resp_irq_tn_ep <- with(d, glm(response ~ irq_tn_total + irq_ep_total, family=binomial(logit)))
summary(glm_resp_irq_tn_ep)
anova(glm_resp_irq_tn_total, glm_resp_irq_tn_ep, test="Chisq")


###--- response by bfi_e_total
glm_resp_bfi_e_total <- with(d, glm(response ~ bfi_e_total, family=binomial(logit)))
summary(glm_resp_bfi_e_total) 

# TN/BFI_E comparison
glm_resp_irq_tn_bfie <- with(d, glm(response ~ irq_tn_total + bfi_e_total, family=binomial(logit)))
summary(glm_resp_irq_tn_bfie)
anova(glm_resp_irq_tn_total, glm_resp_irq_tn_bfie, test="Chisq")
anova(glm_resp_bfi_e_total, glm_resp_irq_tn_bfie, test="Chisq")

# bootstrap CI
boot_resp_irq_tn_bfie <- doBootLogRegression(d, response ~ irq_tn_total + bfi_e_total, mixedEffects = FALSE, numberOfIterations = 1000)
# Coefficient : irq_tn_total 0.0503967 [ -0.009847594 , 0.1103821 ]
# Coefficient : bfi_e_total 0.01037257 [ -0.03492447 , 0.0524174 ]


###--- response by erq_r_total (reappraisal)
glm_resp_erq_r_total <- with(d, glm(response ~ erq_r_total, family=binomial(logit)))
summary(glm_resp_erq_r_total)

# TN/ERQ-R comparison
glm_resp_irq_tn_erqr <- with(d, glm(response ~ irq_tn_total + erq_r_total, family=binomial(logit)))
summary(glm_resp_irq_tn_erqr)
anova(glm_resp_irq_tn_total, glm_resp_irq_tn_erqr, test="Chisq")
anova(glm_resp_erq_r_total, glm_resp_irq_tn_erqr, test="Chisq")

# bootstrap CI
boot_resp_irq_tn_erqr <- doBootLogRegression(d, response ~ irq_tn_total + erq_r_total, mixedEffects = FALSE, numberOfIterations = 1000)
# Coefficient : irq_tn_total 0.05636727 [ 0.001982348 , 0.1153438 ]
# Coefficient : erq_r_total -0.02767298 [ -0.08589706 , 0.02002023 ]


###--- response by erq_s_total (suppression)
glm_resp_erq_s_total <- with(d, glm(response ~ erq_s_total, family=binomial(logit)))
summary(glm_resp_erq_s_total)

# TN/ERQ-S comparison
glm_resp_irq_tn_erqs <- with(d, glm(response ~ irq_tn_total + erq_s_total, family=binomial(logit)))
summary(glm_resp_irq_tn_erqs)
anova(glm_resp_irq_tn_total, glm_resp_irq_tn_erqs, test="Chisq")
anova(glm_resp_erq_s_total, glm_resp_irq_tn_erqs, test="Chisq")

# bootstrap CI
boot_resp_irq_tn_erqs <- doBootLogRegression(d, response ~ irq_tn_total + erq_s_total, mixedEffects = FALSE, numberOfIterations = 1000)
# Coefficient : irq_tn_total 0.06466265 [ -0.002162669 , 0.1356444 ]
# Coefficient : erq_s_total 0.02112124 [ -0.04433635 , 0.09713748 ]
