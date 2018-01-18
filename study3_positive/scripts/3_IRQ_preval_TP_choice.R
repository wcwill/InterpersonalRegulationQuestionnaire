### 3 - IRQ_preval_TP choice analyses: affiliation repsonse
setwd("")
source('doBootstrap.R')

library(nlme)
library(plyr)

###--- Setup ---###
# load data
d <- read.csv('IRQ_preval_TP_choice_proc.csv')

# demographics
mean(d$age) # 34.62
sd(d$age) # 11.26
table(d$gender) # 55.5% male
table(d$ethnicity) # 79.5% caucasian

# as.numeric response
d$response <- as.character(d$response)
d$response[d$response == "person"] = 1
d$response[d$response == "no_person"] = 0
d$response <- as.numeric(d$response)
                     

###--- Analyses: Image ratings ---###
with(d, mean(task_mrate))
with(d, sd(task_mrate))

# test ratings against all 1s
x <- rep(1,200)
with(d, t.test(d$task_mrate, x))
# bootstrap CI
boot_task_mrate <- doBoot(d$task_mrate, x, whichTest = 'difference, unpaired', numberOfIterations = 1000)
# 4.602375 [ 4.445244 , 4.750094 ]


###--- Analyses: Response data ---###
###--- response by task_mrate
glm_resp_task_mrate <- with(d, glm(response ~ task_mrate, family=binomial(logit)))
summary(glm_resp_task_mrate)


###--- response by irq_total
glm_resp_irq_total <- with(d, glm(response ~ irq_total, family=binomial(logit)))
summary(glm_resp_irq_total)
# bootstrap CI
boot_resp_irq_total <- doBootLogRegression(d, response ~ irq_total, mixedEffects = FALSE, numberOfIterations = 1000)
# Coefficient : irq_total 0.08240754 [ 0.01251013 , 0.155376 ]


###--- response by irq subscales
# irq_tp
glm_resp_irq_tp_total <- with(d, glm(response ~ irq_tp_total, family=binomial(logit)))
summary(glm_resp_irq_tp_total)
# bootstrap CI
boot_resp_irq_tp <- doBootLogRegression(d, response ~ irq_tp_total, mixedEffects = FALSE, numberOfIterations = 1000)
# Coefficient : irq_tp_total 0.08053326 [ 0.02246055 , 0.1450074 ]


# irq_tn
glm_resp_irq_tn_total <- with(d, glm(response ~ irq_tn_total, family=binomial(logit)))
summary(glm_resp_irq_tn_total)
# bootstrap CI
boot_resp_irq_tn_total <- doBootLogRegression(d, response ~ irq_tn_total, mixedEffects = FALSE, numberOfIterations = 1000)
# Coefficient : irq_tn_total 0.03819387 [ -0.01467293 , 0.09443323 ]

# TP/TN subscale comparison
glm_resp_irq_tp_tn <- with(d, glm(response ~ irq_tp_total + irq_tn_total, family=binomial(logit)))
summary(glm_resp_irq_tp_tn)
anova(glm_resp_irq_tp_total,glm_resp_irq_tp_tn, test="Chisq")


# irq_en
glm_resp_irq_en_total <- with(d, glm(response ~ irq_en_total, family=binomial(logit)))
summary(glm_resp_irq_en_total)
# bootstrap CI
boot_resp_irq_en_total <- doBootLogRegression(d, response ~ irq_en_total, mixedEffects = FALSE, numberOfIterations = 1000)
# Coefficient : irq_en_total 0.04799276 [ -0.02040214 , 0.1187512 ]

# TP/EN subscale comparison
glm_resp_irq_tp_en <- with(d, glm(response ~ irq_tp_total + irq_en_total, family=binomial(logit)))
summary(glm_resp_irq_tp_en)
anova(glm_resp_irq_tp_total,glm_resp_irq_tp_en, test="Chisq")


# irq_ep
glm_resp_irq_ep_total <- with(d, glm(response ~ irq_ep_total, family=binomial(logit)))
summary(glm_resp_irq_ep_total)
# bootstrap CI
boot_resp_irq_ep_total <- doBootLogRegression(d, response ~ irq_ep_total, mixedEffects = FALSE, numberOfIterations = 1000)
# Coefficient : irq_ep_total 0.06348858 [ -0.006511634 , 0.1306546 ]

# TP/EP subscale comparison
glm_resp_irq_tp_ep <- with(d, glm(response ~ irq_tp_total + irq_ep_total, family=binomial(logit)))
summary(glm_resp_irq_tp_ep)
anova(glm_resp_irq_tp_total,glm_resp_irq_tp_ep, test="Chisq")


###--- response by bfi_e_total
glm_resp_bfi_e_total <- with(d, glm(response ~ bfi_e_total, family=binomial(logit)))
summary(glm_resp_bfi_e_total)

# TP/BFI_E comparison
glm_resp_irq_tp_bfie <- with(d, glm(response ~ irq_tp_total + bfi_e_total, family=binomial(logit)))
summary(glm_resp_irq_tp_bfie)
anova(glm_resp_irq_tp_total, glm_resp_irq_tp_bfie, test="Chisq")
anova(glm_resp_bfi_e_total, glm_resp_irq_tp_bfie, test="Chisq")

# bootstrap CI
boot_resp_irq_tp_bfie <- doBootLogRegression(d, response ~ irq_tp_total + bfi_e_total, mixedEffects = FALSE, numberOfIterations = 1000)
# Coefficient : irq_tp_total 0.07519327 [ 0.01220003 , 0.141701 ]
# Coefficient : bfi_e_total 0.007627972 [ -0.03571283 , 0.05090938 ]


###--- response by erq_r_total (reappraisal)
glm_resp_erq_r_total <- with(d, glm(response ~ erq_r_total, family=binomial(logit)))
summary(glm_resp_erq_r_total)

# TP/ERQ-R comparison
glm_resp_irq_tp_erqr <- with(d, glm(response ~ irq_tp_total + erq_r_total, family=binomial(logit)))
summary(glm_resp_irq_tp_erqr)
anova(glm_resp_irq_tp_total, glm_resp_irq_tp_erqr, test="Chisq")
anova(glm_resp_erq_r_total, glm_resp_irq_tp_erqr, test="Chisq")

# bootstrap CI
boot_resp_irq_tp_erqr <- doBootLogRegression(d, response ~ irq_tp_total + erq_r_total, mixedEffects = FALSE, numberOfIterations = 1000)
# Coefficient : irq_tp_total 0.08193288 [ 0.01947139 , 0.1439067 ]
# Coefficient : erq_r_total 0.003699443 [ -0.0520257 , 0.05813152 ]


###--- response by erq_s_total (suppression)
glm_resp_erq_s_total <- with(d, glm(response ~ erq_s_total, family=binomial(logit)))
summary(glm_resp_erq_s_total)

# TP/ERQ-S comparison
glm_resp_irq_tp_erqs <- with(d, glm(response ~ irq_tp_total + erq_s_total, family=binomial(logit)))
summary(glm_resp_irq_tp_erqs)
anova(glm_resp_irq_tp_total, glm_resp_irq_tp_erqs, test="Chisq")
anova(glm_resp_erq_s_total, glm_resp_irq_tp_erqs, test="Chisq")

# bootstrap CI
boot_resp_irq_tp_erqs <- doBootLogRegression(d, response ~ irq_tp_total + erq_s_total, mixedEffects = FALSE, numberOfIterations = 1000)
# Coefficient : irq_tp_total 0.1060454 [ 0.03812684 , 0.187883 ]
# Coefficient : erq_s_total 0.05739603 [ -0.006342639 , 0.1240894 ]
