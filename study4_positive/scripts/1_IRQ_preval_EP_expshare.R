### 1 - IRQ_preval_EP experience-sharing analyses
setwd("")
source('doBootstrap.R')

# import data
d <- read.csv('IRQ_preval_EP_expshare_proc.csv')

# demographics
mean(d$Age) # 32.43
sd(d$Age) # 10.32
table(d$Gender) # 51.5% male
table(d$Ethnicity) # 77.0% caucasian

# generate IRQ subscales and total scale scores
d$IRQ_TN <- with(d, rowSums(d[26:29]))
d$IRQ_EN <- with(d, rowSums(d[30:33]))
d$IRQ_TP <- with(d, rowSums(d[34:37]))
d$IRQ_EP <- with(d, rowSums(d[38:41]))
d$IRQ_total <- with(d, rowSums(d[64:67]))

# BFI extraversion total
d$BFI_E_2 <- 6 - d$BFI_E_2
d$BFI_E_5 <- 6 - d$BFI_E_5
d$BFI_E_7 <- 6 - d$BFI_E_7
d$BFI_E_total <- with(d, rowSums(d[52:59]))

# ERQ reappraisal total
d$ERQr <- d$ERQ_1 + d$ERQ_3 + d$ERQ_5 + d$ERQ_7 + d$ERQ_8 + d$ERQ_10

# ERQ suppression total
d$ERQs <- d$ERQ_2 + d$ERQ_4 + d$ERQ_6 + d$ERQ_9

# create social support composite score
d$SS_comp <- (d$SS_rate_1 + d$SS_rate_2 + d$SS_rate_3)/3


###---Experience ratings---###
with(d, mean(Exper_rate_1)) # 6.34
with(d, sd(Exper_rate_1)) # .85

x <- rep(1,396)
t.test(d$Exper_rate_1, x)
# bootstrap CI
boot_task_exprate <- doBoot(d$Exper_rate_1, x, whichTest = 'difference, unpaired', numberOfIterations = 1000)
# 5.338384 [ 5.262563 , 5.419192 ]


###--- Social support quality rating analyses ---### 
## Experience ratings
lm_sscomp_exp_rate <- with(d, lm(SS_comp ~ Exper_rate_1))
summary(lm_sscomp_exp_rate)
# bootstrap CI
boot_sscomp_exp_rate <- doBootRegression(d, SS_comp ~ Exper_rate_1, mixedEffects = FALSE, numberOfIterations = 1000)
# Coefficient : Exper_rate_1 0.7447705 [ 0.5698754 , 0.8988972 ]


## IRQ_total
lm_sscomp_irq_total_exp_rate <- with(d, lm(SS_comp ~ IRQ_total + Exper_rate_1))
summary(lm_sscomp_irq_total_exp_rate)
# bootstrap CI
boot_sscomp_irq_total_exp_rate <- doBootRegression(d, SS_comp ~ IRQ_total + Exper_rate_1, mixedEffects = FALSE, numberOfIterations = 1000)
# Coefficient : IRQ_total 0.04672225 [ 0.01358589 , 0.08132192 ]
# Coefficient : Exper_rate_1 0.6956482 [ 0.5244377 , 0.8497842 ]


## IRQ_EP
lm_sscomp_irq_ep <- with(d, lm(SS_comp ~ IRQ_EP))
summary(lm_sscomp_irq_ep)
# bootstrap CI
boot_sscomp_irq_ep <- doBootRegression(d, SS_comp ~ IRQ_EP, mixedEffects = FALSE, numberOfIterations = 1000)
# Coefficient : IRQ_EP 0.07602267 [ 0.04028539 , 0.1106205 ]

# EP/exp_rate comparison
lm_sscomp_irq_ep_exp_rate <- with(d, lm(SS_comp ~ IRQ_EP + Exper_rate_1))
summary(lm_sscomp_irq_ep_exp_rate)
# bootstrap CI
boot_sscomp_irq_ep_exp_rate <- doBootRegression(d, SS_comp ~ IRQ_EP + Exper_rate_1, mixedEffects = FALSE, numberOfIterations = 1000)
# Coefficient : IRQ_EP 0.04823318 [ 0.01919243 , 0.08191184 ]
# Coefficient : Exper_rate_1 0.6905489 [ 0.5287609 , 0.8623829 ]
anova(lm_sscomp_irq_ep, lm_sscomp_irq_ep_exp_rate)
anova(lm_sscomp_exp_rate,lm_sscomp_irq_ep_exp_rate)


## IRQ_TP
lm_sscomp_irq_tp_exp_rate <- with(d, lm(SS_comp ~ IRQ_TP + Exper_rate_1))
summary(lm_sscomp_irq_tp_exp_rate)
# bootstrap CI
boot_sscomp_irq_tp_exp_rate <- doBootRegression(d, SS_comp ~ IRQ_TP + Exper_rate_1, mixedEffects = FALSE, numberOfIterations = 1000)
# Coefficient : IRQ_TP 0.02168384 [ -0.001718953 , 0.04937784 ]
# Coefficient : Exper_rate_1 0.705585 [ 0.549974 , 0.8821775 ]

# EP/TP/exp_rate comparison
lm_sscomp_irq_ep_exp_rate_irq_tp <- with(d, lm(SS_comp ~ IRQ_EP + Exper_rate_1 + IRQ_TP))
summary(lm_sscomp_irq_ep_exp_rate_irq_tp)
anova(lm_sscomp_irq_ep_exp_rate, lm_sscomp_irq_ep_exp_rate_irq_tp)


## IRQ_EN
lm_sscomp_irq_en_exp_rate <- with(d, lm(SS_comp ~ IRQ_EN + Exper_rate_1))
summary(lm_sscomp_irq_en_exp_rate)
# bootstrap CI
boot_sscomp_irq_en_exp_rate <- doBootRegression(d, SS_comp ~ IRQ_EN + Exper_rate_1, mixedEffects = FALSE, numberOfIterations = 1000)
# Coefficient : IRQ_EN 0.04079728 [ 0.01130821 , 0.07271109 ]
# Coefficient : Exper_rate_1 0.7083133 [ 0.5445518 , 0.8557639 ]

# EP/EN/exp_rate comparison
lm_sscomp_irq_ep_exp_rate_irq_en <- with(d, lm(SS_comp ~ IRQ_EP + Exper_rate_1 + IRQ_EN))
summary(lm_sscomp_irq_ep_exp_rate_irq_en)
anova(lm_sscomp_irq_ep_exp_rate, lm_sscomp_irq_ep_exp_rate_irq_en)


## IRQ_TN
lm_sscomp_irq_tn_exp_rate <- with(d, lm(SS_comp ~ IRQ_TN + Exper_rate_1))
summary(lm_sscomp_irq_tn_exp_rate)
# bootstrap CI
boot_sscomp_irq_tn_exp_rate <- doBootRegression(d, SS_comp ~ IRQ_TN + Exper_rate_1, mixedEffects = FALSE, numberOfIterations = 1000)
# Coefficient : IRQ_TN 0.0151843 [ -0.004246074 , 0.03353729 ]
# Coefficient : Exper_rate_1 0.7327838 [ 0.5770998 , 0.8983164 ]

# EP/TN/exp_rate comparison
lm_sscomp_irq_ep_exp_rate_irq_tn <- with(d, lm(SS_comp ~ IRQ_EP + Exper_rate_1 + IRQ_TN))
summary(lm_sscomp_irq_ep_exp_rate_irq_tn)
anova(lm_sscomp_irq_ep_exp_rate, lm_sscomp_irq_ep_exp_rate_irq_tn)


##BFI extraversion
lm_sscomp_bfie_exp_rate <- with(d, lm(SS_comp ~ BFI_E_total + Exper_rate_1))
summary(lm_sscomp_bfie_exp_rate)

# EP/BFIE/exp_rate comparison
lm_sscomp_irq_ep_exp_rate_bfie <- with(d, lm(SS_comp ~ IRQ_EP + Exper_rate_1 + BFI_E_total))
summary(lm_sscomp_irq_ep_exp_rate_bfie)
# bootstrap CI
boot_sscomp_irq_ep_exp_rate_bfie <- doBootRegression(d, SS_comp ~ IRQ_EP + Exper_rate_1 + BFI_E_total, mixedEffects = FALSE, numberOfIterations = 1000)
# Coefficient : IRQ_EP 0.04034988 [ 0.01128699 , 0.07744325 ]
# Coefficient : Exper_rate_1 0.6819538 [ 0.5195671 , 0.8358531 ]
# Coefficient : BFI_E_total 0.01119394 [ -0.003210012 , 0.0272049 ]
anova(lm_sscomp_irq_ep_exp_rate, lm_sscomp_irq_ep_exp_rate_bfie)
anova(lm_sscomp_bfie_exp_rate, lm_sscomp_irq_ep_exp_rate_bfie)


##ERQ reappraisal
lm_sscomp_erqr_exp_rate <- with(d, lm(SS_comp ~ ERQr + Exper_rate_1))
summary(lm_sscomp_erqr_exp_rate)

# EP/ERQ-R/exp_rate comparison
lm_sscomp_irq_ep_exp_rate_erqr <- with(d, lm(SS_comp ~ IRQ_EP + Exper_rate_1 + ERQr))
summary(lm_sscomp_irq_ep_exp_rate_erqr)
# bootstrap CI
boot_sscomp_irq_ep_exp_rate_erqr <- doBootRegression(d, SS_comp ~ IRQ_EP + Exper_rate_1 + ERQr, mixedEffects = FALSE, numberOfIterations = 1000)
# Coefficient : IRQ_EP 0.03971391 [ 0.01185066 , 0.07845957 ]
# Coefficient : Exper_rate_1 0.6377377 [ 0.4723275 , 0.8016764 ]
# Coefficient : ERQr 0.04097823 [ 0.02027468 , 0.06296318 ]
anova(lm_sscomp_irq_ep_exp_rate, lm_sscomp_irq_ep_exp_rate_erqr)
anova(lm_sscomp_erqr_exp_rate, lm_sscomp_irq_ep_exp_rate_erqr)


##ERQ suppression
lm_sscomp_erqs_exp_rate <- with(d, lm(SS_comp ~ ERQs + Exper_rate_1))
summary(lm_sscomp_erqs_exp_rate)

# EP/ERQ-S/exp_rate comparison
lm_sscomp_irq_ep_exp_rate_erqs <- with(d, lm(SS_comp ~ IRQ_EP + Exper_rate_1 + ERQs))
summary(lm_sscomp_irq_ep_exp_rate_erqs)
# bootstrap CI
boot_sscomp_irq_ep_exp_rate_erqs <- doBootRegression(d, SS_comp ~ IRQ_EP + Exper_rate_1 + ERQs, mixedEffects = FALSE, numberOfIterations = 1000)
# Coefficient : IRQ_EP 0.04884772 [ 0.0158596 , 0.08129562 ]
# Coefficient : Exper_rate_1 0.6920618 [ 0.5292414 , 0.8578063 ]
# Coefficient : ERQs -0.0005215467 [ -0.02109308 , 0.02081426 ]
anova(lm_sscomp_irq_ep_exp_rate, lm_sscomp_irq_ep_exp_rate_erqs)
anova(lm_sscomp_erqs_exp_rate, lm_sscomp_irq_ep_exp_rate_erqs)
