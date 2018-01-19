### 1 - IRQ_preval_EN experience-sharing analyses
setwd("")
source('doBootstrap.R')

# import data
d <- read.csv('IRQ_preval_EN_expshare_proc.csv')

# demographics
mean(d$Age) # 34.29
sd(d$Age) # 10.93
table(d$Gender) # 55.8% male
table(d$Ethnicity) # 79.3% caucasian

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
with(d, mean(Exper_rate_1)) # 5.30
with(d, sd(Exper_rate_1)) # 1.40

x <- rep(1,391)
t.test(d$Exper_rate_1, x)
# bootstrap CI
boot_task_exprate <- doBoot(d$Exper_rate_1, x, whichTest = 'difference, unpaired', numberOfIterations = 1000)
# 4.299233 [ 4.15601 , 4.432225 ]


###--- Social support quality rating analyses ---### 
lm_sscomp_irq_total <- with(d, lm(SS_comp ~ IRQ_total))
summary(lm_sscomp_irq_total)
# bootstrap CI
boot_sscomp_irq_total <- doBootRegression(d, SS_comp ~ IRQ_total, mixedEffects = FALSE, numberOfIterations = 1000)
# Coefficient : IRQ_total 0.1216941 [ 0.08467344 , 0.1563722 ]


## IRQ_EN
lm_sscomp_irq_en <- with(d, lm(SS_comp ~ IRQ_EN))
summary(lm_sscomp_irq_en)
# bootstrap CI
boot_sscomp_irq_en <- doBootRegression(d, SS_comp ~ IRQ_EN, mixedEffects = FALSE, numberOfIterations = 1000)
# Coefficient : IRQ_EN 0.1288186 [ 0.09539806 , 0.1606173 ]


## IRQ_TN
lm_sscomp_irq_tn <- with(d, lm(SS_comp ~ IRQ_TN))
summary(lm_sscomp_irq_tn)
# bootstrap CI
boot_sscomp_irq_tn <- doBootRegression(d, SS_comp ~ IRQ_TN, mixedEffects = FALSE, numberOfIterations = 1000)
# Coefficient : IRQ_TN 0.08079872 [ 0.05542391 , 0.1046388 ]

# EN/TN subscale comparison
lm_sscomp_irq_en_tn <- with(d, lm(SS_comp ~ IRQ_EN + IRQ_TN))
summary(lm_sscomp_irq_en_tn)
anova(lm_sscomp_irq_en, lm_sscomp_irq_en_tn)


## IRQ_EP
lm_sscomp_irq_ep <- with(d, lm(SS_comp ~ IRQ_EP))
summary(lm_sscomp_irq_ep)
# bootstrap CI
boot_sscomp_irq_ep <- doBootRegression(d, SS_comp ~ IRQ_EP, mixedEffects = FALSE, numberOfIterations = 1000)
# Coefficient : IRQ_EP 0.08236698 [ 0.04994506 , 0.114597 ]

# EN/EP subscale comparison
lm_sscomp_irq_en_ep <- with(d, lm(SS_comp ~ IRQ_EN + IRQ_EP))
summary(lm_sscomp_irq_en_ep)
anova(lm_sscomp_irq_en, lm_sscomp_irq_en_ep)


## IRQ_TP
lm_sscomp_irq_tp <- with(d, lm(SS_comp ~ IRQ_TP))
summary(lm_sscomp_irq_tp)
# bootstrap CI
boot_sscomp_irq_tp <- doBootRegression(d, SS_comp ~ IRQ_TP, mixedEffects = FALSE, numberOfIterations = 1000)
# Coefficient : IRQ_TP 0.03258407 [ 0.002009293 , 0.06439949 ]

# EN/TP subscale comparison
lm_sscomp_irq_en_tp <- with(d, lm(SS_comp ~ IRQ_EN + IRQ_TP))
summary(lm_sscomp_irq_en_tp)
anova(lm_sscomp_irq_en, lm_sscomp_irq_en_tp)


## Experience ratings
lm_sscomp_exp_rate <- with(d, lm(SS_comp ~ Exper_rate_1))
summary(lm_sscomp_exp_rate)


## BFI extraversion
lm_sscomp_bfie <- with(d, lm(SS_comp ~ BFI_E_total))
summary(lm_sscomp_bfie)
# bootstrap CI
boot_sscomp_bfie <- doBootRegression(d, SS_comp ~ BFI_E_total, mixedEffects = FALSE, numberOfIterations = 1000)
# Coefficient : BFI_E_total 0.03189968 [ 0.01271338 , 0.05154959 ]

# EN/BFIE comparison
lm_sscomp_irq_en_bfie <- with(d, lm(SS_comp ~ IRQ_EN + BFI_E_total))
summary(lm_sscomp_irq_en_bfie)
# bootstrap CI
boot_sscomp_irq_en_bfie <- doBootRegression(d, SS_comp ~ IRQ_EN + BFI_E_total, mixedEffects = FALSE, numberOfIterations = 1000)
# Coefficient : IRQ_EN 0.1244051 [ 0.0913345 , 0.1586924 ]
# Coefficient : BFI_E_total 0.01891365 [ 0.0002461623 , 0.0387039 ]
anova(lm_sscomp_irq_en, lm_sscomp_irq_en_bfie)
anova(lm_sscomp_bfie, lm_sscomp_irq_en_bfie)


##ERQ reappraisal
lm_sscomp_erqr <- with(d, lm(SS_comp ~ ERQr))
summary(lm_sscomp_erqr)
# bootstrap CI
boot_sscomp_erqr <- doBootRegression(d, SS_comp ~ ERQr, mixedEffects = FALSE, numberOfIterations = 1000)
# Coefficient : ERQr 0.05418236 [ 0.02710095 , 0.08381338 ]

# EN/ERQ-R comparison
lm_sscomp_irq_en_erqr <- with(d, lm(SS_comp ~ IRQ_EN + ERQr))
summary(lm_sscomp_irq_en_erqr)
# bootstrap CI
boot_sscomp_irq_en_erqr <- doBootRegression(d, SS_comp ~ IRQ_EN + ERQr, mixedEffects = FALSE, numberOfIterations = 1000)
# Coefficient : IRQ_EN 0.118691 [ 0.08198493 , 0.1494446 ]
# Coefficient : ERQr 0.02884625 [ -4.674454e-05 , 0.05753627 ]
anova(lm_sscomp_irq_en, lm_sscomp_irq_en_erqr)
anova(lm_sscomp_erqr, lm_sscomp_irq_en_erqr)


##ERQ suppression
lm_sscomp_erqs <- with(d, lm(SS_comp ~ ERQs))
summary(lm_sscomp_erqs)
# bootstrap CI
boot_sscomp_erqs <- doBootRegression(d, SS_comp ~ ERQs, mixedEffects = FALSE, numberOfIterations = 1000)
# Coefficient : ERQs -0.04026577 [ -0.06850122 , -0.008803637 ]

# EN/ERQ-S comparison
lm_sscomp_irq_en_erqs <- with(d, lm(SS_comp ~ IRQ_EN + ERQs))
summary(lm_sscomp_irq_en_erqs)
# bootstrap CI
boot_sscomp_irq_en_erqs <- doBootRegression(d, SS_comp ~ IRQ_EN + ERQs, mixedEffects = FALSE, numberOfIterations = 1000)
# Coefficient : IRQ_EN 0.1280337 [ 0.09170617 , 0.1606383 ]
# Coefficient : ERQs -0.00342872 [ -0.03511151 , 0.02589139 ]
anova(lm_sscomp_irq_en, lm_sscomp_irq_en_erqs)
anova(lm_sscomp_erqs, lm_sscomp_irq_en_erqs)
