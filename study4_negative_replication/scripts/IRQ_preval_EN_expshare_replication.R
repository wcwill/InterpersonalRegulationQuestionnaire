# IRQ_preval_EN experience-sharing replication analyses
setwd("")
source('doBootstrap.R')

# # import data
# d <- read.csv('IRQ_preval_EN_expshare_replication_proc.csv')

# # remove duplicate workerids
# d$dup <- duplicated(d$mTurk.ID)
# d <- subset(d, dup == 'FALSE')

# # write dataset without duplicate workers
# write.csv(d, "IRQ_preval_EN_expshare_replication_proc_nodups.csv", row.names=FALSE)

# import data
d <- read.csv('IRQ_preval_EN_expshare_replication_proc_nodups.csv')

# demographics
mean(d$Age)
sd(d$Age)
table(d$Gender)
table(d$Ethnicity)

# generate IRQ subscales and total scale scores
d$IRQ_TN <- with(d, IRQ_1 + IRQ_2 + IRQ_3 + IRQ_4)
d$IRQ_EN <- with(d, IRQ_5 + IRQ_6 + IRQ_7 + IRQ_8)
d$IRQ_TP <- with(d, IRQ_9 + IRQ_10 + IRQ_11 + IRQ_12)
d$IRQ_EP <- with(d, IRQ_13 + IRQ_14 + IRQ_15 + IRQ_16)
d$IRQ_total <- with(d, IRQ_TN + IRQ_EN + IRQ_TP + IRQ_EP)

# COPE sub-scales
d$COPE_e <- with(d, COPE_emo_ins_ss_COPE_5 + COPE_emo_ins_ss_COPE_15)
d$COPE_i <- with(d, COPE_emo_ins_ss_COPE_10 + COPE_emo_ins_ss_COPE_23)

# BFI extraversion
d$BFI_e_BFI_e6R <- 6 - d$BFI_e_BFI_e6R
d$BFI_e_BFI_e21R <- 6 - d$BFI_e_BFI_e21R
d$BFI_e_BFI_e31R <- 6 - d$BFI_e_BFI_e31R
d$BFI_e <- with(d, BFI_e_BFI_e1 + BFI_e_BFI_e6R + BFI_e_BFI_e11 + BFI_e_BFI_e16 + BFI_e_BFI_e21R + BFI_e_BFI_e26 + BFI_e_BFI_e31R + BFI_e_BFI_e36)

# BEQ negative expressivity
d$BEQ_nex_BEQ_nex3 <- 8 - d$BEQ_nex_BEQ_nex3
d$BEQ_nex_BEQ_nex8 <- 8 - d$BEQ_nex_BEQ_nex8
d$BEQ_nex_BEQ_nex9 <- 8 - d$BEQ_nex_BEQ_nex9
d$BEQ_n <- d$BEQ_nex_BEQ_nex3 + d$BEQ_nex_BEQ_nex5 + d$BEQ_nex_BEQ_nex8 + d$BEQ_nex_BEQ_nex9 + d$BEQ_nex_BEQ_nex13 + d$BEQ_nex_BEQ_nex16

# AAS closeness
d$AAS_AAS_8 <- 6 - d$AAS_AAS_8
d$AAS_AAS_13 <- 6 - d$AAS_AAS_13
d$AAS_AAS_17 <- 6 - d$AAS_AAS_17
d$AAS_c <- d$AAS_AAS_1 + d$AAS_AAS_6 + d$AAS_AAS_8 + d$AAS_AAS_12 + d$AAS_AAS_13 + d$AAS_AAS_17

# create social support composite score
d$SS_comp <- (d$SS_rate_1 + d$SS_rate_2 + d$SS_rate_3)/3


###---Experience ratings---###
with(d, mean(Exper_rate_1)) # 5.33
with(d, sd(Exper_rate_1)) # 1.34

x <- rep(1,396)
t.test(d$Exper_rate_1, x)
# bootstrap CI
boot_task_exprate <- doBoot(d$Exper_rate_1, x, whichTest = 'difference, unpaired', numberOfIterations = 1000)
# 4.323232 [ 4.204482 , 4.457071 ]


###--- Social support quality rating analyses ---### 
## IRQ_total
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
# Coefficient : IRQ_EN 0.09079253 [ 0.0517811 , 0.1259961 ]


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


## EN / BFI extraversion comparison
lm_sscomp_bfie <- with(d, lm(SS_comp ~ BFI_e))
summary(lm_sscomp_bfie)
lm_sscomp_irq_en_bfie <- with(d, lm(SS_comp ~ IRQ_EN + BFI_e))
summary(lm_sscomp_irq_en_bfie)
anova(lm_sscomp_irq_en, lm_sscomp_irq_en_bfie)
# bootstrap CI
boot_sscomp_irq_en_bfie <- doBootRegression(d, SS_comp ~ IRQ_EN + BFI_e, mixedEffects = FALSE, numberOfIterations = 1000)
# Coefficient : IRQ_EN 0.07901554 [ 0.04606009 , 0.117781 ]
# Coefficient : BFI_e 0.02880273 [ 0.008023895 , 0.04788249 ]


# EN/COPE_e comparison
lm_sscomp_COPE_e <- with(d, lm(SS_comp ~ COPE_e))
summary(lm_sscomp_COPE_e)
lm_sscomp_irq_en_COPE_e <- with(d, lm(SS_comp ~ IRQ_EN + COPE_e))
summary(lm_sscomp_irq_en_COPE_e)
anova(lm_sscomp_irq_en, lm_sscomp_irq_en_COPE_e)
# bootstrap CI
boot_sscomp_irq_en_COPE_e <- doBootRegression(d, SS_comp ~ IRQ_EN + COPE_e, mixedEffects = FALSE, numberOfIterations = 1000)
# Coefficient : IRQ_EN 0.03629352 [ -0.006254122 , 0.07451135 ]
# Coefficient : COPE_e 0.277688 [ 0.1716604 , 0.3898845 ]


# EN/COPE_i comparison
lm_sscomp_COPE_i <- with(d, lm(SS_comp ~ COPE_i))
summary(lm_sscomp_COPE_i)
lm_sscomp_irq_en_COPE_i <- with(d, lm(SS_comp ~ IRQ_EN + COPE_i))
summary(lm_sscomp_irq_en_COPE_i)
anova(lm_sscomp_irq_en, lm_sscomp_irq_en_COPE_i)
# bootstrap CI
boot_sscomp_irq_en_COPE_i <- doBootRegression(d, SS_comp ~ IRQ_EN + COPE_i, mixedEffects = FALSE, numberOfIterations = 1000)
# Coefficient : IRQ_EN 0.06183654 [ 0.02015688 , 0.1026819 ]
# Coefficient : COPE_i 0.1571219 [ 0.04919081 , 0.2745091 ]


# EN/BEQ_n comparison
lm_sscomp_BEQ_n <- with(d, lm(SS_comp ~ BEQ_n))
summary(lm_sscomp_BEQ_n)
lm_sscomp_irq_en_BEQ_n <- with(d, lm(SS_comp ~ IRQ_EN + BEQ_n))
summary(lm_sscomp_irq_en_BEQ_n)
anova(lm_sscomp_irq_en, lm_sscomp_irq_en_BEQ_n)
# bootstrap CI
boot_sscomp_irq_en_BEQ_n <- doBootRegression(d, SS_comp ~ IRQ_EN + BEQ_n, mixedEffects = FALSE, numberOfIterations = 1000)
# Coefficient : IRQ_EN 0.09384148 [ 0.05815973 , 0.1323415 ]
# Coefficient : BEQ_n -0.01428678 [ -0.03852584 , 0.01199721 ]


# EN/AAS_c comparison
lm_sscomp_AAS_c <- with(d, lm(SS_comp ~ AAS_c))
summary(lm_sscomp_AAS_c)
lm_sscomp_irq_en_AAS_c <- with(d, lm(SS_comp ~ IRQ_EN + AAS_c))
summary(lm_sscomp_irq_en_AAS_c)
anova(lm_sscomp_irq_en, lm_sscomp_irq_en_AAS_c)
# bootstrap CI
boot_sscomp_irq_en_AAS_c <- doBootRegression(d, SS_comp ~ IRQ_EN + AAS_c, mixedEffects = FALSE, numberOfIterations = 1000)
# Coefficient : IRQ_EN 0.06556059 [ 0.02248322 , 0.1095832 ]
# Coefficient : AAS_c 0.04471208 [ 0.01252418 , 0.07980451 ]
