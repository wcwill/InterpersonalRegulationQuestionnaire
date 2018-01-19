# IRQ social network nomination analyses
setwd("")

library(lme4)
library(lavaan)
library(reshape2)
library(psych)
library(GPArotation)
source('doBootstrap.R')

# import data
dNodesTime <- read.csv('dNodesTime.csv', header=T)

###--- subset and reorder variables of interest ---###
vars <- c("ID", "time", "IRQ_TN", "IRQ_TP", "IRQ_EN", "IRQ_EP", "IRQ_TOTAL", "outDegreeFullQ3", "outDegreeFullQ4", "outDegreeFullQ5", "outDegreeFullQ6", "outDegreeFullQ7", "outDegreeFullQ8", "BFI_e") 
dNodesTime <- dNodesTime[vars]

# generate subsets
dNodes0 <- subset(dNodesTime, dNodesTime$time==0) #first time point.
dNodes1 <- subset(dNodesTime, dNodesTime$time==1) #second time point.

# rename variables by timepoint
vars0 <- c("ID", "time", "IRQ_TN", "IRQ_TP", "IRQ_EN", "IRQ_EP", "IRQ_TOTAL", "outDegreeFullQ3_T0", "outDegreeFullQ4_T0", "outDegreeFullQ5_T0", "outDegreeFullQ6_T0", "outDegreeFullQ7_T0", "outDegreeFullQ8_T0", "BFI_e") 
vars1 <- c("ID", "time", "IRQ_TN", "IRQ_TP", "IRQ_EN", "IRQ_EP", "IRQ_TOTAL", "outDegreeFullQ3_T1", "outDegreeFullQ4_T1", "outDegreeFullQ5_T1", "outDegreeFullQ6_T1", "outDegreeFullQ7_T1", "outDegreeFullQ8_T1", "BFI_e") 

colnames(dNodes0) <- vars0
colnames(dNodes1) <- vars1

# merge by ID
dNodes_T01 <- merge(dNodes0, dNodes1, by = "ID")

# generate composite nomination variables
dNodes_T01$outDegreeFull_tendency_comp_T0 <- (dNodes_T01$outDegreeFullQ3_T0 + dNodes_T01$outDegreeFullQ4_T0 + dNodes_T01$outDegreeFullQ5_T0)/3
dNodes_T01$outDegreeFull_tendency_comp_T1 <- (dNodes_T01$outDegreeFullQ3_T1 + dNodes_T01$outDegreeFullQ4_T1 + dNodes_T01$outDegreeFullQ5_T1)/3
dNodes_T01$outDegreeFull_efficacy_comp_T0 <- (dNodes_T01$outDegreeFullQ6_T0 + dNodes_T01$outDegreeFullQ7_T0 + dNodes_T01$outDegreeFullQ8_T0)/3
dNodes_T01$outDegreeFull_efficacy_comp_T1 <- (dNodes_T01$outDegreeFullQ6_T1 + dNodes_T01$outDegreeFullQ7_T1 + dNodes_T01$outDegreeFullQ8_T1)/3

# generate composite IRQ variables
dNodes_T01$IRQ_TNP <- (dNodes_T01$IRQ_TN.x + dNodes_T01$IRQ_TP.x)/2
dNodes_T01$IRQ_ENP <- (dNodes_T01$IRQ_EN.x + dNodes_T01$IRQ_EP.x)/2
dNodes_T01$IRQ_TOTAL <- (dNodes_T01$IRQ_TN.x + dNodes_T01$IRQ_TP.x + dNodes_T01$IRQ_EN.x + dNodes_T01$IRQ_EP.x)/4


###--- nomination factor analyses ---###
# composite nominations at T0
fa.parallel(dNodes0[8:13])
fd0 <- fa(dNodes0[8:13], 2) # oblique rotation
fd0

# bootstrapped with CIs
fd0_ci <- fa(dNodes0[8:13], 2, n.iter=1000)
fd0_ci

# Coefficients and bootstrapped confidence intervals 
#                      low   MR1 upper   low   MR2 upper
# outDegreeFullQ3_T0 -0.27 -0.10  0.10  0.75  0.86  0.98
# outDegreeFullQ4_T0  0.06  0.20  0.42  0.49  0.72  0.93
# outDegreeFullQ5_T0  0.20  0.40  0.69  0.23  0.46  0.73
# outDegreeFullQ6_T0  0.29  0.53  0.84  0.11  0.32  0.59
# outDegreeFullQ7_T0  0.48  0.65  0.83  0.14  0.25  0.41
# outDegreeFullQ8_T0  0.86  0.94  1.01 -0.26 -0.09  0.15
# 
# Interfactor correlations and bootstrapped confidence intervals 
#          lower  estimate  upper
# MR1-MR2  0.45     0.63    0.76


# composite nominations at T1
fa.parallel(dNodes1[8:13])
fd1 <- fa(dNodes1[8:13], 2) # oblique rotation
fd1

# bootstrapped with CIs
fd1_ci <- fa(dNodes1[8:13], 2, n.iter=1000)
fd1_ci

# Coefficients and bootstrapped confidence intervals 
#                      low   MR1 upper   low   MR2 upper
# outDegreeFullQ3_T1 -0.12  0.20  0.59  0.20  0.54  0.96
# outDegreeFullQ4_T1 -0.35 -0.03  0.59  0.58  0.95  1.17
# outDegreeFullQ5_T1  0.04  0.40  0.79  0.13  0.49  1.07
# outDegreeFullQ6_T1  0.36  0.74  1.10 -0.20  0.14  0.65
# outDegreeFullQ7_T1  0.61  0.91  1.17 -0.42 -0.03  0.44
# outDegreeFullQ8_T1  0.44  0.87  1.16 -0.37 -0.03  0.57
# 
# Interfactor correlations and bootstrapped confidence intervals 
#          lower estimate upper
# MR1-MR2  0.13     0.83   1.2


# reliability at T0
d0_alpha <- subset(dNodes0[8:13])
keys.list0 <- list(factor1 = c(1:3), factor2 = c(4:6))
keys0 <- make.keys(6, keys.list0, item.labels=colnames(d0_alpha)) 
res0 <- score.items(keys0, d0_alpha) 

print(res0, short = FALSE)
res0$item.cor

#calculating CIs for each factor
alpha01 <- 0.83
se01 <- 0.062
CI01_min <- alpha01 - 1.96*se01
CI01_max <- alpha01 + 1.96*se01

alpha02 <- 0.85
se02 <- 0.059
CI02_min <- alpha02 - 1.96*se02
CI02_max <- alpha02 + 1.96*se02


# reliability at T1
d1_alpha <- subset(dNodes1[8:13])
keys.list1 <- list(factor1 = c(1:3), factor2 = c(4:6))
keys1 <- make.keys(6, keys.list1, item.labels=colnames(d1_alpha)) 
res1 <- score.items(keys1, d1_alpha) 

print(res1, short = FALSE)
res1$item.cor

#calculating CIs for each factor
alpha11 <- 0.86
se11 <- 0.058
CI11_min <- alpha11 - 1.96*se11
CI11_max <- alpha11 + 1.96*se11

alpha12 <- 0.89
se12 <- 0.054
CI12_min <- alpha12 - 1.96*se12
CI12_max <- alpha12 + 1.96*se12


###--- within-composite relationships (T0/T1) ---###
# support-seeking
with(dNodes_T01, cor.test(outDegreeFull_tendency_comp_T0, outDegreeFull_tendency_comp_T1)) # R = .57
# perceived support
with(dNodes_T01, cor.test(outDegreeFull_efficacy_comp_T0, outDegreeFull_efficacy_comp_T1)) # R = .60


###--- within-composite IRQ analyses: support-seeking at T0 ---###
## support-seeking by IRQ_TNP at T0
lm_IRQ_TNP_tendency_comp_T0 <- with(dNodes_T01, lm(outDegreeFull_tendency_comp_T0 ~ IRQ_TNP))
summary(lm_IRQ_TNP_tendency_comp_T0)
# bootstrap CIs
boot_tendency_comp_T0_irq_tnp <- doBootRegression(dNodes_T01, outDegreeFull_tendency_comp_T0 ~ IRQ_TNP, mixedEffects = FALSE, numberOfIterations = 1000)
# Coefficient : IRQ_TNP 0.103285 [ 0.0446473 , 0.1555872 ]

## support-seeking by IRQ_TNP and BFI extraversion at T0
lm_tendency_comp_T0_BFIe <- with(dNodes_T01, lm(outDegreeFull_tendency_comp_T0 ~ BFI_e.x))
summary(lm_tendency_comp_T0_BFIe)
lm_IRQ_TNP_tendency_comp_T0_BFIe <- with(dNodes_T01, lm(outDegreeFull_tendency_comp_T0 ~ IRQ_TNP + BFI_e.x))
summary(lm_IRQ_TNP_tendency_comp_T0_BFIe)
# bootstrap CIs
boot_tendency_comp_T0_irq_tnp_bfie <- doBootRegression(dNodes_T01, outDegreeFull_tendency_comp_T0 ~ IRQ_TNP + BFI_e.x, mixedEffects = FALSE, numberOfIterations = 1000)
# Coefficient : IRQ_TNP 0.08575744 [ 0.02571996 , 0.1448602 ]
# Coefficient : BFI_e.x 0.3846196 [ 0.05370836 , 0.7279401 ]
anova(lm_IRQ_TNP_tendency_comp_T0, lm_IRQ_TNP_tendency_comp_T0_BFIe)
anova(lm_tendency_comp_T0_BFIe, lm_IRQ_TNP_tendency_comp_T0_BFIe)

## support-seeking by IRQ_TNP and IRQ-ENP at T0
lm_IRQ_TNP_tendency_comp_T0_IRQ_ENP <- with(dNodes_T01, lm(outDegreeFull_tendency_comp_T0 ~ IRQ_TNP + IRQ_ENP))
summary(lm_IRQ_TNP_tendency_comp_T0_IRQ_ENP)
anova(lm_IRQ_TNP_tendency_comp_T0, lm_IRQ_TNP_tendency_comp_T0_IRQ_ENP)


###--- within-composite IRQ analyses: support-seeking at T1 ---###
## support-seeking by IRQ_TNP at T1
lm_IRQ_TNP_tendency_comp_T1 <- with(dNodes_T01, lm(outDegreeFull_tendency_comp_T1 ~ IRQ_TNP))
summary(lm_IRQ_TNP_tendency_comp_T1)
# bootstrap CIs
boot_tendency_comp_T1_irq_tnp <- doBootRegression(dNodes_T01, outDegreeFull_tendency_comp_T1 ~ IRQ_TNP, mixedEffects = FALSE, numberOfIterations = 1000)
# Coefficient : IRQ_TNP 0.07985868 [ 0.01672763 , 0.1389181 ]

## support-seeking by IRQ_TNP and BFI extraversion at T1
lm_tendency_comp_T1_BFIe <- with(dNodes_T01, lm(outDegreeFull_tendency_comp_T1 ~ BFI_e.x))
summary(lm_tendency_comp_T1_BFIe)
lm_IRQ_TNP_tendency_comp_T1_BFIe <- with(dNodes_T01, lm(outDegreeFull_tendency_comp_T1 ~ IRQ_TNP + BFI_e.x))
summary(lm_IRQ_TNP_tendency_comp_T1_BFIe)
# bootstrap CIs
boot_tendency_comp_T1_irq_tnp_bfie <- doBootRegression(dNodes_T01, outDegreeFull_tendency_comp_T1 ~ IRQ_TNP + BFI_e.x, mixedEffects = FALSE, numberOfIterations = 1000)
# Coefficient : IRQ_TNP 0.06546805 [ 0.002936059 , 0.132395 ]
# Coefficient : BFI_e.x 0.3317926 [ 0.007378561 , 0.6492741 ]
anova(lm_IRQ_TNP_tendency_comp_T1, lm_IRQ_TNP_tendency_comp_T1_BFIe)
anova(lm_tendency_comp_T1_BFIe, lm_IRQ_TNP_tendency_comp_T1_BFIe)

## support-seeking by IRQ_TNP and IRQ-ENP at T1
lm_IRQ_TNP_tendency_comp_T1_IRQ_ENP <- with(dNodes_T01, lm(outDegreeFull_tendency_comp_T1 ~ IRQ_TNP + IRQ_ENP))
summary(lm_IRQ_TNP_tendency_comp_T1_IRQ_ENP)
anova(lm_IRQ_TNP_tendency_comp_T1, lm_IRQ_TNP_tendency_comp_T1_IRQ_ENP)


###--- within-composite IRQ analyses: perceived support at T0 ---###
## perceived support by IRQ_ENP at T0
lm_IRQ_ENP_efficacy_comp_T0 <- with(dNodes_T01, lm(outDegreeFull_efficacy_comp_T0 ~ IRQ_ENP))
summary(lm_IRQ_ENP_efficacy_comp_T0)
# bootstrap CIs
boot_efficacy_comp_T0_irq_enp <- doBootRegression(dNodes_T01, outDegreeFull_efficacy_comp_T0 ~ IRQ_ENP, mixedEffects = FALSE, numberOfIterations = 1000)
# Coefficient : IRQ_ENP 0.1010808 [ 0.01873832 , 0.175915 ]

## perceived support by IRQ_ENP and BFI extraversion at T0
lm_efficacy_comp_T0_BFIe <- with(dNodes_T01, lm(outDegreeFull_efficacy_comp_T0 ~ BFI_e.x))
summary(lm_efficacy_comp_T0_BFIe)
lm_IRQ_ENP_efficacy_comp_T0_BFIe <- with(dNodes_T01, lm(outDegreeFull_efficacy_comp_T0 ~ IRQ_ENP + BFI_e.x))
summary(lm_IRQ_ENP_efficacy_comp_T0_BFIe)
# bootstrap CIs
boot_efficacy_comp_T0_irq_enp_bfie <- doBootRegression(dNodes_T01, outDegreeFull_efficacy_comp_T0 ~ IRQ_ENP + BFI_e.x, mixedEffects = FALSE, numberOfIterations = 1000)
# Coefficient : IRQ_ENP 0.09387062 [ 0.01665461 , 0.1723194 ]
# Coefficient : BFI_e.x 0.09138042 [ -0.28307 , 0.4337542 ]
anova(lm_IRQ_ENP_efficacy_comp_T0, lm_IRQ_ENP_efficacy_comp_T0_BFIe)
anova(lm_efficacy_comp_T0_BFIe, lm_IRQ_ENP_efficacy_comp_T0_BFIe)

## perceived support by IRQ_ENP and IRQ-TNP at T0
lm_IRQ_ENP_efficacy_comp_T0_IRQ_TNP <- with(dNodes_T01, lm(outDegreeFull_efficacy_comp_T0 ~ IRQ_ENP + IRQ_TNP))
summary(lm_IRQ_ENP_efficacy_comp_T0_IRQ_TNP)


###--- within-composite IRQ analyses: perceived support at T1 ---###
## perceived support by IRQ_ENP at T1
lm_IRQ_ENP_efficacy_comp_T1 <- with(dNodes_T01, lm(outDegreeFull_efficacy_comp_T1 ~ IRQ_ENP))
summary(lm_IRQ_ENP_efficacy_comp_T1)
# bootstrap CIs
boot_efficacy_comp_T1_irq_enp <- doBootRegression(dNodes_T01, outDegreeFull_efficacy_comp_T1 ~ IRQ_ENP, mixedEffects = FALSE, numberOfIterations = 1000)
# Coefficient : IRQ_ENP 0.09816995 [ 0.006754619 , 0.1952909 ]

## perceived support by IRQ_ENP and BFI extraversion at T1
lm_efficacy_comp_T1_BFIe <- with(dNodes_T01, lm(outDegreeFull_efficacy_comp_T1 ~ BFI_e.x))
summary(lm_efficacy_comp_T1_BFIe)
lm_IRQ_ENP_efficacy_comp_T1_BFIe <- with(dNodes_T01, lm(outDegreeFull_efficacy_comp_T1 ~ IRQ_ENP + BFI_e.x))
summary(lm_IRQ_ENP_efficacy_comp_T1_BFIe)
# bootstrap CIs
boot_efficacy_comp_T1_irq_enp_bfie <- doBootRegression(dNodes_T01, outDegreeFull_efficacy_comp_T1 ~ IRQ_ENP + BFI_e.x, mixedEffects = FALSE, numberOfIterations = 1000)
# Coefficient : IRQ_ENP 0.08853937 [ 0.001070128 , 0.1849566 ]
# Coefficient : BFI_e.x 0.1724444 [ -0.2057233 , 0.5418766 ]
anova(lm_IRQ_ENP_efficacy_comp_T1, lm_IRQ_ENP_efficacy_comp_T1_BFIe)
anova(lm_efficacy_comp_T1_BFIe, lm_IRQ_ENP_efficacy_comp_T1_BFIe)

## perceived support by IRQ_ENP and IRQ-TNP at T1
lm_IRQ_ENP_efficacy_comp_T1_IRQ_TNP <- with(dNodes_T01, lm(outDegreeFull_efficacy_comp_T1 ~ IRQ_ENP + IRQ_TNP))
summary(lm_IRQ_ENP_efficacy_comp_T1_IRQ_TNP)


###--- across-composite relationships (T0/T1) ---###
# T0/T0
with(dNodes_T01, cor.test(outDegreeFull_tendency_comp_T0, outDegreeFull_efficacy_comp_T0)) # R = .72
# T1/T1
with(dNodes_T01, cor.test(outDegreeFull_tendency_comp_T1, outDegreeFull_efficacy_comp_T1)) # R = .80
# T0/T1
with(dNodes_T01, cor.test(outDegreeFull_tendency_comp_T0, outDegreeFull_efficacy_comp_T1)) # R = .44
# T0/T1
with(dNodes_T01, cor.test(outDegreeFull_efficacy_comp_T0, outDegreeFull_tendency_comp_T1)) # R = .54


###--- across-composite IRQ mediation/indirect effect analyses (T0/T1) ---###
## perceived support by IRQ_TNP
# T0
lm_IRQ_TNP_efficacy_comp_T0 <- with(dNodes_T01, lm(outDegreeFull_efficacy_comp_T0 ~ IRQ_TNP))
summary(lm_IRQ_TNP_efficacy_comp_T0)
# bootstrap CIs
boot_efficacy_comp_T0_irq_tnp <- doBootRegression(dNodes_T01, outDegreeFull_efficacy_comp_T0 ~ IRQ_TNP, mixedEffects = FALSE, numberOfIterations = 1000)
# Coefficient : IRQ_TNP 0.04040832 [ -0.01851825 , 0.09859542 ]

# T1
lm_IRQ_TNP_efficacy_comp_T1 <- with(dNodes_T01, lm(outDegreeFull_efficacy_comp_T1 ~ IRQ_TNP))
summary(lm_IRQ_TNP_efficacy_comp_T1)
# bootstrap CIs
boot_efficacy_comp_T1_irq_tnp <- doBootRegression(dNodes_T01, outDegreeFull_efficacy_comp_T1 ~ IRQ_TNP, mixedEffects = FALSE, numberOfIterations = 1000)
# Coefficient : IRQ_TNP 0.01859664 [ -0.04898359 , 0.08448172 ]


## perceived support (T1) by support-seeking (T0)
lm_efficacy_comp_T1_tendency_comp_T0 <- with(dNodes_T01, lm(outDegreeFull_efficacy_comp_T1 ~ outDegreeFull_tendency_comp_T0))
summary(lm_tendency_comp_T0_efficacy_comp_T1)
# bootstrap CIs
boot_efficacy_comp_T1_tendency_comp_T0 <- doBootRegression(dNodes_T01, outDegreeFull_efficacy_comp_T1 ~ outDegreeFull_tendency_comp_T0, mixedEffects = FALSE, numberOfIterations = 1000)
# Coefficient : outDegreeFull_tendency_comp_T0 0.4723236 [ 0.323448 , 0.6071534 ]


## bootstrapped indirect effect (see Rucker et al., 2011, SPP Compass)
bm_IRQ_TNP_tendency_comp_TO_efficacy_comp_T1 <- bm.bootstrapmed(dNodes_T01$IRQ_TNP, dNodes_T01$outDegreeFull_tendency_comp_T0, dNodes_T01$outDegreeFull_efficacy_comp_T1 ,iterations=1000, alpha=.05)
# **
#           c     c'     a     b    ab Sobel Goodman
# Coeff 0.017 -0.033 0.101 0.487 0.049 2.997   3.049
# p val 0.618  0.283 0.001 0.000    NA 0.003   0.002
# [1] Bootstrap results:
# Mean(ab*) p(ab*<ab) 
#     0.050     0.514 
# [1] Uncorrected:
#  2.5% 97.5% 
# 0.017 0.090 
# [1] Bias Corrected:
#  2.9% 97.9% 
# 0.018 0.090 


## support-seeking by IRQ_ENP
# T0
lm_IRQ_ENP_tendency_comp_T0 <- with(dNodes_T01, lm(outDegreeFull_tendency_comp_T0 ~ IRQ_ENP))
summary(lm_IRQ_ENP_tendency_comp_T0)
# bootstrap CIs
boot_tendency_comp_T0_irq_enp <- doBootRegression(dNodes_T01, outDegreeFull_tendency_comp_T0 ~ IRQ_ENP, mixedEffects = FALSE, numberOfIterations = 1000)
# Coefficient : IRQ_ENP 0.1430325 [ 0.06785695 , 0.2085827 ]

# T1
lm_IRQ_ENP_tendency_comp_T1 <- with(dNodes_T01, lm(outDegreeFull_tendency_comp_T1 ~ IRQ_ENP))
summary(lm_IRQ_ENP_tendency_comp_T1)
# bootstrap CIs
boot_tendency_comp_T1_irq_enp <- doBootRegression(dNodes_T01, outDegreeFull_tendency_comp_T1 ~ IRQ_ENP, mixedEffects = FALSE, numberOfIterations = 1000)
# Coefficient : IRQ_ENP 0.1320806 [ 0.0550375 , 0.2062854 ]


## support-seeking (T1) by perceived support (T0)
lm_tendency_comp_T1_efficacy_comp_T0 <- with(dNodes_T01, lm(outDegreeFull_tendency_comp_T1 ~ outDegreeFull_efficacy_comp_T0))
summary(lm_tendency_comp_T1_efficacy_comp_T0)
# bootstrap CIs
boot_tendency_comp_T1_efficacy_comp_T0 <- doBootRegression(dNodes_T01, outDegreeFull_tendency_comp_T1 ~ outDegreeFull_efficacy_comp_T0, mixedEffects = FALSE, numberOfIterations = 1000)
# Coefficient : outDegreeFull_efficacy_comp_T0 0.5309557 [ 0.4072904 , 0.6480495 ]


# bootstrapped mediation
bm_IRQ_ENP_efficacy_comp_TO_tendency_comp_T1 <- bm.bootstrapmed(dNodes_T01$IRQ_ENP, dNodes_T01$outDegreeFull_efficacy_comp_T0, dNodes_T01$outDegreeFull_tendency_comp_T1 ,iterations=1000, alpha=.05)
# *
#           c    c'     a     b    ab Sobel Goodman
# Coeff 0.133 0.082 0.102 0.504 0.051 2.304   2.335
# p val 0.001 0.024 0.017 0.000    NA 0.021   0.020
# [1] Bootstrap results:
# Mean(ab*) p(ab*<ab) 
#     0.051     0.527 
# [1] Uncorrected:
#  2.5% 97.5% 
# 0.009 0.091 
# [1] Bias Corrected:
#  3.4% 98.2% 
# 0.013 0.096 


###--- IRQ total score analyses (T0/T1) ---###
## support-seeking
# T0
lm_IRQ_total_tendency_comp_T0 <- with(dNodes_T01, lm(outDegreeFull_tendency_comp_T0 ~ IRQ_TOTAL))
summary(lm_IRQ_total_tendency_comp_T0)
# bootstrap CIs
boot_tendency_comp_T0_irq_total <- doBootRegression(dNodes_T01, outDegreeFull_tendency_comp_T0 ~ IRQ_TOTAL, mixedEffects = FALSE, numberOfIterations = 1000)
# Coefficient : IRQ_TOTAL 0.1461312 [ 0.07158633 , 0.2109436 ]

# T1
lm_IRQ_total_tendency_comp_T1 <- with(dNodes_T01, lm(outDegreeFull_tendency_comp_T1 ~ IRQ_TOTAL))
summary(lm_IRQ_total_tendency_comp_T1)
# bootstrap CIs
boot_tendency_comp_T1_irq_total <- doBootRegression(dNodes_T01, outDegreeFull_tendency_comp_T1 ~ IRQ_TOTAL, mixedEffects = FALSE, numberOfIterations = 1000)
# Coefficient : IRQ_TOTAL 0.1222704 [ 0.05492248 , 0.1941541 ]


## perceived support
# T0
lm_IRQ_total_efficacy_comp_T0 <- with(dNodes_T01, lm(outDegreeFull_efficacy_comp_T0 ~ IRQ_TOTAL))
summary(lm_IRQ_total_efficacy_comp_T0)
# bootstrap CIs
boot_efficacy_comp_T0_irq_total <- doBootRegression(dNodes_T01, outDegreeFull_efficacy_comp_T0 ~ IRQ_TOTAL, mixedEffects = FALSE, numberOfIterations = 1000)
# Coefficient : IRQ_TOTAL 0.07975929 [ 0.008533465 , 0.1460309 ]

# T1
lm_IRQ_total_efficacy_comp_T1 <- with(dNodes_T01, lm(outDegreeFull_efficacy_comp_T1 ~ IRQ_TOTAL))
summary(lm_IRQ_total_efficacy_comp_T1)
# bootstrap CIs
boot_efficacy_comp_T1_irq_total <- doBootRegression(dNodes_T01, outDegreeFull_efficacy_comp_T1 ~ IRQ_TOTAL, mixedEffects = FALSE, numberOfIterations = 1000)
# Coefficient : IRQ_TOTAL 0.05959875 [ -0.02447418 , 0.1468329 ]
