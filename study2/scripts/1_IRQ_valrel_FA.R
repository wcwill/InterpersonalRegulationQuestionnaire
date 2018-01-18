# 1 - IRQ factor structure and reliability replication analyses
setwd("")
library(psych)
library(GPArotation)

# load data
d <- read.csv('IRQ_valrel_proc.csv')

# demographics
with(d, mean(Age)) # M = 35.32
with(d, sd(Age)) # SD = 11.45
with(d, table(Gender)) # 52.16% male
with(d, table(Race)) # 80.12% caucasian


### replicate 16-item IRQ factor structure
dsub <- d[8:23]

fa.parallel(dsub)
fdpos16 <- fa(dsub,4)
fdpos16            
      
# bootstrapped with CIs
fdpos16_ci <- fa(dsub,4,n.iter=1000)
fdpos16_ci

# Coefficients and bootstrapped confidence intervals 
#           low   MR1 upper   low   MR3 upper   low   MR2 upper   low   MR4 upper
# IRQ1_1   0.02  0.13  0.23 -0.09  0.01  0.12  0.63  0.74  0.84 -0.03  0.06  0.16
# IRQ1_2  -0.07  0.01  0.10  0.08  0.17  0.26  0.66  0.77  0.87 -0.18 -0.10 -0.01
# IRQ1_3  -0.10 -0.01  0.10 -0.05  0.04  0.15  0.61  0.72  0.83  0.09  0.17  0.25
# IRQ1_4   0.13  0.25  0.35 -0.05  0.06  0.19  0.41  0.53  0.65 -0.02  0.08  0.19
# IRQ1_5   0.09  0.18  0.28  0.00  0.08  0.19 -0.11 -0.03  0.05  0.58  0.72  0.85
# IRQ1_6  -0.18 -0.05  0.11  0.12  0.27  0.42  0.07  0.18  0.30  0.31  0.47  0.65
# IRQ1_7  -0.08  0.02  0.12  0.02  0.10  0.20  0.07  0.15  0.24  0.55  0.69  0.86
# IRQ1_8  -0.06  0.06  0.19 -0.17 -0.04  0.11  0.35  0.46  0.57  0.33  0.47  0.59
# IRQ1_9  -0.06  0.03  0.14  0.65  0.80  0.93  0.06  0.14  0.22 -0.11 -0.04  0.03
# IRQ1_10 -0.09 -0.01  0.09  0.80  0.91  0.98 -0.07 -0.01  0.07 -0.08 -0.01  0.08
# IRQ1_11  0.06  0.19  0.32  0.35  0.52  0.68  0.06  0.17  0.28 -0.09  0.02  0.13
# IRQ1_12  0.00  0.09  0.20  0.54  0.68  0.80 -0.16 -0.09  0.00  0.16  0.24  0.33
# IRQ1_13  0.53  0.68  0.79 -0.15 -0.04  0.10  0.09  0.19  0.29 -0.19 -0.09  0.02
# IRQ1_14  0.68  0.85  0.96  0.00  0.10  0.20 -0.08 -0.01  0.08 -0.15 -0.06  0.05
# IRQ1_15  0.60  0.74  0.83 -0.12 -0.01  0.10  0.03  0.11  0.20  0.00  0.09  0.20
# IRQ1_16  0.56  0.70  0.82 -0.04  0.07  0.20 -0.21 -0.12 -0.02  0.10  0.21  0.33
# 
# Interfactor correlations and bootstrapped confidence intervals 
#         lower estimate upper
# MR1-MR3  0.49     0.65  0.70
# MR1-MR2  0.46     0.56  0.69
# MR1-MR4  0.33     0.55  0.64
# MR3-MR2  0.44     0.59  0.69
# MR3-MR4  0.35     0.53  0.62
# MR2-MR4  0.34     0.43  0.60

# MR2 = TN
# MR4 = EN
# MR3 = TP
# MR1 = EP


### reliability analyses
# tendency negative (TN)
keys.list.TN <- list(TN=c(1:4))
keysTN <- make.keys(16, keys.list.TN, item.labels=colnames(dsub)) 
resTN <- score.items(keysTN, dsub) 
print(resTN, short = FALSE) # alpha = .89
#calculating CIs
alpha_TN <- 0.89
se_TN <- 0.031
CI_TN_min <- alpha_TN - 1.96*se_TN
CI_TN_max <- alpha_TN + 1.96*se_TN


# efficacy negative (EN)
keys.list.EN <- list(EN=c(5:8))
keysEN <- make.keys(16, keys.list.EN, item.labels=colnames(dsub)) 
resEN <- score.items(keysEN, dsub) 
print(resEN, short = FALSE) # alpha = .86
#calculating CIs
alpha_EN <- 0.86
se_EN <- 0.033
CI_EN_min <- alpha_EN - 1.96*se_EN
CI_EN_max <- alpha_EN + 1.96*se_EN


# tendency positive (TP)
keys.list.TP <- list(TP=c(9:12))
keysTP <- make.keys(16, keys.list.TP, item.labels=colnames(dsub)) 
resTP <- score.items(keysTP, dsub) 
print(resTP, short = FALSE) # alpha = .90
#calculating CIs
alpha_TP <- 0.90
se_TP <- 0.03
CI_TP_min <- alpha_TP - 1.96*se_TP
CI_TP_max <- alpha_TP + 1.96*se_TP


# efficacy positive (EP)
keys.list.EP <- list(EP=c(13:16))
keysEP <- make.keys(16, keys.list.EP, item.labels=colnames(dsub)) 
resEP <- score.items(keysEP, dsub) 
print(resEP, short = FALSE) # alpha = .88
#calculating CIs
alpha_EP <- 0.88
se_EP <- 0.032
CI_EP_min <- alpha_EP - 1.96*se_EP
CI_EP_max <- alpha_EP + 1.96*se_EP
