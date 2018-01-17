### 4 - IRQ demographics
setwd("")
source('doBootstrap.R')

#load clean data
d <- read.csv('IRQ_clean.csv')
d <- subset(d, select = c(age,sex,ethnicity,X1,X3,X7,X9,X15,X24,X31,X38,X41,X43,X47,X48,X60,X62,X64,X79))

d$irq_tn <- with(d, X3 + X31 + X47 + X79)
d$irq_en <- with(d, X43 + X60 + X62 + X64)
d$irq_tp <- with(d, X24 + X38 + X41 + X48)
d$irq_ep <- with(d, X1 + X7 + X9 + X15)
d$irq_total <- with(d, (irq_tn + irq_en + irq_tp + irq_ep)/4)


###---Demographics analyses---###
# age
lm_age_total <- with(d, lm(irq_total ~ age))
summary(lm_age_total)
boot_age_total <- doBootRegression(d, irq_total ~ age, mixedEffects = FALSE, numberOfIterations = 1000)
# Coefficient : age -0.01009957 [ -0.05945188 , 0.03395985 ]

lm_age_tn <- with(d, lm(irq_tn ~ age))
summary(lm_age_tn)
boot_age_tn <- doBootRegression(d, irq_tn ~ age, mixedEffects = FALSE, numberOfIterations = 1000)
# Coefficient : age -0.03079878 [ -0.0923394 , 0.02995766 ]

lm_age_en <- with(d, lm(irq_en ~ age))
summary(lm_age_en)
boot_age_en <- doBootRegression(d, irq_en ~ age, mixedEffects = FALSE, numberOfIterations = 1000)
# Coefficient : age -0.008632746 [ -0.06343602 , 0.04475089 ]

lm_age_tp <- with(d, lm(irq_tp ~ age))
summary(lm_age_tp)
boot_age_tp <- doBootRegression(d, irq_tp ~ age, mixedEffects = FALSE, numberOfIterations = 1000)
# Coefficient : age 0.008741805 [ -0.04928641 , 0.06805566 ]

lm_age_ep <- with(d, lm(irq_ep ~ age))
summary(lm_age_ep)
boot_age_ep <- doBootRegression(d, irq_ep ~ age, mixedEffects = FALSE, numberOfIterations = 1000)
# Coefficient : age -0.009787231 [ -0.05791907 , 0.03205301 ]


#sex
d$sex[d$sex == 1] <- 0 #male
d$sex[d$sex == 2] <- 1 #female
d$sex <- as.factor(d$sex)

lm_sex_total <- with(d, lm(irq_total ~ sex))
summary(lm_sex_total)
boot_sex_total <- doBootRegression(d, irq_total ~ sex, mixedEffects = FALSE, numberOfIterations = 1000)
# Coefficient : sex1 2.211083 [ 1.301026 , 3.164383 ]

lm_sex_tn <- with(d, lm(irq_tn ~ sex))
summary(lm_sex_tn)
boot_sex_tn <- doBootRegression(d, irq_tn ~ sex, mixedEffects = FALSE, numberOfIterations = 1000)
# Coefficient : sex1 2.623805 [ 1.097103 , 3.928651 ]

lm_sex_en <- with(d, lm(irq_en ~ sex))
summary(lm_sex_en)
boot_sex_en <- doBootRegression(d, irq_en ~ sex, mixedEffects = FALSE, numberOfIterations = 1000)
# Coefficient : sex1 2.396921 [ 1.405324 , 3.349466 ]

lm_sex_tp <- with(d, lm(irq_tp ~ sex))
summary(lm_sex_tp)
boot_sex_tp <- doBootRegression(d, irq_tp ~ sex, mixedEffects = FALSE, numberOfIterations = 1000)
# Coefficient : sex1 3.081404 [ 1.800807 , 4.333694 ]

lm_sex_ep <- with(d, lm(irq_ep ~ sex))
summary(lm_sex_ep)
boot_sex_ep <- doBootRegression(d, irq_ep ~ sex, mixedEffects = FALSE, numberOfIterations = 1000)
# Coefficient : sex1 0.8753835 [ -0.1847452 , 1.862322 ]


#ethnicity
d$ethnicity[d$ethnicity == 5] <- 0 # white
d$ethnicity[d$ethnicity > 0] <- 1 # non-white
d$ethnicity <- as.factor(d$ethnicity)

lm_ethnicity_total <- with(d, lm(irq_total ~ ethnicity))
summary(lm_ethnicity_total)
boot_ethnicity_total <- doBootRegression(d, irq_total ~ ethnicity, mixedEffects = FALSE, numberOfIterations = 1000)
# Coefficient : ethnicity1 0.5822806 [ -0.5299492 , 1.773739 ]

lm_ethnicity_tn <- with(d, lm(irq_tn ~ ethnicity))
summary(lm_ethnicity_tn)
boot_ethnicity_tn <- doBootRegression(d, irq_tn ~ ethnicity, mixedEffects = FALSE, numberOfIterations = 1000)
# Coefficient : ethnicity1 0.4624632 [ -1.193074 , 2.073867 ]

lm_ethnicity_en <- with(d, lm(irq_en ~ ethnicity))
summary(lm_ethnicity_en)
boot_ethnicity_en <- doBootRegression(d, irq_en ~ ethnicity, mixedEffects = FALSE, numberOfIterations = 1000)
# Coefficient : ethnicity1 -0.04493208 [ -1.286545 , 1.25414 ]

lm_ethnicity_tp <- with(d, lm(irq_tp ~ ethnicity))
summary(lm_ethnicity_tp)
boot_ethnicity_tp <- doBootRegression(d, irq_tp ~ ethnicity, mixedEffects = FALSE, numberOfIterations = 1000)
# Coefficient : ethnicity1 0.7936826 [ -0.7381038 , 2.228926 ]

lm_ethnicity_ep <- with(d, lm(irq_ep ~ ethnicity))
summary(lm_ethnicity_ep)
boot_ethnicity_ep <- doBootRegression(d, irq_ep ~ ethnicity, mixedEffects = FALSE, numberOfIterations = 1000)
# Coefficient : ethnicity1 1.20207 [ 0.06750798 , 2.378383 ]
     