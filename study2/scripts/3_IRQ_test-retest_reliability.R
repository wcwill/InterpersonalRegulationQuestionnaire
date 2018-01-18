### 3 - IRQ test-retest reliability analyses
setwd("")

# load datasets
# d1 <- read.csv('IRQ_valrel_proc.csv')
# d2 <- read.csv('IRQ_valrel2_proc.csv')

# generate IRQ subscales and total scale scores
# d1$IRQ1_TN <- with(d1, rowSums(d1[8:11]))
# d1$IRQ1_EN <- with(d1, rowSums(d1[12:15]))
# d1$IRQ1_TP <- with(d1, rowSums(d1[16:19]))
# d1$IRQ1_EP <- with(d1, rowSums(d1[20:23]))
# d1$IRQ1_total <- with(d1, rowSums(d1[62:65]))
# 
# d2$IRQ2_TN <- with(d2, rowSums(d2[2:5]))
# d2$IRQ2_EN <- with(d2, rowSums(d2[6:9]))
# d2$IRQ2_TP <- with(d2, rowSums(d2[10:13]))
# d2$IRQ2_EP <- with(d2, rowSums(d2[14:17]))
# d2$IRQ2_total <- with(d2, rowSums(d2[18:21]))

# merge datasets
# d <- merge(d1, d2, by = "MTurkID")

# write CSV with merged data
# write.csv(d, "IRQ_valrel_merged.csv", row.names = FALSE)

d <- read.csv('IRQ_valrel_merged.csv')

# demographics
with(d, mean(Age)) # M = 36.74
with(d, sd(Age)) # SD = 10.84
with(d, table(Gender)) # 46.96% male
with(d, table(Race)) # 79.13% caucasian


### Analyses
# IRQ_TN
with(d, cor.test(IRQ1_TN, IRQ2_TN)) # .77
# IRQ_EN
with(d, cor.test(IRQ1_EN, IRQ2_EN)) # .77
# IRQ_TP
with(d, cor.test(IRQ1_TP, IRQ2_TP)) # .77
# IRQ_EP
with(d, cor.test(IRQ1_EP, IRQ2_EP)) # .80
# IRQ_total
with(d, cor.test(IRQ1_total, IRQ2_total)) # .84

