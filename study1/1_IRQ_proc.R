### 1 - IRQ processing
library(psych)
library(GPArotation)
setwd("")

#load both raw and relabeled data
d <- read.csv('IRQ_labels.csv')
d2 <- read.csv('IRQ_raw.csv')
d2 <- d2[-1,]

#replace labeled data with full timestamp info from raw data
d$StartDate <- d2$V8
d$EndDate <- d2$V9
rm(d2)

#demographics
mean(d$age) #31.98
sd(d$age) # 11.32
table(d$sex) #58.60% male
table(d$ethnicity) #73.68% caucasian

#calculating RTs
d$StartDate2 <- strptime(d$StartDate, "%Y-%m-%d  %H:%M:%OS")
d$EndDate2 <- strptime(d$EndDate, "%Y-%m-%d  %H:%M:%OS")
d$rt <- d$EndDate2 - d$StartDate2
d$rt <- as.numeric(d$rt)

mean(d$rt) #11.78
sd(d$rt) # 6.66

quantile(d$rt,seq(0,1,.1,)) # split time by 10%
unique(d[d$rt<5.277,'ResponseID'])
qplot(rt,data=d[d$rt<5.277,],geom='histogram')

# remove 29 cases in bottom 10% of RT (less than 5.277 mins)
d <- subset(d, rt > 5.277)

# reversing negative items
d$X17 = 8 - d$X17
d$X18 = 8 - d$X18
d$X19 = 8 - d$X19
d$X20 = 8 - d$X20
d$X21 = 8 - d$X21
d$X22 = 8 - d$X22
d$X27 = 8 - d$X27
d$X28 = 8 - d$X28
d$X44 = 8 - d$X44
d$X45 = 8 - d$X45
d$X46 = 8 - d$X46
d$X49 = 8 - d$X49
d$X50 = 8 - d$X50
d$X51 = 8 - d$X51
d$X52 = 8 - d$X52
d$X53 = 8 - d$X53
d$X56 = 8 - d$X56
d$X68 = 8 - d$X68
d$X70 = 8 - d$X70
d$X71 = 8 - d$X71
d$X72 = 8 - d$X72
d$X73 = 8 - d$X73
d$X74 = 8 - d$X74
d$X76 = 8 - d$X76
d$X81 = 8 - d$X81
d$X82 = 8 - d$X82

# write CSV with processed data
write.csv(d, "IRQ_clean.csv", row.names = FALSE)
