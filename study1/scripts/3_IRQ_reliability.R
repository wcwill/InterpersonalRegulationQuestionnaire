### 3- IRQ reliability analyses
setwd("")

#load clean data
d <- read.csv('IRQ_clean.csv')

### subsetting for IRQ items
d <- subset(d, select = c(X1:X87))


#factor 1
keys.list1 <- list(unique4 = c(3,31,47,79)) # .90
keys1 <- make.keys(87, keys.list1, item.labels=colnames(d)) 
res1 <- score.items(keys1, d)
print(res1)

#calculating CIs from SE for factor 1 'unique4'
alpha1 <- 0.9
se1 <- 0.036
CI1_min <- alpha1 - 1.96*se1
CI1_max <- alpha1 + 1.96*se1


#factor 2
keys.list2 <- list(unique4=c(43,60,62,64)) # .85
keys2 <- make.keys(87, keys.list2, item.labels=colnames(d)) 
res2 <- score.items(keys2, d) 
print(res2)

#calculating CIs from SE for factor 2 'unique4'
alpha2 <- 0.85
se2 <- 0.04
CI2_min <- alpha2 - 1.96*se2
CI2_max <- alpha2 + 1.96*se2


#factor 3
keys.list3 <- list(unique4=c(24,38,41,48)) # .89
keys3 <- make.keys(87, keys.list3, item.labels=colnames(d)) 
res3 <- score.items(keys3, d) 
print(res3)

#calculating CIs from SE for factor 3 'unique4'
alpha3 <- 0.89
se3 <- 0.036
CI3_min <- alpha3 - 1.96*se3
CI3_max <- alpha3 + 1.96*se3


#factor 4
keys.list4 <- list(unique4=c(1,7,9,15)) # .83
keys4 <- make.keys(87, keys.list4, item.labels=colnames(d)) 
res4 <- score.items(keys4, d) 
print(res4)

#calculating CIs from SE for factor 4 'unique4'
alpha4 <- 0.83
se4 <- 0.042
CI4_min <- alpha4 - 1.96*se4
CI4_max <- alpha4 + 1.96*se4
