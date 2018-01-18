### 2 - IRQ factor analyses
library(psych)
library(GPArotation)
setwd("")

#load clean data
d <- read.csv('IRQ_clean.csv')

### Testing ALL items
fa.parallel(d[17:103]) # suggests 6 factors
fd <- fa(d[17:103], 6) # oblique rotation
fd 

plot(fd)
fa.diagram(fd)


### Testing NON-REVERSED items only
dpos <- subset(d, select = c(X1:X16, X23:X26, X29:X43, X47:X48, X54:X55, X57:X67, X69, X75, X77:X80, X83:X87))
fa.parallel(dpos) # suggests 5 factors
fdpos <- fa(dpos, 5) # oblique rotation
fdpos


### Testing REDUCED set of 4 highest loading non-redundant items per factor (16 total)
dpos16 <- subset(dpos, select = c(X1,X3,X7,X9,X15,X24,X31,X38,X41,X43,X47,X48,X60,X62,X64,X79))
fa.parallel(dpos16)
fdpos16 <- fa(dpos16,4)
fdpos16

# bootstrapped with CIs
fdpos16_ci <- fa(dpos16,4,n.iter=1000)
fdpos16_ci

# Coefficients and bootstrapped confidence intervals 
#       low   MR1 upper   low   MR3 upper   low   MR2 upper   low   MR4 upper
# X1   0.01  0.12  0.23 -0.11  0.00  0.13  0.44  0.59  0.75  0.11  0.25  0.40
# X3   0.62  0.74  0.87 -0.05  0.06  0.18 -0.04  0.06  0.16 -0.05  0.07  0.21
# X7   0.22  0.37  0.51 -0.26 -0.12  0.05  0.36  0.51  0.66 -0.22 -0.03  0.17
# X9  -0.10 -0.01  0.08 -0.05  0.04  0.12  0.68  0.84  0.96 -0.06  0.04  0.19
# X15 -0.15 -0.03  0.08  0.01  0.13  0.24  0.55  0.71  0.86 -0.21 -0.05  0.16
# X24  0.07  0.19  0.33  0.43  0.61  0.77 -0.01  0.12  0.25 -0.11  0.00  0.13
# X31  0.56  0.71  0.85 -0.04  0.06  0.18 -0.07  0.02  0.11  0.01  0.11  0.24
# X38 -0.06  0.02  0.13  0.79  0.92  1.01 -0.17 -0.07  0.03 -0.10  0.00  0.13
# X41 -0.18 -0.08  0.06  0.42  0.62  0.77 -0.07  0.06  0.21  0.06  0.22  0.38
# X43  0.00  0.10  0.23  0.01  0.13  0.27 -0.20 -0.08  0.09  0.46  0.73  0.90
# X47  0.67  0.80  0.91 -0.01  0.10  0.21 -0.07  0.03  0.13 -0.22 -0.13  0.01
# X48 -0.02  0.05  0.15  0.63  0.79  0.93  0.01  0.10  0.21 -0.13 -0.01  0.13
# X60  0.04  0.16  0.28 -0.11 -0.01  0.12 -0.02  0.09  0.25  0.42  0.62  0.77
# X62 -0.01  0.13  0.29 -0.12  0.01  0.19 -0.01  0.09  0.22  0.42  0.67  0.83
# X64 -0.26 -0.15 -0.01 -0.02  0.13  0.27  0.11  0.25  0.42  0.37  0.59  0.80
# X79  0.58  0.73  0.87 -0.09  0.02  0.14 -0.12 -0.03  0.05  0.11  0.20  0.30
# 
# Interfactor correlations and bootstrapped confidence intervals 
#          lower estimate upper
# MR1-MR3  0.39     0.59  0.69
# MR1-MR2  0.34     0.44  0.64
# MR1-MR4  0.31     0.54  0.61
# MR3-MR2  0.34     0.47  0.61
# MR3-MR4  0.32     0.56  0.61
# MR2-MR4  0.31     0.53  0.60
