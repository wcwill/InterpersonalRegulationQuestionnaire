# 2d - IRQ convergent/discriminant validity: IRQ-TP scores
setwd("")

library(psych)
library(GPArotation)

# load data
d <- read.csv('IRQ_valrel_proc.csv')

# generate IRQ subscales and total scale scores
d$IRQ_TN <- with(d, rowSums(d[8:11]))
d$IRQ_EN <- with(d, rowSums(d[12:15]))
d$IRQ_TP <- with(d, rowSums(d[16:19]))
d$IRQ_EP <- with(d, rowSums(d[20:23]))
d$IRQ_total <- with(d, rowSums(d[62:65]))


### correlate IRQ scores with convergent/discriminant measures
# Adult Attachment Scale
AAS <- with(d, cor.test(IRQ_TP, AAS_close_sum))

# AQ
AQ <- with(d, cor.test(IRQ_TP, AQ_avg))

# BEQ
BEQ_p <- with(d, cor.test(IRQ_TP, BEQ_pex))
BEQ_n <- with(d, cor.test(IRQ_TP, BEQ_nex))
BEQ_s <- with(d, cor.test(IRQ_TP, BEQ_str))

# BFI - Agreeableness
BFI_a <- with(d, cor.test(IRQ_TP, BFI_a))

# BFI - Conscientiousness
BFI_c <- with(d, cor.test(IRQ_TP, BFI_c))

# BFI - Extraversion
BFI_e <- with(d, cor.test(IRQ_TP, BFI_e))

# BFI - Neuroticism
BFI_n <- with(d, cor.test(IRQ_TP, BFI_n))

# BFI - Openness
BFI_o <- with(d, cor.test(IRQ_TP, BFI_o))

# Barratt Impulsiveness - Self-control
BIS_sc <- with(d, cor.test(IRQ_TP, BIS11_selfcontrol))

# BIS/BAS - Reward responsiveness
BISBAS_rr <- with(d, cor.test(IRQ_TP, BASBAS_drive))

# COPE - active
COPE_a <- with(d, cor.test(IRQ_TP, COPE_active))

# COPE - distraction
COPE_d <- with(d, cor.test(IRQ_TP, COPE_distraction))

# COPE - emotional
COPE_e <- with(d, cor.test(IRQ_TP, COPE_emotional))

# COPE - instrumental
COPE_i <- with(d, cor.test(IRQ_TP, COPE_instrumental)) 

# COPE - venting
COPE_v <- with(d, cor.test(IRQ_TP, COPE_venting)) 

# ERQ - reappraisal
ERQ_r <- with(d, cor.test(IRQ_TP, Reappraisal_avg))

# ERQ - suppression
ERQ_s <- with(d, cor.test(IRQ_TP, Suppression_avg))

# IRI - empathic concern
IRI_ec <- with(d, cor.test(IRQ_TP, IRI_EC)) 

# IRI - perspective-taking
IRI_pt <- with(d, cor.test(IRQ_TP, IRI_PT)) 

# Loneliness
LON <- with(d, cor.test(IRQ_TP, Loneliness_tot)) 

# Need to belong
NTB <- with(d, cor.test(IRQ_TP, NTB_sum)) 

# PANAS - positive
PANAS_p <- with(d, cor.test(IRQ_TP, PANAS_positive))

# PANAS - negative
PANAS_n <- with(d, cor.test(IRQ_TP, PANAS_negative)) 

# Perceived Stress Scale
PSS <- with(d, cor.test(IRQ_TP, PSS_sum))

# Rejection Sensitivity
RS <- with(d, cor.test(IRQ_TP, RS_total)) 

# Social Interaction Anxiety
SIAS <- with(d, cor.test(IRQ_TP, SIAS_total))

# Social Desirability
SD <- with(d, cor.test(IRQ_TP, SSDS_total))

# Subjective Social Status
SSS <- with(d, cor.test(IRQ_TP, SSS_A))
