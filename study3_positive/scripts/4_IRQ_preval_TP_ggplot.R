### 4 - IRQ_preval_TP plot: Affiliation response by IRQ-TP
# run after '3_IRQ_preval_TP_choice' setup

library(ggplot2)
library(reshape2)
source('logithistplot_edit2.R')

# logistic regression with histograms: IRQ-PT
logithistplot(d[,c("irq_tp_total","response")],breaks=10,se=TRUE) + 
  ggtitle("Affiliation by IRQ-PT:\nPositive image-viewing\n") +
  xlab("IRQ-PT") +
  ylab("Probability of affiliation\n") +
  theme(legend.position="none") +
  scale_fill_manual(values=c(NA,NA,"grey","grey","#9ECAE1")) +
  scale_colour_manual(values=c("#3182BD")) +
  scale_x_continuous(breaks=seq(4, 28, 8)) +
  coord_cartesian(ylim=c(0,1), xlim=c(4,28))
# save as PDF (custom: 6" x 6.5")
