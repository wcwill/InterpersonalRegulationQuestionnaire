### 2 - IRQ_preval_EP plot: Support ratings by IRQ-EP
# run after '1_IRQ_preval_EP_expshare' setup

library(ggplot2)

ggplot(d, aes(x=IRQ_EP, y=SS_comp)) +
  theme_bw(base_size = 18) +
  geom_point(shape=19, alpha=1/4, position=position_jitter(width=.5,height=.5)) +    
  geom_smooth(method=lm, size=1.5, colour = "#3182BD", fill = "#9ECAE1") +
  scale_x_continuous(breaks=seq(4, 28, 8)) +
  scale_y_continuous(breaks=seq(1, 7, 2)) +
  coord_cartesian(ylim=c(1,7), xlim=c(4,28)) +
  xlab("IRQ-PE") +
  ylab("Composite support efficacy ratings\n") +
  ggtitle("Perceived support efficacy by\n IRQ-PE: Positive events\n")
# save as PDF (custom: 6" x 6.5")
