### 2 - IRQ_preval_EN plot: Support ratings by IRQ-EN
library(ggplot2)

ggplot(d, aes(x=IRQ_EN, y=SS_comp)) +
  theme_bw(base_size = 18) +
  geom_point(shape=19, alpha=1/4, position=position_jitter(width=.5,height=.5)) +    
  geom_smooth(method=lm, size=1.5, colour = "#3182BD", fill = "#9ECAE1") +
  scale_x_continuous(breaks=seq(4, 28, 8)) +
  scale_y_continuous(breaks=seq(1, 7, 2)) +
  coord_cartesian(ylim=c(1,7), xlim=c(4,28)) +
  xlab("IRQ-NE") +
  ylab("Composite support efficacy ratings\n") +
  ggtitle("Perceived support efficacy by\n IRQ-NE: Negative events\n")
# save as PDF (custom: 6" x 6.5")
