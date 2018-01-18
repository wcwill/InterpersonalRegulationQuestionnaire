### 2 - IRQ_preval_TP setup script
setwd("")

library(reshape2)
library(plyr)

# load choice and task data
d_choiceSection <- read.csv('IRQ_preval_TP_choiceSection_raw.csv')
d_taskSection <- read.csv('IRQ_preval_TP_taskSection_raw.csv')


###--- taskSection ---###
# calculate mean task ratings (task_mrate)
d_task_c2 <- dcast(d_taskSection, workerid ~ n, value.var="response")
d_task_c2$task_mrate <- rowMeans(d_task_c2[2:21], na.rm=TRUE)
d_task_mrate <- as.data.frame(c(d_task_c2[1], d_task_c2[22]))

# subset choice data and append rsum
d_choice <- merge(d_choiceSection, d_task_mrate, by="workerid")

# write to CSV file
write.csv(d_choice, "IRQ_preval_TP_choice_proc.csv", row.names = FALSE)
