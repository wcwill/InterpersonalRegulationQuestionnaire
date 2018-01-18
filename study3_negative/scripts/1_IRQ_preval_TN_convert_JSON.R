### 1 - IRQ_preval_TN convert JSON script: convert JSON results into long-form R dataframe
setwd("")

library(RJSONIO)
library(plyr)

# import data
d <- read.delim("IRQ_preval_TN.tsv", stringsAsFactors = FALSE)

###--- aggregate choiceSection and questionSection data ---###
d_choice <- c()
for(j in 1:nrow(d)) {
  choiceDF <- ddply(d[j,], c("workerid"), function(x) {
    choiceData <- fromJSON(x$Answer.choiceSection)
    qData <- fromJSON(x$Answer.questionSection)
    data.frame(c(choiceData$data[[1]],qData$data[[1]]))
  })
  d_choice <- rbind(d_choice, choiceDF)
}

# write to CSV file
write.csv(d_choice, "IRQ_preval_TN_choiceSection_raw.csv", row.names = FALSE)


###--- aggregate taskSection data ---###
d_taskSection <- c()
for(j in 1:nrow(d)) {
  mathDF <- ddply(d[j,], c("workerid"), function(x) {
    mathData <- fromJSON(x$Answer.taskSection)
    rbind.fill(lapply(mathData$data, function(f) { as.data.frame(Filter(Negate(is.null), f)) }))
  })
  d_taskSection <- rbind(d_taskSection, mathDF)
}

# write to CSV file
write.csv(d_taskSection, "IRQ_preval_TN_taskSection_raw.csv", row.names = FALSE)
