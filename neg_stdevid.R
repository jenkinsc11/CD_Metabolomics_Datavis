library(RColorBrewer)
library(ggplot2)
library(dplyr)

compound_data <- read.csv("negative_runs.csv")

theme_set(theme_classic())

#least similar and most similar and compounds that changed the most 

similar <- c("Name","Formula","Group.Area..Neg_1","Group.Area..Neg_4")
similar_data <- compound_data[similar]
similar_data <-na.omit(similar_data)

simvec <- vector()

for (row in 1:nrow(similar_data)){
  simvec <- c(simvec,sd(similar_data[row,c(3,4)]))
}
similar_data$stdev <- simvec
similar_data <- similar_data[order(similar_data$stdev),]

lowest <- head(similar_data,10)
largest <- tail(similar_data,10)
largest$set <- "largest"
lowest$set <- "lowest"

final_data <- rbind(lowest,largest)

colourCount = length(unique(final_data$Name))
getPalette = colorRampPalette(brewer.pal(9, "Set1"))

final_neg = ggplot(final_data, aes(x=Name, y=stdev, fill = Name )) + geom_bar(stat="identity") +  scale_fill_manual(values = getPalette(colourCount))
final_neg = final_neg + theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_blank(),legend.position="bottom", legend.text = element_text(face="bold"))
final_neg <- final_neg + ggtitle("Stdev Between Sets 1&4 Negative") + xlab("") + ylab("stdev")

final_neg + facet_wrap( ~ set, scales = "free")


different <- c("Name","Formula","Group.Area..Neg_2","Group.Area..Neg_6")
different_data <- compound_data[different]
different_data <-na.omit(different_data)

difvec <- vector()

for (row in 1:nrow(different_data)){
  difvec <- c(difvec,sd(different_data[row,c(3,4)]))
}
different_data$stdev <- difvec
different_data <- different_data[order(different_data$stdev),]

lowest <- head(different_data,10)
largest <- tail(different_data,10)
largest$set <- "largest"
lowest$set <- "lowest"

final_data <- rbind(lowest,largest)

final_neg = ggplot(final_data, aes(x=Name, y=stdev, fill = Name )) + geom_bar(stat="identity") +  scale_fill_manual(values = getPalette(colourCount))
final_neg = final_neg + theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_blank(),legend.position="bottom", legend.text = element_text(face="bold"))
final_neg <- final_neg + ggtitle("Stdev Between Sets 2&6 Negative") + xlab("") + ylab("stdev")

final_neg + facet_wrap( ~ set, scales = "free")
