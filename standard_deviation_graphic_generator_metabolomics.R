library(RColorBrewer)
library(ggplot2)
compound_data <- read.csv("positive_runs.csv")
fields <- c("Name","Formula","Group.Area..1","Group.Area..2","Group.Area..3",
            "Group.Area..4","Group.Area..5","Group.Area..6")
my_data <- compound_data[fields]


vec <- vector()

for (row in 1:nrow(my_data)){
  vec <- c(vec,sd(my_data[row,c(3,4,5,6,7,8)]))
}

my_data$stdev <- vec

my_data <- my_data[order(my_data$stdev),]

lowestpos <- head(my_data,10)
largestpos <- tail(my_data,10)
lowestpos$set <- "Lowest Positive"
largestpos$set <- "Highest Positive"
pos_sets <- rbind(largestpos,lowestpos)
colnames(pos_sets)<- c('Compound_Name','Formula','Group.Area..1', 'Group.Area..2', 'Group.Area..3', 'Group.Area..4', 'Group.Area..5', 'Group.Area..6', 'stdev','set')


compound_data <- read.csv("negative_runs.csv")
fields <- c("Name","Formula","Group.Area..Neg_1","Group.Area..Neg_2","Group.Area..Neg_3",
            "Group.Area..Neg_4","Group.Area..Neg_5","Group.Area..Neg_6")
my_data <- compound_data[fields]


vec <- vector()

for (row in 1:nrow(my_data)){
  vec <- c(vec,sd(my_data[row,c(3,4,5,6,7,8)]))
}

my_data$stdev <- vec

my_data <- my_data[order(my_data$stdev),]

lowestneg <- head(my_data,10)
largestneg <- tail(my_data,10)
lowestneg$set <- "Lowest Negative"
largestneg$set <- "Highest Negative"
negative_sets <- rbind(lowestneg, largestneg)
colnames(negative_sets)<- c('Compound_Name','Formula','Group.Area..1', 'Group.Area..2', 'Group.Area..3', 'Group.Area..4', 'Group.Area..5', 'Group.Area..6', 'stdev','set')
theme_set(theme_classic())

colourCount = length(unique(pos_sets$Compound_Name))
getPalette = colorRampPalette(brewer.pal(9, "Set1"))

posplot = ggplot(pos_sets, aes(x=Compound_Name, y=stdev, fill = Compound_Name )) + geom_bar(stat="identity") +  scale_fill_manual(values = getPalette(colourCount))
posplot = posplot + theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_blank(),legend.position="bottom",legend.text = element_text(face="bold"))
posplot <- posplot + ggtitle("Standard Deviation Between Positive Sets") + xlab("") + ylab("Standard Deviation")

posplot + facet_wrap( ~ set, scales = "free", ncol=2)

colourCount = length(unique(negative_sets$Compound_Name))
getPalette = colorRampPalette(brewer.pal(12, "Paired"))

negplot = ggplot(negative_sets, aes(x=Compound_Name, y=stdev, fill = Compound_Name )) + geom_bar(stat="identity") +  scale_fill_manual(values = getPalette(colourCount))
negplot = negplot + theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_blank(),legend.position="bottom", legend.text = element_text(face="bold"))
negplot <- negplot + ggtitle("Standard Deviation Between Negative Sets") + xlab("") + ylab("Standard Deviation")



negplot + facet_wrap( ~ set, scales = "free", ncol=2)

