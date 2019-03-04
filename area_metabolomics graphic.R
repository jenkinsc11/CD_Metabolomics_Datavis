library(RColorBrewer)
library(ggplot2)
library(dplyr)
negative_data <- read.csv("negative_runs.csv",check.names=FALSE)
positive_data <- read.csv("positive_runs.csv",check.names=FALSE)

positive_group_area_fields <- c("Name","Group Area: 1","Group Area: 2","Group Area: 3","Group Area: 4","Group Area: 5","Group Area: 6")
negative_group_area_fields <- c("Name","Group Area: Neg_1","Group Area: Neg_2","Group Area: Neg_3","Group Area: Neg_4","Group Area: Neg_5","Group Area: Neg_6")


positive_group_area <- positive_data[positive_group_area_fields]
negative_group_area <- negative_data[negative_group_area_fields]



colnames(positive_group_area) <- c('Compound_Name','Area_1','Area_2','Area_3','Area_4','Area_5','Area_6')
colnames(negative_group_area) <- c('Compound_Name','Area_1','Area_2','Area_3','Area_4','Area_5','Area_6')


area_1_pos <- positive_group_area[order(positive_group_area$Area_1),]
area_1_pos <- area_1_pos %>% distinct(Compound_Name, .keep_all = TRUE)
area_1_pos <- tail(area_1_pos,10)
top_1 <- area_1_pos[,c(1,2)]
colnames(top_1) <- c("Compound_Name","Area")
top_1$set <-"Sample_1"

area_2_pos <- positive_group_area[order(positive_group_area$Area_2),]
area_2_pos <- area_2_pos %>% distinct(Compound_Name, .keep_all = TRUE)
area_2_pos <- tail(area_2_pos,10)
top_2 <- area_2_pos[,c(1,3)]
colnames(top_2) <- c("Compound_Name","Area")
top_2$set <-"Sample_2"


area_3_pos <- positive_group_area[order(positive_group_area$Area_3),]
area_3_pos <- area_3_pos %>% distinct(Compound_Name, .keep_all = TRUE)
area_3_pos <- tail(area_3_pos,10)
top_3 <- area_3_pos[,c(1,4)]
colnames(top_3) <- c("Compound_Name","Area")
top_3$set <-"Sample_3"


area_4_pos <- positive_group_area[order(positive_group_area$Area_4),]
area_4_pos <- area_4_pos %>% distinct(Compound_Name, .keep_all = TRUE)
area_4_pos <- tail(area_4_pos,10)
top_4 <- area_4_pos[,c(1,5)]
colnames(top_4) <- c("Compound_Name","Area")
top_4$set <-"Sample_4"


area_5_pos <- positive_group_area[order(positive_group_area$Area_5),]
area_5_pos <- area_5_pos %>% distinct(Compound_Name, .keep_all = TRUE)
area_5_pos <- tail(area_5_pos,10)
top_5 <- area_5_pos[,c(1,6)]
colnames(top_5) <- c("Compound_Name","Area")
top_5$set <-"Sample_5"



area_6_pos <- positive_group_area[order(positive_group_area$Area_6),]
area_6_pos <- area_6_pos %>% distinct(Compound_Name, .keep_all = TRUE)
area_6_pos <- tail(area_6_pos,10)
top_6 <- area_6_pos[,c(1,7)]
colnames(top_6) <- c("Compound_Name","Area")
top_6$set <-"Sample_6"

top_all_pos <- bind_rows(top_1, top_2,top_3,top_4,top_5,top_6)

topplot_pos = ggplot(top_all_pos, aes(x=Compound_Name, y=Area, fill = Compound_Name )) + geom_bar(stat="identity")
topplot_pos = topplot_pos + theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_blank(),legend.position="bottom", legend.text = element_text(face="bold"))
topplot_pos <- topplot_pos + ggtitle("Top Metabolites per Set by Area Positive") + xlab("") + ylab("Area")


topplot_pos + facet_wrap( ~ set, scales = "free")

area_1_neg <- negative_group_area[order(negative_group_area$Area_1),]
area_1_neg <- area_1_neg %>% distinct(Compound_Name, .keep_all = TRUE)
area_1_neg <- tail(area_1_neg,10)
top_1 <- area_1_neg[,c(1,2)]
colnames(top_1) <- c("Compound_Name","Area")
top_1$set <-"Sample_1"

area_2_neg <- negative_group_area[order(negative_group_area$Area_2),]
area_2_neg <- area_2_neg %>% distinct(Compound_Name, .keep_all = TRUE)
area_2_neg <- tail(area_2_neg,10)
top_2 <- area_2_neg[,c(1,3)]
colnames(top_2) <- c("Compound_Name","Area")
top_2$set <-"Sample_2"


area_3_neg <- negative_group_area[order(negative_group_area$Area_3),]
area_3_neg <- area_3_neg %>% distinct(Compound_Name, .keep_all = TRUE)
area_3_neg <- tail(area_3_neg,10)
top_3 <- area_3_neg[,c(1,4)]
colnames(top_3) <- c("Compound_Name","Area")
top_3$set <-"Sample_3"


area_4_neg <- negative_group_area[order(negative_group_area$Area_4),]
area_4_neg <- area_4_neg %>% distinct(Compound_Name, .keep_all = TRUE)
area_4_neg <- tail(area_4_neg,10)
top_4 <- area_4_neg[,c(1,5)]
colnames(top_4) <- c("Compound_Name","Area")
top_4$set <-"Sample_4"


area_5_neg <- negative_group_area[order(negative_group_area$Area_5),]
area_5_neg <- area_5_neg %>% distinct(Compound_Name, .keep_all = TRUE)
area_5_neg <- tail(area_5_neg,10)
top_5 <- area_5_neg[,c(1,6)]
colnames(top_5) <- c("Compound_Name","Area")
top_5$set <-"Sample_5"



area_6_neg <- negative_group_area[order(negative_group_area$Area_6),]
area_6_neg <- area_6_neg %>% distinct(Compound_Name, .keep_all = TRUE)
area_6_neg <- tail(area_6_neg,10)
top_6 <- area_6_neg[,c(1,7)]
colnames(top_6) <- c("Compound_Name","Area")
top_6$set <-"Sample_6"

top_all_neg <- bind_rows(top_1, top_2,top_3,top_4,top_5,top_6)

topplot_neg = ggplot(top_all_neg, aes(x=Compound_Name, y=Area, fill = Compound_Name )) + geom_bar(stat="identity")
topplot_neg = topplot_neg + theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_blank(),legend.position="bottom", legend.text = element_text(face="bold"))
topplot_neg <- topplot_neg + ggtitle("Top Metabolites per Set by Area Negative") + xlab("") + ylab("Area")


topplot_neg + facet_wrap( ~ set, scales = "free")
