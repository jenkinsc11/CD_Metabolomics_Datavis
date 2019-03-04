library(RColorBrewer)
library(ggplot2)
library(dplyr)

negative_data <- read.csv("negative_runs.csv",check.names=FALSE)
positive_data <- read.csv("positive_runs.csv",check.names=FALSE)


one_4_fields <- c("Name","Group Area: 1","Group Area: 4",
                  "Ratio: (1) / (4)","Log2 Fold Change: (1) / (4)","Adj. P-value: (1) / (4)")
two_6_fields <- c("Name","Group Area: 2","Group Area: 6",
                  "Ratio: (6) / (2)","Log2 Fold Change: (6) / (2)","Adj. P-value: (6) / (2)")
Neg_one_4_fields <- c("Name","Group Area: Neg_1","Group Area: Neg_4",
                      "Ratio: (Neg_4) / (Neg_1)","Log2 Fold Change: (Neg_4) / (Neg_1)","Adj. P-value: (Neg_4) / (Neg_1)")
Neg_two_6_fields <- c("Name","Group Area: Neg_2","Group Area: Neg_6",
                      "Ratio: (Neg_6) / (Neg_2)","Log2 Fold Change: (Neg_6) / (Neg_2)","Adj. P-value: (Neg_6) / (Neg_2)")


positive_1_4 <- positive_data[one_4_fields]
positive_2_6 <- positive_data[two_6_fields]
negative_1_4 <- negative_data[Neg_one_4_fields]
negative_2_6 <- negative_data[Neg_two_6_fields]

colnames(positive_1_4)<- c('Compound_Name','Area_1', 'Area_4', 'Ratio', 'Log2', 'P_value')
colnames(positive_2_6)<- c('Compound_Name','Area_2', 'Area_6', 'Ratio', 'Log2', 'P_value')
colnames(negative_1_4)<- c('Compound_Name','Area_1', 'Area_4', 'Ratio', 'Log2', 'P_value')
colnames(negative_2_6)<- c('Compound_Name','Area_2', 'Area_6', 'Ratio', 'Log2', 'P_value')


positive_1_4$set <- "positive_1_4"
positive_2_6$set <- "positive_2_6"
negative_1_4$set <- "negative_1_4"
negative_2_6$set <- "negative_2_6"


positive_1_4 <- subset(positive_1_4, P_value<=0.05)
positive_2_6 <- subset(positive_2_6, P_value<=0.05)
negative_1_4 <- subset(negative_1_4, P_value<=0.05)
negative_2_6 <- subset(negative_2_6, P_value<=0.05)

positive_1_4$abs <- abs(positive_1_4$Log2)
positive_2_6$abs <- abs(positive_2_6$Log2)
negative_1_4$abs <- abs(negative_1_4$Log2)
negative_2_6$abs <- abs(negative_2_6$Log2)

log_1_positive_1_4 <- subset(positive_1_4, abs>=1)
log_1_positive_2_6 <- subset(positive_2_6, abs>=1)
log_1_negative_1_4 <- subset(negative_1_4, abs>=1)
log_1_negative_2_6 <- subset(negative_2_6, abs>=1)

log_1.5_positive_1_4 <- subset(positive_1_4, abs>=1.5)
log_1.5_positive_2_6 <- subset(positive_2_6, abs>=1.5)
log_1.5_negative_1_4 <- subset(negative_1_4, abs>=1.5)
log_1.5_negative_2_6 <- subset(negative_2_6, abs>=1.5)

log_2_positive_1_4 <- subset(positive_1_4, abs>=2)
log_2_positive_2_6 <- subset(positive_2_6, abs>=2)
log_2_negative_1_4 <- subset(negative_1_4, abs>=2)
log_2_negative_2_6 <- subset(negative_2_6, abs>=2)

log_2.5_positive_1_4 <- subset(positive_1_4, abs>=2.5)
log_2.5_positive_2_6 <- subset(positive_2_6, abs>=2.5)
log_2.5_negative_1_4 <- subset(negative_1_4, abs>=2.5)
log_2.5_negative_2_6 <- subset(negative_2_6, abs>=2.5)

log_3_positive_1_4 <- subset(positive_1_4, abs>=3)
log_3_positive_2_6 <- subset(positive_2_6, abs>=3)
log_3_negative_1_4 <- subset(negative_1_4, abs>=3)
log_3_negative_2_6 <- subset(negative_2_6, abs>=3)

log_3.5_positive_1_4 <- subset(positive_1_4, abs>=5)
log_3.5_positive_2_6 <- subset(positive_2_6, abs>=5)
log_3.5_negative_1_4 <- subset(negative_1_4, abs>=5)
log_3.5_negative_2_6 <- subset(negative_2_6, abs>=5)

log_7_positive_1_4 <- subset(positive_1_4, abs>=7)
log_7_positive_2_6 <- subset(positive_2_6, abs>=7)
log_7_negative_1_4 <- subset(negative_1_4, abs>=7)
log_7_negative_2_6 <- subset(negative_2_6, abs>=7)