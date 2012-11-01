# prabhas -- # setwd("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/")
# salah -- # setwd("~/Dropbox/Nigeria 661 Baseline Data Cleaning/")
source("scripts/formhub.R")
library(stringr)

merged_education <- read.csv("in_process_data/merged/Education_661_Merged.csv",
                          header=TRUE, na.strings=c('999', '9999', '99999', '999999'))
merged_health <- read.csv("in_process_data/merged/Health_661_Merged.csv",
                          header=TRUE, na.strings=c('999', '9999', '99999', '999999'))
merged_water <- read.csv("in_process_data/merged/Water_661_Merged.csv")

# merged_education2 <- subset(merged_education, subset=(as.numeric(as.character(num_benches)) > 10000))