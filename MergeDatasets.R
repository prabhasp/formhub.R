# prabhas -- # setwd("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/")
source("scripts/formhub.R")
library(stringr)
extraSchema = setNames(data.frame(rbind(
                    c("mylga", "select one", "LGA"),
                    c("mylga_state", "select one", "State"))),
              c("name", "type", "label"))
#education <- formhubRead("raw_data/Education_05_06_2012_2012_10_05_09_56_27.csv",
#                         "raw_data/json_schemas/Education_05_06_2012.json",extraSchema = extraSchema)

education <- read.csv("raw_data/Education_05_06_2012_2012_10_05_09_56_27.csv", header = TRUE)
education2 <- read.csv("raw_data/Education_17_04_2012_2012_10_05_16_37_54.csv", header = TRUE)
education3 <- read.csv("raw_data/Education_22_05_2012_2012_10_22_11_16_03.csv", header = TRUE)
health <- read.csv("raw_data/Health_05_06_2012_2012_10_03_16_48_41.csv", header = TRUE)
health2 <- read.csv("raw_data/Health_17_04_2012_2012_10_03_16_49_35.csv", header = TRUE)
health3 <- read.csv("raw_data/Health_22_05_2012_2012_10_03_16_51_22.csv", header = TRUE)
water <- read.csv("raw_data/Water_05_06_2012_2012_10_01_15_00_38.csv", header = TRUE)
water2 <- read.csv("raw_data/Water_22_05_2012_2012_10_23_17_19_10.csv", header = TRUE)
water3 <- read.csv("raw_data/Water_24_04_2012_2012_10_05_15_10_29.csv", header = TRUE)

nigeria_remove <- function(df) { removecolumns(df, "mylga_.*_in_.*") }
education <- nigeria_remove(education)
education2 <- nigeria_remove(education2)
education3 <- nigeria_remove(education3)
(names(education) == names(education2)) && (names(education2) == names(education3)) && (names(education3) == names(education))
health <- nigeria_remove(health)
health2 <- nigeria_remove(health2)
health3 <- nigeria_remove(health3)
(names(health) == names(health2)) && (names(health2) == names(health3)) && (names(health3) == names(health))
water <- nigeria_remove(water)
water2 <- nigeria_remove(water2)
water3 <- nigeria_remove(water3)
(names(water) == names(water2)) && (names(water2) == names(water3)) && (names(water3) == names(water))

merged_education <- rbind(education, education2, education3)
merged_health <- rbind(health, health2, health3)
merged_water <- rbind(water, water2, water3)

write.csv(merged_education, "in_process_data/merged/Education_661_Merged.csv")
write.csv(merged_health, "in_process_data/merged/Health_661_Merged.csv")
write.csv(merged_water, "in_process_data/merged/Water_661_Merged.csv")