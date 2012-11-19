# prabhas -- # setwd("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/")
# prabhas -- # source("~/Code/nga_cleaning_scripts/formhub.R")
source("scripts/formhub.R")

extraSchema = setNames(data.frame(rbind(
                    c("mylga", "select one", "LGA"),
                    c("mylga_state", "select one", "State"))),
              c("name", "type", "label"))
dropCols = c("mylga_.*_in_.*", ".*_calc") # cascading selects; calcs with propagated 999s

education <- formhubRead("raw_data/Education_05_06_2012_2012_10_05_09_56_27.csv",
                         "raw_data/json_schemas/Education_05_06_2012.json",
                         extraSchema = extraSchema, dropCols=dropCols)
education2 <- formhubRead("raw_data/Education_17_04_2012_2012_10_05_16_37_54.csv",
                         "raw_data/json_schemas/Education_05_06_2012.json",
                         extraSchema = extraSchema, dropCols=dropCols)
education3 <- formhubRead("raw_data/Education_22_05_2012_2012_10_22_11_16_03.csv",
                         "raw_data/json_schemas/Education_05_06_2012.json",
                         extraSchema = extraSchema, dropCols=dropCols)

health <- formhubRead("raw_data/Health_05_06_2012_2012_10_03_16_48_41.csv",
                      "raw_data/json_schemas/Health_17_04_2012.json",
                      extraSchema = extraSchema, dropCols=dropCols)
health2 <- formhubRead("raw_data/Health_17_04_2012_2012_10_03_16_49_35.csv",
                      "raw_data/json_schemas/Health_17_04_2012.json",
                      extraSchema = extraSchema, dropCols=dropCols)
health3 <- formhubRead("raw_data/Health_22_05_2012_2012_10_03_16_51_22.csv",
                      "raw_data/json_schemas/Health_17_04_2012.json",
                      extraSchema = extraSchema, dropCols=dropCols)

water <- read.csv("raw_data/Water_05_06_2012_2012_10_01_15_00_38.csv")
water2 <- read.csv("raw_data/Water_22_05_2012_2012_10_23_17_19_10.csv")
water3 <- read.csv("raw_data/Water_24_04_2012_2012_10_05_15_10_29.csv")

#water <- formhubRead("raw_data/Water_05_06_2012_2012_10_01_15_00_38.csv",
#                     "raw_data/json_schemas/Water_05_06_2012.json",
#                     extraSchema = extraSchema, dropCols=dropCols)
#water2 <- formhubRead("raw_data/Water_22_05_2012_2012_10_23_17_19_10.csv",
#                     "raw_data/json_schemas/Water_05_06_2012.json",
#                     extraSchema = extraSchema, dropCols=dropCols)
#water3 <- formhubRead("raw_data/Water_24_04_2012_2012_10_05_15_10_29.csv",
#                     "raw_data/json_schemas/Water_05_06_2012.json",
#                     extraSchema = extraSchema, dropCols=dropCols)

(names(education) == names(education2)) && (names(education2) == names(education3)) && (names(education3) == names(education))
(names(health) == names(health2)) && (names(health2) == names(health3)) && (names(health3) == names(health))
(names(water) == names(water2)) && (names(water2) == names(water3)) && (names(water3) == names(water))

water <- removeColumns(water, 'mylga_.*_in_.*')
water2 <- removeColumns(water2, 'mylga_.*_in_.*')
water3 <- removeColumns(water3, 'mylga_.*_in_.*')

merged_education <- rbind(education, education2, education3)
merged_health <- rbind(health, health2, health3)
merged_water <- rbind(water, water2, water3)



write.csv(merged_education, "in_process_data/merged/Education_661_Merged.csv")
write.csv(merged_health, "in_process_data/merged/Health_661_Merged.csv")
write.csv(merged_water, "in_process_data/merged/Water_661_Merged.csv")