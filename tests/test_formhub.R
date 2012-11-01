library(testthat)
# setwd("~/Code/nga_cleaning_scripts/")
test_dir("tests/")
edu_datafile <- "fixtures/edu1.csv"
edu_schemafile <- "fixtures/edu1.json"
extraSchema = setNames(data.frame(rbind(
                    c("mylga", "select one", "LGA"), 
                    c("mylga_state", "select one", "State"))),
                       c("name", "type", "label"))
edu_df <- formhubRead(edu_datafile, edu_schemafile, extraSchema=extraSchema)

test_that("formhubRead converted types properly", {
  expect_equal(1, 1)
  expect_true(is.factor(edu_df$mylga_zone))
  expect_true(is.factor(edu_df$mylga))
  expect_true(is.factor(edu_df$mylga_state))
  expect_true(is.numeric(edu_df$num_students_total_gender.num_students_female))
})
