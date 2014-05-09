require(testthat)
require(stringr)

test_dir = ""
# test_dir = "~/Code/formhub.R/tests/"
# test_file("~/Code/formhub.R/tests/test_formhub_funcs.R")

edu_datafile <- str_c(test_dir, "fixtures/edu1.csv")
edu_formfile <- str_c(test_dir, "fixtures/edu1.json")
good_eats_datafile <- str_c(test_dir, "fixtures/good_eats.csv")
good_eats_formfile <- str_c(test_dir, "fixtures/good_eats.json")
pde_datafile <- str_c(test_dir, "fixtures/pde.csv")
pde_formfile <- str_c(test_dir, "fixtures/pde.json")


# Header Names with Labels works
test_that("replaceHeaderNamesWithLabels basically works", {
  edu_formhubObj <- formhubRead(edu_datafile, edu_formfile)
  good_eats <- formhubRead(good_eats_datafile, good_eats_formfile)
  
  edu_formhubObj_replaced <- replaceHeaderNamesWithLabels(edu_formhubObj)
  good_eats_replaced <- replaceHeaderNamesWithLabels(good_eats)
  
  expect_equal(class(edu_formhubObj_replaced), "data.frame")
  expect_equal(class(good_eats_replaced), "data.frame")
  
  expect_false(any(names(edu_formhubObj_replaced) == "NA"))
  expect_false(any(names(good_eats_replaced) == "NA"))
  
  expect_true(all(c("submit_date", "imei", "X_gps_longitude","Rating","Type of Eat", "Food Pic")
              %in% names(good_eats_replaced)))
  expect_true(all(c("subscriberid", "uuid", "2.1 Respondent\031s Name", "X_gps_latitude",
                    "LGA", "mylga", "num_ss_total_calc", "TOTAL students in Senior Secondary 1 thugh 3",
                    "23. What type(s) of power sources are available at this school? >> Generator",
                    "23. What type(s) of power sources are available at this school? >> None")
              %in% names(edu_formhubObj_replaced)))
})
 
test_that("replaceAllNamesWithLabels works on good_eats", {
  good_eats <- formhubRead(good_eats_datafile, good_eats_formfile)
  good_eats_replaced <- replaceAllNamesWithLabels(good_eats)
  
  expect_equal(class(good_eats_replaced), "data.frame")
  expect_false(any(names(good_eats_replaced) == "NA"))
  expect_true(all(c("submit_date", "imei", "X_gps_longitude","Rating","Type of Eat", "Food Pic")
                  %in% names(good_eats_replaced)))
  
  expect_false("bad" %in% good_eats_replaced$Rating)
  expect_true("What was I thinking" %in% good_eats_replaced$Rating)
  expect_equivalent(table(good_eats_replaced$Rating)["What was I thinking"], 25)
  
  expect_true("Low Risk" %in% good_eats_replaced[,"Risk Factor"])
})

test_that("replace Functions work with dot-replaced names", {
  good_eats <- formhubRead(good_eats_datafile, good_eats_formfile)
  names(good_eats)[3] <- 'food.type' # R replaces characters with dot sometimes
                                     # doesn't replace _, but we don't have better
                                     # test data at the moment
  good_eats_replaced <- replaceAllNamesWithLabels(good_eats)
  good_eats_names_replaced <- replaceHeaderNamesWithLabels(good_eats)
  
  expect_equal(class(good_eats_replaced), "data.frame")
  expect_equal(class(good_eats_names_replaced), "data.frame")
  
  expect_false(any(names(good_eats_replaced) == "NA"))
  expect_true("Type of Eat" %in% names(good_eats_replaced))
  expect_true("Dinner" %in% good_eats_replaced[,"Type of Eat"])
  
  expect_false(any(names(good_eats_names_replaced) == "NA"))
  expect_true("Type of Eat" %in% names(good_eats_names_replaced))
})

# Replace works with multi-lingual forms
test_that("replace Functions work with multi-lingual forms", {
    pde <- formhubRead(pde_datafile, pde_formfile)
    pde_names_replaced <- replaceHeaderNamesWithLabels(pde)
    pde_replaced_en <- replaceAllNamesWithLabels(pde, "English")
    pde_replaced_fr <- replaceAllNamesWithLabels(pde, "French")
    
    expect_equal(class(pde_replaced_en), "data.frame")
    expect_equal(class(pde_replaced_fr), "data.frame")
    expect_equal(class(pde_names_replaced), "data.frame")
    
    expect_false(any(names(pde_replaced_en) == "NA"))
    expect_false(any(names(pde_replaced_fr) == "NA"))
    expect_true("A-2.4 Commune" %in% names(pde_replaced_en))
    expect_true("A-2.4 Commune" %in% names(pde_replaced_fr))
    expect_true("Grid is further than 500m" %in% 
        pde_replaced_en[,"A-3.1 A quelle distance estimez-vous le reseau electrique national EDH de cet etablissment."])
    expect_true("Information non disponible/Ne sais pas" %in% 
        pde_replaced_fr[,"A-3.1 A quelle distance estimez-vous le reseau electrique national EDH de cet etablissment."])
    
    expect_false(any(names(pde_names_replaced) == "NA"))
    expect_true("A-2.4 Commune" %in% names(pde_names_replaced))
})

# Adding photo URLs work
test_that("adding photo urls works", {
  good_eats <- formhubRead(good_eats_datafile, good_eats_formfile)
  good_eats_with_photo_urls <- addPhotoURLs(good_eats, 'mberg')
  # check that new columns were added
  expect_true(all(c("food_photo_URL_original", "food_photo_URL_medium", "food_photo_URL_small",
                "location_photo_URL_original", "location_photo_URL_medium",
                "location_photo_URL_small") %in% names(good_eats_with_photo_urls)))
  # check that if original is blank, so is medium and small
  expect_true(all(which(good_eats_with_photo_urls$food_photo_URL_original == "") ==
              intersect(which(good_eats_with_photo_urls$food_photo_URL_small == ""),
                        which(good_eats_with_photo_urls$food_photo_URL_medium == ""))))
  # check one of the URLs
  expect_equal(subset(good_eats_with_photo_urls, description == "Fistikli")$location_photo_URL_original,
               "https://formhub.org/attachment/?media_file=mberg/attachments/1325233817453.jpg")
  expect_equal(subset(good_eats_with_photo_urls, description == "Fistikli")$location_photo_URL_medium,
               "https://formhub.org/attachment/medium?media_file=mberg/attachments/1325233817453.jpg")
  expect_equal(subset(good_eats_with_photo_urls, description == "Fistikli")$location_photo_URL_small,
               "https://formhub.org/attachment/small?media_file=mberg/attachments/1325233817453.jpg")
})

# Re-mapping field values works
test_that("remapColumns can map values", {
  edu_formhubObj <- formhubRead(edu_datafile, edu_formfile)
  edu_mapped <- remapColumns(edu_formhubObj, remap_list = list(c("yes"=TRUE, "no"=FALSE)))
  expect_equal(mean(edu_formhubObj$nomadic_school_yn == 'yes'), mean(edu_mapped$nomadic_school_yn))
  
  good_eats <- formhubRead(good_eats_datafile, good_eats_formfile)
  ge_mapped <- remapColumns(good_eats, remap_list = list(c("high_risk" = TRUE, "medium_risk" = FALSE, "low_risk" = NA)))
  expect_equal(mean(ge_mapped$risk_factor),
               sum(good_eats$risk_factor =="high_risk") / sum(good_eats$risk_factor %in% c("high_risk", "medium_risk")))
})

test_that("remapColumns doesn't map values unless all values in data are in remap_list", {
  edu_formhubObj <- formhubRead(edu_datafile, edu_formfile)
  edu_mapped <- remapColumns(edu_formhubObj, remap_list = list(c("yes"=TRUE)))
  expect_match(edu_formhubObj$nomadic_school_yn, edu_mapped$nomadic_school_yn)
  
  good_eats <- formhubRead(good_eats_datafile, good_eats_formfile)
  ge_mapped <- remapColumns(good_eats, remap_list = list(c("high_risk" = TRUE, "medium_risk" = FALSE)))
  expect_true(good_eats == ge_mapped)
})