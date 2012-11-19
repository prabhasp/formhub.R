## ALIASES / PREP ##

e <- merged_education

## SNAPSHOT ##
ed <- data.frame(uuid=e$uuid, mylga=e$mylga, mylga_state=e$mylga_state, mylga_zone=e$mylga_zone)
# ed <- e
ed$`Type` <- e$education_type
ed$school_1kmplus_catchment_area <- e$km_to_catchment_area > 1
ed$potable_water <- 
  (e$water.pipe_water & e$pipe_water_repair_time == 'yes') |
  (e$water.tube_well & e$borehole_tubewell_repair_time == 'yes')
  # notes: water.tube_well = borehole or tubewell; not including "protected" wellsh here; 
  # repair_time is really a question about functionality, a typo
ed$num_classrms_need_maj_repairs <- e$num_classrms_need_maj_repairs
ed$num_tchrs_with_nce <- e$num_tchrs_qualification.num_tchrs_w_nce

ed$num_textbooks <-  
  ifelse(e$level_of_education %in% c('primary_only', 'preprimary_and_primary'),  
     e$manuals_pry.num_math_textbook_pry + e$manuals_pry.num_english_textbook_pry + e$manuals_pry.num_soc_science_textbook_pry + e$manuals_pry.num_science_textbook_pry,
  ifelse(e$level_of_education %in% c('junior_and_senior_sec', 'juniors_sec_only'),
    e$manuals_js.num_math_textbook_js + e$manuals_js.num_english_textbook_js + e$manuals_js.num_soc_science_textbook_js + e$manuals_js.num_science_textbook_js,
  ifelse(e$level_of_education %in% c('primary_and_junior_sec', 'primary_junior_and_senior_sec'),
    e$manuals_pry.num_math_textbook_pry + e$manuals_pry.num_english_textbook_pry + e$manuals_pry.num_soc_science_textbook_pry + e$manuals_pry.num_science_textbook_pry +
      e$manuals_js.num_math_textbook_js + e$manuals_js.num_english_textbook_js + e$manuals_js.num_soc_science_textbook_js + e$manuals_js.num_science_textbook_js,
  0)))
ed$textbook_to_pupil_ratio <- ed$num_textbooks / e$num_students_total_gender.num_students_total

ed$natl_curriculum_yn <- e$natl_curriculum_yn

## ACCESS ##
# see school_1kmplus_catchment_area from above
ed$school_1kmplus_secondary_school <- e$km_to_secondary_school > 1
ed$students_living_3kmplus_school <- e$num_students_frthr_than_3km

## PARTICIPATION ##
ed$male_to_female_student_ratio <- e$num_students_total_gender.num_students_male / e$num_students_total_gender.num_students_female

## Infrastructure: Water & San ##
ed$education_improved_sanitation <- 
  (as.logical(e$toilet.flush_or_pour_flush_improved) & e$flush_improved_functional_yn == 'yes') |
  (as.logical(e$toilet.ventilated_improved) & e$vip_latrine_functional_yn == 'yes') |
  (as.logical(e$toilet.pit_latrine_with_slab) & e$slab_pit_latrine_functional_yn == 'yes')
ed$gender_separated_toilets_yn <- (e$num_toilet.num_toilet_boy > 1) & (e$num_toilet.num_toilet_girl > 1)
ed$pupil_toilet_ratio_facility <- e$num_students_total_gender.num_students_total / 
  (e$num_toilet.num_toilet_boy + e$num_toilet.num_toilet_girl + e$num_toilet.num_toilet_both)
  # can't trust the xform calculations because of "999" numbers

## Infrastructure: Building Structure ##
ed$power_access <- 
  (as.logical(e$power_sources.generator) & e$generator_funct_yn == 'yes') |
  (as.logical(e$power_sources.solar_system) & e$solar_funct_yn == 'yes') |
  (as.logical(e$power_sources.grid) & e$grid_funct_yn == 'yes')
ed$num_classrms_need_min_repairs <- e$num_classrms_need_min_repairs
ed$covered_roof_good_condi <- (e$covered_roof_yn == 'yes_good_condition')

## Infrastructure: Health and Safety ##
ed$access_clinic_dispensary <- e$health_services_yn == 'yes_clinic_dispensary'
ed$access_first_aid <- e$health_services_yn == 'first_aid_kit'
ed$wall_fence_good_condi <- (e$boundary_wall_fence_yn == 'yes_good_condition')

## Infrastructure: Learning Environment ##
ed$pupil_classrm_ratio <- e$num_students_total_gender.num_students_total / e$num_classrms_total
   # actually, lets just make sure to re-calculate totals in the outlier scripts
ed$classes_outside_yn <- e$classes_outside_yn
ed$two_shifts_yn <- e$two_shifts_yn
ed$multigrade_classrms <- e$multigrade_teaching_yn

## Furniture ##
ed$chalkboard_each_classroom_yn <- e$num_classrms_total == e$num_classrm_w_chalkboard
ed$pupil_bench_ratio <- e$num_students_total_gender.num_students_total / e$num_benches
ed$pupil_desk_ratio <- e$num_students_total_gender.num_students_total / e$num_desks

## Adequacy of Staffing ##
ed$pupil_tchr_ratio <- e$num_students_total_gender.num_students_total / e$num_tchrs.num_tchrs_total
ed$teacher_nonteachingstaff_ratio <- e$num_tchrs.num_tchrs_total / (e$num_sr_staff_total)
ed$num_tchrs_with_nce <- e$num_tchrs_qualification.num_tchrs_w_nce
ed$num_tchrs_attended_training <- e$num_tchrs_attended_training

## Institutional Development ##
ed$tchr_pay_delay <- e$times_tchr_pay_delay_pastyr > 0
ed$tchr_pay_miss <- e$times_tchr_pay_miss_pastyr > 0

## Curriculum Issues ##
ed$textbook_to_pupil_ratio <- ed$num_textbooks / e$num_students_total_gender.num_students_total
ed$provide_exercise_books_yn <- e$provide_exercise_books_yn
ed$provide_pens_yn <- e$provide_pens_yn
ed$teacher_guide_yn <- e$teacher_guide_yn
ed$functioning_library_yn <- e$functioning_library_yn


str(ed)
summary(ed)
head(ed)