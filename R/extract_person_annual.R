extract_person_annual <- function(regYr){
  # This extracts the person annual table elements and clean data types
  # Inputs: MASTER Reg file that has been passed through clean_schoolyear
  # Output: tbl_person_annual attributes wiht the right types but no other
  #         data cleaning.
  person_annual_attributes <- c('studentid', 'sasid', 'last_name', 'grade', 
                                'lunch', 'lep', 'iep', 'plp', 'sec504', 
                                'graduated', 'willrepeatgr', 'isrepeatinggr',
                                'disab', 'spedprogrm', 'str_name', 'apt_no', 
                                'city', 'state', 'zip_code', 'sum_absent',
                                'sum_excabsent', 'sum_tardy', 'sum_on_time',
                                'sum_suspend', 'sum_enrolled', 'sum_present',
                                'sum_soccurin', 'sum_soccurout', 'schoolyear')
  tbl_person_annual <- regYr[, person_annual_attributes]
  tbl_person_annual[,1:3] <- apply(apply(tbl_person_annual[,1:3], 2, 
                                         as.character), 
                                   2, str_trim, side='both')
  tbl_person_annual$grade <- ifelse(tbl_person_annual$grade==0, NA, 
                                    tbl_person_annual$grade)
  tbl_person_annual$grade <- factor(tbl_person_annual$grade, 
                                    labels = c(1,2,3,4,5,6,7,8,9,10,11,12,
                                               'Full-day K', 
                                               'Full-day Pre-K',
                                               'Morning Pre-K', 
                                               'Afternoon Pre-K'))
  tbl_person_annual$lunch <- str_trim(as.character(tbl_person_annual$lunch), 
                                      side='both')
  tbl_person_annual$lunch <- with(tbl_person_annual, 
                                  ifelse(lunch=='F', 'Free Lunch',                              
                                    ifelse(lunch=='R', 'Reduced Lunch',                                            
                                           ifelse(lunch=='P','Paid Lunch',
                                                  NA))))
  tbl_person_annual$lunch <- factor(tbl_person_annual$lunch)
  levels(tbl_person_annual$disab) <- gsub('[[:punct:]]|[\\|]| ', '', 
                                          levels(tbl_person_annual$disab))
  levels(tbl_person_annual$disab) <- gsub('NE|^[B-Z]$', '', 
                                          levels(tbl_person_annual$disab))
  # Play with a tryCatch here to add NE to those that need it, and if nothing
  # works to just exit gracefully without factorizing the disability category.
  tbl_person_annual$disab <- factor(factor(tbl_person_annual$disab, 
                                    labels = c('None', 'Autism', 
                                               'Developmental Delay', 
                                               'Emotional Disturbance', 
                                               'Hearing Impairment',
                                               'Learning Disabled', 
                                               'Multiple Disabilities',
                                               'Mental Retardation', 
                                               'OI', 
                                               'Other Hearing Impairment', 
                                               'Speech/Language',
                                               'Speech Only', 
                                               'Traumatic Brain Injury',
                                               'Visual Impairment')))
  tbl_person_annual[, c('str_name', 'apt_no', 'city', 'state')] <- 
    apply(apply(tbl_person_annual[, c('str_name', 'apt_no', 'city', 'state')],
                2, as.character), 2, str_trim, side='both')
  tbl_person_annual$zip_code <- as.factor(sprintf('%05i', 
                                                  tbl_person_annual$zip_code))
  return(tbl_person_annual)
}
