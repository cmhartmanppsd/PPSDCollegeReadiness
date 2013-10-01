extract_person <- function(regYr){
  # This extracts the person table elements and cleans the data types
  # Input: MASTER Reg file that has been passed through clean_schoolyear
  # Output: tbl_person attributes with the right types but no other data
  # cleaning.
  person_attributes <- c('studentid', 'sasid', 'dob', 'first_name', 'last_name', 
                         'student_lang', 'parent_lang', 'birth_place', 'sex', 
                         'schoolyear')
  tbl_person <- regYr[, person_attributes]  
  tbl_person[,-3] <- apply(apply(regYr[, person_attributes[-3]], 2, 
                                 as.character),
                      2, str_trim, side='both')
  tbl_person <- merge(tbl_person, regYr[,c('studentid','sasid','race')])
  if(class(tbl_person$race)=='factor'){
    levels(tbl_person$race) <- str_trim(levels(tbl_person$race), side='both')
  }
  if(length(unique(tbl_person$race))==5){
    tbl_person$race <- factor(tbl_person$race, exclude=c("",NA), 
                              labels = c('Asian', 'Black', 'Hispanic',
                                         'Native American', 'White'))
  }else{
    tbl_person$race <- factor(tbl_person$race, exclude=c("",NA),
                              labels = c('Asian', 'Black', 'Hispanic', 
                                         'Multi Racial', 'Native American', 
                                         'Pacific Islander', 'White'))
  }
  tbl_person$sex <- ifelse(!tbl_person$sex %in% c('F','M'), NA, tbl_person$sex) 
  tbl_person$sex <- factor(tbl_person$sex, labels = c('Female', 'Male'))
  if(!class(tbl_person$dob) %in% c('POSIXct', 'POSIXlt', 'date', 'datetime')){
    tbl_person$dob <- as.Date(tbl_person$dob, '%m/%d/%Y')
  }
  return(tbl_person)
}
