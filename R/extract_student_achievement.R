extract_student_achievement <- function(regYr){
  id_attributes <-  c('studentid', 'sasid', 'schoolyear', 'last_name', 'grade')
  achievement_attributes <- c('reaal', 'reascsc', 'matal', 'matscsc', 'wrial',
                              'wriscsc', 'testgrade_N')
  tbl_student_achievement <- regYr[, c(id_attributes, achievement_attributes)]
  tbl_student_achievement$grade <- ifelse(tbl_student_achievement$grade==0, NA, 
                                          tbl_student_achievement$grade)
  
  levels(tbl_student_achievement$testgrade_N) <- 
    str_trim(levels(tbl_student_achievement$testgrade_N), side='both')
  tbl_student_achievement$testgrade_N <- 
    with(tbl_student_achievement, ifelse(as.character(testgrade_N)=="", NA, 
                                         as.character(testgrade_N)))
  tbl_student_achievement$testgrade_N <- 
    with(tbl_student_achievement, factor(testgrade_N, exclude=c("",NA), 
                                  labels = 
            sort(as.numeric(unique(testgrade_N)[!is.na(unique(testgrade_N))]))))
  tbl_student_achievement <- subset(tbl_student_achievement, 
                                    !is.na(testgrade_N))
  lvls <- as.numeric(as.character(levels(tbl_student_achievement$testgrade_N)))
  tbl_student_achievement$contentgrade_N <- 
    as.numeric(as.character(tbl_student_achievement$testgrade_N)) - 1
  tbl_student_achievement[, c('reascsc', 'matscsc')] <- 
    lapply(lapply(tbl_student_achievement[, c('reascsc', 'matscsc')], 
                  as.character),
           as.numeric)
  tbl_student_achievement <- 
    ddply(tbl_student_achievement, .(contentgrade_N), mutate, 
          reanormal = ifelse(!is.na(reascsc), (reascsc - mean(reascsc, 
                                                              na.rm=TRUE)) /
                                               sd(reascsc, na.rm=TRUE), reascsc),
          matnormal = ifelse(!is.na(matscsc), (matscsc - mean(matscsc, 
                                                              na.rm=TRUE)) /
                                              sd(matscsc, na.rm=TRUE), matscsc))
  return(tbl_student_achievement)
}
