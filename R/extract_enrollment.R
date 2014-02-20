extract_enrollment <- function(regYr){
  id_attributes <-  c('studentid', 'sasid', 'schoolyear', 'last_name', 'grade')
  enr_attributes <- grep('^(schno|school|exit|enroll|adm|ada|description)', 
                         names(regYr), value=TRUE)
  tbl_stud_enroll <- regYr[,c(id_attributes, enr_attributes)] 
  by_school <- list(mode='any', length=length(enr_attributes)/9)
  if(by_school$length>1){
    for(i in 1:by_school$length){
      tempSch <- tbl_stud_enroll[, c(id_attributes, 
                                     grep(paste('*_Sch', i, '$', sep=''), 
                                          names(tbl_stud_enroll), 
                                          value=TRUE))]
      names(tempSch) <- gsub(paste('*_Sch', i, '$', sep=''),'', names(tempSch))
      by_school[[i]] <- tempSch
    }
    tbl_stud_enroll <- do.call(rbind, by_school)
  }
  tbl_stud_enroll <- subset(tbl_stud_enroll, !is.na(schno))
  # Something below breaks in 2010_2011 that has to be fixed.
  tbl_stud_enroll[, c('enroll_date', 'exit_date')] <- 
    lapply(tbl_stud_enroll[, c('enroll_date', 'exit_date')], as.character)
  # Convert character dates like 05/29/1987 to their spss equivalent in seconds
  if(length(grep('\\d{2}[./]\\d{2}[./]\\d{4}', tbl_stud_enroll$enroll_date))>=1){
    tbl_stud_enroll$enroll_date <- with(tbl_stud_enroll, 
      ifelse(grepl('\\d{2}[./]\\d{2}[./]\\d{4}', enroll_date),
            difftime(as.Date(tbl_stud_enroll, format = '%m/%d/%Y'),
                     '1582-10-14',
                     units='secs'),
            enroll_date))
  }
  if(length(grep('\\d{2}[./]\\d{2}[./]\\d{4}', tbl_stud_enroll$exit_date))>=1){
    tbl_stud_enroll$enroll_date <- with(tbl_stud_enroll, 
      ifelse(grepl('\\d{2}[./]\\d{2}[./]\\d{4}', exit_date),
             difftime(as.Date(exit_date, format = '%m/%d/%Y'),
                      '1582-10-14',
                      units='secs'),
             enroll_date))
  }
  tbl_stud_enroll[, c('enroll_date', 'exit_date')] <- 
    lapply(tbl_stud_enroll[, c('enroll_date', 'exit_date')], as.double)
  tbl_stud_enroll[, c('enroll_date', 'exit_date')] <- 
    lapply(lapply(tbl_stud_enroll[, c('enroll_date', 'exit_date')], as.POSIXct, 
                  origin='1582/10/14'), # origin for SPSS dates
           as.Date, format='%Y-%m-%d')
  row.names(tbl_stud_enroll) <- NULL
  return(tbl_stud_enroll)
}
