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
    apply(tbl_stud_enroll[, c('enroll_date', 'exit_date')], 2, as.character)
  # I don't know why this throws an error sometimes. It seems like it should
  # work even if there are no wrongly formatted dates. However, since it does 
  # throw an error related to lacking an origin, I put a try() wrapper around
  # the code first and only execute it if I know it won't fail.
  if(class(try(difftime(as.Date(tbl_stud_enroll[grep('[[:punct:]]', 
                                                 tbl_stud_enroll$exit_date), 
                                                 'exit_date'], 
                                 format="%m/%d/%Y"), 
                "1582-10-14", units='secs'), silent=TRUE))!="try-error"){
    tbl_stud_enroll[grep('[[:punct:]]', 
                         tbl_stud_enroll$exit_date),'exit_date'] <- 
      difftime(as.Date(tbl_stud_enroll[grep('[[:punct:]]', 
                                            tbl_stud_enroll$exit_date), 
                                       'exit_date'], 
                       format="%m/%d/%Y"), 
               "1582-10-14", units='secs')
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
