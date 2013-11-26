build_tables <- function(regYr, year){
  if(exists('executedAt')==TRUE){
    logAt <- paste('logs/', executedAt, 'build.log', sep='')
  }else{
    logAt <- paste('logs/', paste(Sys.time()), 'build.log', sep='')
  }
  cat(paste(Sys.time(), '\n\n'), file=logAt, sep='\n', append=TRUE)
  regYr <- clean_schoolyear(regYr, year)
  buildLog <- capture.output(head(regYr)[1:10])
  cat('clean_schoolyear', year, buildLog, '\n\n', file=logAt, sep='\n', 
      append=TRUE)
  noSASID <- missingSASID(regYr)
  buildLog <- capture.output(head(noSASID))
  cat('noSASID', year, buildLog, '\n\n', file=logAt, sep='\n', 
      append=TRUE)
  regYr <- removeNoSASID(regYr)
  buildLog <- capture.output(head(regYr)[1:10])
  cat('removeNoSASID', year, buildLog, '\n\n', file=logAt, sep='\n',
      append=TRUE)
  duplicates <- duplicateSASID(regYr)
  buildLog <- capture.output(head(duplicates))
  cat('duplicates', year, buildLog, '\n\n', file=logAt, sep='\n', 
      append=TRUE)
  tbl_person <- extract_person(regYr)
  buildLog <- capture.output(head(tbl_person)[1:10])
  cat('tbl_person', year, buildLog, '\n\n', file=logAt, sep='\n', 
      append=TRUE)
  tbl_person_annual <- extract_person_annual(regYr)
  buildLog <- capture.output(head(tbl_person_annual)[1:10])
  cat('tbl_person_annual', year, buildLog, '\n\n', file=logAt, sep='\n', 
      append=TRUE)
  tbl_enrollment <- extract_enrollment(regYr)
  buildLog <- capture.output(head(tbl_enrollment)[1:10])
  cat('tbl_enrollment', buildLog, year, '\n\n', file=logAt, sep='\n', 
      append=TRUE)
  tbl_achievement <- extract_student_achievement(regYr)
  buildLog <- capture.output(head(tbl_achievement)[1:10])
  cat('tbl_achievement', buildLog, year, '\n\n', file=logAt, sep='\n', 
      append=TRUE)
  tbl_course <- extract_course(regYr)
  buildLog <- capture.output(head(tbl_course)[1:10])
  cat('tbl_course', buildLog, year, '\n\n', file=logAt, sep='\n', 
      append=TRUE)
  tables <- list(person=tbl_person, person_annual=tbl_person_annual, 
                 enrollment=tbl_enrollment, achievement=tbl_achievement,
                 course=tbl_course, missingSASID=noSASID, dupes=duplicates)
  return(tables)
}

build_tables_later <- function(stu, enr, gra, year){
  if(exists('executedAt')==TRUE){
    logAt <- paste('logs/', executedAt, 'build.log', sep='')
  }else{
    logAt <- paste('logs/', paste(Sys.time()), 'build.log', sep='')
  }
  noSASID <- missingSASID(stu)
  buildLog <- capture.output(head(noSASID))
  cat('noSASID', year, buildLog, '\n\n', file=logAt, sep='\n', 
      append=TRUE)
  duplicates <- duplicateSASID(stu)
  buildLog <- capture.output(head(duplicates))
  cat('duplicates', year, buildLog, '\n\n', file=logAt, sep='\n', 
      append=TRUE)
  tbl_person <- extract_person(stu)
  buildLog <- capture.output(head(tbl_person)[1:10])
  cat('tbl_person', year, buildLog, '\n\n', file=logAt, sep='\n',
      append=TRUE)
  tbl_person_annual <- extract_person_annual(stu)
  buildLog <- capture.output(head(tbl_person_annual)[1:10])
  cat('tbl_person_annual', year, buildLog, '\n\n', file=logAt, sep='\n', 
      append=TRUE)
  tbl_enrollment <- enr
  buildLog <- capture.output(head(tbl_enrollment)[1:10])
  cat('tbl_enrollment', buildLog, year, '\n\n', file=logAt, sep='\n', 
      append=TRUE)
  # Achievement

  tbl_course <- extract_course(gra)
  buildLog <- capture.output(head(tbl_course)[1:10])
  cat('tbl_course', buildLog, year, '\n\n', file=logAt, sep='\n', 
      append=TRUE)
  tables <- list(person=tbl_person, person_annual=tbl_person_annual, 
                 enrollment=tbl_enrollment, achievement=tbl_achievement,
                 course=tbl_course, missingSASID=noSASID, dupes=duplicates)
  
}