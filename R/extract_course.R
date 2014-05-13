extract_course <- function(regYr){
  id_attributes <-  c('studentid', 'schyr')
  course_attributes <- grep('^(cum|teacher|courseno|coursedesc|type|credits)', 
                            names(regYr), value=TRUE)
  tbl_course <- regYr[,c(id_attributes, course_attributes)] 
  by_course <- list(mode='any', length=length(which(grepl('courseno',
                                                          x = course_attributes,
                                                          ignore.case=TRUE))))
  for(i in 1:by_course$length){
    tempSch <- tbl_course[, c(id_attributes, 
                              grep(sprintf("%02d",i), 
                                   names(tbl_course), 
                                   value=TRUE))]
    names(tempSch) <- gsub(sprintf("%02d", i), '', names(tempSch))
    by_course[[i]] <- tempSch
  }
  tbl_course <- do.call(rbind, by_course)
  tbl_course <- subset(tbl_course, !is.na(courseno))
  row.names(tbl_course) <- NULL
  quarters <- grep('^cum', names(tbl_course), value=TRUE)
  key_vars <- names(tbl_course)[which(!names(tbl_course) %in% c('course_grade',
                                                                quarters))]
  tbl_course[,quarters] <- lapply(tbl_course[,quarters], as.character)
  tbl_course <- melt(tbl_course, id.vars=c(key_vars),
                     value.name='course_grade')
  tbl_course$course_grade <- str_trim(tbl_course$course_grade, side='both')
  tbl_course$coursedesc <- str_trim(tbl_course$coursedesc, side='both')
  require(car)
  tbl_course$gpa <- recode(as.character(tbl_course$course_grade), 
                           "'A+' = 4.33; 'A' = 4.00; 'A-' = 3.67; 'B+' = 3.33;
                           'B' = 3.00; 'B-' = 2.67; 'C+' = 2.33; 'C' =2.00; 
                           'C-' = 1.67; 'D+' =1.33; 'D' = 1.00; 'D-' = 0.67;
                           'E+' = 0; 'E' = 0; 'E-' = 0; 'F+' = 0; 'F' = 0; 
                           else=NA")
  return(tbl_course)
}