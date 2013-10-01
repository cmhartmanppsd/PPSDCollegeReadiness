duplicateSASID <- function(regYr){
  duplicates <- subset(regYr, 
                       regYr$sasid %in% subset(regYr$sasid, 
                                               duplicated(regYr$sasid) &
                                               is.na(regYr$sasid)==FALSE),
                       select = c('studentid', 'sasid', 'schoolyear'))
  return(duplicates)
}