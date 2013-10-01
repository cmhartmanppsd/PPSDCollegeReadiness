missingSASID <- function(regYr){
  # Collect students with no SASID. Because all of these records are blank, this
  # data.frame will be used to look for data on these students in future years
  # to determine if these IDs are included completely in error or represent an
  # error in previous data cleaning resulting in missing data that will need to
  # be recovered from the source system.
  return(subset(regYr, is.na(regYr$sasid)==TRUE, 
                select=c('studentid', 'schoolyear')))
}