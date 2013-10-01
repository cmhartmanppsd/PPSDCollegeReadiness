removeNoSASID <- function(regYr){
  # Remove those without SASID because they are all blank records.
  count_i <- nrow(regYr)
  # Count the number of students that are removed this way.
  regYr <- subset(regYr, is.na(regYr$sasid)==FALSE & 
                         is.na(regYr$schno_Sch1)==FALSE)
  recordsRemoved <- count_i - nrow(regYr)
  print(recordsRemoved)
  return(regYr)
}
