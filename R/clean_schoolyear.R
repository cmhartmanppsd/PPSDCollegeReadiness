clean_schoolyear <- function(regYr, year){
  # Remove the appended year values and add an attribute for the school year
  # so the file is "long" when merged rather than wide.
  
  # rm most appended yrs.
  names(regYr) <- gsub(paste('_{0,1}',
                             substr(as.character(year-1),3,4), 
                             substr(as.character(year),3,4),
                             sep=''), '', names(regYr))
  ########
  # Need to work on safely removing other year-appended values. May be able to
  # just remove the leading _ from the gsub above but needs more rigorous
  # testing.
  ########
  regYr$schoolyear <- paste(year-1, year, sep='_')
  return(regYr)
}