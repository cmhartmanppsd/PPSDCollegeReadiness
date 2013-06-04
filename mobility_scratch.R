# Mobility
calc_moves <- function(df, sid='sasid', schid='schno'){
  # df is a data.frame that minimally contains a student ID (default 'sasid'),
  # a school ID (default 'schno'), and two dates an enrollment date and an 
  # exit date for each sid-schid combination.

  # Load required packages
  require(plyr)
  require(data.table)
  
  # Type checking inputs.
  if (!inherits(df$enroll_date, "Date") | !inherits(df$exit_date, "Date"))
      stop("Both enroll_date and exit_date must be Date objects")

  # Generate results data table
  output <- data.frame(id = as.character(unique(df[[sid]])),
                       moves = vector(mode = 'numeric', 
                                      length = length(unique(df[sid]))))
  #output$id <- as.character(id)
  output <- data.table(output, key='id')
  for(i in 1:(length(df[[sid]])-1)){
    # If we're looking at the same student
    if(df[sid][i,]==df[sid][(i+1),]){
      # And that student has less than 14 days between their exit and next
      # enrollment date, and it's the same school
      if(as.numeric(difftime(df[['enroll_date']][i+1], 
                             df[['exit_date']][i], units='days'))<14 &
         df[schid][(i+1),]==df[schid][i,]){
        # Break out so that there is no move counted and we start at the
        # second stint at that school.
        next
      }else if(as.numeric(difftime(df[['enroll_date']][i+1], 
                                   df[['exit_date']][i], 
                          units='days'))<14){
        # When you have less than fourteen days between schools, this is
        # a direct move and no other school was likely attended between
        # this time. Therefore, this counts as one move.
        output[as.character(df[[sid]][i]), moves:=moves+1L] 
        # print(output)
      }else{
        # Student has same ID as one below (at least two entries), and
        # difference between the exit and enroll dates are greater than 14,
        # then this counts as two moves (one out of district, then back 
        # into district)
        output[as.character(df[[sid]][i]), moves:=moves+2L] 
        # print(output)
      }
    }
  }
  # Need to add logic for students who enter after YYYY-09-15 and leave before
  # YYYY-06-01
  output
}