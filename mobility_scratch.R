# Mobility
calc_moves <- function(df, 
                       enrollby,
                       exitby,
                       gap=14,
                       sid='sasid', 
                       schid='schno',
                       enroll_date='enroll_date',
                       exit_date='exit_date')){
  # df is a data.frame that minimally contains a student ID (default 'sasid'),
  # a school ID (default 'schno'), and two dates an enrollment date and an 
  # exit date for each sid-schid combination.

  # Load required packages
  require(plyr)
  require(data.table)
  # Type checking inputs.
  if (!inherits(df[[enroll_date]], "Date") | !inherits(df[[exit_date]], "Date"))
      stop("Both enroll_date and exit_date must be Date objects")
  # Check if enrollby and exitby arguments are supplied. When they aren't,
  # assign them to default dates of -09-15 and -06-01 of the min and max year.
  # If they are assigned but are not date objects then 
  if(is.null(enrollby)){
    enrollby <- as.Date(paste(year(min(df$enroll_date, na.rm=TRUE)),
                              '-09-15', sep=''), format='%Y-%m-%d')
  }else{
    if(is.na(as.Date(enrollby, format="%Y-%m-%d"))){
      enrollby <- as.Date(paste(year(min(df$enroll_date, na.rm=TRUE)),
                                '-09-15', sep=''), format='%Y-%m-%d'
      warning(paste("enrollby must be a string with format %Y-%m-%d,",
                    "defaulting to", 
                    enrollby, sep=' '))
    }else{
      enrollby <- as.Date(enrollby, format="%Y-%m-%d")
    }
  }
  if(is.null(exitby)){
    exitby <- as.Date(paste(year(max(df$exit_date, na.rm=TRUE)),
                            '-06-01', sep=''), format='%Y-%m-%d')
  }else{
    if(is.na(as.Date(exitby, format="%Y-%m-%d"))){
      exitby <- as.Date(paste(year(max(df$exit_date, na.rm=TRUE)),
                                '-06-01', sep=''), format='%Y-%m-%d')
      warning(paste("exitby must be a string with format %Y-%m-%d,",
                    "defaulting to", 
                    exitby, sep=' '))
    }else{
      exitby <- as.Date(exitby, format="%Y-%m-%d")
    }
  }

  # Generate results data table
  output <- data.frame(id = as.character(unique(df[[sid]])),
                       moves = vector(mode = 'numeric', 
                                      length = length(unique(df[[sid]]))))
  #output$id <- as.character(id)
  output <- data.table(output, key='id')
  for(i in 1:(length(df[[sid]])-1)){
    # If this is the first time the student is listed (and not the very first
    # student so that the prior student is undefined) check if enroll date is
    # after YYYY-09-15. If so, add 1 move.
    if(i>1 && df[sid][i,]!=df[sid][(i-1),]){
      if(df[['enroll_date']][i]>enrollby){
        output[as.character(df[[sid]][i]), moves:=moves+1L]
      }
    }else if(i==1){
      if(df[['enroll_date']][i]>enrollby){
      output[as.character(df[[sid]][i]), moves:=moves+1L]
      }
    }
    # If we're looking at the same student
    if(df[sid][i,]==df[sid][(i+1),]){
      # And that student has less than gap days between their exit and next
      # enrollment date, and it's the same school
      if(as.numeric(difftime(df[['enroll_date']][i+1], 
                             df[['exit_date']][i], units='days'))<gap &
         df[schid][(i+1),]==df[schid][i,]){
        # Break out so that there is no move counted and we start at the
        # second stint at that school.
        next
      }else if(as.numeric(difftime(df[['enroll_date']][i+1], 
                                   df[['exit_date']][i], 
                          units='days'))<gap){
        # When you have less than fourteen days between schools, this is
        # a direct move and no other school was likely attended between
        # this time. Therefore, this counts as one move.
        output[as.character(df[[sid]][i]), moves:=moves+1L] 
        # print(output)
      }else{
        # Student has same ID as one below (at least two entries), and
        # difference between the exit and enroll dates are greater than gap,
        # then this counts as two moves (one out of district, then back 
        # into district)
        output[as.character(df[[sid]][i]), moves:=moves+2L] 
        # print(output)
      }
    }else{
      # Should trigger if the next student number doesn't match meaning this is
      # the last record for that student in the file.
      if(is.na(df[['exit_date']][i])){
        next
      }
      else if(df[['exit_date']][i]<exitby){
        output[as.character(df[[sid]][i]), moves:=moves+1L]
      }
    }
  }
  # Need to add logic for students who enter after YYYY-09-15 and leave before
  # YYYY-06-01
  output
}
