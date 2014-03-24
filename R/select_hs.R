select_hs <- function(df, type="first"){
  if(type=='first'){
  hs <- df %.% 
        filter(grade %in% c(9, 10, 11, 12)) %.% 
        group_by(sasid) %.% 
        summarize(enroll_date = min(enroll_date))
  hs <- left_join(hs, df %.% select(sasid, enroll_date, 
                                    schno, school, grade, 
                                    exit_type, schoolyear))
  }
  else if(type=="last"){
    hs <- df %.% 
      filter(grade %in% c(9, 10, 11, 12)) %.% 
      group_by(sasid) %.% 
      summarize(enroll_date = max(enroll_date))
    hs <- left_join(hs, df %.% select(sasid, enroll_date, 
                                      schno, school, grade, 
                                      exit_type, schoolyear))
  }
  else if(type=="long"){
    hs <- df %.%
          filter(grade %in% c(9, 10, 11, 12)) %.%
          group_by(sasid, schno, school) %.%
          summarize(total_adm = sum(adm))
    hs <- ungroup(hs) %.% 
          group_by(sasid) %.%
          filter(total_adm == max(total_adm)) %.%
          select(sasid, schno, school)
  }
  else{
    stop(paste("Unrecognized type=", type))
  }
  names(hs)[-1] <- paste(names(hs)[-1], '_', type,sep='')
  return(hs)
}

# test.hs <- data.frame(sasid = c(rep(10001,4)),
#                       schno = c(1, 1, 2, 2),
#                       school = c('Adams', 'Adams', 'Washington', 'Washington'),
#                       adm = c(180, 98, 82, 180),
#                       enroll_date = as.Date(c("2000-09-01", "2001-09-01", 
#                                               "2002-01-01", "2003-09-01"), 
#                                             format='%Y-%m-%d'),
#                       exit_type = c("Promoted", "Moved", 
#                                     "Promoted", "Graduated"),
#                       grade = c(11,11,12,12))