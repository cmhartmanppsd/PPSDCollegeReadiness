select_hs <- function(df, type="first"){
  dt <- data.table(df, key=c('sasid'))
  if(type=="first"){
    hs <- dt[grade %in% c(9,10,11,12), .SD[which.min(enroll_date)], 
             by=key(dt)][]
#     test <- dt[grade %in% c(9,10,11,12), 
#                list(enroll_date=min(enroll_date, na.rm=TRUE)), by=key(dt)] 
#     setkeyv(test, c('sasid','enroll_date'))
#     setkeyv(dt, c('sasid', 'enroll_date'))
#     test <- test[dt]
  }
  else if(type=="last"){
    hs <- dt[grade %in% c(9,10,11,12), .SD[which.max(exit_date)], by=key(dt)]
  }
  else if(type=="long"){
    hs <- dt[grade %in% c(9,10,11,12), list(total_adm = sum(adm)), 
             by=c(key(dt),'schno')]
    setkeyv(hs, c('sasid', 'schno'))
    setkeyv(dt, c('sasid', 'schno'))
    hs<- hs[dt[grade %in% c(9, 10, 11, 12)]][,.SD[which.max(total_adm)], 
                                             by=sasid]
  }
  else{
    stop(paste("Unrecognized type=", type))
  }
  return(as.data.frame(hs[,list(sasid, schno, school, schoolyear, 
                                grade, exit_type)]))
}
