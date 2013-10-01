modal_person_attribute <- function(df, attribute){
  # df: rbind of all person tables from all years
  # attribute: vector name to calculate the modal value
  # Calculate the number of instances an attributed is associated with an id
  dt <- data.table(df, key='sasid')
  mode <- dt[, rle(as.character(.SD[[attribute]])), by=sasid]
  setnames(mode, c('sasid', 'counts', as.character(attribute)))
  setkeyv(mode, c('sasid', 'counts'))
  # Only include attributes with the maximum values. This is equivalent to the
  # mode with two records when there is a tie.
  mode <- mode[,subset(.SD, counts==max(counts)), by=sasid]
  mode[,counts:=NULL]
  setnames(mode, c('sasid', attribute))
  setkeyv(mode, c('sasid',attribute))
  # Produce the maximum year value associated with each ID-attribute pairing    
  setkeyv(dt, c('sasid',attribute))
  if(class(mode[[attribute]])!=class(dt[[attribute]]) &
     TRUE %in% grepl('-',mode[[attribute]])){
    mode[[attribute]] <- as.Date(mode[[attribute]], format='%Y-%m-%d')
  }
  mode <- dt[,list(schoolyear=max(schoolyear)), by=c("sasid", attribute)][mode]
  setkeyv(mode, c('sasid', 'schoolyear'))
  # Select the last observation for each ID, which is equivalent to the highest
  # schoolyear value associated with the most frequent attribute.
  result <- mode[,lapply(.SD, tail, 1), by=sasid]
  # Remove the schoolyear to clean up the result
  result <- result[,schoolyear:=NULL]
  return(result)
}
