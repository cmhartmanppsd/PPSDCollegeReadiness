modal_person_attribute_dt <- function(df, attribute){
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

modal_test <- data.frame(sasid = c('1000', '1001', '1000', '1000', 
                                   '1005', '1005', rep('1006',4)),
                         race = c('Black', 'White', 'Black', 'Hispanic', 'White', 'White',
                                  rep('Black',2), rep('Hispanic',2)),
                         schoolyear = c('2005_2006', '2005_2006', '2006_2007', '2007_2008',
                                  '2009_2010', '2010_2011', '2006_2007', '2007_2008',
                                  '2009_2010','2010_2011'))
modal_test <- data.frame(sasid = c('1000', '1001', '1000', '1000', 
                                   '1005', '1005', rep('1006',4)),
                         race = c('Black', 'White', 'Black', 'Hispanic', 'White', 'White',
                                  rep('Black',2), rep('Hispanic',2)),
                         year = as.numeric(c('2006', '2006', '2007', '2008',
                                  '2010', '2011', '2007', '2008',
                                  '2010','2011')))

modal_person_attribute <- function(x, sid, attribute, year){
  grouping <- lapply(list(sid, attribute), as.symbol)
  original <- x
  max_attributes <- x %.% 
                    regroup(grouping) %.%
                    summarize(count = n()) %.%
                    filter(count == max(count))
  recent_max <- left_join(original, max_attributes) %.%
                regroup(list(grouping[[1]])) %.%
                filter(!is.na(count) & count == max(count))
  if(TRUE %in% grepl('_', recent_max[[year]])){
    recent_max[[year]] <- gsub(pattern='[0-9]{4}_([0-9]{4})', '\\1', recent_max[[year]])
  }
  results <- recent_max %.% 
             regroup(list(grouping[[1]])) %.%
             filter(year == max(year))
  return(results[,c(sid, attribute)])
}