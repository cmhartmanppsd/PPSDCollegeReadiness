modal_person_attribute1 <- function(df, attribute){
  # df: rbind of all person tables from all years
  # attribute: vector name to calculate the modal value
  # Calculate the number of instances an attributed is associated with an id
  mode <- do.call(rbind, 
                  tapply(as.character(df[[attribute]]), df$sasid, 
                         function(x) data.frame(attribute=rle(x)$values, 
                                                counts=rle(x)$lengths)))
  names(mode) <- c(as.character(attribute), 'counts')
  # Clean up
  mode$sasid <- row.names(mode)
  row.names(mode) <- NULL
  # Remove the R .1, .2 notation for repeat SASIDs from tapply
  mode$sasid <- gsub('[.][0-9]$','', mode$sasid)
  # Only include attributes with the maximum values. This is equivalen to the
  # mode with two records when there is a tie.
  mode <- ddply(mode, 'sasid', function(mode) subset(mode, 
                                                     counts==max(counts)))
  # Subset of all cases where there is a duplicate mode for the attribute
  dupes <- subset(mode, sasid %in% subset(mode$sasid, duplicated(sasid)))
  # Create a non-duplicated modal value subset
  mode <- subset(mode, sasid %in% (sasid %w/o% dupes$sasid))
  # Produce the maximum year value associated with each ID-attribute pairing    
  max_year <- aggregate(schoolyear~ sasid + df[[attribute]], data=df, max)
  names(max_year)[2] <- as.character(attribute)
  # Resolve the duplicates. This merges in the year value for each of the
  # modal attributes, arranges the result by ID and descending school year, 
  # and remove the duplicates.
  resolved <- subset(arrange(merge(dupes, max_year, 
                                   all.x=TRUE, all.y=FALSE), 
                             sasid, desc(schoolyear)), 
                     !duplicated(sasid))
  # Create the result dataframe that contains ID and the corrected attribute
  # values.
  resolved <- resolved[, c('sasid', attribute)]
  mode <- mode[, c('sasid', attribute)]
  result <- rbind(mode, resolved)
  return(result)
}