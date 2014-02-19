cutoff_matrix <- function(df, classifier, outcome, breaks=seq(1,0,-.01)){
  df <- subset(df, !is.na(classifier) & !is.na(outcome))
  require(dplyr)
  results <- data.frame(cutoff=vector(mode='numeric', length=length(breaks)),
                        true_pos=vector(mode='numeric', length=length(breaks)),
                        true_neg=vector(mode='numeric', length=length(breaks)),
                        false_pos=vector(mode='numeric', length=length(breaks)),
                        false_neg=vector(mode='numeric', length=length(breaks)))
  for(i in seq(1, length(breaks))){
    threshold <- breaks[i]
    #j <- data.frame(table(df[[classifier]]>value, df[[outcome]]))
    results[i,1] <- threshold
    results[i,2] <- nrow(filter(df, df[[outcome]] == 'Y' & df[[classifier]] >= threshold))
    results[i,3] <- nrow(filter(df, df[[outcome]] == 'N' & df[[classifier]] <  threshold))
    results[i,4] <- nrow(filter(df, df[[outcome]] == 'N' & df[[classifier]] >= threshold))
    results[i,5] <- nrow(filter(df, df[[outcome]] == 'Y' & df[[classifier]] <  threshold))
  }
  results <- mutate(results, 
                    true_neg_rate  = true_neg / (true_neg + false_neg), 
                    true_pos_rate  = true_pos / (true_pos + false_pos), 
                    false_pos_rate = false_pos / (true_pos + false_pos),
                    false_neg_rate = false_neg / (true_neg + false_neg),
                    accuracy       = (true_pos + true_neg) / 
                                     (true_pos + true_neg + false_pos + false_neg))
  return(results)
}