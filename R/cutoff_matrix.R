cutoff_matrix <- function(df, classifier, outcome, breaks=seq(1,0,-.01)){
  results <- data.frame(cutoff=vector(mode='numeric', length=length(breaks)),
                        true_pos=vector(mode='numeric', length=length(breaks)),
                        true_neg=vector(mode='numeric', length=length(breaks)),
                        false_pos=vector(mode='numeric', length=length(breaks)),
                        false_neg=vector(mode='numeric', length=length(breaks)))
  for(i in seq(1, length(breaks))){
    value <- breaks[i]
    j <- data.frame(table(df[[classifier]]>value, df[[outcome]]))
    results[i,1] <- value
    results[i,2] <- subset(j, Var1==TRUE & Var2=='Y')$Freq                     
    results[i,3] <- subset(j, Var1==FALSE & Var2=='N')$Freq
    results[i,4] <- subset(j, Var1==TRUE & Var2=='N')$Freq 
    results[i,5] <- subset(j, Var1==FALSE & Var2=='Y')$Freq
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