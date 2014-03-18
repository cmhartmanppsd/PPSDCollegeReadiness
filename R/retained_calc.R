retained_calc <- function(x, sid='sasid', grade = 'grade', grade_value = 9){
  grouping <- lapply(list(sid, grade), as.symbol)
  x <- x %.%
       regroup(grouping) %.%
       summarize(count = n())
  x$retained <- ifelse(x$count > 1, 'Y', 'N')
  x <- filter(x, grade == grade_value)
  x[, c(sid, 'retained')]
}