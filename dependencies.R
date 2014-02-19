# Environment Setup
require(plyr)
require(foreign)
require(reshape2)
require(ggplot2)
require(scales)
require(stringr)
require(data.table)
require(car)
require(ROCR)
require(pROC)
require(rstudio)
require(dplyr)
# Standard helper functions
"%w/o%" <- function(x, y) x[!x %in% y]
