# Environment Setup
require(foreign)
require(reshape2)
require(ggplot2)
require(scales)
require(stringr)
require(data.table)
require(pROC)
require(rstudio)
require(plyr)
require(dplyr)
# Standard helper functions
"%w/o%" <- function(x, y) x[!x %in% y]
