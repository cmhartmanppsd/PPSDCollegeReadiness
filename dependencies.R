# Environment Setup
require(foreign)
require(plyr)
require(dplyr)
require(ggplot2)
require(reshape2)
require(scales)
require(stringr)
require(pROC)
require(rstudio)
require(data.table)

# Standard helper functions
"%w/o%" <- function(x, y) x[!x %in% y]
