# Environment Setup
library(MASS)
library(foreign)
library(lubridate)
library(plyr)
library(ggplot2)
library(reshape2)
library(scales)
library(stringr)
library(pROC)
library(rstudio)
library(data.table)
library(knitr)
library(xtable)
library(sjPlot)
library(caret)
library(magrittr)
library(dplyr)



# Standard helper functions
"%w/o%" <- function(x, y) x[!x %in% y]