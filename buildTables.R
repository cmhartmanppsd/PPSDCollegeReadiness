# Build individual year tables.

setwd('/Users/jason/R Projects/PPSDColllegeReadiness/')

source('dependencies.R')
source('functions.R')
source('load.R')

executedAt <- paste(Sys.time())
tables2004_2005 <- build_tables(reg0405, 2005)
tables2005_2006 <- build_tables(reg0506, 2006)
tables2006_2007 <- build_tables(reg0607, 2007)