# Build individual year tables.

setwd('/Users/jason/R Projects/PPSDColllegeReadiness/')

source('dependencies.R')
source('functions.R')
source('load.R')

executedAt <- paste(Sys.time())
tables2004_2005 <- build_tables(reg0405, 2005)
tables2005_2006 <- build_tables(reg0506, 2006)
tables2006_2007 <- build_tables(reg0607, 2007)
tables2007_2008 <- build_tables(reg0708, 2008)
tables2008_2009 <- build_tables(reg0809, 2009)
tables2009_2010 <- build_tables(reg0910, 2010)
tables2010_2011 <- build_tables(reg1011, 2011)

early_race <- modal_person_attribute(rbind(tables2005_2006$person, 
                                           tables2006_2007$person, 
                                           tables2007_2008$person,
                                           tables2008_2009$person, 
                                           tables2009_2010$person), 'race')
late_race <- modal_person_attribute(tables2010_2011$person, 'race')
early_race$schoolyear <- '2009_2010'
late_race$schoolyear <- '2010_2011'
race <- modal_person_attribute(rbind(early_race, late_race), 'race')

sex <- modal_person_attribute(rbind(tables2005_2006$person, 
                                    tables2006_2007$person, 
                                    tables2007_2008$person,
                                    tables2008_2009$person, 
                                    tables2009_2010$person,
                                    tables2010_2011$person), 'sex')