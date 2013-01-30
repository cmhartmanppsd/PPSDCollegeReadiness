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
###

# Build cannonical race for person table
early_race <- modal_person_attribute(rbind(tables2005_2006$person, 
                                           tables2006_2007$person, 
                                           tables2007_2008$person,
                                           tables2008_2009$person, 
                                           tables2009_2010$person), 'race')
late_race <- modal_person_attribute(tables2010_2011$person, 'race')
early_race$schoolyear <- '2009_2010'
late_race$schoolyear <- '2010_2011'
race <- modal_person_attribute(rbind(early_race, late_race), 'race')

# Build cannonical sex for person table
sex <- modal_person_attribute(rbind(tables2005_2006$person, 
                                    tables2006_2007$person, 
                                    tables2007_2008$person,
                                    tables2008_2009$person, 
                                    tables2009_2010$person,
                                    tables2010_2011$person), 'sex')

# Build cannonical graduation flag and date for person table
graduated <- rbind(tables2005_2006$enroll, tables2006_2007$enroll, 
                   tables2007_2008$enroll, tables2008_2009$enroll,
                   tables2009_2010$enroll, tables2010_2011$enroll)
first9th <- aggregate(schoolyear~sasid, data=subset(graduated, grade==9), min)
names(first9th)[2] <- 'first9th'
graduated$graduated <- with(graduated, ifelse(exit_type==15, 'Y','N'))
graduated$grad_date <- with(graduated, ifelse(graduated=='Y', exit_date, NA))
graduated$grad_date <- as.Date(graduated$grad_date, origin='1970-01-01')
graduated <- graduated[,c('sasid', 'graduated', 'grad_date')]

# Build cannonical dob
dob <- modal_person_attribute(rbind(tables2005_2006$person, 
                                    tables2006_2007$person, 
                                    tables2007_2008$person,
                                    tables2008_2009$person, 
                                    tables2009_2010$person,
                                    tables2010_2011$person), 'dob')

# Build cannonical student_lang
student_lang <- modal_person_attribute(rbind(tables2005_2006$person, 
                                             tables2006_2007$person, 
                                             tables2007_2008$person,
                                             tables2008_2009$person, 
                                             tables2009_2010$person,
                                             tables2010_2011$person), 
                                       'student_lang')

# Build cannonical parent_lang
parent_lang <- modal_person_attribute(rbind(tables2005_2006$person, 
                                            tables2006_2007$person, 
                                            tables2007_2008$person,
                                            tables2008_2009$person, 
                                            tables2009_2010$person,
                                            tables2010_2011$person), 
                                      'parent_lang')


person <- rbind(tables2005_2006$person, tables2006_2007$person, 
                tables2007_2008$person, tables2008_2009$person, 
                tables2009_2010$person)[,c('sasid', 'studentid')]

person <- merge(merge(merge(merge(merge(merge(person, race), sex), graduated), 
                            dob), student_lang), parent_lang)