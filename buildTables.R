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
graduated <- subset(graduated, !is.na(grad_date))

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

# first HS
first_hs <- select_hs(rbind(tables2005_2006$enrollment, 
                            tables2006_2007$enrollment, 
                            tables2007_2008$enrollment, 
                            tables2008_2009$enrollment, 
                            tables2009_2010$enrollment, 
                            tables2010_2011$enrollment), 'first')
names(first_hs)[-1] <- paste(names(first_hs)[-1], '_first',sep='')

# last HS
last_hs <- select_hs(rbind(tables2005_2006$enrollment, 
                            tables2006_2007$enrollment, 
                            tables2007_2008$enrollment, 
                            tables2008_2009$enrollment, 
                            tables2009_2010$enrollment, 
                            tables2010_2011$enrollment), 'last')

names(last_hs)[-1] <- paste(names(last_hs)[-1], '_last',sep='')

# long HS
long_hs <- select_hs(rbind(tables2005_2006$enrollment, 
                            tables2006_2007$enrollment, 
                            tables2007_2008$enrollment, 
                            tables2008_2009$enrollment, 
                            tables2009_2010$enrollment, 
                            tables2010_2011$enrollment), 'long')
long_hs[,schoolyear:=NULL]
long_hs[,grade:=NULL]
long_hs[,exit_type:=NULL]

names(long_hs)[-1] <- paste(names(long_hs)[-1], '_long',sep='')

person <- rbind(tables2005_2006$person, tables2006_2007$person, 
                tables2007_2008$person, tables2008_2009$person, 
                tables2009_2010$person)[,c('sasid', 'studentid')]
person <- subset(person, !duplicated(sasid))
person <- merge(person, race)
person <- merge(person, sex)
person <- merge(person, student_lang)
person <- merge(person, parent_lang)

# High School Cohorts
hsdata <- merge(person, first_hs)
hsdata <- merge(hsdata, long_hs)
hsdata <- merge(hsdata, last_hs)

hsdata$graduated <- with(hsdata, ifelse(exit_type_last==15, 'Y','N'))

hsdata$still_enrl <- with(hsdata, ifelse(exit_type_last %in% 
                                           c(31, 30), 'Y', 'N'))

hsdata$transfer_out <- with(hsdata, ifelse(exit_type_last %in% 
                                             c(seq(1,14,1), 17), 'Y', 'N'))

hsdata$ged <- with(hsdata, ifelse(exit_type_last==23, 'Y','N'))

hsdata$dropout <- with(hsdata, ifelse(exit_type_last %in% c(21, 20, 25), 'Y', 
                                      'N')) 

hsdata$disappear <- with(hsdata, ifelse(exit_type_last==97, 'Y', 'N')) 

hsgrad2007 <- subset(hsdata, (schoolyear_first=='2007_2008' & grade_first==9) | (schoolyear_first=='2008_2009' & grade_first==10) | (schoolyear_first=='2009_2010' & grade_first==11) | (schoolyear_first=='2010_2011' & grade_first==12))

hsgrad2007 <- subset(hsgrad2007, transfer_out=='N')

test <-merge(hsgrad2007, tables2006_2007$achievement, all.x=TRUE)

# graduated$grad_date <- with(graduated, ifelse(graduated=='Y', exit_date, NA))
