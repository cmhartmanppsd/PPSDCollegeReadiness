# Build individual year tables.

# setwd('/Users/jason/R Projects/PPSDColllegeReadiness/')

# source('dependencies.R')
# source('load.R')

executedAt <- paste(Sys.time())
# tables2004_2005 <- build_tables(reg0405, 2005)
tables2005_2006 <- build_tables(reg0506, 2006)
print('2006')
tables2006_2007 <- build_tables(reg0607, 2007)
print('2007')
tables2007_2008 <- build_tables(reg0708, 2008)
print('2008')
tables2008_2009 <- build_tables(reg0809, 2009)
print('2009')
tables2009_2010 <- build_tables(reg0910, 2010)
print('2010')
tables2010_2011 <- build_tables(reg1011, 2011)
print('2011')
###

person_attributes <- c('studentid', 'sasid', 'dob', 'first_name', 'last_name',
                       'student_lang', 'parent_lang', 'birth_place', 'sex',
                       'schoolyear', 'race')
person_annual_attributes <- c('studentid', 'sasid', 'last_name', 'grade', 
                              'lunch', 'lep', 'iep', 'plp', 'sec504', 
                              'graduated', 'willrepeatgr', 'isrepeatinggr',
                              "disab", "spedprogrm", "str_name", "apt_no", 
                              "city", "state", "zip_code", "sum_absent",
                              "sum_excabsent", "sum_tardy", "sum_on_time",
                              "sum_suspend", "sum_enrolled", "sum_present",
                              "sum_soccurin", "sum_soccurout", "schoolyear")
marks1112 <- read.csv('/Volumes/ProvidenceFiles/NewMarks/Marks_2011_2012.csv', 
                      stringsAsFactors=FALSE)
marks1112$studentid <- as.character(marks1112$studentid)
tables2011_2012 <- list(person = subset(stu2011_12, 
                                        select = person_attributes),
                        person_annual = subset(stu2011_12,
                                               select = person_annual_attributes),
                        enrollment = enr2011_12,
                        course = extract_course(marks1112),
                        achievement = achievement1112)
tables2011_2012$person <- subset(tables2011_2012$person, !duplicated(tables2011_2012$person))
tables2011_2012$person$sex <- ifelse(!tables2011_2012$person$sex %in% c('F','M'), 
                                     NA, tables2011_2012$person$sex) 
tables2011_2012$person$sex<- factor(tables2011_2012$person$sex, 
                                    labels = c('Female', 'Male'))
tables2011_2012$person_annual <- subset(tables2011_2012$person_annual, 
                                        !duplicated(tables2011_2012$person_annual))
tables2011_2012$person_annual$studentid <- as.character(tables2011_2012$person_annual$studentid)
tables2011_2012$course <- left_join(tables2011_2012$course,
                                    tables2011_2012$person_annual %.%
                                    select(studentid, sasid, grade))
tables2011_2012$course$credits <- gsub("\\*","", tables2011_2012$course$credits)
tables2011_2012$course$credits <- str_trim(tables2011_2012$course$credits)
marks1213 <- read.csv('/Volumes/ProvidenceFiles/NewMarks/Marks_2012_2013.csv',
                      stringsAsFactors=FALSE)
marks1213$studentid <- as.character(marks1213$studentid)
tables2012_2013 <-  list(person = subset(stu2012_13, 
                                         select = person_attributes),
                         person_annual = subset(stu2012_13,
                                                select = person_annual_attributes),
                         enrollment = enr2012_13,
                         course = extract_course(marks1213))
tables2012_2013$person <- subset(tables2012_2013$person, !duplicated(tables2012_2013$person))
tables2012_2013$person$sex <- ifelse(!tables2012_2013$person$sex %in% c('F','M'), 
                                     NA, tables2012_2013$person$sex) 
tables2012_2013$person$sex<- factor(tables2012_2013$person$sex, 
                                    labels = c('Female', 'Male'))
tables2012_2013$person_annual <- subset(tables2012_2013$person_annual, 
                                        !duplicated(tables2012_2013$person_annual))
tables2012_2013$person_annual$studentid <- as.character(tables2012_2013$person_annual$studentid)
tables2012_2013$course <- left_join(tables2012_2013$course,
                                    tables2012_2013$person_annual %.%
                                    select(studentid, sasid, grade))
tables2012_2013$course$credits <- gsub("\\*","", tables2012_2013$course$credits)
tables2012_2013$course$credits <- str_trim(tables2012_2013$course$credits)