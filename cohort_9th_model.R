# Cohort 9th grade models
hs0607 <- subset(person, (schoolyear_first=='2006_2007' & grade_first==9) | 
                         (schoolyear_first=='2007_2008' & grade_first==10) | 
                         (schoolyear_first=='2008_2009' & grade_first==11) | 
                         (schoolyear_first=='2009_2010' & grade_first==12))

hs0708 <- subset(person, (schoolyear_first=='2007_2008' & grade_first==9) | 
                         (schoolyear_first=='2008_2009' & grade_first==10) | 
                         (schoolyear_first=='2009_2010' & grade_first==11) | 
                         (schoolyear_first=='2010_2011' & grade_first==12))
# Calculate Attendance
attendance <- subset(ldply(list(tables2005_2006$person_annual, 
                                tables2006_2007$person_annual, 
                                tables2007_2008$person_annual, 
                                tables2008_2009$person_annual,
                                tables2009_2010$person_annual, 
                                tables2010_2011$person_annual),
                           mutate, attendance = sum_present/sum_enrolled,
                                   tardy = sum_tardy/sum_enrolled,
                                   suspended = sum_suspend), 
                     select = c('sasid', 'schoolyear', 'attendance', 'tardy', 
                                'suspended'))

hs0708 <- merge(hs0708, subset(attendance, schoolyear=='2006_2007')[, 
                               !names(attendance) %in% c('schoolyear')], 
                all.x=TRUE)
names(hs0708)[which(names(hs0708) %in% c('suspended'))] <- 'suspend8th'
names(hs0708)[which(names(hs0708) %in% c('tardy'))] <- 'tardy8th'
names(hs0708)[which(names(hs0708) %in% c('attendance'))] <- 'attendance8th'

hs0607 <- merge(hs0607, subset(attendance, schoolyear=='2005_2006')[, 
                               !names(attendance) %in% c('schoolyear')], 
                all.x=TRUE)
names(hs0607)[which(names(hs0607) %in% c('suspended'))] <- 'suspend8th'
names(hs0607)[which(names(hs0607) %in% c('tardy'))] <- 'tardy8th'
names(hs0607)[which(names(hs0607) %in% c('attendance'))] <- 'attendance8th'


hs0607$attendnormal <- (hs0607$attendance8th - 
                        mean(hs0607$attendance8th, na.rm=TRUE)) / 
                        sd(hs0607$attendance8th, na.rm=TRUE)

hs0708$attendnormal <- (hs0708$attendance8th - 
                        mean(hs0708$attendance8th, na.rm=TRUE)) / 
                        sd(hs0708$attendance8th, na.rm=TRUE)

# Calculate age when student enters 9th grade for the first time.
hs0708 <- mutate(hs0708, ageHS = age_calc(dob, as.Date('2007-09-01'), 
                                          units='months'))
hs0607 <- mutate(hs0607, ageHS = age_calc(dob, as.Date('2006-09-01'), 
                                          units='months'))

mobile9th_0607 <- moves_calc(subset(tables2006_2007$enrollment, 
                                    sasid %in% hs0607$sasid))
mobile9th_0708 <- moves_calc(subset(tables2007_2008$enrollment, 
                                    sasid %in% hs0708$sasid))
names(mobile9th_0607)[1] <- 'sasid'
names(mobile9th_0708)[1] <- 'sasid'
hs0607 <- merge(hs0607, mobile9th_0607, all.x=TRUE)
hs0708 <- merge(hs0708, mobile9th_0708, all.x=TRUE)

## Course Performance, 8th and 9th
# Calculate 8th grade course performance, first by calculating gpa by student
eighthgpa_0607 <- aggregate(data=subset(tables2005_2006$course, grade==8),
                            gpa ~ sasid, FUN="mean")
eighthgpa_0708 <- aggregate(data=subset(tables2006_2007$course, grade==8),
                            gpa ~ sasid, FUN="mean")

# Append 8th to the gpa
names(eighthgpa_0607)[2] <- paste(names(eighthgpa_0607)[2], '8th', sep='')
names(eighthgpa_0708)[2] <- paste(names(eighthgpa_0708)[2], '8th', sep='')
# Then calculate gpa by course subject area, making the data wide

subjeighthgpa_0708 <- dcast(aggregate(data=subset(tables2006_2007$course, 
                                                  grade==8),
                                      gpa ~ sasid + subject, FUN="mean"), 
                            sasid ~ subject, value.var="gpa")

subjeighthgpa_0607 <- dcast(aggregate(data=subset(tables2005_2006$course, 
                                                  grade==8),
                                      gpa ~ sasid + subject, FUN="mean"), 
                            sasid ~ subject, value.var="gpa")


# Append 8th to the end of each subject gpa
names(subjeighthgpa_0607)[-1] <- paste(names(subjeighthgpa_0607)[-1], 
                                       "8th", sep='')
names(subjeighthgpa_0607)[which(names(subjeighthgpa_0607) %in% 
                                c('non-core8th'))] <- 'noncore8th'
names(subjeighthgpa_0708)[-1] <- paste(names(subjeighthgpa_0708)[-1], 
                                       "8th", sep='')
names(subjeighthgpa_0708)[which(names(subjeighthgpa_0708) %in% 
                                c('non-core8th'))] <- 'noncore8th'


eightcourseperf_0607 <- merge(eighthgpa_0607, subjeighthgpa_0607)
eightcourseperf_0708 <- merge(eighthgpa_0708, subjeighthgpa_0708)

# Merge prior course performance data with high school set and clean up.
hs0607 <- merge(hs0607, eightcourseperf_0607, all.x=TRUE)
hs0708 <- merge(hs0708, eightcourseperf_0708, all.x=TRUE)

rm(eightcourseperf_0708)
rm(eightcourseperf_0607)
rm(eighthgpa_0607)
rm(eighthgpa_0708)
rm(subjeighthgpa_0607)
rm(subjeighthgpa_0708)

# Calculate 9th grade course performance, first by calculating gpa by student
ninthgpa_0607 <- aggregate(data=subset(tables2006_2007$course, grade==9),
                            gpa ~ sasid, FUN="mean")
ninthgpa_0708 <- aggregate(data=subset(tables2007_2008$course, grade==9),
                            gpa ~ sasid, FUN="mean")

# Append 9th to the gpa
names(ninthgpa_0607)[2] <- paste(names(ninthgpa_0607)[2], '9th', sep='')
names(ninthgpa_0708)[2] <- paste(names(ninthgpa_0708)[2], '9th', sep='')
# Then calculate gpa by course subject area, making the data wide

subjninthgpa_0708 <- dcast(aggregate(data=subset(tables2007_2008$course, 
                                                  grade==9),
                                      gpa ~ sasid + subject, FUN="mean"), 
                            sasid ~ subject, value.var="gpa")

subjninthgpa_0607 <- dcast(aggregate(data=subset(tables2006_2007$course, 
                                                  grade==9),
                                      gpa ~ sasid + subject, FUN="mean"), 
                            sasid ~ subject, value.var="gpa")


# Append 9th to the end of each subject gpa
names(subjninthgpa_0607)[-1] <- paste(names(subjninthgpa_0607)[-1], 
                                       "9th", sep='')
names(subjninthgpa_0607)[which(names(subjninthgpa_0607) %in% 
                                  c('non-core9th'))] <- 'noncore9th'
names(subjninthgpa_0708)[-1] <- paste(names(subjninthgpa_0708)[-1], 
                                       "9th", sep='')
names(subjninthgpa_0708)[which(names(subjninthgpa_0708) %in% 
                                  c('non-core9th'))] <- 'noncore9th'


ninthcourseperf_0607 <- merge(ninthgpa_0607, subjninthgpa_0607)
ninthcourseperf_0708 <- merge(ninthgpa_0708, subjninthgpa_0708)

# Merge prior course performance data with high school set and clean up.
hs0607 <- merge(hs0607, ninthcourseperf_0607, all.x=TRUE)
hs0708 <- merge(hs0708, ninthcourseperf_0708, all.x=TRUE)

# Occupational Courses
occupation_desc_0607 <- unique(grep(pattern='occup', 
                                    x=tables2006_2007$course$coursedesc, 
                                    ignore.case=TRUE, value=TRUE))
occupation_desc_0708 <- unique(grep(pattern='occup', 
                                    x=tables2007_2008$course$coursedesc, 
                                    ignore.case=TRUE, value=TRUE))
courses_0607 <- tables2006_2007$course[,c('sasid', 'coursedesc', 'subject')]
courses_0607$occupational <- with(tables2006_2007$course, 
                                  ifelse(coursedesc %in% occupation_desc_0607, 
                                         TRUE, FALSE))
courses_0607 <- courses_0607[, !names(courses_0607) %in% c('coursedesc')]
courses_0607 <- unique(courses_0607)
# hs0607$elaocc <- with(courses_0607, ifelse(hs0607$sasid %in% sasid & 
#                                      subject=='ela' & 
#                                      occupational==TRUE, 'Y', 'N'))
# courses_0607 <- as.data.frame(prop.table(table(courses_0607$sasid, courses_0607$occupational),1))
# names(courses_0607) <- c('sasid', 'Var2', 'occupational')
# courses_0607

rm(ninthcourseperf_0708)
rm(ninthcourseperf_0607)
rm(ninthgpa_0607)
rm(ninthgpa_0708)
rm(subjninthgpa_0607)
rm(subjninthgpa_0708)



# Retention
retain_0607 <- subset(tables2006_2007$person_annual, 
                      grade==9 & isrepeatinggr=='N')[,c('sasid','willrepeatgr')]

retain_0708 <- subset(tables2007_2008$person_annual, 
                      grade==9 & isrepeatinggr=='N')[,c('sasid','willrepeatgr')]
hs0607 <- merge(hs0607, retain_0607, all.x=TRUE)
hs0708 <- merge(hs0708, retain_0708, all.x=TRUE)

rm(retain_0607)
rm(retain_0708)

hs0607$year <- '2006_2007'
hs0708$year <- '2007_2008'

hsall_9th <- rbind(hs0607, hs0708)



hscohort_9th <- subset(hsall_9th, transfer_out=='N')
