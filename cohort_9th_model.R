# Cohort 9th grade models
hs0607 <- subset(person, (schoolyear_first=='2006_2007' & grade_first==9) | 
                         (schoolyear_first=='2007_2008' & grade_first==10) | 
                         (schoolyear_first=='2008_2009' & grade_first==11) | 
                         (schoolyear_first=='2009_2010' & grade_first==12)))

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
eighthgpa_0708 <- aggregate(data=subset(tables2007_2008$course, grade==8),
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
names(subjeighthgpa)[-1] <- paste(names(subjeighthgpa)[-1], "8th", sep='')
names(subjeighthgpa)[which(names(subjeighthgpa) %in% c('non-core8th'))] <- 
  'noncore8th'

eightcourseperf <- merge(eighthgpa, subjeighthgpa)

# Merge prior course performance data with high school set and clean up.
hs0708 <- merge(hs0708, eightcourseperf, all.x=TRUE)
rm(eightcourseperf)
rm(eighthgpa)
rm(subjeighthgpa)

hs0708 <- merge(hs0708, eightfails, all.x=TRUE)
