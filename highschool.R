# High School Cohort building

# This script will build one high school cohort. In the future, this could be
# refactored as a big function/routine to create any arbitrary cohort from the
# person table. This script assumes that person.R has already been succesfully
# run.

# Form cohort that entered high school for the first time in Providence in the
# 2007_2008 school year.
hs0708 <- subset(person, (schoolyear_first=='2007_2008' & grade_first==9) | 
                         (schoolyear_first=='2008_2009' & grade_first==10) | 
                         (schoolyear_first=='2009_2010' & grade_first==11) | 
                         (schoolyear_first=='2010_2011' & grade_first==12))

# Attendance Calculations
# Annual attendance calculated by year based on sum_present and sum_enrolled.
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

# Add 8th grade characteristics that might be predictive of later outcomes.
# Suspensions in 8th grade
hs0708 <- merge(hs0708, subset(attendance, schoolyear=='2006_2007')[, 
					                     !names(attendance) %in% c('schoolyear')], 
				        all.x=TRUE)
names(hs0708)[which(names(hs0708) %in% c('suspended'))] <- 'suspend8th'
names(hs0708)[which(names(hs0708) %in% c('tardy'))] <- 'tardy8th'
names(hs0708)[which(names(hs0708) %in% c('attendance'))] <- 'attendance8th'

# Calculate age when student enters 9th grade for the first time.
hs0708 <- mutate(hs0708, ageHS = age_calc(dob, as.Date('2007-09-01'), 
                                          units='months'))

# Calculate mobility for students
# Eighth Grade year
mobile8th <- calc_moves(subset(tables2006_2007$enrollment, 
                               sasid %in% hs0708$sasid))
names(mobile8th)[1] <- 'sasid'
hs0708 <- merge(hs0708, mobile8th, all.x=TRUE)h

# Bring in 8th grade performance on standardized tests.
hs0708 <- merge(hs0708, subset(tables2006_2007$achievement, testgrade_N==8 &
                               testgrade_N==grade, select=-c(schoolyear,
                                                             last_name,
                                                             contentgrade_N)),
                all.x=TRUE)

# Calculate 8th grade course performance, first by calculating gpa by student
eighthgpa <- aggregate(data=subset(tables2006_2007$course, grade==8),
                       gpa ~ sasid, FUN="mean")

# Append 8th to the gpa
names(eighthgpa)[2] <- paste(names(eighthgpa)[2], '8th', sep='')

# Then calculate gpa by course subject area, making the data wide
subjeighthgpa <- dcast(aggregate(data=subset(tables2006_2007$course, grade==8),
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

# 9th grade quarter 1 course performance
ninthgpa <- aggregate(data=subset(tables2007_2008$course, grade==9), 
                      gpa ~ sasid + variable, FUN="mean")
ninthqtrgpa <- dcast(ninthgpa, sasid ~ variable, value.var='gpa')
ninthqtrgpa <- ninthqtrgpa[,c(1,2)]
names(ninthqtrgpa)[2] <- 'gpa9thqtr1'

hs0708 <- merge(hs0708, ninthqtrgpa, all.x=TRUE)
hs0708$reaal <- str_trim(as.character(hs0708$reaal), side='both')
hs0708$matal <- str_trim(as.character(hs0708$matal), side='both')
rm(ninthqtrgpa)

# Repeat 9th?
repeater9th <- subset(tables2007_2008$person_annual, 
                      grade==9 & isrepeatinggr=='N')[,c('sasid','willrepeatgr')]
hs0708 <- merge(hs0708, repeater9th, all.x=TRUE)



# For accountability purposes, students who transfer out are excluded from their
# high school's graduation rate. This analysis will primarily exclude them as
# well because their outcomes are unknown. Later analysis will likely predict 
# outcomes for these students based on observed characteristics and see how they
# are different or similar from students who do not leave Providence.

hscohort0708 <- subset(hs0708, transfer_out=='N')
hscohort0708$attendnormal <- (hscohort0708$attendance8th-mean(hscohort0708$attendance8th, na.rm=TRUE))/sd(hscohort0708$attendance8th, na.rm=TRUE)
