# Build HS Cohorts for both 9-10 early warning and 11-12 college success.

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

## Ever FRPL/LEP/SPED
annual_demos <- rbind(tables2005_2006$person_annual, 
                      tables2006_2007$person_annual, 
                      tables2007_2008$person_annual,
                      tables2008_2009$person_annual, 
                      tables2009_2010$person_annual)

annual_demos <- data.table(annual_demos, key = 'sasid')
attendance <- annual_demos[, list(totalpresent = sum(sum_present), 
                                  totalenroll = sum(sum_enrolled)), 
                           by=key(annual_demos)]

attendance$propattend <- with(attendance, totalpresent/totalenroll)

everELL <- subset(annual_demos, lep=='Y', select=c('sasid', 'lep'))
everELL <- subset(everELL, !duplicated(sasid))

everIEP <- subset(annual_demos, iep=='Y', select=c('sasid', 'iep'))
everIEP <- subset(everIEP, !duplicated(sasid))

person <- rbind(tables2005_2006$person, tables2006_2007$person, 
                tables2007_2008$person, tables2008_2009$person, 
                tables2009_2010$person)[,c('sasid', 'studentid')]
person <- subset(person, !duplicated(sasid))
person <- merge(person, race)
person <- merge(person, sex)
person <- merge(person, student_lang)
person <- merge(person, parent_lang)
person <- merge(person, dob)
person <- merge(person, attendance)
person <- merge(person, everELL, all.x=TRUE)
person <- merge(person, everIEP, all.x=TRUE)
person$lep <- as.character(person$lep)
person$iep <- as.character(person$iep)
person$lep <- with(person, ifelse(is.na(lep)==TRUE,'N',lep))
person$iep <- with(person, ifelse(is.na(iep)==TRUE, 'N', iep))
# Clean up
rm(race)
rm(sex)
rm(student_lang)
rm(parent_lang)
rm(dob)
rm(early_race)
rm(late_race)
rm(attendance)
rm(everELL)
rm(everIEP)


# High School Cohorts
hsdata <- merge(person, first_hs)
hsdata <- merge(hsdata, long_hs)
hsdata <- merge(hsdata, last_hs)

rm(first_hs)
rm(long_hs)
rm(last_hs)

hsdata$graduated <- with(hsdata, ifelse(exit_type_last==15, 'Y','N'))

hsdata$still_enrl <- with(hsdata, ifelse(exit_type_last %in% 
                                           c(31, 30), 'Y', 'N'))

hsdata$transfer_out <- with(hsdata, ifelse(exit_type_last %in% 
                                             c(seq(1,14,1), 17), 'Y', 'N'))

hsdata$ged <- with(hsdata, ifelse(exit_type_last==23, 'Y','N'))

hsdata$dropout <- with(hsdata, ifelse(exit_type_last %in% c(21, 20, 25), 'Y', 
                                      'N')) 

hsdata$disappear <- with(hsdata, ifelse(exit_type_last==97, 'Y', 'N')) 

hsgrad2007 <- subset(hsdata, (schoolyear_first=='2007_2008' & grade_first==9) | 
                             (schoolyear_first=='2008_2009' & grade_first==10) | 
                             (schoolyear_first=='2009_2010' & grade_first==11) | 
                             (schoolyear_first=='2010_2011' & grade_first==12))

hsgrad2007 <- subset(hsgrad2007, transfer_out=='N')

hsgrad2007 <- merge(hsgrad2007, 
                    tables2006_2007$person_annual[,c('sasid', 'sum_suspend')])
names(hsgrad2007)[which(names(hsgrad2007) %in% c('sum_suspend'))] <- 'eighthsuspend'

hsgrad2007 <- merge(hsgrad2007, 
                    tables2006_2007$person_annual[,c('sasid', 'sum_tardy')])
names(hsgrad2007)[which(names(hsgrad2007) %in% c('sum_tardy'))] <- 'eighthtardy'

# Prior performance
hsgrad2007 <- merge(hsgrad2007, 
                    subset(tables2006_2007$achievement, testgrade_N==8 & 
                           testgrade_N==grade, select= -c(schoolyear, 
                                                          last_name,
                                                          contentgrade_N)), 
                    all.x=TRUE)
# Eight Grade Attendance

hsgrad2007 <- merge(hsgrad2007,
                    subset(tables2006_2007$person_annual, grade==8,
                           select=c(sasid, grade, sum_enrolled, sum_present)))
hsgrad2007 <- mutate(hsgrad2007, 
                     eightattend=(sum_present/sum_enrolled),
                     ageHS=age_calc(dob, as.Date('2007-09-01'),units='months'))



hsgrad2007$race <- relevel(hsgrad2007$race, ref='White')
hsgrad2007$schno_first <- as.factor(hsgrad2007$schno_first)
hsgrad2007$schno_first <- relevel(hsgrad2007$schno_first, ref='164')

# Grades
# 8th grade GPA
eighthgpa <- aggregate(data=subset(tables2006_2007$course, grade==8), 
                      gpa ~ sasid + variable, FUN="mean")
eighthgpa <- dcast(eighthgpa, sasid ~ variable, value.var='gpa')
hsgrad2007 <- merge(hsgrad2007, eighthgpa, all.x=TRUE)
hsgrad2007 <- mutate(hsgrad2007,
                     gpaeighth=rowSums(cbind(cum_qtr1, cum_qtr2, cum_qtr3, 
                                             cum_qtr4), na.rm=TRUE)/
                               apply(cbind(cum_qtr1, cum_qtr2, cum_qtr3, 
                                           cum_qtr4), 1 , 
                              function(x) length(subset(x, !is.na(x)==TRUE))))

hsgrad2007$schno_first <- as.character(hsgrad2007$schno_first)

basemodel8thgrade <- glm(as.numeric(as.factor(graduated))-1 ~ sex + eightattend
                        + gpaeighth + reanormal + ageHS + I(schno_first=='164'),
                         data=hsgrad2007, family=binomial(link='logit'))

hsgrad2007$eightprediction <- predict(basemodel8thgrade, newdata=hsgrad2007, type='response')

# 9th grade GPA
ninthgpa <- aggregate(data=subset(tables2007_2008$course, grade==9), 
                      gpa ~ sasid + variable, FUN="mean")
ninthgpa <- dcast(ninthgpa, sasid ~ variable, value.var='gpa')
trajectory <- melt(ninthgpa, id.vars='sasid')
trajectory <- data.table(trajectory, key='sasid')
trajectory$variable <- recode(trajectory$variable, "'cum_qtr1'=1; 'cum_qtr2'=2; 
                                                    'cum_qtr3'=3; 'cum_qtr4'=4")
trajectory$variable <- as.numeric(trajectory$variable)
trajectory <- trajectory[,list(traj=lm(value~variable, 
                                       data=.SD)$coefficients[2]),by=sasid]
ninthgpa <- mutate(ninthgpa, 
                   gpa=(rowSums(cbind(cum_qtr1, cum_qtr2, cum_qtr3, cum_qtr4), 
                            na.rm=TRUE)/
                        apply(cbind(cum_qtr1, cum_qtr2, cum_qtr3, cum_qtr4), 1 , 
                              function(x) length(subset(x, !is.na(x)==TRUE)))))
ninthgpa <- merge(ninthgpa, trajectory, all.x=TRUE)
hsgrad2007 <-merge(hsgrad2007, ninthgpa, all.x=TRUE)

# Repeats 9th grade
repeater9th <- subset(tables2007_2008$person_annual, 
                      grade==9 & isrepeatinggr=='N')[,c('sasid','willrepeatgr')]
hsgrad2007 <- merge(hsgrad2007, repeater9th, all.x=TRUE)

## 10th grade GPA
tenthgpa <- aggregate(data=subset(tables2008_2009$course, grade %in% c(9,10)),
                      gpa ~ sasid + variable + grade, FUN='mean')
tenthgpa <- dcast(tenthgpa, sasid + grade ~ variable, value.var='gpa')
tenthgpa <- mutate(tenthgpa, 
                   gpa=(rowSums(cbind(cum_qtr1, cum_qtr2, cum_qtr3, cum_qtr4), 
                                na.rm=TRUE)/
                        apply(cbind(cum_qtr1, cum_qtr2, cum_qtr3, cum_qtr4), 
                              1, function(x) length(subset(x, 
                                                           !is.na(x)==TRUE)))))
tenthgpa <- subset(tenthgpa, sasid %in% ninthgpa$sasid)




# graduated$grad_date <- with(graduated, ifelse(graduated=='Y', exit_date, NA))