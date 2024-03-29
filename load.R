# Loading SPSS files
# Note: this will fail unless you mount the encrypted .dmg at
# /Users/jason/Desktop/ProvidenceFiles.dmg

reg0405 <- read.spss('/Volumes/ProvidenceFiles/MasterFiles/REG Master_0405.sav',
                     to.data.frame=TRUE,
                     use.value.labels=TRUE,
                     trim.factor.names=TRUE,
                     use.missings=TRUE)
reg0405$sasid_0405 <- as.character(reg0405$sasid_0405)
reg0506 <- read.spss('/Volumes/ProvidenceFiles/MasterFiles/REG Master_0506.sav',
                     to.data.frame=TRUE,
                     use.value.labels=TRUE,
                     trim.factor.names=TRUE,
                     use.missings=TRUE)
reg0506$sasid_0506 <- as.character(reg0506$sasid_0506)
reg0607 <- read.spss('/Volumes/ProvidenceFiles/MasterFiles/REG Master_0607.sav',
                     to.data.frame=TRUE,
                     use.value.labels=TRUE,
                     trim.factor.names=TRUE,
                     use.missings=TRUE)
reg0607$sasid_0607 <- as.character(reg0607$sasid_0607)
reg0708 <- read.spss('/Volumes/ProvidenceFiles/MasterFiles/REG Master_0708.sav',
                     to.data.frame=TRUE,
                     use.value.labels=TRUE,
                     trim.factor.names=TRUE,
                     use.missings=TRUE)
reg0708$sasid_0708 <- as.character(reg0708$sasid_0708)
reg0809 <- read.spss('/Volumes/ProvidenceFiles/MasterFiles/REG Master_0809.sav',
                     to.data.frame=TRUE,
                     use.value.labels=TRUE,
                     trim.factor.names=TRUE,
                     use.missings=TRUE)
reg0809$sasid_0809 <- as.character(reg0809$sasid_0809)
reg0910 <- read.spss('/Volumes/ProvidenceFiles/MasterFiles/REG Master_0910.sav',
                     to.data.frame=TRUE,
                     use.value.labels=TRUE,
                     trim.factor.names=TRUE,
                     use.missings=TRUE)
reg0910$sasid_0910 <- as.character(reg0910$sasid_0910)
reg1011 <- read.spss('/Volumes/ProvidenceFiles/MasterFiles/REG Master_1011.sav',
                     to.data.frame=TRUE,
                     use.value.labels=TRUE,
                     trim.factor.names=TRUE,
                     use.missings=TRUE)
reg1011$sasid_1011 <- as.character(reg1011$sasid_1011)
## Newer Data
stu2011_12 <- read.csv('/Volumes/ProvidenceFiles/REGData/Student_2011_2012.csv')
names(stu2011_12) <- c('schoolyear', 'studentid', 'sasid', 'last_name', 
                       'first_name', 'schno', 'school', 'grade', 'dob', 'sex', 
                       'ed_type', 'hr', 'hr_tchr', 'street_no', 'str_name', 
                       'apt_no', 'city', 'state', 'zip_code', 'race', 'lunch', 
                       'lep', 'iep', 'plp', 'sec504', 'disab', 'spedprogrm', 
                       'sum_absent', 'sum_excabsent', 'sum_tardy', 
                       'sum_on_time','sum_suspend', 'sum_enrolled', 
                       'sum_present', 'sum_soccurin', 'sum_soccurout', 
                       'privacy', 'willrepeatgr', 'isrepeatinggr', 'dropout',
                       'graduated', 'enroll_date', 'exit_date', 'exit_type', 
                       'exit_description', 'adm', 'ada', 'is_bused', 'ripta', 
                       'student_lang', 'parent_lang', 'birth_place')
stu2011_12$dob <- as.Date(stu2011_12$dob, format='%m/%d/%Y')
stu2011_12$schoolyear <- '2011_2012'
stu2011_12$sum_present <- with(stu2011_12, ifelse(sum_present < 0, NA, sum_present))
enr2011_12 <- read.csv('/Volumes/ProvidenceFiles/REGData/Enrollment_2011_2012.csv')
enr2011_12 <- merge(enr2011_12, 
                    subset(stu2011_12[, c('studentid', 'sasid', 'grade', 'last_name')],
                           !duplicated(stu2011_12[, c('studentid', 'sasid', 
                                                      'grade', 'last_name')])),
                    by.x = 'id', by.y = 'studentid')
names(enr2011_12)[which(names(enr2011_12)=='id')] <- 'studentid'
names(enr2011_12)[which(names(enr2011_12)=='schyr')] <- 'schoolyear'
names(enr2011_12)[which(names(enr2011_12)=='schyr')] <- 'schoolyear'
enr2011_12$enroll_date <- as.Date(enr2011_12$enroll_date, format='%m/%d/%Y')
enr2011_12$exit_date <- as.Date(enr2011_12$exit_date, format='%m/%d/%Y')
enr2011_12$lastname <- NULL
enr2011_12$firstname <- NULL
grades2011_12 <- read.csv('/Volumes/ProvidenceFiles/REGData/Marks_2011_2012.csv')
grades2011_12 <- merge(grades2011_12, stu2011_12[, c('studentid', 'sasid', 
                                                     'grade')],
                       by = 'studentid')
names(grades2011_12)[which(names(grades2011_12)=='schyr')] <- 'schoolyear'

# 2012_13
stu2012_13 <- read.csv('/Volumes/ProvidenceFiles/REGData/Student_2012_2013.csv')
names(stu2012_13) <- c('schoolyear', 'studentid', 'sasid', 'last_name', 
                       'first_name', 'schno', 'school', 'grade', 'dob', 'sex', 
                       'ed_type', 'hr', 'hr_tchr', 'street_no', 'str_name', 
                       'apt_no', 'city', 'state', 'zip_code', 'race', 'lunch', 
                       'lep', 'iep', 'plp', 'sec504', 'disab', 'spedprogrm', 
                       'sum_absent', 'sum_excabsent', 'sum_tardy', 
                       'sum_on_time','sum_suspend', 'sum_enrolled', 
                       'sum_present', 'sum_soccurin', 'sum_soccurout', 
                       'privacy', 'willrepeatgr', 'isrepeatinggr', 'dropout',
                       'graduated', 'enroll_date', 'exit_date', 'exit_type', 
                       'exit_description', 'adm', 'ada', 'is_bused', 'ripta', 
                       'student_lang', 'parent_lang', 'birth_place')
stu2012_13$dob <- as.Date(stu2012_13$dob, format='%m/%d/%Y')
stu2012_13$schoolyear <- '2012_2013'
stu2012_13$sum_present <- with(stu2012_13, ifelse(sum_present < 0, NA, sum_present))
enr2012_13 <- read.csv('/Volumes/ProvidenceFiles/REGData/Enrollment_2012_2013.csv')
enr2012_13 <- merge(enr2012_13, 
                    subset(stu2012_13[, c('studentid', 'sasid', 'grade', 'last_name')],
                           !duplicated(stu2012_13[, c('studentid', 'sasid', 
                                                      'grade', 'last_name')])),
                    by.x = 'id', by.y = 'studentid')
names(enr2012_13)[which(names(enr2012_13)=='id')] <- 'studentid'
names(enr2012_13)[which(names(enr2012_13)=='schyr')] <- 'schoolyear'
enr2012_13$enroll_date <- as.Date(enr2012_13$enroll_date, format='%m/%d/%Y')
enr2012_13$exit_date <- as.Date(enr2012_13$exit_date, format='%m/%d/%Y')
enr2012_13$lastname <- NULL
enr2012_13$firstname <- NULL

grades2012_13 <- read.csv('/Volumes/ProvidenceFiles/REGData/Marks_2012_2013.csv')
grades2012_13 <- merge(grades2012_13, stu2012_13[, c('studentid', 'sasid', 
                                                     'grade')],
                       by = 'studentid')
names(grades2012_13)[which(names(grades2012_13)=='schyr')] <- 'schoolyear'

achievement1112 <- read.csv('/Volumes/ProvidenceFiles/NECAP/NECAPFall2011.csv',
                            stringsAsFactors=FALSE)
names(achievement1112) <- tolower(names(achievement1112))
achievement1112 <- achievement1112 %.%
                   select(rptstudid, lname, grade, reaal, reascaledscore, 
                          matal, matscaledscore, wrial, wriscaledscore)
names(achievement1112) <- c('sasid', 'last_name', 'grade', 'reaal', 'reascsc',
                            'matal', 'matscsc', 'wrial', 'wriscsc')
achievement1112$schoolyear <- '2011_2012'
achievement1112$testgrade_N <- substr(achievement1112$matscsc, 1, 1)
achievement1112$testgrade_N <- as.numeric(achievement1112$testgrade_N)
achievement1112$testgrade_N <- ifelse(achievement1112$testgrade_N == 1, 11,
                                      achievement1112$testgrade_N)
achievement1112 <- achievement1112 %.%
                   mutate(contentgrade_N = testgrade_N - 1)
achievement1112[,c('reascsc', 'matscsc')] <- 
  lapply(achievement1112[,c('reascsc', 'matscsc')], as.numeric)
achievement1112 <- achievement1112 %.%
                   group_by(testgrade_N) %.%
                   mutate(reanormal = (reascsc - mean(reascsc, 
                                                      na.rm=TRUE)) /
                            sd(reascsc, na.rm=TRUE), 
                          matnormal = (matscsc - mean(matscsc, 
                                                      na.rm=TRUE)) /
                            sd(matscsc, na.rm=TRUE))
achievement1112 <- left_join(achievement1112, stu2011_12[, c('sasid', 'studentid')])

achievement1213 <- read.csv('/Volumes/ProvidenceFiles/NECAP/NECAPFall2012.csv',
                            stringsAsFactors=FALSE)
names(achievement1213) <- tolower(names(achievement1213))
names(achievement1213)[seq(20, 25, 1)] <- c('rearawscore', 'reascaledscore',
                                            'reaal', 'matrawscore', 'matscaledscore', 'matal')
achievement1213 <- achievement1213 %>%
  filter(datasource != 'AA') %>%
  select(rptstudid, lname, grade, reaal, reascaledscore, 
         matal, matscaledscore)
names(achievement1213) <- c('sasid', 'last_name', 'grade', 'reaal', 'reascsc',
                            'matal', 'matscsc')
achievement1213$schoolyear <- '2012_2013'
achievement1213$testgrade_N <- substr(achievement1213$matscsc, 1, 1)
achievement1213$testgrade_N <- as.numeric(achievement1213$testgrade_N)
achievement1213$testgrade_N <- ifelse(achievement1213$testgrade_N == 1, 11,
                                      achievement1213$testgrade_N)
achievement1213 <- achievement1213 %.%
  mutate(contentgrade_N = testgrade_N - 1)
achievement1213[,c('reascsc', 'matscsc')] <- 
  lapply(achievement1213[,c('reascsc', 'matscsc')], as.numeric)
achievement1213 <- achievement1213 %.%
  group_by(testgrade_N) %.%
  mutate(reanormal = (reascsc - mean(reascsc, 
                                     na.rm=TRUE)) /
           sd(reascsc, na.rm=TRUE), 
         matnormal = (matscsc - mean(matscsc, 
                                     na.rm=TRUE)) /
           sd(matscsc, na.rm=TRUE))
achievement1213 <- left_join(achievement1213, stu2011_12[, c('sasid', 'studentid')])
