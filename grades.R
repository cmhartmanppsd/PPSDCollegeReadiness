# grades 2010_2011
grades_09_2010_2011 <- subset(tables2010_2011$course, grade==9)
grades_09_2010_2011$course_grade <- ifelse(grades_09_2010_2011$course_grade=='',
                                           NA, grades_09_2010_2011$course_grade)
grades_09_2010_2011$course_grade <- gsub(pattern="[EF].*$", 
                                         replacement="F",
                                         x=grades_09_2010_2011$course_grade)
grades_09_2010_2011$course_grade <- gsub(pattern="[INSU].*$", 
                                         replacement=NA,
                                         x=grades_09_2010_2011$course_grade)

ggplot(data=subset(grades_09_2010_2011, !is.na(course_grade)), 
       aes(x=course_grade, fill=subject))  + 
geom_bar(aes(y=(..count..)/sum(..count..))) + facet_grid(subject~.)

# 2011_2012
courses11_12 <- extract_course(grades2011_12)
grades_09_2011_2012 <- subset(courses11_12, grade==9)
grades_09_2011_2012$course_grade <- ifelse(grades_09_2011_2012$course_grade=='',
                                           NA, grades_09_2011_2012$course_grade)
grades_09_2011_2012$course_grade <- gsub(pattern="[EF].*$", 
                                         replacement="F",
                                         x=grades_09_2011_2012$course_grade)
grades_09_2011_2012$course_grade <- gsub(pattern="[INSU].*$", 
                                         replacement=NA,
                                         x=grades_09_2011_2012$course_grade)
# 2012_2013
courses12_13 <- extract_course(grades2012_13)
grades_09_2012_2013 <- subset(courses12_13, grade==9)
grades_09_2012_2013$course_grade <- ifelse(grades_09_2012_2013$course_grade=='',
                                           NA, grades_09_2012_2013$course_grade)
grades_09_2012_2013$course_grade <- gsub(pattern="[EF].*$", 
                                         replacement="F",
                                         x=grades_09_2012_2013$course_grade)
grades_09_2012_2013$course_grade <- gsub(pattern="[INSU].*$", 
                                         replacement=NA,
                                         x=grades_09_2012_2013$course_grade)

overallgrades <- aggregate(data=courses12_13, 
                           gpa ~ sasid + courseno + grade + coursedesc + type, 
                           FUN="mean")
# Counts the number of grades below a 1.0 (D)
fails <- aggregate(data=subset(overallgrades, grade>=6 & gpa<1.0),
                   gpa ~ sasid + grade, FUN="length")
fails <- merge(fails, stu2012_13[,c('sasid','isrepeatinggr')], all.x=TRUE)
# Gets the number of courses with grades for each student
counts <- aggregate(data=subset(overallgrades, grade>=6),
                  gpa ~ sasid + grade, FUN="length")
counts <- merge(counts, stu2012_13[,c('sasid','isrepeatinggr')], all.x=TRUE)
# Subsets failures to only those with at least 4 courses with grades
fails <- subset(fails, sasid %in% subset(counts, gpa>4)$sasid)
# Subset Counts to greater than 4 courses with grades
counts <- subset(counts, gpa>4)


# First Time Ninth Grade Fail versus 8th Grade Performance
grades_ninth_1213 <- subset(tables2012_2013$person_annual, 
                            grade==9 & isrepeatinggr=='N',
                            select = c('studentid', 'sasid', 'grade', 'lunch',
                                       'sum_absent', 'sum_tardy', 'sum_on_time',
                                       'sum_suspend', 'sum_enrolled', 
                                       'sum_present', 'sum_soccurin',
                                       'sum_soccurout'))
grades_ninth_1213 <- merge(grades_ninth_1213,
                           person[,c('sasid', 'studentid','race', 'sex',
                                     'student_lang', 'parent_lang', 'dob',
                                     'lep', 'iep')], all.x=TRUE)
grades_ninth_1213$age_9th <- age_calc(grades_ninth_1213$dob, 
                                      as.Date('2012-09-01'),
                                      units='months',
                                      precise=FALSE)
grades_ninth_1213 <- merge(grades_ninth_1213, 
                           subset(attendance, schoolyear=='2011_2012'), 
                           all.x=TRUE)
grades_ninth_1213$schoolyear <- NULL
names(grades_ninth_1213)[which(names(grades_ninth_1213) %in% c('suspended'))] <- 'suspend8th'
names(grades_ninth_1213)[which(names(grades_ninth_1213) %in% c('tardy'))] <- 'tardy8th'
names(grades_ninth_1213)[which(names(grades_ninth_1213) %in% c('attendance'))] <- 'attendance8th'
grade_ninth_1213 <- merge(grades_ninth_1213, 
                          subset(attendance, schoolyear=='2012_2013'),
                          all.x=TRUE)
eighthgradeperf <- aggregate(data=subset(tables2011_2012$course, 
                                         sasid %in% grade_ninth_1213$sasid &
                                         grade==8),
                             gpa ~ sasid + courseno + grade + coursedesc,
                             FUN='mean')
eightfails <- aggregate(data=subset(eighthgradeperf, gpa<1.0),
                        gpa ~ sasid + grade, FUN="length")
eightfails$grade <- NULL
names(eightfails) <- c('sasid', 'fails8th')
grade_ninth_1213 <- merge(grade_ninth_1213, eightfails, all.x=TRUE)

# How many students who are repeating fail a course?
fails_repeaters <- as.data.frame(with(subset(fails, 
                                             isrepeatinggr=='Y'), 
                                      table(grade, gpa>=1)))
fails_repeaters$Var2 <- NULL
names(fails_repeaters) <- c('grade','nFail')
number_repeaters <- as.data.frame(with(subset(counts, 
                                              isrepeatinggr=='Y'), 
                                       table(grade))) 
names(number_repeaters) <- c('grade','n')
fails_repeaters <- merge(fails_repeaters, number_repeaters)
fails_repeaters <- mutate(fails_repeaters, pFails = nFail/n)

plot_fails_repeaters <- ggplot(data=subset(fails_repeaters, 
                                           as.numeric(as.character(grade))>8), 
                               aes(grade, pFails)) + 
                        geom_bar(stat='identity')
plot_fails_repeaters <- plot_fails_repeaters + 
                        xlab('Grade') +
                        scale_y_continuous(labels = percent_format(),
                                           limits=c(0,1),
                                           breaks=seq(0,1,.05)) +
                        theme(axis.title.y = element_blank()) +
                        ggtitle('Proportion of Repeating Students Failing \nat least One Course')

# Prior Performance Map
# prior_perf_1213 <- merge(subset(courses12_13, grade==9), 
#                          subset(courses11_12, grade==8), 
#                          all.x=TRUE, by='sasid')



