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

ggplot(data=subset(grades_09_2010_2011, !is.na(course_grade)), aes(x=course_grade, fill=subject))  + geom_bar(aes(y=(..count..)/sum(..count..))) + facet_grid(subject~.)

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
