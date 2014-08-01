# This is an analysis of the 9th students from 2011-2012 and 2012-2013
# Run only after reaching the point where hs_new9 is fully created in
# finareport.Rmd, approximately line 1915.

valid_cases <- select(hs_new9, sasid)

gpa <- rbind(tables2005_2006$course,
             tables2006_2007$course,
             tables2007_2008$course,
             tables2008_2009$course,
             tables2009_2010$course,
             tables2010_2011$course,
             tables2011_2012$course,
             tables2012_2013$course) %>%
       group_by(sasid, grade, schyr) %>%
       summarize(gpa = mean(gpa, na.rm=TRUE)) %.%
       select(sasid, gpa, grade, schyr)

gpa <- inner_join(gpa, valid_cases)

gpa7th <- ungroup(gpa) %>%
          filter(grade == 7) %>%
          select(sasid, gpa)

names(gpa7th)[2] <- 'gpa7th'

attendance7th <- attendance %>%
                 filter(grade == 7) %>%
                 select(sasid, attendance, tardy, suspended)
names(attendance7th)[c(2, 3, 4)] <- paste0(names(attendance7th)[c(2, 3, 4)],
                                          '7th')
attribute <- c('studentid', 'sasid', 'first_name', 'last_name', 'student_lang',
               'parent_lang', 'birth_place', 'race')
tables2011_2012$person[, attribute] <- 
  lapply(tables2011_2012$person[, attribute], as.character)
tables2012_2013$person[, attribute] <- 
  lapply(tables2012_2013$person[, attribute], as.character)


age <- rbind_list(tables2005_2006$person,
                  tables2006_2007$person,
                  tables2007_2008$person,
                  tables2008_2009$person,
                  tables2009_2010$person,
                  tables2010_2011$person,
                  tables2011_2012$person,
                  tables2012_2013$person) %>%
       select(sasid, dob) %>%
       unique(.)


tables2011_2012$person_annual$sasid <- 
  as.character(tables2011_2012$person_annual$sasid)
tables2012_2013$person_annual$sasid <- 
  as.character(tables2012_2013$person_annual$sasid)



age <- left_join(age,
                 rbind_list(tables2005_2006$person_annual %>%
                              select(sasid, grade, schoolyear),
                            tables2006_2007$person_annual %>%
                              select(sasid, grade, schoolyear),
                            tables2007_2008$person_annual %>%
                              select(sasid, grade, schoolyear),
                            tables2008_2009$person_annual %>%
                              select(sasid, grade, schoolyear),
                            tables2009_2010$person_annual %>%
                              select(sasid, grade, schoolyear),
                            tables2010_2011$person_annual %>%
                              select(sasid, grade, schoolyear)))

age7th <- age %>%
          filter(grade == 7) %>%
          mutate(age = age_calc(dob, 
                                as.Date(paste(substr(x = schoolyear,1,4),
                                              '09', '01', sep = '-')), 
                                        units='months')) %>%
          select(sasid, age7th = age)


hs_new9 <- left_join(hs_new9, gpa7th)
rm(gpa7th)
hs_new9 <- left_join(hs_new9, attendance7th)
rm(attendance7th)
hs_new9 <- left_join(hs_new9, age7th)
rm(age7th)

# 74%
ggplot(data = hs_new9, aes(classification9GMt, gpa7th)) +
stat_summary(fun.data="box_quant", 
             probs=c(0.25, 0.75), 
             geom='pointrange', 
             position = position_dodge(width = 0.5), 
             size=1.5) +
geom_hline(yintercept=2.5, color = '#0571B0') +
scale_x_discrete('9th Grade Model Classification') +
scale_y_continuous('Grade 7 GPA',
                   breaks = seq(0, 4.5, .5), 
                   limits = c(0, 4.33)) +
ggtitle('Grade 7 GPA and High School Readiness')

ggplot(data = hs_new9, aes(classification9GMt, attendance7th)) +
  stat_summary(fun.data="box_quant", 
               probs=c(0.25, 0.75), 
               geom='pointrange', 
               position = position_dodge(width = 0.5), 
               size=1.5) +
  scale_x_discrete('9th Grade Model Classification') +
  scale_y_continuous('Grade 7 Attendance',
                     breaks = seq(.7, 1, .05), 
                     limits = c(.7, 1)) +
  ggtitle('Grade 7 GPA and High School Readiness')

ggplot(data = melt(prop.table(table(hs_new9$classification9GMt,
                                    hs_new9$suspended7th>=1), 2)),
       aes(Var2, value, fill=Var1)) +
geom_bar(stat='identity', position='stack') +
scale_fill_manual('',
                  values=c('#0571B0', '#FFCC00', '#FF9942', '#CA0020'),
                  breaks=c('Action', 'Warning', 'Watch', 'On-track')) +
scale_y_continuous('', 
                   labels = percent_format(), 
                   breaks = seq(0,1,.1),
                   limits = c(0, 1.05),
                   expand = c(0, 0)) +
scale_x_discrete('Suspended in 7th Grade',
                 labels = c('No', 'Yes')) 

ggplot(data = melt(prop.table(table(hs_new9$classification9GMt,
                                    hs_new9$age7th>156), 2)),
       aes(Var2, value, fill=Var1)) +
  geom_bar(stat='identity', position='stack') +
  scale_fill_manual('',
                    values=c('#0571B0', '#FFCC00', '#FF9942', '#CA0020'),
                    breaks=c('Action', 'Warning', 'Watch', 'On-track')) +
  scale_y_continuous('', 
                     labels = percent_format(), 
                     breaks = seq(0,1,.1),
                     limits = c(0, 1.05),
                     expand = c(0, 0)) +
  scale_x_discrete('Over age in 7th Grade',
                   labels = c('No', 'Yes')) 



prop.table(table(hs_new9$classification9GMt, 
                 hs_new9$attendance7th<.9 & 
                 hs_new9$gpa7th<2.5 & 
                 hs_new9$suspended7th>=1 &
#                 hs_new9$age7th>156),
           2)
#####
gpa6th <- ungroup(gpa) %>%
  filter(grade == 6) %>%
  select(sasid, gpa)

names(gpa6th)[2] <- 'gpa6th'

attendance6th <- attendance %>%
  filter(grade == 6) %>%
  select(sasid, attendance, tardy, suspended)
names(attendance6th)[c(2, 3, 4)] <- paste0(names(attendance6th)[c(2, 3, 4)],
                                           '6th')

age6th <- age %>%
  filter(grade == 6) %>%
  mutate(age = age_calc(dob, 
                        as.Date(paste(substr(x = schoolyear,1,4),
                                      '09', '01', sep = '-')), 
                        units='months')) %>%
  select(sasid, age6th = age)


hs_new9 <- left_join(hs_new9, gpa6th)
rm(gpa6th)
hs_new9 <- left_join(hs_new9, attendance6th)
rm(attendance6th)
hs_new9 <- left_join(hs_new9, age6th)
rm(age6th)


ggplot(data = hs_new9, aes(classification9GMt, gpa6th)) +
stat_summary(fun.data="box_quant", 
             probs=c(0.25, 0.75), 
             geom='pointrange', 
             position = position_dodge(width = 0.5), 
             size=1.5) +
geom_hline(yintercept=2.5, color = '#0571B0') +
scale_x_discrete('9th Grade Model Classification') +
scale_y_continuous('Grade 6 GPA',
                   breaks = seq(0, 4.5, .5), 
                   limits = c(0, 4.33)) +
ggtitle('Grade 6 GPA and High School Readiness')

ggplot(data = hs_new9, aes(classification9GMt, attendance6th)) +
stat_summary(fun.data="box_quant", 
             probs=c(0.25, 0.75), 
             geom='pointrange', 
             position = position_dodge(width = 0.5), 
             size=1.5) +
geom_hline(yintercept=0.92, color = '#0571B0') +
scale_x_discrete('9th Grade Model Classification') +
scale_y_continuous('Grade 6 Attendance',
                   breaks = seq(.7, 1, .05), 
                   limits = c(.7, 1)) +
ggtitle('Grade 6 Attendance and High School Readiness')

ggplot(data = melt(prop.table(table(hs_new9$classification9GMt,
                                    hs_new9$suspended6th>=1), 2)),
       aes(Var2, value, fill=Var1)) +
geom_bar(stat='identity', position='stack') +
scale_fill_manual('',
                  values=c('#0571B0', '#FFCC00', '#FF9942', '#CA0020'),
                  breaks=c('Action', 'Warning', 'Watch', 'On-track')) +
scale_y_continuous('', 
                   labels = percent_format(), 
                   breaks = seq(0,1,.1),
                   limits = c(0, 1.05),
                   expand = c(0, 0)) +
scale_x_discrete('Suspended in 6th Grade',
                 labels = c('No', 'Yes')) 

ggplot(data = melt(prop.table(table(hs_new9$classification9GMt,
                                    hs_new9$age6th>144), 2)),
       aes(Var2, value, fill=Var1)) +
  geom_bar(stat='identity', position='stack') +
  scale_fill_manual('',
                    values=c('#0571B0', '#FFCC00', '#FF9942', '#CA0020'),
                    breaks=c('Action', 'Warning', 'Watch', 'On-track')) +
  scale_y_continuous('', 
                     labels = percent_format(), 
                     breaks = seq(0,1,.1),
                     limits = c(0, 1.05),
                     expand = c(0, 0)) +
  scale_x_discrete('Over age in 6th Grade',
                   labels = c('No', 'Yes')) 



prop.table(table(hs_new9$classification9GMt, 
                 hs_new9$attendance6th<.92 & 
                 hs_new9$gpa6th<2.5 & 
                 hs_new9$suspended6th>=1 &
                 hs_new9$age6th > 144), 
           2)

