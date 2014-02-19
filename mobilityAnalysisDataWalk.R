# Mobility Information Over Time

moves2005_2006 <- moves_calc(tables2005_2006$enrollment)
# 2005-2006 This studentid == 8302578 is causing a strange problem. It could be
# beacuse the exit and next enroll date are equal. Investigate this further.
moves2006_2007 <- moves_calc(tables2006_2007$enrollment)
moves2007_2008 <- moves_calc(tables2007_2008$enrollment)
moves2008_2009 <- moves_calc(tables2008_2009$enrollment)
moves2009_2010 <- moves_calc(tables2009_2010$enrollment)
moves2010_2011 <- moves_calc(tables2010_2011$enrollment)

ggplot(data=rbind(data.frame(prop.table(table(moves2006_2007$moves)), 
                             year='2006_07'),
                  data.frame(prop.table(table(moves2007_2008$moves)), 
                             year='2007_08'),
                  data.frame(prop.table(table(moves2008_2009$moves)), 
                             year='2008_09'),
                  data.frame(prop.table(table(moves2009_2010$moves)), 
                             year='2009_10'),
                  data.frame(prop.table(table(moves2010_2011$moves)), 
                             year='2010_11')),
       aes(year, Freq, fill=Var1)) + geom_bar(stat='identity') + 
geom_text(data=subset(rbind(data.frame(prop.table(table(moves2006_2007$moves)), 
                                       year='2006_07'),
                            data.frame(prop.table(table(moves2007_2008$moves)), 
                                       year='2007_08'),
                            data.frame(prop.table(table(moves2008_2009$moves)), 
                                       year='2008_09'),
                            data.frame(prop.table(table(moves2009_2010$moves)), 
                                       year='2009_10'),
                            data.frame(prop.table(table(moves2010_2011$moves)), 
                                       year='2010_11')),
                      Var1 == 0 | Var1==1 | Var1==2), 
          aes(label=round(Freq,3)*100), size = 5, hjust = .5, 
              vjust = 1.5, position='stack') +
scale_fill_manual('Moves', values=rev(brewer.pal(8, "GnBu"))[-1]) +
scale_x_discrete('') + 
scale_y_continuous('', labels = percent_format()) +
guides(fill = guide_legend(reverse = TRUE)) +
ggtitle('Student Mobility, PPSD') +
theme(plot.title = element_text(size=18))

# Disagg Race
necap_race_1011 <- merge(tables2010_2011$achievement, 
                    tables2010_2011$person[,c('sasid','race', 'student_lang')], 
                    all.x=TRUE)
require(stringr)
necap_race_1011$matal <- str_trim(as.character(necap_race$matal), side='both')
necap_race_1011$reaal <- str_trim(as.character(necap_race$reaal), side='both')
necap_race_1011 <- merge(necap_race_1011, 
                         tables2010_2011$person_annual[, c('sasid', 'iep', 
                                                           'lep','lunch')])





disaggresults11 <- subset(as.data.frame(with(subset(necap_race_1011, grade==11), prop.table(table(race, matal %in% c('3','4')),1))), Var2==TRUE)
disaggresults11$Var2 <- NULL
disaggresults11$subj <- 'Math'
disaggresults11$Freq <- 'Proficient'
disaggresults11$year <- '2010_11'

# Math
ggplot(data=subset(as.data.frame(with(subset(necap_race_1011, 
                                             grade==11), 
                   prop.table(table(race, matal %in% c('3','4')),1))), 
                   Var2==TRUE), 
       aes(race, Freq)) + 
geom_bar(stat='identity') +
geom_text(aes(label=round(Freq, 3)*100, size=5, vjust=-1.5)) +
scale_y_continuous('',
                   labels= percent_format(), 
                   limits=c(0,.5), 
                   breaks=seq(0,.5,.05)) +
scale_x_discrete('') +
ggtitle('Math Proficiency 11th Grade 2010_11, All Students') +
theme(plot.title = element_text(size=18))

## Non Classical
ggplot(data=subset(as.data.frame(with(subset(
                   necap_race_1011, 
                   sasid %in% subset(tables2010_2011$enrollment, 
                                     schno !=164)$sasid & grade==11), 
                   prop.table(table(race, matal %in% c('3','4')), 1))), 
                   Var2==TRUE), 
       aes(race, Freq)) + 
geom_bar(stat='identity') +
geom_text(aes(label=round(Freq, 3)*100, size=5, vjust=-1.5)) +
scale_y_continuous('',
                   labels= percent_format(), 
                   limits=c(0,.5), 
                   breaks=seq(0,.5,.05)) +
scale_x_discrete('') +
ggtitle('Math Proficiency 11th Grade 2010_11, Without Classical') +
theme(plot.title = element_text(size=18))

# Reading
ggplot(data=subset(as.data.frame(with(subset(necap_race_1011, 
                                             grade==11), 
                   prop.table(table(race, reaal %in% c('3','4')),1))), 
                   Var2==TRUE), 
       aes(race, Freq)) + 
geom_bar(stat='identity') +
geom_text(aes(label=round(Freq, 3)*100, size=5, vjust=-1.5)) +
scale_y_continuous('',
                   labels= percent_format(), 
                   limits=c(0, 1), 
                   breaks=seq(0,1,.1)) +
scale_x_discrete('') +
ggtitle('Reading Proficiency 11th Grade 2010_11, All Students') +
theme(plot.title = element_text(size=18))

## Non Classical
ggplot(data=subset(as.data.frame(with(subset(
                   necap_race_1011, 
                   sasid %in% subset(tables2010_2011$enrollment, 
                                     schno !=164)$sasid & grade==11), 
                   prop.table(table(race, reaal %in% c('3','4')), 1))), 
                   Var2==TRUE), 
       aes(race, Freq)) + 
geom_bar(stat='identity') +
geom_text(aes(label=round(Freq, 3)*100, size=5, vjust=-1.5)) +
scale_y_continuous('',
                   labels= percent_format(), 
                   limits=c(0,1), 
                   breaks=seq(0,1,.1)) +
scale_x_discrete('') +
ggtitle('Reading Proficiency 11th Grade 2010_11, Without Classical') +
theme(plot.title = element_text(size=18))


ggplot(data=subset(tables2010_2011$course, grade>5 & !is.na(gpa)), 
       aes(as.factor(grade), gpa)) + 
geom_boxplot() +
scale_x_discrete('Grade') +
scale_y_continuous('Average Course Grade', breaks=seq(0,4.4,.2)) +
ggtitle('Secondary Course Performance') +
theme(plot.title = element_text(size=18))

failures <- subset(as.data.frame(prop.table(table(tables2010_2011$course$grade, tables2010_2011$course$gpa==0),1)), as.character(Var1) !='5' & Var2==TRUE)
names(failures) <- c('Grade', 'BS', 'Proportion')
failures$BS <- NULL

ggplot(failures, aes(Grade, Proportion)) + 
geom_bar(stat='identity') +
scale_x_discrete('Grade') +
scale_y_continuous('', 
                   labels= percent_format(), 
                   limits=c(0,.5), 
                   breaks=seq(0,.5,.05)) +
ggtitle('Proportion of Grades Below D') +
theme(plot.title = element_text(size=18))
