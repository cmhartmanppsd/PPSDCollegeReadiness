ggplot(data=data.frame(prop.table(table(cut(hscohort0708$predict8th, 
                                            breaks=c(seq(0,1,.05))), 
                                        hscohort0708$graduated),1)), 
       aes(x=Var1, y=Freq, fill=Var2)) + 
  geom_bar(stat='identity') +
  scale_x_discrete(name='Probability of Graduation',
                   labels=paste(percent(seq(0,.95,.05)), 
                                '-', 
                                percent(seq(.05,1,.05)), sep='')) +
  scale_y_continuous(name='', breaks=NULL) +
  scale_fill_manual(name='Graduated', 
                    guide = guide_legend(reverse=TRUE), 
                    labels=c('No', 'Yes'),
                    values=c(brewer_pal('seq')(9)[c(5,7)])) +
  theme(axis.text.y=element_blank(), 
        axis.text.x= element_text(angle=60, vjust=.5),
        plot.title = element_text(face="bold")) +
  ggtitle('Relationship Between Prediction and Graduation')

incomingrisk <- with(subset(hs0708, schno_first %in% c(169, 139, 164, 174, 
                                                       175, 167, 188, 181, 182, 
                                                       183, 150, 173, 193, 189, 
                                                       117)),
                     subset(data.frame(prop.table(table(school_first, 
                                                        predict8th<.5), 1)), 
                            Var2==TRUE))
incomingrisk$Var2 <- NULL
names(incomingrisk) <- c('School', 'HighRisk')
incomingrisk$School <- recode(as.character(incomingrisk$School), 
                              "'ADELAIDE AVE HIGH   '='JORGE ALVAREZ HIGH'")

incomingrisk <- subset(incomingrisk, !is.na(HighRisk))
incomingrisk <- incomingrisk[-length(incomingrisk$School),]
incomingrisk$School <- str_trim(as.character(incomingrisk$School), 'both')

ggplot(data=incomingrisk, aes(reorder(School, HighRisk), HighRisk)) + 
  geom_bar(stat='identity', fill='#2171B5') + 
  scale_y_continuous(labels=percent_format(), 
                     limits=c(0,.5), 
                     breaks=seq(0,.5,.05),
                     name='') +
  scale_x_discrete('') +
  ggtitle('Proportion of Incoming Freshman with Low Likelihood of Graduating\n Fall 2007') +
  theme(axis.text.x = element_text(angle=60, vjust=.5))

racerisk <- with(hs0708, subset(data.frame(prop.table(table(race, predict8th<.5), 1)), Var2==TRUE))
racerisk$Var2 <- NULL
names(racerisk) <- c('Ethnicity', 'HighRisk')
racerisk <- subset(racerisk, !is.na(HighRisk))
ggplot(data=racerisk, aes(Ethnicity, HighRisk)) +
  geom_bar(stat='identity', fill='#2171B5') +
  scale_y_continuous(labels=percent_format(),
                     limits=c(0, .5),
                     breaks=seq(0, .5, .05),
                     name='') +
  scale_x_discrete('') +
  ggtitle('Proportion of Incoming Freshman with Low Likelihood of Graduating\n Fall 2007') +
  theme(axis.text.x = element_text(angle=60, vjust=.5))

sexrisk <- with(hs0708, subset(data.frame(prop.table(table(sex, predict8th<.5), 1)), Var2==TRUE))
sexrisk$Var2 <- NULL
names(sexrisk) <- c('Sex', 'HighRisk')
sexrisk <- subset(sexrisk, !is.na(HighRisk))
ggplot(data=sexrisk, aes(Sex, HighRisk)) +
  geom_bar(stat='identity', fill='#2171B5') +
  scale_y_continuous(labels=percent_format(),
                     limits=c(0, .5),
                     breaks=seq(0, .5, .05),
                     name='') +
  scale_x_discrete('') +
  ggtitle('Proportion of Incoming Freshman with Low Likelihood of Graduating\n Fall 2007') +
  theme(axis.text.x = element_text(angle=60, vjust=.5))

parent_langrisk <- with(hs0708, subset(data.frame(prop.table(table(parent_lang, predict8th<.5), 1)), Var2==TRUE))
parent_langrisk$Var2 <- NULL
names(parent_langrisk) <- c('ParentLang', 'HighRisk')
parent_langrisk <- subset(parent_langrisk, !is.na(HighRisk))
ggplot(data=parent_langrisk, aes(ParentLang, HighRisk)) +
  geom_bar(stat='identity', fill='#2171B5') +
  scale_y_continuous(labels=percent_format(),
                     limits=c(0, .7),
                     breaks=seq(0, .7, .05),
                     name='') +
  scale_x_discrete('Parent Language') +
  ggtitle('Proportion of Incoming Freshman with Low Likelihood of Graduating\n Fall 2007') +
  theme(axis.text.x = element_text(angle=90, vjust=.5))


leprisk <- with(hs0708, subset(data.frame(prop.table(table(lep, predict8th<.5), 1)), Var2==TRUE))
leprisk$Var2 <- NULL
names(leprisk) <- c('LEP', 'HighRisk')
leprisk <- subset(leprisk, !is.na(HighRisk))
ggplot(data=leprisk, aes(LEP, HighRisk)) +
  geom_bar(stat='identity', fill='#2171B5') +
  scale_y_continuous(labels=percent_format(),
                     limits=c(0, .5),
                     breaks=seq(0, .5, .05),
                     name='') +
  scale_x_discrete('LEP Status') +
  ggtitle('Proportion of Incoming Freshman with Low Likelihood of Graduating\n Fall 2007') +
  theme(axis.text.x = element_text(vjust=.5))




ieprisk <- with(hs0708, subset(data.frame(prop.table(table(iep, predict8th<.5), 1)), Var2==TRUE))
ieprisk$Var2 <- NULL
names(ieprisk) <- c('iep', 'HighRisk')
ieprisk <- subset(ieprisk, !is.na(HighRisk))
ggplot(data=ieprisk, aes(iep, HighRisk)) +
  geom_bar(stat='identity', fill='#2171B5') +
  scale_y_continuous(labels=percent_format(),
                     limits=c(0, .5),
                     breaks=seq(0, .5, .05),
                     name='') +
  scale_x_discrete('iep Status') +
  ggtitle('Proportion of Incoming Freshman with Low Likelihood of Graduating\n Fall 2007') +
  theme(axis.text.x = element_text(vjust=.5))


matalrisk <- with(hs0708, subset(data.frame(prop.table(table(matal, predict8th<.5), 1)), Var2==TRUE))
matalrisk$Var2 <- NULL
names(matalrisk) <- c('matal', 'HighRisk')
matalrisk <- subset(matalrisk, !is.na(HighRisk))
ggplot(data=matalrisk, aes(matal, HighRisk)) +
  geom_bar(stat='identity', fill='#2171B5') +
  scale_y_continuous(labels=percent_format(),
                     limits=c(0, .5),
                     breaks=seq(0, .5, .05),
                     name='') +
  scale_x_discrete('Math NECAP') +
  ggtitle('Proportion of Incoming Freshman with Low Likelihood of Graduating\n Fall 2007') +
  theme(axis.text.x = element_text(vjust=.5))


reaalrisk <- with(hs0708, subset(data.frame(prop.table(table(reaal, predict8th<.5), 1)), Var2==TRUE))
reaalrisk$Var2 <- NULL
names(reaalrisk) <- c('reaal', 'HighRisk')
reaalrisk <- subset(reaalrisk, !is.na(HighRisk))
ggplot(data=reaalrisk, aes(reaal, HighRisk)) +
  geom_bar(stat='identity', fill='#2171B5') +
  scale_y_continuous(labels=percent_format(),
                     limits=c(0, .5),
                     breaks=seq(0, .5, .05),
                     name='') +
  scale_x_discrete('Reading NECAP') +
  ggtitle('Proportion of Incoming Freshman with Low Likelihood of Graduating\n Fall 2007') +
  theme(axis.text.x = element_text(vjust=.5))


movesrisk <- with(hs0708, subset(data.frame(prop.table(table(moves, predict8th<.5), 1)), Var2==TRUE))
movesrisk$Var2 <- NULL
names(movesrisk) <- c('moves', 'HighRisk')
movesrisk <- subset(movesrisk, !is.na(HighRisk))
ggplot(data=movesrisk, aes(moves, HighRisk)) +
  geom_bar(stat='identity', fill='#2171B5') +
  scale_y_continuous(labels=percent_format(),
                     limits=c(0, 1),
                     breaks=seq(0, 1, .1),
                     name='') +
  scale_x_discrete('Moves') +
  ggtitle('Proportion of Incoming Freshman with Low Likelihood of Graduating\n Fall 2007') +
  theme(axis.text.x = element_text(vjust=.5))



tardyrisk <- with(hs0708, subset(data.frame(prop.table(table(cut(tardy8th, breaks=c(seq(0,1,.05))), predict8th<.5), 1)), Var2==TRUE))
tardyrisk$Var2 <- NULL
tardyrisk <- tardyrisk[1:10,]
names(tardyrisk) <- c('tardy', 'HighRisk')
tardyrisk <- subset(tardyrisk, !is.na(HighRisk))
ggplot(data=tardyrisk, aes(tardy, HighRisk)) +
  geom_bar(stat='identity', fill='#2171B5') +
  scale_y_continuous(labels=percent_format(),
                     limits=c(0, 1),
                     breaks=seq(0, 1, .1),
                     name='') +
  scale_x_discrete('Tardiness') +
  ggtitle('Proportion of Incoming Freshman with Low Likelihood of Graduating\n Fall 2007') +
  theme(axis.text.x = element_text(angle=90, vjust=.5))


attendancerisk <- with(hs0708, subset(data.frame(prop.table(table(cut(attendance8th, breaks=c(seq(0,1,.05))), predict8th<.5), 1)), Var2==TRUE))
attendancerisk$Var2 <- NULL
names(attendancerisk) <- c('attendance', 'HighRisk')
attendancerisk <- subset(attendancerisk, !is.na(HighRisk))
ggplot(data=attendancerisk, aes(attendance, HighRisk)) +
  geom_bar(stat='identity', fill='#2171B5') +
  scale_y_continuous(labels=percent_format(),
                     limits=c(0, 1),
                     breaks=seq(0, 1, .1),
                     name='') +
  scale_x_discrete('Attendance') +
  ggtitle('Proportion of Incoming Freshman with Low Likelihood of Graduating\n Fall 2007') +
  theme(axis.text.x = element_text(angle=90, vjust=.5))

# 9th grade GPA Quarter 1 Impact

gpa9threview <- aggregate(gpa9thqtr1~I(cut(predict8th, breaks=c(seq(0,1,.1))))+graduated, mean, data=subset(hscohort0708))
names(gpa9threview)[1] <-'probability'
gpa9thlength <- aggregate(gpa9thqtr1~I(cut(predict8th, breaks=c(seq(0,1,.1))))+graduated, length, data=subset(hscohort0708))
gpa9thsummarystats <- hscohort0708
gpa9thsummarystats$probgroup <- cut(gpa9thsummarystats$predict8th, breaks=c(seq(0,1,.1)))
summarystats <- ddply(gpa9thsummarystats, "probgroup", summarize, mean=mean(gpa9thqtr1, na.rm=TRUE), sd=sd(gpa9thqtr1, na.rm=TRUE))
names(summarystats)[1] <- 'probability'
names(gpa9thlength)[1] <- 'probability'
names(gpa9thlength)[3] <-'n'
gpa9threview <- merge(gpa9threview, gpa9thlength)
gpa9threview <- merge(gpa9threview, summarystats)
gpa9threview <- transform(gpa9threview, gpa_z = (gpa9thqtr1-mean)/sd,
                                        gpa_c = gpa9thqtr1-mean)


ggplot(subset(gpa9threview, n>10), aes(probability, gpa_c, fill=graduated)) + geom_bar(position='dodge', stat='identity') + scale_y_continuous('Centered-GPA', breaks=seq(-1,1,.1), limits=c(-1,1)) + scale_x_discrete('Probability of Graduating') + ggtitle('The Difference in First Quarter GPA among Students with Similar Entering Likelhood\n n>10')

ggplot(subset(gpa9threview, n>10), 
       aes(probability, gpa9thqtr1, color=graduated)) + 
geom_point(size=4) + 
scale_y_continuous('Centered-GPA', 
                   breaks=seq(0,3,.1), 
                   limits=c(0,3)) + 
scale_x_discrete('Probability of Graduating') + 
ggtitle('The Difference in First Quarter GPA among Students with Similar Entering Likelhood\n n>10')

## Grades
courperfgraph <- merge(tables2010_2011$course[, c('studentid','sasid','schoolyear',
                                                  'grade','gpa', 'subject')], 
                       subset(tables2010_2011$enrollment, 
                              schno!='164', 
                              select=c('studentid', 'sasid', 'grade')))

failures <- subset(as.data.frame(prop.table(table(courperfgraph$grade, 
                                                  courperfgraph$gpa==0),1)), 
                   as.character(Var1) !='5' & Var2==TRUE)
names(failures) <- c('Grade', 'BS', 'Proportion')
failures$BS <- NULL

courperffull <- tables2010_2011$course[, c('studentid','sasid','schoolyear',
                                            'grade','gpa', 'subject')]

failuresfull <- subset(as.data.frame(
                        prop.table(table(tables2010_2011$course$grade, 
                                         tables2010_2011$course$gpa==0),1)), 
                       as.character(Var1) !='5' & Var2==TRUE)
names(failuresfull) <- c('Grade', 'BS', 'Proportion')
failuresfull$BS <- NULL


multiplot(ggplot(data=subset(courperffull, 
                             grade>5 & !is.na(gpa)),
                 aes(as.factor(grade), gpa)) +
          geom_boxplot() +
          scale_x_discrete('Grade') +
          scale_y_continuous('Average Course Grade', breaks=seq(0,4.4,.2)) +
          ggtitle('Secondary Course Performance') +
          theme(plot.title = element_text(size=18)),
          ggplot(failuresfull, aes(Grade, Proportion)) +
          geom_bar(stat='identity') +
          scale_x_discrete('Grade') +
          scale_y_continuous('',
                             labels= percent_format(),
                             limits=c(0,.5),
                             breaks=seq(0,.5,.05)) +
          ggtitle('Proportion of Grades Below D') +
          theme(plot.title = element_text(size=18)),
          ggplot(data=subset(courperfgraph, 
                             grade>5 & !is.na(gpa)),
                 aes(as.factor(grade), gpa)) +
          geom_boxplot() +
          scale_x_discrete('Grade') +
          scale_y_continuous('Average Course Grade', breaks=seq(0,4.4,.2)) +
          ggtitle('Secondary Course Performance, no Classical') +
          theme(plot.title = element_text(size=18)),
          ggplot(failures, aes(Grade, Proportion)) +
          geom_bar(stat='identity') +
          scale_x_discrete('Grade') +
          scale_y_continuous('',
                             labels= percent_format(),
                             limits=c(0,.5),
                             breaks=seq(0,.5,.05)) +
          ggtitle('Proportion of Grades Below D, no Classical') +
            theme(plot.title = element_text(size=18)),cols=2)