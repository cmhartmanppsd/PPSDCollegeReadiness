# Models testing predictivity of high school outcomes from 8th grade and first 
# semester ninth grade data.

basemodel8thgrade <- glm(as.numeric(as.factor(graduated))-1 ~ sex + 
                         attendance8th + gpa8th + reanormal + 
                         I(ageHS<167) + I(schno_first=='164'),
                         data=hscohort0708, family=binomial(link='logit'))
modelqtr1 <- glm(as.numeric(as.factor(graduated))-1 ~ sex + attendance8th + 
                 gpa8th + reanormal + I(ageHS<167) + I(schno_first=='164') +
                 gpa9thqtr1,
                 data=hscohort0708, family=binomial(link='logit'))

hs0708$predict8th<- predict(basemodel8thgrade, newdata=hs0708, type='response')
hs0708$predictqtr1<- predict(modelqtr1, newdata=hs0708, type='response')
# Not sure where the NAs come from.

repeat9th.glm <- glm(as.numeric(as.factor(willrepeatgr))-1 ~ sex + attendance8th
                     + gpa8th + reanormal + ageHS + I(schno_first=='164'),
                  data=hscohort0708, family=binomial(link='logit'))


with(subset(hs0708, transfer_out=='N'), 
     plot(performance(prediction(predict8th, graduated), 
                      'acc'), avg='vertical'))
abline(v=.45, col='red')
title('Prediction Accuracy by Cutoff')
text(.6, .6, 'Proposed Cutoff')
˜
with(subset(hs0708, transfer_out=='N'), 
  plot.roc(roc(graduated~predict8th, auc=TRUE, ci=TRUE), 
           print.auc=TRUE, ci.type='shape'))
title("ROC of the Base Eighth Grade Model")

ggplot(data=hs0708, aes(school_first, predict8th)) + 
geom_boxplot() + 
scale_y_continuous(labels=percent_format()) +
theme(axis.text.x = element_text(angle = 60, hjust = 1))

incomingrisk <- with(subset(hs0708, schno_first %in% c(169, 139, 164, 174, 
                                                       175, 167, 188, 181, 182, 
                                                       183, 150, 173, 193, 189, 
                                                       117)),
                     subset(data.frame(prop.table(table(school_first, 
                                                        predict8th<.45), 1)), 
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
scale_x_discrete(name='High School') +
ggtitle('Proportion of Incoming Freshman with Low Likelihood of Graduating\n Fall 2007') +
theme(axis.text.x = element_blank())

ggplot(subset(hs0708, !is.na(graduated)), aes(predict8th, fill=graduated)) + 
geom_bar(binwidth=.05) +  
scale_x_continuous(name='Probability of Graduation', 
                   breaks=seq(0,1,.2), 
                   labels=percent_format()) + 
scale_y_discrete(name='', breaks=NULL) + 
scale_fill_manual(name='Graduated', 
                    guide = guide_legend(reverse=TRUE), 
                    labels=c('No', 'Yes'),
                    values=c(brewer_pal('seq')(9)[c(5,7)])) +
theme(axis.text.y=element_blank(), plot.title = element_text(face="bold")) +
ggtitle('Relationship Between Prediction and Graduation')

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

plot_odds(basemodel8thgrade) + ggtitle('Logit Regression Odds Ratios with 95% Confidence Intervals')



# Paritition tree
require(party)
hs0708tree <- subset(hs0708, !is.na(graduated))
hs0708tree$numGrad <- ifelse(hs0708tree$graduated=='Y', 1, 0)
plot(ctree(numGrad ~ sex + iep + attendance8th + gpa8th + reaal + suspend8th + 
                     ageHS + I(schno_first=='164'), data=hs0708tree, 
           controls = ctree_control(minsplit = 50)), 
     type='simple')

