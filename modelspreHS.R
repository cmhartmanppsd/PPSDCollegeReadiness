# Models testing predictivity of high school outcomes from 8th grade and first 
# semester ninth grade data.

basemodel8thgrade <- glm(as.numeric(as.factor(graduated))-1 ~ sex + 
                         attendance8th + gpa8th + reanormal + 
                         ageHS + I(schno_first=='164'),
                         data=hscohort0708, family=binomial(link='logit'))
hs0708$predict8th<- predict(basemodel8thgrade, newdata=hs0708, type='response')

repeat9th.glm <- glm(as.numeric(as.factor(willrepeatgr))-1 ~ sex + attendance8th
                     + gpa8th + reanormal + ageHS + I(schno_first=='164'),
                     data=hscohort0708, family=binomial(link='logit'))


with(subset(hs0708, transfer_out=='N'), 
     plot(performance(prediction(predict8th, graduated), 
                      'acc'), avg='vertical'))
abline(v=.45, col='red')
title('Prediction Accuracy by Cutoff')
text(.6, .6, 'Proposed Cutoff')

ggplot(data=hs0708, aes(school_first, predict8th)) + 
geom_boxplot() + 
scale_y_continuous(labels=percent_format()) +
theme(axis.text.x = element_text(angle = 60, hjust = 1))

incomingrisk <- subset(data.frame(prop.table(table(hs0708$school_first, 
                                                   hs0708$predict8th<.45), 1)), 
                       Var2==TRUE)
incomingrisk$Var2 <- NULL
names(incomingrisk) <- c('School', 'HighRisk')
incomingrisk <- subset(incomingrisk, !is.na(HighRisk))

ggplot(data=incomingrisk, aes(School, HighRisk)) + 
geom_bar() + scale_y_continuous(labels=percent_format()) +
theme(axis.text.x = element_text(angle = 60, hjust = 1))