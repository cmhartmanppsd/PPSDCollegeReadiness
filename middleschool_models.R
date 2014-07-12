middle_school <- subset(annual_demos, grade %in% c(6,7,8))
middle_school$grade <- as.numeric(as.character(middle_school$grade))
middle_school <- filter(middle_school,
                        grade>=7 & schoolyear %in% c('2010_2011') |
                        grade>=8 & schoolyear %in% c('2011_2012') |
                        schoolyear %in% c('2005_2006', '2006_2007',
                                          '2007_2008', '2008_2009',
                                          '2009_2010'))
                        
                        
middle_school <- select(middle_school, 
                        studentid, sasid, grade, lunch, lep, iep, schoolyear)
middle_school <- left_join(middle_school, all_years)
attendance$grade <- as.numeric(as.character(attendance$grade))
middle_school <- left_join(middle_school, attendance)

achievement_all_yrs <- rbind(tables2005_2006$achievement,
                             tables2006_2007$achievement,
                             tables2007_2008$achievement,
                             tables2008_2009$achievement,
                             tables2009_2010$achievement,
                             tables2010_2011$achievement,
                             tables2011_2012$achievement,
                             tables2012_2013$achievement)
achievement_all_yrs$studentid <- as.character(achievement_all_yrs$studentid)
achievement_all_yrs$grade <- as.numeric(as.character(achievement_all_yrs$grade))
achievement_all_yrs <- filter(achievement_all_yrs,
                              testgrade_N == grade)
achievement_all_yrs <- select(achievement_all_yrs,
                              studentid, sasid, schoolyear, grade, reaal,
                              reascsc, matal, matscsc, wrial, wriscsc,
                              reanormal, matnormal)
achievement_all_yrs[, c('studentid', 'sasid')] <- 
  lapply(achievement_all_yrs[, c('studentid', 'sasid')], as.character)

achievement_all_yrs[, c('reaal', 'matal', 'wrial')] <-
  lapply(achievement_all_yrs[, c('reaal', 'matal', 'wrial')], as.character)

achievement_all_yrs[, c('reaal', 'matal', 'wrial')] <-
  lapply(achievement_all_yrs[, c('reaal', 'matal', 'wrial')], str_trim)

achievement_all_yrs$reaal <- ifelse(!achievement_all_yrs$reaal %in% 
                                        seq(1,4,1),
                                      NA,
                                      achievement_all_yrs$reaal)
achievement_all_yrs$matal <- ifelse(!achievement_all_yrs$matal %in% 
                                        seq(1,4,1),
                                      NA,
                                      achievement_all_yrs$matal)
achievement_all_yrs$wrial <- ifelse(!achievement_all_yrs$wrial %in% 
                                        seq(1,4,1),
                                      NA,
                                      achievement_all_yrs$wrial)

middle_school <- left_join(middle_school, achievement_all_yrs)

gpa <- rbind(tables2005_2006$course,
             tables2006_2007$course,
             tables2007_2008$course,
             tables2008_2009$course,
             tables2009_2010$course,
             tables2010_2011$course,
             tables2011_2012$course,
             tables2012_2013$course) %.%
       group_by(sasid, grade, schyr) %.%
       summarize(gpa = mean(gpa, na.rm=TRUE))

gpa <- gpa[, c('sasid', 'grade', 'schyr', 'gpa')]
names(gpa)[which(names(gpa)=='schyr')] <- 'schoolyear'
gpa$schoolyear <- paste(gpa$schoolyear-1, gpa$schoolyear, sep='_')
gpa$grade <- as.numeric(as.character(gpa$grade))


fails <- rbind(tables2005_2006$course,
               tables2006_2007$course,
               tables2007_2008$course,
               tables2008_2009$course,
               tables2009_2010$course,
               tables2010_2011$course,
               tables2011_2012$course,
               tables2012_2013$course) %.%
         group_by(sasid, courseno, grade, schyr) %.%
         summarize(gpa = mean(gpa, na.rm=TRUE)) %.%
         filter(gpa <= 1.0)

fails <- ungroup(fails) %.%
         group_by(sasid, grade, schyr) %.%
         summarize(fails = n())

fails <- fails[, c('sasid', 'fails', 'grade', 'schyr')]
names(fails)[which(names(fails)=='schyr')] <- 'schoolyear'
fails$schoolyear <- paste(fails$schoolyear-1, fails$schoolyear, sep='_')
fails$grade <- as.numeric(as.character(fails$grade))

middle_school <- left_join(middle_school, gpa)
middle_school <- left_join(middle_school, fails)

first9th <- all_years_enr %.% 
            filter(grade == 9) %.% 
            group_by(sasid) %.% 
            summarize(enroll_date = min(enroll_date))

first9th$schoolyear_9 <- 
  ifelse(month(first9th$enroll_date) %in% seq(8,12,1),
         paste(year(first9th$enroll_date), 
               year(first9th$enroll_date) + 1, 
               sep = "_"),
         paste(year(first9th$enroll_date) - 1,
               year(first9th$enroll_date),
               sep = "_"))

middle_school <- left_join(middle_school,
                           first9th[, c('sasid', 'schoolyear_9')])

middle_school <- mutate(middle_school,
                        age = age_calc(dob,
                                       as.Date(paste(substr(schoolyear, 1, 4), 
                                                     '09', '01', sep='-')),
                                       units = 'months'))

middle_school <- left_join(middle_school,
                           retained_calc(x = annual_demos, 
                                         sid = 'sasid',
                                         grade = 'grade',
                                         grade_value = 9))
names(middle_school)[length(names(middle_school))] <- 'retained9th'

middle_school <- filter(middle_school, !is.na(retained9th))
middle_school$promoted <- as.factor(ifelse(middle_school$retained9th=='Y', 
                                           'N', 'Y'))
ms_6 <- subset(middle_school, grade==6)
ms_7 <- subset(middle_school, grade==7)
ms_8 <- subset(middle_school, grade==8)

# Models
require(caret)
set.seed(101)
## Grade 6
trainIndex <- createDataPartition(ms_6$promoted,
                                  p = .8, list=FALSE)
ms_6_train <- ms_6[trainIndex,]
ms_6_test <- ms_6[-trainIndex,]
predictors <- c('attendance', 'gpa', 'lunch', 'lep', 'iep', 'sex',
                'suspended', 'fails', 'age', 'matal', 'reaal',
                'matscsc', 'reascsc')
set.seed(101)
# model.6step <- train(ms_6_train[, predictors],
#                      ms_6_train[, c('retained9th')],
#                      method='glmStepAIC',
#                      tuneLength = 5,
#                      trControl = trainControl(method = "repeatedcv", number = 10,
#                                               repeats = 10, classProbs=TRUE,
#                                               summaryFunction = twoClassSummary,
#                                               seeds = NA),
#                      metric='ROC')
# model.6ctree <- train(ms_6_train[, predictors],
#                       ms_6_train[, c('retained9th')],
#                       method = 'ctree',
#                       maxdepth = 3,
#                       tuneLength = 5,
#                       trControl = trainControl(method = "repeatedcv", number = 10,
#                                                repeats = 10, classProbs=TRUE,
#                                                summaryFunction = twoClassSummary,
#                                                seeds = NA),
#                       metric='ROC')

fourStats <- function (data, lev = levels(data$obs), model = NULL) {
  ## This code will get use the area under the ROC curve and the
  ## sensitivity and specificity values using the current candidate
  ## value of the probability threshold.
  out <- c(twoClassSummary(data, lev = levels(data$obs), model = NULL))
  
  ## The best possible model has sensitivity of 1 and specifity of 1. 
  ## How far are we from that value?
  coords <- matrix(c(1, 1, out["Spec"], out["Sens"]), 
                   ncol = 2, 
                   byrow = TRUE)
  colnames(coords) <- c("Spec", "Sens")
  rownames(coords) <- c("Best", "Current")
  c(out, Dist = dist(coords)[1])
}

model.6glm <- train(promoted ~ gpa + I(attendance*10) + I(reaal>2) + I(age>144),
                    data = ms_6_train,
                    method = 'glm',
                    tuneLength = 5,
                    trControl = trainControl(method = "repeatedcv", number = 10,
                                             repeats = 10, classProbs=TRUE,
                                             summaryFunction = twoClassSummary,
                                             seeds = NA),
                    metric='ROC')

model.6c5  <- train(promoted ~ gpa + attendance + I(reaal>2) + I(age>144) + sex +
                    iep + lep,
                    data = ms_6_train,
                    method='ctree',
                    tuneLength = 5,
                    trControl = trainControl(method = "repeatedcv", number = 10,
                                             repeats = 10, classProbs=TRUE,
                                             summaryFunction = twoClassSummary,
                                             seeds = NA),
                    metric='fourStats')
#ms_6_test <- subset(ms_6_test, !is.na(retained9th))
test <- na.omit(ms_6_test[,c('gpa', 'attendance', 'reaal', 'age', 'promoted')])
roc_6 <- roc(test$promoted, 
             predict(model.6glm, newdata=test, type='prob')$Y,
             auc=TRUE, ci=TRUE)

sjp.glm(model.6glm$finalModel, 
        title='Grade 6 Odds Ratios for 9th Grade Retention',
        showModelSummary = FALSE,
        theme = 'bw',
        borderColor = 'white')

# Classifications
ms_6_train <- na.omit(ms_6_train[,c('gpa', 'age', 'attendance', 
                                    'reaal', 'promoted')])
ms_6_train$predict <- predict(model.6glm, 
                              newdata = ms_6_train, 
                              type = 'prob')$Y

m1_matrix <- cutoff_matrix(ms_6_train, 'predict', 'promoted')
thresholds6glm <- data.frame(classification = as.factor(c('Watch', 
                                                          'Warning', 'Action')),
                             threshold = c(m1_matrix[which.min(abs(m1_matrix$true_neg_rate - .6)),]$cutoff,
                                           m1_matrix[which.min(abs(m1_matrix$true_neg_rate - .7)),]$cutoff,
                                           m1_matrix[which.min(abs(m1_matrix$true_neg_rate - .8)),]$cutoff))

# Compute coverage: what proportion of those who fail to graduate are identified
thresholds6glm$coverage <- NA
coverage <- as.data.frame(table(
  ms_6_train$promoted, 
  ms_6_train$predict <= subset(thresholds6glm, 
                               classification=='Action')$threshold))
thresholds6glm[thresholds6glm$classification == 'Action', ]$coverage <- 
  coverage[coverage$Var2==TRUE & coverage$Var1 == 'N', 'Freq'] / 
  sum(coverage[coverage$Var1 == 'N', ]$Freq)
coverage <- as.data.frame(table(
  ms_6_train$promoted, 
  ms_6_train$predict  <= subset(thresholds6glm, 
                                       classification=='Warning')$threshold))
thresholds6glm[thresholds6glm$classification == 'Warning', ]$coverage <- 
  coverage[coverage$Var2==TRUE & coverage$Var1 == 'N', 'Freq'] / 
  sum(coverage[coverage$Var1 == 'N', ]$Freq)






## Grade 7
set.seed(101)
trainIndex <- createDataPartition(ms_7$promoted,
                                  p = .8, list=FALSE)
ms_7_train <- ms_7[trainIndex,]
ms_7_test <- ms_7[-trainIndex,]
predictors <- c('attendance', 'gpa', 'lunch', 'lep', 'iep', 'sex',
                'suspended', 'fails', 'age', 'matal', 'reaal',
                'matscsc', 'reascsc')
set.seed(101)
# model.7step <- train(ms_7_train[, predictors],
#                      ms_7_train[, c('retained9th')],
#                      method='glmStepAIC',
#                      tuneLength = 5,
#                      trControl = trainControl(method = "repeatedcv", number = 10,
#                                               repeats = 10, classProbs=TRUE,
#                                               summaryFunction = twoClassSummary,
#                                               seeds = NA),
#                      metric='ROC')
# model.7ctree <- train(ms_7_train[, predictors],
#                       ms_7_train[, c('retained9th')],
#                       method = 'ctree',
#                       maxdepth = 3,
#                       tuneLength = 5,
#                       trControl = trainControl(method = "repeatedcv", number = 10,
#                                                repeats = 10, classProbs=TRUE,
#                                                summaryFunction = twoClassSummary,
#                                                seeds = NA),
#                       metric='ROC')
model.7glm <- train(promoted ~ gpa + I(attendance*10) + I(suspended>0) + 
                                  I(reaal>2) + I(age>156),
                    data = ms_7_train,
                    method = 'glm',
                    tuneLength = 5,
                    trControl = trainControl(method = "repeatedcv", number = 10,
                                             repeats = 10, classProbs=TRUE,
                                             summaryFunction = twoClassSummary,
                                             seeds = NA),
                    metric='ROC')

test <- na.omit(ms_7_test[,c('gpa', 'attendance', 'age', 'reaal', 'suspended', 
                             'promoted')])

load('/Users/jason/Downloads/bowersEWS.rda')
grade_7_comps <- subset(bowersEWS, grade==7)
roc_7 <- roc(test$promoted, 
             predict(model.7glm, newdata=test, type='prob')$Y,
             auc=TRUE, ci=TRUE)
rocobj1 <- plot.roc(roc_7, 
                    main="7GM Specifications",
                    col="#1c61b6",
                    print.auc=TRUE,
                    legacy.axes=TRUE)
points(x=grade_7_comps$specificity, 
       y=grade_7_comps$sensitivity, col='gray', pch = 20)

sjp.glm(model.7glm$finalModel, 
        title='Grade 7 Odds Ratios for 9th Grade Retention',
        showModelSummary = FALSE,
        theme = 'bw',
        borderColor = 'white')

# Classifications
ms_7_train <- na.omit(ms_7_train[,c('gpa', 'age', 'attendance', 
                                    'reaal', 'suspended', 'promoted')])
ms_7_train$predict <- predict(model.7glm, 
                              newdata = ms_7_train, 
                              type = 'prob')$Y

# ms_7_test <- na.omit(ms_7_test[,c('gpa', 'age', 'attendance', 
#                                     'reaal', 'suspended', 'promoted')])
# ms_7_test$predict <- predict(model.7glm, 
#                               newdata = ms_7_test, 
#                               type = 'prob')$Y
# 

m1_matrix <- cutoff_matrix(ms_7_train, 'predict', 'promoted')
thresholds7glm <- data.frame(classification = as.factor(c('Watch', 
                                                          'Warning', 'Action')),
                             threshold = c(m1_matrix[which.min(abs(m1_matrix$true_neg_rate - .6)),]$cutoff,
                                           m1_matrix[which.min(abs(m1_matrix$true_neg_rate - .7)),]$cutoff,
                                           m1_matrix[which.min(abs(m1_matrix$true_neg_rate - .8)),]$cutoff))

# Compute coverage: what proportion of those who fail to graduate are identified
thresholds7glm$coverage <- NA
coverage <- as.data.frame(table(
  ms_7_train$promoted, 
  ms_7_train$predict <= subset(thresholds7glm, 
                               classification=='Action')$threshold))
thresholds7glm[thresholds7glm$classification == 'Action', ]$coverage <- 
  coverage[coverage$Var2==TRUE & coverage$Var1 == 'N', 'Freq'] / 
  sum(coverage[coverage$Var1 == 'N', ]$Freq)
coverage <- as.data.frame(table(
  ms_7_train$promoted, 
  ms_7_train$predict  <= subset(thresholds7glm, 
                                classification=='Warning')$threshold))
thresholds7glm[thresholds7glm$classification == 'Warning', ]$coverage <- 
  coverage[coverage$Var2==TRUE & coverage$Var1 == 'N', 'Freq'] / 
  sum(coverage[coverage$Var1 == 'N', ]$Freq)
