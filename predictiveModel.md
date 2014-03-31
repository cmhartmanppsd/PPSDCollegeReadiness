Pathways to Graduation
========================================================


```r
# , echo=FALSE, results='hide'}
setwd("/Users/jason/Dropbox/code/PPSDCollegeReadiness/")
source("dependencies.R")
```

```
## Loading required package: foreign
## Loading required package: plyr
## Loading required package: dplyr
## 
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:plyr':
## 
##     arrange, desc, failwith, id, mutate, summarise, summarize
## 
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
## 
## Loading required package: ggplot2
## Loading required package: reshape2
## Loading required package: scales
## Loading required package: stringr
## Loading required package: pROC
## Type 'citation("pROC")' for a citation.
## 
## Attaching package: 'pROC'
## 
## The following objects are masked from 'package:stats':
## 
##     cov, smooth, var
## 
## Loading required package: rstudio
## Loading required package: data.table
## 
## Attaching package: 'data.table'
## 
## The following object is masked from 'package:dplyr':
## 
##     last
```

```r
file.sources = list.files("/Users/jason/Dropbox/code/PPSDCollegeReadiness/R", 
    pattern = "*.R$", full.names = TRUE, ignore.case = TRUE)
invisible(sapply(file.sources, source, .GlobalEnv))
source("load.R")
```

```
## Warning: /Volumes/ProvidenceFiles/MasterFiles/REG Master_0405.sav:
## Unrecognized record type 7, subtype 18 encountered in system file
```

```
## re-encoding from CP1252
```

```
## Warning: /Volumes/ProvidenceFiles/MasterFiles/REG Master_0506.sav:
## Unrecognized record type 7, subtype 18 encountered in system file
```

```
## re-encoding from CP1252
```

```
## Warning: /Volumes/ProvidenceFiles/MasterFiles/REG Master_0607.sav:
## Unrecognized record type 7, subtype 18 encountered in system file
```

```
## re-encoding from CP1252
```

```
## Warning: /Volumes/ProvidenceFiles/MasterFiles/REG Master_0708.sav:
## Unrecognized record type 7, subtype 18 encountered in system file
```

```
## re-encoding from CP1252
```

```
## Warning: /Volumes/ProvidenceFiles/MasterFiles/REG Master_0809.sav:
## Unrecognized record type 7, subtype 18 encountered in system file
```

```
## re-encoding from CP1252
```

```
## Warning: /Volumes/ProvidenceFiles/MasterFiles/REG Master_0910.sav:
## Unrecognized record type 7, subtype 18 encountered in system file
```

```
## re-encoding from CP1252
```

```
## Warning: /Volumes/ProvidenceFiles/MasterFiles/REG Master_1011.sav:
## Unrecognized record type 7, subtype 18 encountered in system file
```

```
## re-encoding from CP1252
```

```r
source("buildTables.R")
```

```
## [1] 328
```

```
## Loading required package: car
```

```
## [1] "2006"
## [1] 638
## [1] "2007"
## [1] 1289
## [1] "2008"
## [1] 1415
## [1] "2009"
## [1] 1227
## [1] "2010"
## [1] 732
```

```
## Warning: invalid factor level, NA generated
## Warning: invalid factor level, NA generated
## Warning: invalid factor level, NA generated
```

```
## [1] "2011"
```

```
## Warning: invalid factor level, NA generated
## Warning: invalid factor level, NA generated
```

```r
source("person.R")
```

```
## Joining by: c("sid", "attribute")
## Joining by: c("sid", "attribute")
## Joining by: c("sid", "attribute")
## Joining by: c("sid", "attribute")
## Joining by: c("sid", "attribute")
## Joining by: c("sid", "attribute")
## Joining by: c("sid", "attribute")
```

```
## Warning: invalid factor level, NA generated
## Warning: invalid factor level, NA generated
## Warning: invalid factor level, NA generated
```

```
## Joining by: c("sasid", "enroll_date")
## Joining by: c("sasid", "enroll_date")
## Joining by: c("sasid", "schno")
## Joining by: "sasid"
## Joining by: "sasid"
## Joining by: "sasid"
## Joining by: "sasid"
## Joining by: "sasid"
## Joining by: "sasid"
## Joining by: "sasid"
## Joining by: "sasid"
## Joining by: "sasid"
## Joining by: "sasid"
```

```r
source("attendance.R")
```

```
## Warning: invalid factor level, NA generated
## Warning: invalid factor level, NA generated
## Warning: invalid factor level, NA generated
```

```r
attendance$sasid <- as.character(attendance$sasid)
```

We present a new visualization that will assist in understanding where and how students fall off-track on their path from middle school through high school graduation. Our first cohort of students were 8th graders in the 2006-2007 school year. 

```r
starting_grade <- filter(tables2006_2007$person_annual, grade == 8)
```

We can observe how many of these students are first-time 8th graders:


```r
# Check how many of these are first-time eighth graders
table(starting_grade$sasid %in% filter(tables2005_2006$person_annual, grade == 
    8)$sasid)
```

```
## 
## FALSE  TRUE 
##  2091    60
```


We also observe how many students attended Providence public schools the prior year:

```r
# Check how many students were in the district the prior year
table(starting_grade$sasid %in% tables2005_2006$person_annual$sasid)
```

```
## 
## FALSE  TRUE 
##   224  1927
```

How many students who were PPSD 8th graders who attend high school in PPSD the following year?

```r
prop.table(table(starting_grade$sasid %in% filter(tables2007_2008$person_annual, 
    grade == 9)$sasid))
```

```
## 
##  FALSE   TRUE 
## 0.1748 0.8252
```



```r
# Build cohort for predictive model
hs0708 <- subset(person, (schoolyear_first == "2007_2008" & grade_first == 9) | 
    (schoolyear_first == "2008_2009" & grade_first == 10) | (schoolyear_first == 
    "2009_2010" & grade_first == 11) | (schoolyear_first == "2010_2011" & grade_first == 
    12))
hs0809 <- subset(person, (schoolyear_first == "2008_2009" & grade_first == 9) | 
    (schoolyear_first == "2009_2010" & grade_first == 10) | (schoolyear_first == 
    "2010_2011" & grade_first == 11) | (schoolyear_first == "2011_2012" & grade_first == 
    12))
hs0910 <- subset(person, (schoolyear_first == "2009_2010" & grade_first == 9) | 
    (schoolyear_first == "2010_2011" & grade_first == 10) | (schoolyear_first == 
    "2011_2012" & grade_first == 11) | (schoolyear_first == "2012_2013" & grade_first == 
    12))
hs0708$sasid <- as.character(hs0708$sasid)
hs0809$sasid <- as.character(hs0809$sasid)
hs0910$sasid <- as.character(hs0910$sasid)

hs0708 <- left_join(hs0708, attendance %.% filter(schoolyear == "2006_2007") %.% 
    dplyr::select(sasid, attendance, tardy, suspended))
```

```
## Joining by: "sasid"
```

```r
hs0809 <- left_join(hs0809, attendance %.% filter(schoolyear == "2007_2008") %.% 
    dplyr::select(sasid, attendance, tardy, suspended))
```

```
## Joining by: "sasid"
```

```r
hs0910 <- left_join(hs0910, attendance %.% filter(schoolyear == "2008_2009") %.% 
    dplyr::select(sasid, attendance, tardy, suspended))
```

```
## Joining by: "sasid"
```

```r

names(hs0708)[which(names(hs0708) %in% c("suspended"))] <- "suspend8th"
names(hs0708)[which(names(hs0708) %in% c("tardy"))] <- "tardy8th"
names(hs0708)[which(names(hs0708) %in% c("attendance"))] <- "attendance8th"
names(hs0809)[which(names(hs0809) %in% c("suspended"))] <- "suspend8th"
names(hs0809)[which(names(hs0809) %in% c("tardy"))] <- "tardy8th"
names(hs0809)[which(names(hs0809) %in% c("attendance"))] <- "attendance8th"
names(hs0910)[which(names(hs0910) %in% c("suspended"))] <- "suspend8th"
names(hs0910)[which(names(hs0910) %in% c("tardy"))] <- "tardy8th"
names(hs0910)[which(names(hs0910) %in% c("attendance"))] <- "attendance8th"

# Calculate age when student enters 9th grade for the first time.
hs0708 <- mutate(hs0708, ageHS = age_calc(dob, as.Date("2007-09-01"), units = "months"))
hs0809 <- mutate(hs0809, ageHS = age_calc(dob, as.Date("2008-09-01"), units = "months"))
hs0910 <- mutate(hs0910, ageHS = age_calc(dob, as.Date("2009-09-01"), units = "months"))
# Calculate mobility for students Eighth Grade year
mobile0607 <- moves_calc(subset(tables2006_2007$enrollment, sasid %in% hs0708$sasid))

mobile0607$sasid <- as.character(mobile0607$sasid)
hs0708 <- left_join(hs0708, mobile0607)
```

```
## Joining by: "sasid"
```

```r

mobile0708 <- moves_calc(subset(tables2007_2008$enrollment, sasid %in% hs0809$sasid))

mobile0708$sasid <- as.character(mobile0708$sasid)
hs0809 <- left_join(hs0809, mobile0708)
```

```
## Joining by: "sasid"
```

```r

mobile0809 <- moves_calc(subset(tables2008_2009$enrollment, sasid %in% hs0910$sasid))

mobile0809$sasid <- as.character(mobile0809$sasid)
hs0910 <- left_join(hs0910, mobile0809)
```

```
## Joining by: "sasid"
```

```r

# Bring in 8th grade performance on standardized tests.
tables2006_2007$achievement$sasid <- as.character(tables2006_2007$achievement$sasid)
tables2006_2007$achievement$studentid <- as.character(tables2006_2007$achievement$studentid)

tables2007_2008$achievement$sasid <- as.character(tables2007_2008$achievement$sasid)
tables2007_2008$achievement$studentid <- as.character(tables2007_2008$achievement$studentid)

tables2008_2009$achievement$sasid <- as.character(tables2008_2009$achievement$sasid)
tables2008_2009$achievement$studentid <- as.character(tables2008_2009$achievement$studentid)


hs0708 <- left_join(hs0708, tables2006_2007$achievement %.% filter(testgrade_N == 
    8, testgrade_N == grade) %.% dplyr::select(sasid, grade, reaal, reascsc, 
    matal, matscsc, testgrade_N, reanormal, matnormal))
```

```
## Joining by: "sasid"
```

```r
hs0809 <- left_join(hs0809, tables2007_2008$achievement %.% filter(testgrade_N == 
    8, testgrade_N == grade) %.% dplyr::select(sasid, grade, reaal, reascsc, 
    matal, matscsc, testgrade_N, reanormal, matnormal))
```

```
## Joining by: "sasid"
```

```r
hs0910 <- left_join(hs0910, tables2008_2009$achievement %.% filter(testgrade_N == 
    8, testgrade_N == grade) %.% dplyr::select(sasid, grade, reaal, reascsc, 
    matal, matscsc, testgrade_N, reanormal, matnormal))
```

```
## Joining by: "sasid"
```

```r

# Attendance normalized
hs0708 <- mutate(hs0708, attendnormal = (attendance8th - mean(attendance8th, 
    na.rm = TRUE))/sd(attendance8th, na.rm = TRUE))
hs0809 <- mutate(hs0809, attendnormal = (attendance8th - mean(attendance8th, 
    na.rm = TRUE))/sd(attendance8th, na.rm = TRUE))
hs0910 <- mutate(hs0910, attendnormal = (attendance8th - mean(attendance8th, 
    na.rm = TRUE))/sd(attendance8th, na.rm = TRUE))
# GPA in 8th grade calculations
gpa0607 <- tables2006_2007$course %.% filter(grade == 8) %.% group_by(sasid) %.% 
    dplyr::summarise(gpa8th = mean(gpa, na.rm = TRUE))
gpa0607$sasid <- as.character(gpa0607$sasid)
hs0708 <- left_join(hs0708, gpa0607)
```

```
## Joining by: "sasid"
```

```r

gpa0708 <- tables2007_2008$course %.% filter(grade == 8) %.% group_by(sasid) %.% 
    dplyr::summarise(gpa8th = mean(gpa, na.rm = TRUE))
gpa0708$sasid <- as.character(gpa0708$sasid)
hs0809 <- left_join(hs0809, gpa0708)
```

```
## Joining by: "sasid"
```

```r

gpa0809 <- tables2008_2009$course %.% filter(grade == 8) %.% group_by(sasid) %.% 
    dplyr::summarise(gpa8th = mean(gpa, na.rm = TRUE))
gpa0809$sasid <- as.character(gpa0809$sasid)
hs0910 <- left_join(hs0910, gpa0809)
```

```
## Joining by: "sasid"
```

```r

# Course Failures
fails0607 <- tables2006_2007$course %.% filter(grade == 8) %.% group_by(sasid, 
    courseno) %.% dplyr::summarise(gpa = mean(gpa, na.rm = TRUE)) %.% filter(gpa <= 
    1) %.% dplyr::summarise(fails = n())

fails0708 <- tables2007_2008$course %.% filter(grade == 8) %.% group_by(sasid, 
    courseno) %.% dplyr::summarise(gpa = mean(gpa, na.rm = TRUE)) %.% filter(gpa <= 
    1) %.% dplyr::summarise(fails = n())

fails0809 <- tables2008_2009$course %.% filter(grade == 8) %.% group_by(sasid, 
    courseno) %.% dplyr::summarise(gpa = mean(gpa, na.rm = TRUE)) %.% filter(gpa <= 
    1) %.% dplyr::summarise(fails = n())

fails0607$sasid <- as.character(fails0607$sasid)
fails0708$sasid <- as.character(fails0708$sasid)
fails0809$sasid <- as.character(fails0809$sasid)

hs0708 <- left_join(hs0708, fails0607)
```

```
## Joining by: "sasid"
```

```r
hs0708$fails <- with(hs0708, ifelse(is.na(fails) & !is.na(gpa8th), 0, fails))
hs0809 <- left_join(hs0809, fails0708)
```

```
## Joining by: "sasid"
```

```r
hs0809$fails <- with(hs0809, ifelse(is.na(fails) & !is.na(gpa8th), 0, fails))
hs0910 <- left_join(hs0910, fails0809)
```

```
## Joining by: "sasid"
```

```r
hs0910$fails <- with(hs0910, ifelse(is.na(fails) & !is.na(gpa8th), 0, fails))
```


The model for predicting graduation based on 8th grade data is built only with students who attend a Providence Public School as their final high school prior to graduating, dropping out, or transferring to a GED program. Therefore, students whose final exit code from the Providence Public School District is a transfer to another public school, they are not included. These students are excluded because we have no way of knowing whether or not these students ultimately graduate and their graduating is impacted substantially by non-Providence schools. Although they are excluded from the model, we can predict the likelihood that these students will graduate. Because students who are mobile tend to be disproportionately disadvantaged relative to their peers, it is likely that our model for these students is *positively biased*, i.e. these students will have higher probabilities of successfully graduating than is likely true because peers with similar observable characteristics have some unobserved advantages which resulted in being less mobile.


```r
hscohort <- rbind(hs0708, hs0809, hs0910)
hscohort <- filter(hscohort, transfer_out == "N")
hscohort$schoolyear_first <- as.factor(hscohort$schoolyear_first)
basemodel8thgrade <- glm(as.numeric(as.factor(graduated)) - 1 ~ attendnormal + 
    gpa8th + matnormal + reanormal + I(ageHS < 190), data = hscohort, family = binomial(link = "logit"))
require(lme4)
```

```
## Loading required package: lme4
## Loading required package: lattice
## Loading required package: Matrix
## 
## Attaching package: 'lme4'
## 
## The following object is masked from 'package:ggplot2':
## 
##     fortify
```

```r
mixedmodel8thgrade <- glmer(as.numeric(as.factor(graduated)) - 1 ~ attendnormal + 
    gpa8th + reanormal + matnormal + I(ageHS < 190) + (1 | schoolyear_first), 
    data = hscohort, family = binomial(link = "logit"))
starting_grade <- left_join(subset(starting_grade, select = c("sasid", "willrepeatgr", 
    "isrepeatinggr", "disab", "spedprogrm")), subset(hs0708, select = c("sasid", 
    "race", "sex", "parent_lang", "lep", "iep", "transfer_out", "ged", "dropout", 
    "disappear", "graduated", "attendnormal", "attendance8th", "gpa8th", "reanormal", 
    "reaal", "matnormal", "matal", "ageHS", "suspend8th")))
```

```
## Joining by: "sasid"
```

```r
starting_grade$predict8th <- predict(basemodel8thgrade, newdata = starting_grade, 
    type = "response")
summary(basemodel8thgrade)
```

```
## 
## Call:
## glm(formula = as.numeric(as.factor(graduated)) - 1 ~ attendnormal + 
##     gpa8th + matnormal + reanormal + I(ageHS < 190), family = binomial(link = "logit"), 
##     data = hscohort)
## 
## Deviance Residuals: 
##    Min      1Q  Median      3Q     Max  
## -2.693  -0.619   0.426   0.696   2.978  
## 
## Coefficients:
##                    Estimate Std. Error z value Pr(>|z|)    
## (Intercept)         -2.1645     0.2926   -7.40  1.4e-13 ***
## attendnormal         0.9902     0.0704   14.06  < 2e-16 ***
## gpa8th               0.8485     0.0580   14.64  < 2e-16 ***
## matnormal            0.1362     0.0609    2.24    0.025 *  
## reanormal            0.1325     0.0618    2.15    0.032 *  
## I(ageHS < 190)TRUE   1.0828     0.2638    4.10  4.1e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 4849.1  on 3911  degrees of freedom
## Residual deviance: 3582.9  on 3906  degrees of freedom
##   (1969 observations deleted due to missingness)
## AIC: 3595
## 
## Number of Fisher Scoring iterations: 5
```

```r
summary(starting_grade$predict8th)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##     0.0     0.5     0.8     0.7     0.9     1.0     522
```

Predictive models are traditionally evaluated on their ability to correctly classify, or sort, students so that predicted outcomes match the actual outcomes. Our model outputs a probability of graduating for each student that can take on any value between 0 and 1. In order to sort students into those who are at risk of failing to graduate and those who are not, we have to select threshold probabilities above which students are not considered at risk and below which they are.

Thresholds for predictive data are often set at the point with the highest *accuracy*. Accuracy is highest where the proportion of students who are correctly classified as graduating or not graduating is the greatest. Therefore, accuracy values *true positives*, identifying students who will graduate, the same as *true negatives*, identifying students who will not graduate. Beacuse we want to intervene on the behalf of students who are falling behind, accuracy alone may not be the best way to set thresholds. Instead, we want to maximize the *true negative rate*, ensuring we capture as many students who fail to graduate as possible, while maintaining the best accuracy possible. In practice, this means the best cutoff thresholds for high risk may have slightly less than the maximum accuracy.

The Rhode Island Department of Education (RIDE) has been developing an Early Warning System using statewide data. They have set thresholds for high, moderate, and low risk by examining the true negative rate and accuracy. The true negative rate for high, moderate, and low risk are 0.8, 0.7, and 0.6.


```r
cutoff_matrix(starting_grade, "predict8th", "graduated")
```

```
##     cutoff true_pos true_neg false_pos false_neg true_neg_rate
## 1     1.00        0      642         0       987        0.3941
## 2     0.99        0      642         0       987        0.3941
## 3     0.98        5      641         1       982        0.3949
## 4     0.97       34      636         6       953        0.4003
## 5     0.96       63      636         6       924        0.4077
## 6     0.95      107      633         9       880        0.4184
## 7     0.94      158      631        11       829        0.4322
## 8     0.93      197      627        15       790        0.4425
## 9     0.92      238      623        19       749        0.4541
## 10    0.91      277      613        29       710        0.4633
## 11    0.90      305      605        37       682        0.4701
## 12    0.89      331      601        41       656        0.4781
## 13    0.88      375      593        49       612        0.4921
## 14    0.87      412      583        59       575        0.5035
## 15    0.86      435      574        68       552        0.5098
## 16    0.85      459      569        73       528        0.5187
## 17    0.84      482      561        81       505        0.5263
## 18    0.83      513      551        91       474        0.5376
## 19    0.82      539      538       104       448        0.5456
## 20    0.81      565      525       117       422        0.5544
## 21    0.80      584      519       123       403        0.5629
## 22    0.79      600      513       129       387        0.5700
## 23    0.78      619      507       135       368        0.5794
## 24    0.77      638      501       141       349        0.5894
## 25    0.76      651      496       146       336        0.5962
## 26    0.75      663      490       152       324        0.6020
## 27    0.74      676      478       164       311        0.6058
## 28    0.73      696      462       180       291        0.6135
## 29    0.72      708      455       187       279        0.6199
## 30    0.71      719      450       192       268        0.6267
## 31    0.70      734      440       202       253        0.6349
## 32    0.69      746      432       210       241        0.6419
## 33    0.68      751      427       215       236        0.6440
## 34    0.67      764      416       226       223        0.6510
## 35    0.66      783      406       236       204        0.6656
## 36    0.65      791      398       244       196        0.6700
## 37    0.64      802      392       250       185        0.6794
## 38    0.63      810      386       256       177        0.6856
## 39    0.62      821      377       265       166        0.6943
## 40    0.61      828      370       272       159        0.6994
## 41    0.60      836      366       276       151        0.7079
## 42    0.59      842      357       285       145        0.7112
## 43    0.58      845      351       291       142        0.7120
## 44    0.57      849      345       297       138        0.7143
## 45    0.56      858      336       306       129        0.7226
## 46    0.55      862      328       314       125        0.7241
## 47    0.54      868      326       316       119        0.7326
## 48    0.53      870      316       326       117        0.7298
## 49    0.52      879      310       332       108        0.7416
## 50    0.51      882      304       338       105        0.7433
## 51    0.50      889      293       349        98        0.7494
## 52    0.49      892      287       355        95        0.7513
## 53    0.48      900      282       360        87        0.7642
## 54    0.47      903      278       364        84        0.7680
## 55    0.46      908      271       371        79        0.7743
## 56    0.45      911      266       376        76        0.7778
## 57    0.44      916      260       382        71        0.7855
## 58    0.43      921      252       390        66        0.7925
## 59    0.42      924      245       397        63        0.7955
## 60    0.41      929      241       401        58        0.8060
## 61    0.40      932      233       409        55        0.8090
## 62    0.39      937      228       414        50        0.8201
## 63    0.38      941      222       420        46        0.8284
## 64    0.37      945      213       429        42        0.8353
## 65    0.36      948      207       435        39        0.8415
## 66    0.35      954      202       440        33        0.8596
## 67    0.34      957      198       444        30        0.8684
## 68    0.33      958      192       450        29        0.8688
## 69    0.32      959      186       456        28        0.8692
## 70    0.31      960      183       459        27        0.8714
## 71    0.30      961      175       467        26        0.8706
## 72    0.29      963      172       470        24        0.8776
## 73    0.28      967      164       478        20        0.8913
## 74    0.27      968      156       486        19        0.8914
## 75    0.26      971      153       489        16        0.9053
## 76    0.25      971      148       494        16        0.9024
## 77    0.24      973      143       499        14        0.9108
## 78    0.23      975      136       506        12        0.9189
## 79    0.22      976      131       511        11        0.9225
## 80    0.21      976      125       517        11        0.9191
## 81    0.20      979      115       527         8        0.9350
## 82    0.19      979      110       532         8        0.9322
## 83    0.18      979      105       537         8        0.9292
## 84    0.17      981      102       540         6        0.9444
## 85    0.16      981       98       544         6        0.9423
## 86    0.15      981       93       549         6        0.9394
## 87    0.14      982       91       551         5        0.9479
## 88    0.13      983       86       556         4        0.9556
## 89    0.12      983       81       561         4        0.9529
## 90    0.11      983       71       571         4        0.9467
## 91    0.10      983       66       576         4        0.9429
## 92    0.09      983       61       581         4        0.9385
## 93    0.08      983       52       590         4        0.9286
## 94    0.07      984       44       598         3        0.9362
## 95    0.06      985       38       604         2        0.9500
## 96    0.05      986       33       609         1        0.9706
## 97    0.04      986       25       617         1        0.9615
## 98    0.03      986       18       624         1        0.9474
## 99    0.02      986       10       632         1        0.9091
## 100   0.01      987        2       640         0        1.0000
## 101   0.00      987        0       642         0           NaN
##     true_pos_rate false_pos_rate false_neg_rate accuracy
## 1             NaN            NaN        0.60589   0.3941
## 2             NaN            NaN        0.60589   0.3941
## 3          0.8333        0.16667        0.60505   0.3966
## 4          0.8500        0.15000        0.59975   0.4113
## 5          0.9130        0.08696        0.59231   0.4291
## 6          0.9224        0.07759        0.58163   0.4543
## 7          0.9349        0.06509        0.56781   0.4843
## 8          0.9292        0.07075        0.55752   0.5058
## 9          0.9261        0.07393        0.54592   0.5285
## 10         0.9052        0.09477        0.53666   0.5463
## 11         0.8918        0.10819        0.52991   0.5586
## 12         0.8898        0.11022        0.52188   0.5721
## 13         0.8844        0.11557        0.50788   0.5942
## 14         0.8747        0.12527        0.49655   0.6108
## 15         0.8648        0.13519        0.49023   0.6194
## 16         0.8628        0.13722        0.48131   0.6311
## 17         0.8561        0.14387        0.47373   0.6403
## 18         0.8493        0.15066        0.46244   0.6532
## 19         0.8383        0.16174        0.45436   0.6611
## 20         0.8284        0.17155        0.44562   0.6691
## 21         0.8260        0.17397        0.43709   0.6771
## 22         0.8230        0.17695        0.43000   0.6832
## 23         0.8210        0.17905        0.42057   0.6912
## 24         0.8190        0.18100        0.41059   0.6992
## 25         0.8168        0.18319        0.40385   0.7041
## 26         0.8135        0.18650        0.39803   0.7078
## 27         0.8048        0.19524        0.39417   0.7084
## 28         0.7945        0.20548        0.38645   0.7109
## 29         0.7911        0.20894        0.38011   0.7139
## 30         0.7892        0.21076        0.37326   0.7176
## 31         0.7842        0.21581        0.36508   0.7207
## 32         0.7803        0.21967        0.35810   0.7231
## 33         0.7774        0.22257        0.35596   0.7231
## 34         0.7717        0.22828        0.34898   0.7244
## 35         0.7684        0.23160        0.33443   0.7299
## 36         0.7643        0.23575        0.32997   0.7299
## 37         0.7624        0.23764        0.32062   0.7330
## 38         0.7598        0.24015        0.31439   0.7342
## 39         0.7560        0.24401        0.30571   0.7354
## 40         0.7527        0.24727        0.30057   0.7354
## 41         0.7518        0.24820        0.29207   0.7379
## 42         0.7471        0.25288        0.28884   0.7360
## 43         0.7438        0.25616        0.28803   0.7342
## 44         0.7408        0.25916        0.28571   0.7330
## 45         0.7371        0.26289        0.27742   0.7330
## 46         0.7330        0.26701        0.27594   0.7305
## 47         0.7331        0.26689        0.26742   0.7330
## 48         0.7274        0.27258        0.27021   0.7281
## 49         0.7258        0.27415        0.25837   0.7299
## 50         0.7230        0.27705        0.25672   0.7281
## 51         0.7181        0.28191        0.25064   0.7256
## 52         0.7153        0.28468        0.24869   0.7238
## 53         0.7143        0.28571        0.23577   0.7256
## 54         0.7127        0.28729        0.23204   0.7250
## 55         0.7099        0.29007        0.22571   0.7238
## 56         0.7078        0.29215        0.22222   0.7225
## 57         0.7057        0.29430        0.21450   0.7219
## 58         0.7025        0.29748        0.20755   0.7201
## 59         0.6995        0.30053        0.20455   0.7176
## 60         0.6985        0.30150        0.19398   0.7182
## 61         0.6950        0.30500        0.19097   0.7152
## 62         0.6936        0.30644        0.17986   0.7152
## 63         0.6914        0.30860        0.17164   0.7139
## 64         0.6878        0.31223        0.16471   0.7109
## 65         0.6855        0.31453        0.15854   0.7090
## 66         0.6844        0.31564        0.14043   0.7096
## 67         0.6831        0.31692        0.13158   0.7090
## 68         0.6804        0.31960        0.13122   0.7060
## 69         0.6777        0.32226        0.13084   0.7029
## 70         0.6765        0.32347        0.12857   0.7017
## 71         0.6730        0.32703        0.12935   0.6974
## 72         0.6720        0.32798        0.12245   0.6967
## 73         0.6692        0.33080        0.10870   0.6943
## 74         0.6657        0.33425        0.10857   0.6900
## 75         0.6651        0.33493        0.09467   0.6900
## 76         0.6628        0.33720        0.09756   0.6869
## 77         0.6610        0.33899        0.08917   0.6851
## 78         0.6583        0.34166        0.08108   0.6820
## 79         0.6564        0.34364        0.07746   0.6796
## 80         0.6537        0.34628        0.08088   0.6759
## 81         0.6501        0.34993        0.06504   0.6716
## 82         0.6479        0.35208        0.06780   0.6685
## 83         0.6458        0.35422        0.07080   0.6654
## 84         0.6450        0.35503        0.05556   0.6648
## 85         0.6433        0.35672        0.05769   0.6624
## 86         0.6412        0.35882        0.06061   0.6593
## 87         0.6406        0.35943        0.05208   0.6587
## 88         0.6387        0.36127        0.04444   0.6562
## 89         0.6367        0.36334        0.04706   0.6532
## 90         0.6326        0.36744        0.05333   0.6470
## 91         0.6305        0.36947        0.05714   0.6440
## 92         0.6285        0.37148        0.06154   0.6409
## 93         0.6249        0.37508        0.07143   0.6354
## 94         0.6220        0.37800        0.06383   0.6311
## 95         0.6199        0.38011        0.05000   0.6280
## 96         0.6182        0.38182        0.02941   0.6255
## 97         0.6151        0.38490        0.03846   0.6206
## 98         0.6124        0.38758        0.05263   0.6163
## 99         0.6094        0.39061        0.09091   0.6114
## 100        0.6066        0.39336        0.00000   0.6071
## 101        0.6059        0.39411            NaN   0.6059
```


In examining the results of our predictive model, we find that at a true negative rate of 0.8, the accuracy of our classifier is 71%. The maximum accuracy is approximately 73.5%, at which the true negative rate is 0.69. Therefore, we feel our data supports using the 0.8 threshold for high risk because the modest tradeoff in accuracy is more than made up by the increase in true negative rate.

There are two key ways to compare the model developed in this work to the RIDE Early Warning System aggregate indicator. The first measure is *coverage*, or what proportion of total drop outs are identified by the measure. For the same reasons we are maximizing the *true negative rate*, we want to be sure that our risk thresholds identify as many of the students who ultimately fail to graduate as possible. Our model captures 37% of all drop outs in the 8th grade, a consderable improvement on the state's model which captures fewer than 20% of all drop outs. The state's model, however, is slightly more accurate, at approximately 80% versus 71%. We believe this model represents a better tradeoff between accuracy and coverage. Many more students would be incorrectly identified as in need of supports under the state model in order to capture 37% of all drop outs.


```r
# First Quarter Grades
gpa_qtr1_0708 <- tables2007_2008$course %.% filter(grade == 9 & sasid %in% hscohort$sasid) %.% 
    group_by(sasid, variable) %.% summarize(gpa_qtr1_9th = mean(gpa, na.rm = TRUE)) %.% 
    filter(variable == "cum_qtr1") %.% select(sasid, gpa_qtr1_9th)
gpa_qtr1_0809 <- tables2008_2009$course %.% filter(grade == 9 & sasid %in% hscohort$sasid) %.% 
    group_by(sasid, variable) %.% summarize(gpa_qtr1_9th = mean(gpa, na.rm = TRUE)) %.% 
    filter(variable == "cum_qtr1") %.% select(sasid, gpa_qtr1_9th)
gpa_qtr1_0910 <- tables2009_2010$course %.% filter(grade == 9 & sasid %in% hscohort$sasid) %.% 
    group_by(sasid, variable) %.% summarize(gpa_qtr1_9th = mean(gpa, na.rm = TRUE)) %.% 
    filter(variable == "cum_qtr1") %.% select(sasid, gpa_qtr1_9th)

# First Quarter Course Failures
fails_qtr1_0708 <- tables2007_2008$course %.% filter(grade == 9 & sasid %in% 
    hscohort$sasid) %.% group_by(sasid, variable, courseno) %.% summarize(gpa = mean(gpa, 
    na.rm = TRUE)) %.% filter(gpa <= 1 & variable == "cum_qtr1") %.% summarize(fails_qtr1 = n())

fails_qtr1_0809 <- tables2008_2009$course %.% filter(grade == 9 & sasid %in% 
    hscohort$sasid & !sasid %in% gpa_qtr1_0708) %.% group_by(sasid, variable, 
    courseno) %.% summarize(gpa = mean(gpa, na.rm = TRUE)) %.% filter(gpa <= 
    1 & variable == "cum_qtr1") %.% summarize(fails_qtr1 = n())

fails_qtr1_0910 <- tables2009_2010$course %.% filter(grade == 9 & sasid %in% 
    hscohort$sasid & !sasid %in% gpa_qtr1_0809) %.% group_by(sasid, variable, 
    courseno) %.% summarize(gpa = mean(gpa, na.rm = TRUE)) %.% filter(gpa <= 
    1 & variable == "cum_qtr1") %.% summarize(fails_qtr1 = n())


# hscohort <- left_join(hscohort, rbind(gpa_qtr1_0708, gpa_qtr1_0809,
# gpa_qtr1_0910) %.% select(sasid, fails_qtr1)) hscohort <-
# left_join(hscohort, rbind(fails_qtr1_0708, fails_qtr1_0809,
# fails_qtr1_0910) %.% select(sasid, fails_qtr1))
```

The model used to predict graduation for ninth grade students is restricted to first-time ninth grade students who do not transfer out of Providence Public Schools.

Because of the exclusion of students who transfer out, we may be concerned that we are biasing our model since students who transfer out may represent more vulnerable students who are less likely to graduate than their peers. A quick check to see if this is accurate is to examine whether students who transfer out are dramatically different in key ways from those who remain. (I could do a more extensive chart here) One of the key early indicators of likelihood to graduate is 9th grade retention. There is a massive differential in graduation rates between students who are retained versus those who are not (approximately 15% versus nearly 60%).

However, students who transfer out are only retained at moderately higher rates which is unlikely to represent any true relationship. For example, for first-time high school freshman in 2007-2008, 14% of students who remained in Providence Public schools for the entirety of their high school enrollment were retained in 9th grade compared to ~16.5% of those who transfer out of Providence Public Schools.


```r
hs0708 <- subset(person, (schoolyear_first == "2007_2008" & grade_first == 9) | 
    (schoolyear_first == "2008_2009" & grade_first == 10) | (schoolyear_first == 
    "2009_2010" & grade_first == 11) | (schoolyear_first == "2010_2011" & grade_first == 
    12))
hs0708$retained <- ifelse(hs0708$sasid %in% filter(hs0708, grade_first == 9 & 
    sasid %in% filter(tables2008_2009$person_annual, grade == 9)$sasid)$sasid, 
    "Y", "N")


hs0809 <- subset(person, (schoolyear_first == "2008_2009" & grade_first == 9) | 
    (schoolyear_first == "2009_2010" & grade_first == 10) | (schoolyear_first == 
    "2010_2011" & grade_first == 11) | (schoolyear_first == "2011_2012" & grade_first == 
    12))

hs0809$retained <- ifelse(hs0809$sasid %in% filter(hs0809, grade_first == 9 & 
    sasid %in% filter(tables2009_2010$person_annual, grade == 9)$sasid)$sasid, 
    "Y", "N")
hs0910 <- subset(person, (schoolyear_first == "2009_2010" & grade_first == 9) | 
    (schoolyear_first == "2010_2011" & grade_first == 10) | (schoolyear_first == 
    "2011_2012" & grade_first == 11) | (schoolyear_first == "2012_2013" & grade_first == 
    12))
hs0910$retained <- ifelse(hs0910$sasid %in% filter(hs0910, grade_first == 9 & 
    sasid %in% filter(tables2010_2011$person_annual, grade == 9)$sasid)$sasid, 
    "Y", "N")

hs0708$sasid <- as.character(hs0708$sasid)
hs0809$sasid <- as.character(hs0809$sasid)
hs0910$sasid <- as.character(hs0910$sasid)

# Attendance is same as previous code for cohorts except that I use the
# school year that is the same as the grade_first value so that it
# represents 9th grade attendance not 8th grade.

hs0708 <- left_join(hs0708, attendance %.% filter(schoolyear == "2007_2008") %.% 
    select(sasid, attendance, tardy, suspended))
```

```
## Joining by: "sasid"
```

```r
hs0809 <- left_join(hs0809, attendance %.% filter(schoolyear == "2008_2009") %.% 
    select(sasid, attendance, tardy, suspended))
```

```
## Joining by: "sasid"
```

```r
hs0910 <- left_join(hs0910, attendance %.% filter(schoolyear == "2009_2010") %.% 
    select(sasid, attendance, tardy, suspended))
```

```
## Joining by: "sasid"
```

```r
hs0708 <- mutate(hs0708, attendnormal = (attendance - mean(attendance, na.rm = TRUE))/sd(attendance, 
    na.rm = TRUE))
hs0809 <- mutate(hs0809, attendnormal = (attendance - mean(attendance, na.rm = TRUE))/sd(attendance, 
    na.rm = TRUE))
hs0910 <- mutate(hs0910, attendnormal = (attendance - mean(attendance, na.rm = TRUE))/sd(attendance, 
    na.rm = TRUE))
# Calculate age when student enters 9th grade for the first time.
hs0708 <- mutate(hs0708, ageHS = age_calc(dob, as.Date("2007-09-01"), units = "months"))
hs0809 <- mutate(hs0809, ageHS = age_calc(dob, as.Date("2008-09-01"), units = "months"))
hs0910 <- mutate(hs0910, ageHS = age_calc(dob, as.Date("2009-09-01"), units = "months"))


# GPA in 8th grade tables already exist
hs0708 <- left_join(hs0708, gpa0607)
```

```
## Joining by: "sasid"
```

```r
hs0809 <- left_join(hs0809, gpa0708)
```

```
## Joining by: "sasid"
```

```r
hs0910 <- left_join(hs0910, gpa0809)
```

```
## Joining by: "sasid"
```

```r
rm(gpa0607)
rm(gpa0708)
rm(gpa0809)

# GPA in 9th grade mimic 8th grade calculations
gpa0708 <- tables2007_2008$course %.% filter(grade == 9) %.% group_by(sasid) %.% 
    summarize(gpa9th = mean(gpa, na.rm = TRUE))
gpa0708$sasid <- as.character(gpa0708$sasid)
hs0708 <- left_join(hs0708, gpa0708)
```

```
## Joining by: "sasid"
```

```r

gpa0809 <- tables2008_2009$course %.% filter(grade == 9) %.% group_by(sasid) %.% 
    summarize(gpa9th = mean(gpa, na.rm = TRUE))
hs0809 <- left_join(hs0809, gpa0809)
```

```
## Joining by: "sasid"
```

```r

gpa0910 <- tables2009_2010$course %.% filter(grade == 9) %.% group_by(sasid) %.% 
    summarize(gpa9th = mean(gpa, na.rm = TRUE))
gpa0910$sasid <- as.character(gpa0910$sasid)
hs0910 <- left_join(hs0910, gpa0910)
```

```
## Joining by: "sasid"
```

```r

# Course Failures
fails0708 <- tables2007_2008$course %.% filter(grade == 9) %.% group_by(sasid, 
    courseno) %.% summarize(gpa = mean(gpa, na.rm = TRUE)) %.% filter(gpa <= 
    1) %.% summarize(fails = n())

fails0809 <- tables2008_2009$course %.% filter(grade == 9) %.% group_by(sasid, 
    courseno) %.% summarize(gpa = mean(gpa, na.rm = TRUE)) %.% filter(gpa <= 
    1) %.% summarize(fails = n())

fails0910 <- tables2009_2010$course %.% filter(grade == 9) %.% group_by(sasid, 
    courseno) %.% summarize(gpa = mean(gpa, na.rm = TRUE)) %.% filter(gpa <= 
    1) %.% summarize(fails = n())

fails0708$sasid <- as.character(fails0708$sasid)
fails0809$sasid <- as.character(fails0809$sasid)
fails0910$sasid <- as.character(fails0910$sasid)

hs0708 <- left_join(hs0708, fails0708)
```

```
## Joining by: "sasid"
```

```r
hs0708$fails <- with(hs0708, ifelse(is.na(fails) & !is.na(gpa9th), 0, fails))
hs0809 <- left_join(hs0809, fails0809)
```

```
## Joining by: "sasid"
```

```r
hs0809$fails <- with(hs0809, ifelse(is.na(fails) & !is.na(gpa9th), 0, fails))
hs0910 <- left_join(hs0910, fails0910)
```

```
## Joining by: "sasid"
```

```r
hs0910$fails <- with(hs0910, ifelse(is.na(fails) & !is.na(gpa9th), 0, fails))

# Mobility Calculations
mobile0708 <- moves_calc(filter(tables2007_2008$enrollment, sasid %in% hs0708$sasid))
mobile0708$sasid <- as.character(mobile0708$sasid)
hs0708 <- left_join(hs0708, mobile0708)
```

```
## Joining by: "sasid"
```

```r

mobile0809 <- moves_calc(filter(tables2008_2009$enrollment, sasid %in% hs0809$sasid))
mobile0809$sasid <- as.character(mobile0809$sasid)
hs0809 <- left_join(hs0809, mobile0809)
```

```
## Joining by: "sasid"
```

```r

mobile0910 <- moves_calc(filter(tables2009_2010$enrollment, sasid %in% hs0910$sasid))
mobile0910$sasid <- as.character(mobile0910$sasid)
hs0910 <- left_join(hs0910, mobile0910)
```

```
## Joining by: "sasid"
```



```r
# descriminate outcome analysis range of scores for suspensions cluster use
# outcome for differentiation in the independent variable k-means clustering
# -- try the 5 credits model
hscohort <- rbind(hs0708, hs0809, hs0910)
hscohort <- filter(hscohort, transfer_out == "N")
hscohort$schoolyear_first <- as.factor(hscohort$schoolyear_first)
basemodel9thgrade <- glm(as.numeric(as.factor(graduated)) - 1 ~ attendnormal + 
    gpa8th + gpa9th + fails + retained + I(moves > 1) + suspended * I(iep == 
    "Y") + I(ageHS > 190) + I(schno_first == "164"), data = hscohort, family = binomial(link = "logit"))
summary(basemodel9thgrade)
```

```
## 
## Call:
## glm(formula = as.numeric(as.factor(graduated)) - 1 ~ attendnormal + 
##     gpa8th + gpa9th + fails + retained + I(moves > 1) + suspended * 
##     I(iep == "Y") + I(ageHS > 190) + I(schno_first == "164"), 
##     family = binomial(link = "logit"), data = hscohort)
## 
## Deviance Residuals: 
##    Min      1Q  Median      3Q     Max  
## -2.784  -0.414   0.358   0.616   2.773  
## 
## Coefficients:
##                             Estimate Std. Error z value Pr(>|z|)    
## (Intercept)                  -0.5286     0.2883   -1.83   0.0667 .  
## attendnormal                  0.8896     0.0944    9.42  < 2e-16 ***
## gpa8th                        0.4883     0.0679    7.19  6.6e-13 ***
## gpa9th                        0.3885     0.1044    3.72   0.0002 ***
## fails                        -0.0887     0.0425   -2.09   0.0370 *  
## retainedY                    -0.8971     0.1358   -6.61  4.0e-11 ***
## I(moves > 1)TRUE             -0.4150     0.2920   -1.42   0.1553    
## suspended                    -0.0874     0.0288   -3.03   0.0024 ** 
## I(iep == "Y")TRUE            -0.2948     0.1197   -2.46   0.0138 *  
## I(ageHS > 190)TRUE           -1.3325     0.2546   -5.23  1.7e-07 ***
## I(schno_first == "164")TRUE   0.5091     0.2388    2.13   0.0330 *  
## suspended:I(iep == "Y")TRUE   0.1055     0.0484    2.18   0.0293 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 4854.4  on 3869  degrees of freedom
## Residual deviance: 3119.0  on 3858  degrees of freedom
##   (2010 observations deleted due to missingness)
## AIC: 3143
## 
## Number of Fisher Scoring iterations: 5
```

```r
plot(roc(graduated ~ predict(basemodel9thgrade, newdata = hscohort, type = "response"), 
    hscohort), print.auc = TRUE)
```

![plot of chunk Grade9Model](figure/Grade9Model.png) 

```
## 
## Call:
## roc.formula(formula = graduated ~ predict(basemodel9thgrade,     newdata = hscohort, type = "response"), data = hscohort)
## 
## Data: predict(basemodel9thgrade, newdata = hscohort, type = "response") in 1240 controls (graduated N) < 2630 cases (graduated Y).
## Area under the curve: 0.873
```




```r
hs0607 <- subset(person, (schoolyear_first == "2006_2007" & grade_first == 9) | 
    (schoolyear_first == "2007_2008" & grade_first == 10) | (schoolyear_first == 
    "2008_2009" & grade_first == 11) | (schoolyear_first == "2009_2010" & grade_first == 
    12))
hs0708 <- subset(person, (schoolyear_first == "2007_2008" & grade_first == 9) | 
    (schoolyear_first == "2008_2009" & grade_first == 10) | (schoolyear_first == 
    "2009_2010" & grade_first == 11) | (schoolyear_first == "2010_2011" & grade_first == 
    12))
hs0809 <- subset(person, (schoolyear_first == "2008_2009" & grade_first == 9) | 
    (schoolyear_first == "2009_2010" & grade_first == 10) | (schoolyear_first == 
    "2010_2011" & grade_first == 11) | (schoolyear_first == "2011_2012" & grade_first == 
    12))
hs0910 <- subset(person, (schoolyear_first == "2009_2010" & grade_first == 9) | 
    (schoolyear_first == "2010_2011" & grade_first == 10) | (schoolyear_first == 
    "2011_2012" & grade_first == 11) | (schoolyear_first == "2012_2013" & grade_first == 
    12))

# Repeated 9th grade
hs0607 <- left_join(hs0607, retained_calc(x = annual_demos, sid = "sasid", grade = "grade", 
    grade_value = 9))
```

```
## Joining by: "sasid"
```

```r
names(hs0607)[length(names(hs0607))] <- "retained9th"
hs0708 <- left_join(hs0708, retained_calc(x = annual_demos, sid = "sasid", grade = "grade", 
    grade_value = 9))
```

```
## Joining by: "sasid"
```

```r
names(hs0708)[length(names(hs0708))] <- "retained9th"

hs0809 <- left_join(hs0809, retained_calc(x = annual_demos, sid = "sasid", grade = "grade", 
    grade_value = 9))
```

```
## Joining by: "sasid"
```

```r
names(hs0809)[length(names(hs0809))] <- "retained9th"

hs0910 <- left_join(hs0910, retained_calc(x = annual_demos, sid = "sasid", grade = "grade", 
    grade_value = 9))
```

```
## Joining by: "sasid"
```

```r
names(hs0910)[length(names(hs0910))] <- "retained9th"

# Repeated 10th grade
hs0607 <- left_join(hs0607, retained_calc(x = annual_demos, sid = "sasid", grade = "grade", 
    grade_value = 10))
```

```
## Joining by: "sasid"
```

```r
names(hs0607)[length(names(hs0607))] <- "retained10th"
hs0708 <- left_join(hs0708, retained_calc(x = annual_demos, sid = "sasid", grade = "grade", 
    grade_value = 10))
```

```
## Joining by: "sasid"
```

```r
names(hs0708)[length(names(hs0708))] <- "retained10th"

hs0809 <- left_join(hs0809, retained_calc(x = annual_demos, sid = "sasid", grade = "grade", 
    grade_value = 10))
```

```
## Joining by: "sasid"
```

```r
names(hs0809)[length(names(hs0809))] <- "retained10th"

hs0910 <- left_join(hs0910, retained_calc(x = annual_demos, sid = "sasid", grade = "grade", 
    grade_value = 10))
```

```
## Joining by: "sasid"
```

```r
names(hs0910)[length(names(hs0910))] <- "retained10th"



hs0607$sasid <- as.character(hs0607$sasid)
hs0708$sasid <- as.character(hs0708$sasid)
hs0809$sasid <- as.character(hs0809$sasid)
hs0910$sasid <- as.character(hs0910$sasid)

# Attendance is for the second year in the system... but this may not be
# 10th grade for students who were not promoted...

hs0607 <- left_join(hs0607, attendance %.% filter(grade == 10) %.% group_by(sasid) %.% 
    filter(row_number(desc(schoolyear)) == 1) %.% select(sasid, attendance, 
    tardy, suspended))
```

```
## Joining by: "sasid"
```

```r
hs0708 <- left_join(hs0708, attendance %.% filter(grade == 10) %.% group_by(sasid) %.% 
    filter(row_number(desc(schoolyear)) == 1) %.% select(sasid, attendance, 
    tardy, suspended))
```

```
## Joining by: "sasid"
```

```r
hs0809 <- left_join(hs0809, attendance %.% filter(grade == 10) %.% group_by(sasid) %.% 
    filter(row_number(desc(schoolyear)) == 1) %.% select(sasid, attendance, 
    tardy, suspended))
```

```
## Joining by: "sasid"
```

```r
hs0910 <- left_join(hs0910, attendance %.% filter(grade == 10) %.% group_by(sasid) %.% 
    filter(row_number(desc(schoolyear)) == 1) %.% select(sasid, attendance, 
    tardy, suspended))
```

```
## Joining by: "sasid"
```

```r

hs0607 <- mutate(hs0607, attendnormal = scale(attendance))
hs0708 <- mutate(hs0708, attendnormal = scale(attendance))
hs0809 <- mutate(hs0809, attendnormal = scale(attendance))
hs0910 <- mutate(hs0910, attendnormal = scale(attendance))

# Calculate age when student enters 9th grade for the first time.
hs0607 <- mutate(hs0607, ageHS = age_calc(dob, as.Date("2006-09-01"), units = "months"))
hs0708 <- mutate(hs0708, ageHS = age_calc(dob, as.Date("2007-09-01"), units = "months"))
hs0809 <- mutate(hs0809, ageHS = age_calc(dob, as.Date("2008-09-01"), units = "months"))
hs0910 <- mutate(hs0910, ageHS = age_calc(dob, as.Date("2009-09-01"), units = "months"))

# GPA tables
gpa <- rbind(tables2007_2008$course, tables2008_2009$course, tables2009_2010$course, 
    tables2010_2011$course, tables2011_2012$course) %.% filter(grade == 10) %.% 
    group_by(sasid, schoolyear) %.% summarize(gpa10th = mean(gpa, na.rm = TRUE)) %.% 
    filter(row_number(desc(schoolyear)) == 1) %.% select(sasid, gpa10th)

fails <- rbind(tables2007_2008$course, tables2008_2009$course, tables2009_2010$course, 
    tables2010_2011$course, tables2011_2012$course) %.% filter(grade == 10) %.% 
    group_by(sasid) %.% filter(schoolyear == min(schoolyear)) %.% group_by(sasid, 
    courseno) %.% summarize(gpa10th = mean(gpa, na.rm = TRUE)) %.% filter(gpa10th <= 
    1) %.% group_by(sasid) %.% summarize(fails = n()) %.% select(sasid, fails)

# Mobility Calculations
mobile0607 <- moves_calc(filter(tables2007_2008$enrollment, grade == 10))
mobile0607$schoolyear <- "2007_2008"
mobile0708 <- moves_calc(filter(tables2008_2009$enrollment, grade == 10))
mobile0708$schoolyear <- "2008_2009"
mobile0809 <- moves_calc(filter(tables2009_2010$enrollment, grade == 10))
mobile0809$schoolyear <- "2009_2010"
mobile0910 <- moves_calc(filter(tables2010_2011$enrollment, grade == 10))
mobile0910$schoolyear <- "2010_2011"
mobile1011 <- moves_calc(filter(tables2011_2012$enrollment, grade == 10))
```

```
## Warning: the condition has length > 1 and only the first element will be used
## Warning: no non-missing arguments to min; returning Inf
## Warning: no non-missing arguments to min; returning Inf
```

```
## Error: argument is of length zero
```

```r
mobile1011$schoolyear <- "2011_2012"
```

```
## Error: object 'mobile1011' not found
```

```r

mobile <- rbind(mobile0607, mobile0708, mobile0809, mobile0910, mobile1011) %.% 
    group_by(sasid) %.% filter(schoolyear == min(schoolyear))
```

```
## Error: object 'mobile1011' not found
```

```r
# mobile0708 <- moves_calc(filter(tables2007_2 008$enrollment, sasid %in%
# hs0708$sasid)) mobile0708$sasid <- as.character(mobile0708$sasid) hs0708
# <- left_join(hs0708, mobile0708) mobile0809 <-
# moves_calc(filter(tables2008_2009$enrollment, sasid %in% hs0809$sasid))
# mobile0809$sasid <- as.character(mobile0809$sasid) hs0809 <-
# left_join(hs0809, mobile0809) mobile0910 <-
# moves_calc(filter(tables2009_2010$enrollment, sasid %in% hs0910$sasid))
# mobile0910$sasid <- as.character(mobile0910$sasid) hs0910 <-
# left_join(hs0910, mobile0910)
```



```r
hscohort <- rbind(hs0607, hs0708, hs0809, hs0910)
hscohort <- left_join(hscohort, gpa)
```

```
## Joining by: "sasid"
```

```r
hscohort <- left_join(hscohort, fails)
```

```
## Joining by: "sasid"
```

```r
hsochort <- filter(hscohort, transfer_out == "N")
basemodel10thgrade <- glm(as.numeric(as.factor(graduated)) - 1 ~ attendnormal + 
    gpa10th + retained9th + retained10th + fails + suspended * I(iep == "Y") + 
    I(ageHS > 190) + I(schno_first == "164"), data = hscohort, family = binomial(link = "logit"))
```

