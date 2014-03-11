Pathways to Graduation
========================================================


```r
# , echo=FALSE, results='hide'}
setwd("/Users/jason/Dropbox/code/PPSDCollegeReadiness/")
source("dependencies.R")
```

```
## Loading required package: foreign
## Loading required package: reshape2
## Loading required package: ggplot2
## Loading required package: scales
## Loading required package: stringr
## Loading required package: data.table
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
## Loading required package: plyr
## Loading required package: dplyr
## 
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:plyr':
## 
##     arrange, desc, failwith, id, mutate, summarise
## 
## The following object is masked from 'package:data.table':
## 
##     last
## 
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
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
##   studentid      sasid        dob first_name last_name student_lang
## 1   4000014 1000085228 10/30/1988       RUTH   SANTANA      SPANISH
## 2   4000028 1000085169 09/21/1989      JAMES     REYES      SPANISH
## 3   4000045 1000084993 05/30/1988       JOHN  OCTAVIUS       FRENCH
## 4   4000053 1000119139 02/24/1988       JOEL    VALDEZ      SPANISH
## 5   4000064 1000118731 03/13/1988    YOHANNY   BENZANT      SPANISH
## 6   4000072 1000088531 02/09/1989   COURTNEY      LINK      ENGLISH
##   parent_lang      birth_place sex schoolyear
## 1     SPANISH     USA-NEW YORK   F  2005_2006
## 2     SPANISH   DOMINICAN REP.   M  2005_2006
## 3      FRENCH            HAITI   M  2005_2006
## 4     SPANISH USA-RHODE ISLAND   M  2005_2006
## 5     SPANISH   DOMINICAN REP.   F  2005_2006
## 6     ENGLISH USA-RHODE ISLAND   F  2005_2006
```

```
## Loading required package: car
```

```
## [1] "2006"
## [1] 638
##   studentid      sasid        dob first_name last_name student_lang
## 1   4000014 1000085228 10/30/1988       RUTH   SANTANA      SPANISH
## 2   4000028 1000085169 09/21/1989      JAMES     REYES      SPANISH
## 3   4000045 1000084993 05/30/1988       JOHN  OCTAVIUS       FRENCH
## 4   4000064 1000118731 03/13/1988    YOHANNY   BENZANT      SPANISH
## 5   4000089 1000084724 01/03/1989    LAMONTE  COPELAND      ENGLISH
## 6   4000097 1000131570 03/19/1989  JACQUELIN   VAZQUEZ      ENGLISH
##   parent_lang      birth_place sex schoolyear
## 1     SPANISH     USA-NEW YORK   F  2006_2007
## 2     SPANISH   DOMINICAN REP.   M  2006_2007
## 3      FRENCH            HAITI   M  2006_2007
## 4     SPANISH   DOMINICAN REP.   F  2006_2007
## 5     ENGLISH USA-RHODE ISLAND   M  2006_2007
## 6     ENGLISH USA-RHODE ISLAND   F  2006_2007
## [1] "2007"
## [1] 1289
##   studentid      sasid        dob first_name last_name student_lang
## 1   4000089 1000084724 01/03/1989    LAMONTE  COPELAND      ENGLISH
## 2   4000301 1000088554 11/14/1989     EDWARD    SANTOS      SPANISH
## 3   4000306 1000084665 06/23/1989     GRECIA VELASQUEZ      SPANISH
## 4   4000354 1000182217 07/18/1989       ERIC    VALDEZ      ENGLISH
## 5   4000360 1000116726 12/12/1989     TESHEL     WHITE      ENGLISH
## 7   4000450 1000131155 11/11/1989      BRIAN    POMPEY      ENGLISH
##   parent_lang      birth_place sex schoolyear
## 1     ENGLISH USA-RHODE ISLAND   M  2007_2008
## 2     SPANISH   DOMINICAN REP.   M  2007_2008
## 3     SPANISH USA-RHODE ISLAND   F  2007_2008
## 4     ENGLISH USA-RHODE ISLAND   M  2007_2008
## 5     ENGLISH USA-RHODE ISLAND   F  2007_2008
## 7     ENGLISH USA-RHODE ISLAND   M  2007_2008
## [1] "2008"
## [1] 1415
##   studentid      sasid        dob first_name last_name student_lang
## 2   4000484 1000085048 09/07/1990 JACQUELINE     LOPEZ      SPANISH
## 3   4000495 1000088566 09/09/1990      JAMON     BROWN      ENGLISH
## 4   4000496 1000084732 06/02/1990      RANDY    HOPPER      ENGLISH
## 5   4000504 1000078562 12/06/1990     MAYLEE    TORRES      ENGLISH
## 6   4000516 1000084548 11/19/1990    ANTHONY      TODD      ENGLISH
## 7   4000524 1000089144 12/14/1990    DARLENE    RAPOSO      PORTUGU
##   parent_lang      birth_place sex schoolyear
## 2     SPANISH USA-RHODE ISLAND   F  2008_2009
## 3     ENGLISH USA-RHODE ISLAND   M  2008_2009
## 4     ENGLISH USA-RHODE ISLAND   M  2008_2009
## 5     ENGLISH USA-RHODE ISLAND   F  2008_2009
## 6     ENGLISH USA-RHODE ISLAND   M  2008_2009
## 7     PORTUGU USA-RHODE ISLAND   F  2008_2009
## [1] "2009"
## [1] 1227
##   studentid      sasid        dob  first_name last_name student_lang
## 1   4000548 1000084873 08/30/1990      DANIEL  BERNARDO      ENGLISH
## 2   4000582 1000160311 08/28/1990      DWAYNE    MONROE      ENGLISH
## 3   4000624 1000096747 09/05/1990    COURTNEY    JULIUS      ENGLISH
## 4   4000648 1000078719 12/13/1990       JULIO    ALICEA      SPANISH
## 5   4000673 1000131207 03/29/1990      SANDRA    MORAIS      ENGLISH
## 6   4000693 1000241449 03/04/1990 CHRISTOPHER  OLIVEIRA      ENGLISH
##   parent_lang      birth_place sex schoolyear
## 1     ENGLISH USA-RHODE ISLAND   M  2009_2010
## 2     ENGLISH                    M  2009_2010
## 3     ENGLISH USA-RHODE ISLAND   F  2009_2010
## 4     SPANISH USA-RHODE ISLAND   M  2009_2010
## 5     ENGLISH USA-RHODE ISLAND   F  2009_2010
## 6  PORTUGUESE USA-RHODE ISLAND   M  2009_2010
```

```
## Warning: NAs introduced by coercion
```

```
## [1] "2010"
## [1] 732
##     studentid      sasid        dob first_name  last_name student_lang
## 674   8309005 1000002055 09/14/1996     STEVEN      TRAYA      ENGLISH
## 675   8309007 1000002056 08/25/1994      BUDDY      TRAYA      ENGLISH
## 676   8336263 1000005102 08/23/1992     PERRIN     HAWVER      ENGLISH
## 677   8326817 1000005852 12/22/1993     BRIANA     BRIGGS      ENGLISH
## 678   8344027 1000006156 02/03/1994      SUADE     ROGERS      ENGLISH
## 680   8287169 1000006310 11/09/1995       PAUL DE ANGELIS      ENGLISH
##     parent_lang      birth_place sex schoolyear
## 674     ENGLISH      PHILIPPINES   M  2010_2011
## 675     ENGLISH      PHILIPPINES   M  2010_2011
## 676     ENGLISH USA-RHODE ISLAND   M  2010_2011
## 677     ENGLISH USA-RHODE ISLAND   F  2010_2011
## 678     ENGLISH     USA-NEW YORK   M  2010_2011
## 680     ENGLISH USA-RHODE ISLAND   M  2010_2011
```

```
## Warning: invalid factor level, NA generated
## Warning: invalid factor level, NA generated
## Warning: invalid factor level, NA generated
## Warning: NAs introduced by coercion
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
## [1] "Race Complete"
## [1] "Sex Complete"
## [1] "dob Complete"
## [1] "Student_lang Complete"
## [1] "parent_lang Complete"
```

```
## Warning: invalid factor level, NA generated
## Warning: invalid factor level, NA generated
## Warning: invalid factor level, NA generated
```

```
## [1] "everELL Complete"
## [1] "everIEP Complete"
## [1] "first_hs Complete"
## [1] "last_hs Complete"
## [1] "long_hs Complete"
## [1] "Remove duplicate sasid"
```

```
## Joining by: "sasid"
```

```
## [1] "Join Race"
```

```
## Joining by: "sasid"
```

```
## [1] "Join Sex"
```

```
## Joining by: "sasid"
```

```
## [1] "Join Student_Lang"
```

```
## Joining by: "sasid"
```

```
## [1] "Join Parent_Lang"
```

```
## Joining by: "sasid"
```

```
## [1] "Join dob"
```

```
## Joining by: "sasid"
```

```
## [1] "Join everELL"
```

```
## Joining by: "sasid"
```

```
## [1] "Join everIEP"
```

```
## Joining by: "sasid"
```

```
## [1] "Join 1st HS"
```

```
## Joining by: "sasid"
```

```
## [1] "Join Long HS"
```

```
## Joining by: "sasid"
```

```
## [1] "Join Last HS"
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
source("attendance.R")
```

```
## Warning: invalid factor level, NA generated
## Warning: invalid factor level, NA generated
## Warning: invalid factor level, NA generated
```

```r
attendance$sasid <- as.character(attendance$sasid)

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
# Calculate mobility for students Eighth Grade year mobile8th <-
# moves_calc(subset(tables2006_2007$enrollment, sasid %in% hs0708$sasid))

# mobile8th$sasid <- as.character(mobile8th$sasid) hs0708 <-
# left_join(hs0708, mobile8th)

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
    gpa8th + reanormal + matnormal + I(ageHS < 190), data = hscohort, family = binomial(link = "logit"))
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```r
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
##     gpa8th + reanormal + matnormal + I(ageHS < 190), family = binomial(link = "logit"), 
##     data = hscohort)
## 
## Deviance Residuals: 
##    Min      1Q  Median      3Q     Max  
## -2.763  -0.742   0.457   0.731   3.026  
## 
## Coefficients:
##                    Estimate Std. Error z value Pr(>|z|)    
## (Intercept)         -2.0866     0.2868   -7.28  3.5e-13 ***
## attendnormal         1.0006     0.0763   13.11  < 2e-16 ***
## gpa8th               0.7826     0.0555   14.10  < 2e-16 ***
## reanormal            0.1098     0.0594    1.85   0.0645 .  
## matnormal            0.1886     0.0590    3.20   0.0014 ** 
## I(ageHS < 190)TRUE   1.0416     0.2600    4.01  6.2e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 4955.6  on 3936  degrees of freedom
## Residual deviance: 3797.8  on 3931  degrees of freedom
##   (2095 observations deleted due to missingness)
## AIC: 3810
## 
## Number of Fisher Scoring iterations: 5
```

```r
summary(starting_grade$predict8th)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##     0.0     0.5     0.7     0.7     0.9     1.0     522
```

Predictive models are traditionally evaluated on their ability to correctly classify, or sort, students so that predicted outcomes match the actual outcomes. Our model outputs a probability of graduating for each student that can take on any value between 0 and 1. In order to sort students into those who are at risk of failing to graduate and those who are not, we have to select threshold probabilities above which students are not considered at risk and below which they are.

Thresholds for predictive data are often set at the point with the highest *accuracy*. Accuracy is highest where the proportion of students who are correctly classified as graduating or not graduating is the greatest. Therefore, accuracy values *true positives*, identifying students who will graduate, the same as *true negatives*, identifying students who will not graduate. Beacuse we want to intervene on the behalf of students who are falling behind, accuracy alone may not be the best way to set thresholds. Instead, we want to maximize the *true negative rate*, ensuring we capture as many students who fail to graduate as possible, while maintaining the best accuracy possible. In practice, this means the best cutoff thresholds for high risk may have slightly less than the maximum accuracy.

The Rhode Island Department of Education (RIDE) has been developing an Early Warning System using statewide data. They have set thresholds for high, moderate, and low risk by examining the true negative rate and accuracy. The true negative rate for high, moderate, and low risk are 0.8, 0.7, and 0.6.


```r
cutoff_matrix(starting_grade, "predict8th", "graduated")
```

```
##     cutoff true_pos true_neg false_pos false_neg true_neg_rate
## 1     1.00        0      672         0       956        0.4128
## 2     0.99        0      672         0       956        0.4128
## 3     0.98        1      672         0       955        0.4130
## 4     0.97       19      671         1       937        0.4173
## 5     0.96       43      666         6       913        0.4218
## 6     0.95       77      664         8       879        0.4303
## 7     0.94      113      663         9       843        0.4402
## 8     0.93      160      661        11       796        0.4537
## 9     0.92      194      658        14       762        0.4634
## 10    0.91      234      653        19       722        0.4749
## 11    0.90      269      646        26       687        0.4846
## 12    0.89      293      639        33       663        0.4908
## 13    0.88      320      631        41       636        0.4980
## 14    0.87      356      626        46       600        0.5106
## 15    0.86      386      615        57       570        0.5190
## 16    0.85      416      601        71       540        0.5267
## 17    0.84      436      595        77       520        0.5336
## 18    0.83      458      584        88       498        0.5397
## 19    0.82      484      577        95       472        0.5500
## 20    0.81      506      565       107       450        0.5567
## 21    0.80      528      556       116       428        0.5650
## 22    0.79      554      547       125       402        0.5764
## 23    0.78      571      536       136       385        0.5820
## 24    0.77      588      531       141       368        0.5907
## 25    0.76      612      524       148       344        0.6037
## 26    0.75      622      519       153       334        0.6084
## 27    0.74      637      513       159       319        0.6166
## 28    0.73      646      503       169       310        0.6187
## 29    0.72      659      492       180       297        0.6236
## 30    0.71      675      480       192       281        0.6307
## 31    0.70      686      470       202       270        0.6351
## 32    0.69      703      459       213       253        0.6447
## 33    0.68      712      452       220       244        0.6494
## 34    0.67      722      446       226       234        0.6559
## 35    0.66      731      439       233       225        0.6611
## 36    0.65      747      428       244       209        0.6719
## 37    0.64      756      421       251       200        0.6779
## 38    0.63      772      413       259       184        0.6918
## 39    0.62      779      404       268       177        0.6954
## 40    0.61      783      395       277       173        0.6954
## 41    0.60      791      389       283       165        0.7022
## 42    0.59      801      381       291       155        0.7108
## 43    0.58      810      372       300       146        0.7181
## 44    0.57      817      364       308       139        0.7237
## 45    0.56      821      357       315       135        0.7256
## 46    0.55      826      352       320       130        0.7303
## 47    0.54      830      342       330       126        0.7308
## 48    0.53      833      335       337       123        0.7314
## 49    0.52      838      332       340       118        0.7378
## 50    0.51      847      325       347       109        0.7488
## 51    0.50      855      318       354       101        0.7589
## 52    0.49      858      310       362        98        0.7598
## 53    0.48      863      299       373        93        0.7628
## 54    0.47      869      291       381        87        0.7698
## 55    0.46      871      286       386        85        0.7709
## 56    0.45      876      282       390        80        0.7790
## 57    0.44      881      274       398        75        0.7851
## 58    0.43      883      268       404        73        0.7859
## 59    0.42      888      261       411        68        0.7933
## 60    0.41      890      255       417        66        0.7944
## 61    0.40      893      245       427        63        0.7955
## 62    0.39      899      237       435        57        0.8061
## 63    0.38      904      234       438        52        0.8182
## 64    0.37      910      228       444        46        0.8321
## 65    0.36      913      218       454        43        0.8352
## 66    0.35      915      210       462        41        0.8367
## 67    0.34      916      204       468        40        0.8361
## 68    0.33      921      199       473        35        0.8504
## 69    0.32      925      195       477        31        0.8628
## 70    0.31      926      187       485        30        0.8618
## 71    0.30      929      179       493        27        0.8689
## 72    0.29      931      173       499        25        0.8737
## 73    0.28      933      167       505        23        0.8789
## 74    0.27      935      161       511        21        0.8846
## 75    0.26      935      159       513        21        0.8833
## 76    0.25      938      151       521        18        0.8935
## 77    0.24      942      145       527        14        0.9119
## 78    0.23      942      142       530        14        0.9103
## 79    0.22      944      138       534        12        0.9200
## 80    0.21      945      127       545        11        0.9203
## 81    0.20      946      119       553        10        0.9225
## 82    0.19      948      114       558         8        0.9344
## 83    0.18      948      109       563         8        0.9316
## 84    0.17      948      103       569         8        0.9279
## 85    0.16      950      101       571         6        0.9439
## 86    0.15      950       97       575         6        0.9417
## 87    0.14      951       93       579         5        0.9490
## 88    0.13      951       90       582         5        0.9474
## 89    0.12      952       85       587         4        0.9551
## 90    0.11      952       78       594         4        0.9512
## 91    0.10      952       70       602         4        0.9459
## 92    0.09      952       63       609         4        0.9403
## 93    0.08      952       54       618         4        0.9310
## 94    0.07      953       48       624         3        0.9412
## 95    0.06      954       38       634         2        0.9500
## 96    0.05      955       35       637         1        0.9722
## 97    0.04      955       27       645         1        0.9643
## 98    0.03      955       18       654         1        0.9474
## 99    0.02      955       11       661         1        0.9167
## 100   0.01      956        2       670         0        1.0000
## 101   0.00      956        0       672         0           NaN
##     true_pos_rate false_pos_rate false_neg_rate accuracy
## 1             NaN            NaN        0.58722   0.4128
## 2             NaN            NaN        0.58722   0.4128
## 3          1.0000        0.00000        0.58697   0.4134
## 4          0.9500        0.05000        0.58271   0.4238
## 5          0.8776        0.12245        0.57821   0.4355
## 6          0.9059        0.09412        0.56967   0.4552
## 7          0.9262        0.07377        0.55976   0.4767
## 8          0.9357        0.06433        0.54633   0.5043
## 9          0.9327        0.06731        0.53662   0.5233
## 10         0.9249        0.07510        0.52509   0.5448
## 11         0.9119        0.08814        0.51538   0.5620
## 12         0.8988        0.10123        0.50922   0.5725
## 13         0.8864        0.11357        0.50197   0.5842
## 14         0.8856        0.11443        0.48940   0.6032
## 15         0.8713        0.12867        0.48101   0.6149
## 16         0.8542        0.14579        0.47327   0.6247
## 17         0.8499        0.15010        0.46637   0.6333
## 18         0.8388        0.16117        0.46026   0.6400
## 19         0.8359        0.16408        0.44995   0.6517
## 20         0.8254        0.17455        0.44335   0.6579
## 21         0.8199        0.18012        0.43496   0.6658
## 22         0.8159        0.18409        0.42360   0.6763
## 23         0.8076        0.19236        0.41802   0.6800
## 24         0.8066        0.19342        0.40934   0.6873
## 25         0.8053        0.19474        0.39631   0.6978
## 26         0.8026        0.19742        0.39156   0.7009
## 27         0.8003        0.19975        0.38341   0.7064
## 28         0.7926        0.20736        0.38130   0.7058
## 29         0.7855        0.21454        0.37643   0.7070
## 30         0.7785        0.22145        0.36925   0.7095
## 31         0.7725        0.22748        0.36486   0.7101
## 32         0.7675        0.23253        0.35534   0.7138
## 33         0.7639        0.23605        0.35057   0.7150
## 34         0.7616        0.23840        0.34412   0.7174
## 35         0.7583        0.24170        0.33886   0.7187
## 36         0.7538        0.24622        0.32810   0.7217
## 37         0.7507        0.24926        0.32206   0.7230
## 38         0.7488        0.25121        0.30821   0.7279
## 39         0.7440        0.25597        0.30465   0.7267
## 40         0.7387        0.26132        0.30458   0.7236
## 41         0.7365        0.26350        0.29783   0.7248
## 42         0.7335        0.26648        0.28918   0.7260
## 43         0.7297        0.27027        0.28185   0.7260
## 44         0.7262        0.27378        0.27634   0.7254
## 45         0.7227        0.27729        0.27439   0.7236
## 46         0.7208        0.27923        0.26971   0.7236
## 47         0.7155        0.28448        0.26923   0.7199
## 48         0.7120        0.28803        0.26856   0.7174
## 49         0.7114        0.28862        0.26222   0.7187
## 50         0.7094        0.29062        0.25115   0.7199
## 51         0.7072        0.29280        0.24105   0.7205
## 52         0.7033        0.29672        0.24020   0.7174
## 53         0.6982        0.30178        0.23724   0.7138
## 54         0.6952        0.30480        0.23016   0.7125
## 55         0.6929        0.30708        0.22911   0.7107
## 56         0.6919        0.30806        0.22099   0.7113
## 57         0.6888        0.31118        0.21490   0.7095
## 58         0.6861        0.31391        0.21408   0.7070
## 59         0.6836        0.31640        0.20669   0.7058
## 60         0.6809        0.31905        0.20561   0.7033
## 61         0.6765        0.32348        0.20455   0.6990
## 62         0.6739        0.32609        0.19388   0.6978
## 63         0.6736        0.32638        0.18182   0.6990
## 64         0.6721        0.32792        0.16788   0.6990
## 65         0.6679        0.33211        0.16475   0.6947
## 66         0.6645        0.33551        0.16335   0.6910
## 67         0.6618        0.33815        0.16393   0.6880
## 68         0.6607        0.33931        0.14957   0.6880
## 69         0.6598        0.34023        0.13717   0.6880
## 70         0.6563        0.34373        0.13825   0.6837
## 71         0.6533        0.34669        0.13107   0.6806
## 72         0.6510        0.34895        0.12626   0.6781
## 73         0.6488        0.35118        0.12105   0.6757
## 74         0.6466        0.35339        0.11538   0.6732
## 75         0.6457        0.35428        0.11667   0.6720
## 76         0.6429        0.35709        0.10651   0.6689
## 77         0.6413        0.35875        0.08805   0.6677
## 78         0.6399        0.36005        0.08974   0.6658
## 79         0.6387        0.36130        0.08000   0.6646
## 80         0.6342        0.36577        0.07971   0.6585
## 81         0.6311        0.36891        0.07752   0.6542
## 82         0.6295        0.37052        0.06557   0.6523
## 83         0.6274        0.37260        0.06838   0.6493
## 84         0.6249        0.37508        0.07207   0.6456
## 85         0.6246        0.37541        0.05607   0.6456
## 86         0.6230        0.37705        0.05825   0.6431
## 87         0.6216        0.37843        0.05102   0.6413
## 88         0.6204        0.37965        0.05263   0.6394
## 89         0.6186        0.38142        0.04494   0.6370
## 90         0.6158        0.38422        0.04878   0.6327
## 91         0.6126        0.38739        0.05405   0.6278
## 92         0.6099        0.39013        0.05970   0.6235
## 93         0.6064        0.39363        0.06897   0.6179
## 94         0.6043        0.39569        0.05882   0.6149
## 95         0.6008        0.39924        0.05000   0.6093
## 96         0.5999        0.40013        0.02778   0.6081
## 97         0.5969        0.40313        0.03571   0.6032
## 98         0.5935        0.40646        0.05263   0.5977
## 99         0.5910        0.40903        0.08333   0.5934
## 100        0.5879        0.41205        0.00000   0.5885
## 101        0.5872        0.41278            NaN   0.5872
```


In examining the results of our predictive model, we find that at a true negative rate of 0.8, the accuracy of our classifier is 71%. The maximum accuracy is approximately 73.5%, at which the true negative rate is 0.69. Therefore, we feel our data supports using the 0.8 threshold for high risk because the modest tradeoff in accuracy is more than made up by the increase in true negative rate.

There are two key ways to compare the model developed in this work to the RIDE Early Warning System aggregate indicator. The first measure is *coverage*, or what proportion of total drop outs are identified by the measure. For the same reasons we are maximizing the *true negative rate*, we want to be sure that our risk thresholds identify as many of the students who ultimately fail to graduate as possible. Our model captures 37% of all drop outs in the 8th grade, a consderable improvement on the state's model which captures fewer than 20% of all drop outs. The state's model, however, is slightly more accurate, at approximately 80% versus 71%. We believe this model represents a better tradeoff between accuracy and coverage. Many more students would be incorrectly identified as in need of supports under the state model in order to capture 37% of all drop outs.
