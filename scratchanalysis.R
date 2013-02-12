eight2005 <- merge(tables2005_2006$person[,c('sasid', 'dob', 'sex', 
                                             'schoolyear', 'race')],
                   tables2005_2006$person_annual[,c('sasid', 'grade', 'lunch', 
                                                    'lep', 'iep', 
                                                    'willrepeatgr', 
                                                    'isrepeatinggr', 
                                                    'sum_tardy', 'sum_present',
                                                    'sum_enrolled' , 
                                                    'sum_suspend')])

eight2005 <- merge(eight2005, tables2005_2006$achievement[, c('sasid', 
                                                              'reanormal',
                                                              'matnormal')])
eight2005 <- subset(eight2005, grade==8)
eight2005 <- merge(eight2005, subset(tables2006_2007$enrollment, grade==9), 
                   by=c('sasid'), all.y=TRUE)
eight2005 <- subset(eight2005, !is.na(eight2005$schoolyear.x))
eight2005 <- arrange(eight2005, order(sasid, -adm))
eigth2005$dupe <- duplicated(eight2005$sasid)

eight2005 <- eight2005[!duplicated(eight2005$sasid),]
eight2005 <- mutate(eight2005, perc_attnd = sum_present/sum_enrolled) 


ninth2006 <- subset(tables2006_2007$enrollment, grade==9)
ninth2006 <- merge(ninth2006, tables2005_2006$achievement[, c('sasid', 
                                                              'reanormal', 
                                                              'matnormal')])

mattiles <- quantile(ninth2006$matnormal, c(.25, .5, .75), na.rm=TRUE)

ninth2006$mattile <- with(ninth2006, 
                          ifelse(matnormal <= mattiles[1], 1,
                             ifelse(matnormal <= mattiles[2] & 
                                    matnormal > mattiles[1], 2,
                                    ifelse(matnormal <= mattiles[3] & 
                                           matnormal > mattiles[2], 3, 4))))

ggplot(subset(melt(prop.table(table(ninth2006$school, ninth2006$mattile),1)),!is.na(value)),
       aes(factor(Var2), value)) + geom_boxplot()
ggplot(data=subset(ninth2006, schno==123 | schno==117), aes(reanormal, fill=school)) + 
  geom_density(alpha=.2)

##############
### Needs Logic to recombine but otherwise correct.
tmp_person <- rbind(tables2005_2006$person, tables2006_2007$person, 
                tables2007_2008$person, tables2008_2009$person,
                tables2009_2010$person)
tbl_person <- tables2010_2011$person
tmp_person <- subset(tmp_person, sasid %in% (sasid %w/o% tbl_person$sasid))

test <- do.call(rbind, tapply(as.character(tmp_person$race), tmp_person$sasid, 
                              function(x) data.frame(race=rle(x)$values, 
                                                     counts=rle(x)$lengths)))
test$sasid <- row.names(test)
row.names(test) <- NULL
test$sasid <- gsub('[.][0-9]$','', test$sasid)
test <- ddply(test, 'sasid', function(test) subset(test, counts==max(counts)))
head(subset(test, sasid %in% subset(test$sasid, duplicated(sasid))))

tmp_test <- subset(test, sasid %in% subset(test$sasid, duplicated(sasid)))
tmp_test_year <- aggregate(schoolyear~sasid+race, data=tmp_person, max)
corrected_race <- subset(arrange(merge(tmp_test, tmp_test_year, 
                                       all.x=TRUE, all.y=FALSE), 
                                 sasid, desc(schoolyear)), 
                         !duplicated(sasid))[,-3]
### First time in 9th grade
tmp_enroll <- rbind(tables2005_2006$enroll, tables2006_2007$enroll, 
                    tables2007_2008$enroll, tables2008_2009$enroll,
                    tables2009_2010$enroll, tables2010_2011$enroll)
first9th <- aggregate(schoolyear~sasid, data=subset(tmp_enroll, grade==9), min)
names(first9th)[2] <- 'first9th'
tmp_enroll$graduated <- with(tmp_enroll, ifelse(exit_type==15, 'Y','N'))
tmp_enroll$grad_date <- with(tmp_enroll, ifelse(graduated=='Y', exit_date, NA))
tmp_enroll$grad_date <- as.Date(tmp_enroll$grad_date, origin='1970-01-01')

##############

#nsc_test <- read.csv('/Volumes/ProvidenceFiles/NSC Data Nov. 2012.csv')
################

multiRace <- subset(test, grepl('[.]', test$sasid))
#duplicated removes the trues to get the first observation

## Grades

