# 11th Grade Graduation Rates
eleventh_gr <- rbind(subset(tables2011_2012$person_annual, 
                            isrepeatinggr!='Y' & grade==11,
                            select = c('sasid', 'grade', 'schoolyear')),
                     subset(tables2010_2011$person_annual, 
                            isrepeatinggr!='Y' & grade==11,
                            select = c('sasid', 'grade', 'schoolyear')),
                     subset(tables2009_2010$person_annual, 
                            isrepeatinggr!='Y' & grade==11,
                            select = c('sasid', 'grade', 'schoolyear')),
                     subset(tables2008_2009$person_annual, 
                            isrepeatinggr!='Y' & grade==11,
                            select = c('sasid', 'grade', 'schoolyear')),
                     subset(tables2007_2008$person_annual, 
                            isrepeatinggr!='Y' & grade==11,
                            select = c('sasid', 'grade', 'schoolyear')),
                     subset(tables2006_2007$person_annual, 
                            isrepeatinggr!='Y' & grade==11,
                            select = c('sasid', 'grade', 'schoolyear')),
                     subset(tables2005_2006$person_annual, 
                            isrepeatinggr!='Y' & grade==11,
                            select = c('sasid', 'grade', 'schoolyear')))



prop.table(table(merge(eleventh_gr, person[,c('sasid','graduated')])$graduated))
