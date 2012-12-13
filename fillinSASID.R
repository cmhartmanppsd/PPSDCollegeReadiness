# Fill in missing SASIDs where available in future datasets

# Scrap to resolve duplicates
tables2004_2005$missingSASID <- merge(tables2004_2005$missingSASID, 
                                      tables2005_2006$person[c('sasid', 
                                                               'studentid')],
                                      by='studentid', all.x=TRUE)
merge(subset(tables2004_2005$missingSASID, is.na(tables2004_2005$missingSASID$sasid)),
      tables2005_2006$person[c('sasid','studentid')], by='studentid')

dupetest <- (merge(merge(tables2004_2005$dupes, 
      tables2005_2006$person[c('studentid','sasid','schoolyear')], 
      by=c('studentid','sasid'), all.x=TRUE), 
      tables2006_2007$person[c('studentid', 'sasid','schoolyear')],
      by=c('studentid','sasid'), all.x=TRUE)

arrange(merge(merge(tables2006_2007$dupes, 
                    tables2004_2005$person[c('studentid','sasid','schoolyear')], 
                    by=c('studentid','sasid'), all.x=TRUE), 
              tables2005_2006$person[c('studentid', 'sasid','schoolyear')],
              by=c('studentid','sasid'), all.x=TRUE), sasid)
             
#Slight changes