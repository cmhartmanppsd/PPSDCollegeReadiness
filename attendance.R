# Calculate attendance table for all students
attendance <- rbind(tables2005_2006$person_annual, 
                   tables2006_2007$person_annual, 
                   tables2007_2008$person_annual, 
                   tables2008_2009$person_annual,
                   tables2009_2010$person_annual, 
                   tables2010_2011$person_annual,
                   tables2011_2012$person_annual,
                   tables2012_2013$person_annual) %.%
              mutate(attendance = sum_present/sum_enrolled,
                     tardy = sum_tardy/sum_enrolled,
                     suspended = sum_suspend) %.%
              select(sasid, schoolyear, attendance, tardy, suspended)
