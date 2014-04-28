# Person Table

all_years <- rbind(tables2005_2006$person, 
                   tables2006_2007$person, 
                   tables2007_2008$person,
                   tables2008_2009$person, 
                   tables2009_2010$person,
                   tables2010_2011$person,
                   tables2011_2012$person,
                   tables2012_2013$person)

# Demographics

# Race has been identified two ways in the data. In 2010-2011 additional
# racial categories became available. For this reason, newer indicators of race
# take precedent over past identifiers.

# Select the most often reported race in the earlier period. If there are more
# than one mode, then the most recently reported modal value is selected.
early_race <- modal_person_attribute(rbind(tables2005_2006$person, 
                                           tables2006_2007$person, 
                                           tables2007_2008$person,
                                           tables2008_2009$person, 
                                           tables2009_2010$person),
                                     sid = 'sasid',
                                     attribute = 'race',
                                     year = 'schoolyear')

# Select the most often reported race in the later period. Although there is one
# year in the current data, this will be helpful as additional years of data are
# added.
late_race <- modal_person_attribute(rbind(tables2010_2011$person,
                                          tables2011_2012$person,
                                          tables2012_2013$person),
                                    sid = 'sasid',
                                    attribute = 'race',
                                    year = 'schoolyear')

# Set dates/year for the early and late period modal races.
early_race$schoolyear <- '2009_2010'
late_race$schoolyear <- '2010_2011'

# Select the most recent modal attribute. Since there is one record for race in
# each period, this has the same effect of choosing the late_race value if there
# is one for a student and the early_race value is there is no late_race value.
# This may be slower (CPU time) than other methods, but is done because it
# reuses existing code.
race <- modal_person_attribute(rbind(early_race, late_race), 
                               sid = 'sasid',
                               attribute = 'race',
                               year = 'schoolyear')
# Select the most often reported sex. 
sex <- modal_person_attribute(all_years, 
                              sid = 'sasid',
                              attribute = 'sex',
                              year = 'schoolyear')
# Select the most often reported DOB
dob <- modal_person_attribute(all_years, 
                              sid = 'sasid', 
                              attribute = 'dob', 
                              year = 'schoolyear')
# Select the most often reported student_language
student_lang <- modal_person_attribute(all_years, 
                                       sid = 'sasid',
                                       attribute = 'student_lang',
                                       year = 'schoolyear')
# Select the most often reported parent_language
parent_lang <- modal_person_attribute(all_years, 
                                      sid = 'sasid',
                                      attribute = 'parent_lang',
                                      year = 'schoolyear')
# Ever FRPL/LEP/SPED
# In these three categories, the business rule is not to select the most recent
# modal value. Rather, we report a student as having been FRPL-eligible, LEP, or
# SPED if they have, at any point in the observed time period, been reported as
# such.

# Build a single table with person_annual characteristics
annual_demos <- rbind(tables2005_2006$person_annual, 
                      tables2006_2007$person_annual, 
                      tables2007_2008$person_annual,
                      tables2008_2009$person_annual, 
                      tables2009_2010$person_annual,
                      tables2011_2012$person_annual,
                      tables2012_2013$person_annual)

# Select all instances where there was an LEP status of Y and remove duplicate
# student records for when they were reported as LEP in multiple years.
everELL <- subset(annual_demos, lep=='Y', select=c('sasid', 'lep'))
everELL <- subset(everELL, !duplicated(sasid))
# Select all instances where there was an IEP status of Y and remove duplicate
# student records for when they were reported as IEP in multiple years.
everIEP <- subset(annual_demos, iep=='Y', select=c('sasid', 'iep'))
everIEP <- subset(everIEP, !duplicated(sasid))
# HS of Record Calculations
# Calculate three different high school types that students might be assigned to
# as the high school of record. The first high school ever attended, the high
# school attended the longest, and the final high school attended.

# Select the first high school attended and add _first to variables associated
# with it.
all_years_enr <- rbind(tables2005_2006$enrollment, 
                       tables2006_2007$enrollment, 
                       tables2007_2008$enrollment, 
                       tables2008_2009$enrollment, 
                       tables2009_2010$enrollment, 
                       tables2010_2011$enrollment,
                       tables2011_2012$enrollment,
                       tables2012_2013$enrollment)
all_years_enr$sasid <- as.character(all_years_enr$sasid)
all_years_enr$school <- str_trim(all_years_enr$school, side='both')
first_hs <- select_hs(all_years_enr, 'first')
# Select the last high school attended with same process as the first.
last_hs <- select_hs(all_years_enr, 'last')
# Select the longest attended high school with same process as the others.
# The select_hs() function looks for the greatest sum of days enrolled to find
# the longest high school attended.
long_hs <- select_hs(all_years_enr, 'long')

# Build the person table, with canonical values for each of the above.
# IDs across all observed years
person <- all_years[,c('sasid', 'studentid')]
# Remove duplicated SASIDs to ensure uniqueness by ID
person <- filter(person, !duplicated(sasid))
# Merge the calculated cannonical demographics
person <- left_join(person, race)
person <- left_join(person, sex)
person <- left_join(person, student_lang)
person <- left_join(person, parent_lang)
person <- left_join(person, dob)
# Comparison level for race should be White, not Asian (default alphabetical)
person$race <- relevel(person$race, ref='White')


# Because the "ever" demographics are a list of all SASIDs were characteristic
# is =='Y', all.x is required to keep those students who don't have the trait.
# Then, all of the coerced NAs in the trait column will be assigned N. This has
# the effect of assuming that all NAs in the original data are Ns.
person <- left_join(person, everELL)
person$lep <- as.character(person$lep)
person$lep <- with(person, ifelse(is.na(lep)==TRUE, 'N', lep))
person$lep <- factor(person$lep, levels=c('N','Y'), ordered=TRUE)
person <- left_join(person, everIEP)
person$iep <- as.character(person$iep)
person$iep <- with(person, ifelse(is.na(iep)==TRUE, 'N', iep))
person$iep <- factor(person$iep, levels=c('N','Y'), ordered=TRUE)

# HS enrollment data is similar to the "ever" category where some students may
# not have data because they haven't enrolled in HS in the period given. The
# merges are done with all.x=TRUE and students who have NAs for the first_hs
# values should all have not attended high school at any time between the
# 2005_2006 school year and 2010_2011
person <- left_join(person, first_hs)
person <- left_join(person, long_hs)
person <- left_join(person, last_hs)
# There are late submissions that have not traditionally been made to the data
# office where students graudate after the end of the school year. This is an
# updated list of sasids and exit dates for students who actually graduated.
# Virtually no students graduated past the 6/24/11 date of expected graduation,
# even though it has been 2 years. Therefore, I am only focusing on updating the
# outcomes for these additional graduates.

update_grads <- read.csv("/Volumes/ProvidenceFiles/updatedexitcodes/updates.csv")
update_grads$exit_date <- as.Date(as.character(update_grads$exit_date), 
                                  format='%m/%d/%y')
update_grads$sasid <- as.character(update_grads$sasid)


# Calcuate high school outcomes, using exit_type_last to determine which
# students who have entered high school met one of six criteria: graduated,
# still enrolled, transferred out (of the district), transfered into a GED
# program, dropped out of schoool, or "disappeared" (unknown enrollment or
# completion status).
person$exit_type_last <- ifelse(person$sasid %in% update_grads$sasid, 
                                15, person$exit_type_last)
person$graduated <- with(person, 
						 ifelse(exit_type_last==15, 'Y','N'))
person$still_enrl <- with(person, 
						  ifelse(exit_type_last %in% c(31, 30), 'Y', 'N'))
person$transfer_out <- with(person, 
							ifelse(exit_type_last %in% c(seq(1, 14, 1), 17), 
								   'Y', 'N'))
person$ged <- with(person, ifelse(exit_type_last==23, 'Y', 'N'))
person$dropout <- with(person, 
					   ifelse(exit_type_last %in% c(20, 21, 25), 'Y', 'N'))
person$disappear <- with(person, ifelse(exit_type_last==97, 'Y', 'N'))

# Label which students repeat 9th grade.
# repeat9th <- subset(tables2007_2008$person_annual, 
# 					grade==9 & isrepeatinggr=='N')[, c('sasid', 'willrepeatgr')]
# levels(repeat9th$willrepeatgr) <- c(NA, 'N', 'Y')
# person <- merge(person, repeat9th, all.x=TRUE)					

# Remove tables that won't be used in later analysis
rm(race)
rm(sex)
rm(student_lang)
rm(parent_lang)
rm(dob)
rm(everELL)
rm(everIEP)
rm(early_race)
rm(late_race)
rm(first_hs)
rm(long_hs)
rm(last_hs)
# rm(repeat9th)
