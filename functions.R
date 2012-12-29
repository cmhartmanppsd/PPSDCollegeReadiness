clean_schoolyear <- function(regYr, year){
  # Remove the appended year values and add an attribute for the school year
  # so the file is "long" when merged rather than wide.
  
  # rm most appended yrs.
  names(regYr) <- gsub(paste('_',
                             substr(as.character(year-1),3,4), 
                             substr(as.character(year),3,4),
                             sep=''), '', names(regYr))
  ########
  # Need to work on safely removing other year-appended values. May be able to
  # just remove the leading _ from the gsub above but needs more rigorous
  # testing.
  ########
  regYr$schoolyear <- paste(year-1, year, sep='_')
  return(regYr)
}

missingSASID <- function(regYr){
	# Collect students with no SASID. Because all of these records are blank, this
	# data.frame will be used to look for data on these students in future years
	# to determine if these IDs are included completely in error or represent an
	# error in previous data cleaning resulting in missing data that will need to
	# be recovered from the source system.
	return(subset(regYr, is.na(regYr$sasid)==TRUE, 
								select=c('studentid', 'schoolyear')))
}

removeNoSASID <- function(regYr){
	# Remove those without SASID because they are all blank records.
	count_i <- nrow(regYr)
	# Count the number of students that are removed this way.
	regYr <- subset(regYr, is.na(regYr$sasid)==FALSE & 
												 is.na(regYr$schno_Sch1)==FALSE)
	recordsRemoved <- count_i - nrow(regYr)
	print(recordsRemoved)
	return(regYr)
}

duplicateSASID <- function(regYr){
  duplicates <- subset(regYr, 
                       regYr$sasid %in% subset(regYr$sasid, 
                                               duplicated(regYr$sasid) &
                                               is.na(regYr$sasid)==FALSE),
                       select = c('studentid', 'sasid', 'schoolyear'))
	return(duplicates)
}

extract_person <- function(regYr){
	# This extracts the person table elements and cleans the data types
	# Input: MASTER Reg file that has been passed through clean_schoolyear
	# Output: tbl_person attributes with the right types but no other data
	# cleaning.
	person_attributes <- c('studentid', 'sasid', 'dob', 'first_name', 'last_name', 
												 'student_lang', 'parent_lang', 'birth_place', 'sex', 
                         'schoolyear')
	tbl_person <- regYr[, person_attributes]	
	tbl_person[,-3] <- apply(apply(regYr[, person_attributes[-3]], 2, 
                                 as.character),
											2, str_trim, side='both')
  tbl_person <- merge(tbl_person, regYr[,c('studentid','sasid','race')])
	if(class(tbl_person$race)=='factor'){
		levels(tbl_person$race) <- str_trim(levels(tbl_person$race), side='both')
	}
	tbl_person$race <- factor(tbl_person$race, labels = c('Asian', 'Black', 
																												'Hispanic',
													 															'Native American', 
																												'White'))
	tbl_person$sex <- ifelse(!tbl_person$sex %in% c('F','M'), NA, tbl_person$sex)	
	tbl_person$sex <- factor(tbl_person$sex, labels = c('Female', 'Male'))
	if(!class(tbl_person$dob) %in% c('POSIXct', 'POSIXlt', 'date', 'datetime')){
    tbl_person$dob <- as.Date(tbl_person$dob, '%m/%d/%Y')
	}
  return(tbl_person)
}

extract_person_annual <- function(regYr){
	# This extracts the person annual table elements and clean data types
	# Inputs: MASTER Reg file that has been passed through clean_schoolyear
	# Output: tbl_person_annual attributes wiht the right types but no other
	# 				data cleaning.
	person_annual_attributes <- c('studentid', 'sasid', 'last_name', 'grade', 
																'lunch', 'lep', 'iep', 'plp', 'sec504', 
																'graduated', 'willrepeatgr', 'isrepeatinggr',
																'disab', 'spedprogrm', 'str_name', 'apt_no', 
																'city', 'state', 'zip_code', 'sum_absent',
                                'sum_excabsent', 'sum_tardy', 'sum_on_time',
                                'sum_suspend', 'sum_enrolled', 'sum_present',
                                'sum_soccurin', 'sum_soccurout')
	tbl_person_annual <- regYr[, person_annual_attributes]
	tbl_person_annual[,1:3] <- apply(apply(tbl_person_annual[,1:3], 2, 
																				 as.character), 
																	 2, str_trim, side='both')
	tbl_person_annual$grade <- ifelse(tbl_person_annual$grade==0, NA, 
                                    tbl_person_annual$grade)
  tbl_person_annual$grade <- factor(tbl_person_annual$grade, 
																		labels = c(1,2,3,4,5,6,7,8,9,10,11,12,
			 																				 'Full-day K', 
																							 'Full-day Pre-K',
																							 'Morning Pre-K', 
																							 'Afternoon Pre-K'))
	tbl_person_annual$lunch <- str_trim(as.character(tbl_person_annual$lunch), 
																			side='both')
	tbl_person_annual$lunch <- with(tbl_person_annual, 
																	ifelse(lunch=='F', 'Free Lunch', 															
																		ifelse(lunch=='R', 'Reduced Lunch', 																					 
																					 ifelse(lunch=='P','Paid Lunch',
																						 			NA))))
	tbl_person_annual$lunch <- factor(tbl_person_annual$lunch)
	levels(tbl_person_annual$disab) <- gsub('[[:punct:]]|[\\|]| ', '', 
																	        levels(tbl_person_annual$disab))
  levels(tbl_person_annual$disab) <- gsub('NE|^[B-Z]$', '', 
                                          levels(tbl_person_annual$disab))
	# Play with a tryCatch here to add NE to those that need it, and if nothing
  # works to just exit gracefully without factorizing the disability category.
  tbl_person_annual$disab <- factor(factor(tbl_person_annual$disab, 
																		labels = c('None', 'Autism', 
																							 'Developmental Delay', 
																							 'Emotional Disturbance', 
																							 'Hearing Impairment',
																							 'Learning Disabled', 
																							 'Multiple Disabilities',
																							 'Mental Retardation', 
																							 'OI', 
																							 'Other Hearing Impairment', 
																							 'Speech/Language',
																							 'Speech Only', 
																							 'Traumatic Brain Injury',
																							 'Visual Impairment')))
	tbl_person_annual[, c('str_name', 'apt_no', 'city', 'state')] <- 
		apply(apply(tbl_person_annual[, c('str_name', 'apt_no', 'city', 'state')],
								2, as.character), 2, str_trim, side='both')
	tbl_person_annual$zip_code <- as.factor(sprintf('%05i', 
																									tbl_person_annual$zip_code))
  return(tbl_person_annual)
}

extract_enrollment <- function(regYr){
  id_attributes <-  c('studentid', 'sasid', 'schoolyear', 'last_name', 'grade')
  enr_attributes <- grep('^(schno|school|exit|enroll|adm|ada|description)', 
                         names(regYr), value=TRUE)
  tbl_stud_enroll <- regYr[,c(id_attributes, enr_attributes)] 
  by_school <- list(mode='any', length=length(enr_attributes)/8)
  for(i in 1:length(by_school)){
    tempSch <- tbl_stud_enroll[, c(id_attributes, 
                                   grep(paste('*_Sch', i, '$', sep=''), 
                                        names(tbl_stud_enroll), 
                                        value=TRUE))]
    names(tempSch) <- gsub(paste('*_Sch', i, '$', sep=''),'', names(tempSch))
    by_school[[i]] <- tempSch
  }
  tbl_stud_enroll <- do.call(rbind, by_school)
  tbl_stud_enroll <- subset(tbl_stud_enroll, !is.na(schno))
  tbl_stud_enroll[grep('[[:punct:]]', tbl_stud_enroll$exit_date),'exit_date'] <- 
    difftime(as.Date(tbl_stud_enroll[grep('[[:punct:]]', 
                                          tbl_stud_enroll$exit_date), 
                                    'exit_date'], format="%m/%d/%Y"), 
             "1582-10-14", units='secs')
  tbl_stud_enroll$exit_date <- as.double(tbl_stud_enroll$exit_date)
  tbl_stud_enroll[, c('enroll_date', 'exit_date')] <- 
    lapply(lapply(tbl_stud_enroll[, c('enroll_date', 'exit_date')], as.POSIXct, 
                  origin='1582/10/14'), # origin for SPSS dates
           as.Date, format='%Y-%m-%d')
  row.names(tbl_stud_enroll) <- NULL
	return(tbl_stud_enroll)
}

build_tables <- function(regYr, year){
  if(exists('executedAt')==TRUE){
    logAt <- paste('logs/', executedAt, 'build.log', sep='')
  }else{
    logAt <- paste('logs/', paste(Sys.time()), 'build.log', sep='')
  }
	cat(paste(Sys.time(), '\n\n'), file=logAt, sep='\n', append=TRUE)
	regYr <- clean_schoolyear(regYr, year)
	buildLog <- capture.output(head(regYr)[1:10])
	cat('clean_schoolyear', year, buildLog, '\n\n', file=logAt, sep='\n', 
			append=TRUE)
	noSASID <- missingSASID(regYr)
	buildLog <- capture.output(head(noSASID))
	cat('noSASID', year, buildLog, '\n\n', file=logAt, sep='\n', 
			append=TRUE)
	regYr <- removeNoSASID(regYr)
	buildLog <- capture.output(head(regYr)[1:10])
	cat('removeNoSASID', year, buildLog, '\n\n', file=logAt, sep='\n',
			append=TRUE)
	duplicates <- duplicateSASID(regYr)
	buildLog <- capture.output(head(duplicates))
	cat('duplicates', year, buildLog, '\n\n', file=logAt, sep='\n', 
			append=TRUE)
	tbl_person <- extract_person(regYr)
	buildLog <- capture.output(head(tbl_person)[1:10])
	cat('tbl_person', year, buildLog, '\n\n', file=logAt, sep='\n', 
			append=TRUE)
	tbl_person_annual <- extract_person_annual(regYr)
	buildLog <- capture.output(head(tbl_person_annual)[1:10])
	cat('tbl_person_annual', year, buildLog, '\n\n', file=logAt, sep='\n', 
			append=TRUE)
	tbl_enrollment <- extract_enrollment(regYr)
	buildLog <- capture.output(head(tbl_enrollment)[1:10])
	cat('tbl_enrollment', buildLog, year, '\n\n', file=logAt, sep='\n', 
			append=TRUE)
	tables <- list(person=tbl_person, person_annual=tbl_person_annual, 
                 enrollment=tbl_enrollment, missingSASID=noSASID, 
								 dupes=duplicates)
	return(tables)
}
