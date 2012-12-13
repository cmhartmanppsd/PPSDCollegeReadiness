# ToDo
## Purpose
This file contains both a list of ToDos and scratch thoughts/notes about next steps.
## Take Action

## Scratch Ideas
### Duplicates
The sequencing of student duplicate resolution poses several challenges to an idealized automated process. In the interest of avoiding as much interactive use of R for the data cleaning, I have had several thoughts about how to combine duplicate students. It is important to note that in none of the files are there duplicate `studentids`, instead there are duplicate `sasid`. This makes sense given the source system.

I am currently thinking the right sequence is to run the build_tables() function first, then resolve duplicates. By doing a merge with all of the file years, I can record a year for each time a particular `studentid` and `sasid` combination appears. 

Once this identifies the "true" `studentid` (mode?), I can change that `studentid` to the "true" `studentid` in all tables. For the tbl_person and tbl_enrollment tables this should have no impact. Both of those tables are designed to have multiple copies of each student (although long-term tbl_person is collapsed to be unique by ID. However, this process is unaffected by the presence of an additional record for that student since they'll all be using modes to determine the "true" value). 

The `person_annual` table poses a few unique challenges. The student information will have to be combined. For example, the sum of days attended will have to be the sum of that column for each record. However, this does pose a challenge with data like address, where there is always going to be two equally valid values. The rule for resolving this may have to be to use the value with the persistent `studentid`, dropping the values in the less common `studentid`.
