# Loading SPSS files
# Note: this will fail unless you mount the encrypted .dmg at
# /Users/jason/Desktop/ProvidenceFiles.dmg

reg0405 <- read.spss('/Volumes/ProvidenceFiles/MasterFiles/REG Master_0405.sav',
                     to.data.frame=TRUE,
                     use.value.labels=TRUE,
                     trim.factor.names=TRUE,
                     use.missings=TRUE)
reg0506 <- read.spss('/Volumes/ProvidenceFiles/MasterFiles/REG Master_0506.sav',
                     to.data.frame=TRUE,
                     use.value.labels=TRUE,
                     trim.factor.names=TRUE,
                     use.missings=TRUE)
reg0607 <- read.spss('/Volumes/ProvidenceFiles/MasterFiles/REG Master_0607.sav',
                     to.data.frame=TRUE,
                     use.value.labels=TRUE,
                     trim.factor.names=TRUE,
                     use.missings=TRUE)
