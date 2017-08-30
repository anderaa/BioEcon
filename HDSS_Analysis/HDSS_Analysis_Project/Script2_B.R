
rm(list=ls())


########################################################################################################################
# import data

# read the data
dogTable         <- read.delim("HDSS_r1-12_csv/1-Dogs.csv", sep=',', na.strings = c('', 'NA'))
litterTable      <- read.delim("HDSS_r1-12_csv/1-Litter.csv", sep=',', na.strings = c('', 'NA'))
residencyTable   <- read.delim("HDSS_r1-12_csv/1-Residency.csv", sep=',', na.strings = c('', 'NA'))
standsTable      <- read.delim("HDSS_r1-12_csv/1-Stands.csv", sep=',', na.strings = c('', 'NA'))
vaccinationTable <- read.delim("HDSS_r1-12_csv/1-Vaccination.csv", sep=',', na.strings = c('', 'NA'))
########################################################################################################################


########################################################################################################################
# some basic data organization

# create list of unique dog_id's
uniqueDogList <- unique(c(as.character(dogTable$dog_id),
                          as.character(litterTable$dog_id),
                          as.character(residencyTable$dog_id),
                          as.character(vaccinationTable$dog_id)))

# begin organization of data
dateOfBirth      <- rep(NA, length(uniqueDogList))
sex              <- rep(NA, length(uniqueDogList))
dateOfSter       <- rep(NA, length(uniqueDogList))
sterilized       <- rep(NA, length(uniqueDogList))
dateOfEntry      <- rep(NA, length(uniqueDogList))
dateOfExit       <- rep(NA, length(uniqueDogList))
exitEvent        <- rep(NA, length(uniqueDogList))
deathEvent       <- rep(NA, length(uniqueDogList))
dateOfVacc       <- rep(NA, length(uniqueDogList))
standOfRes       <- rep(NA, length(uniqueDogList))

# fill in the vectors created above
for(i in 1:length(uniqueDogList)) { 
  # get dog id
  dogToFind <- uniqueDogList[i]
  # extract the info on dogToFind
  dateOfBirth[i] <- as.character(dogTable$date_birth[match(dogToFind, as.character(dogTable$dog_id))])
  sex[i]         <- dogTable$sex[match(dogToFind, as.character(dogTable$dog_id))]
  dateOfSter[i]  <- as.character(dogTable$sterilization_year[match(dogToFind, as.character(dogTable$dog_id))])
  sterilized[i]  <- as.character(dogTable$sterilized[match(dogToFind, as.character(dogTable$dog_id))])
  dateOfEntry[i] <- as.character(residencyTable$date_entry[match(dogToFind, as.character(residencyTable$dog_id))])
  dateOfExit[i]  <- as.character(residencyTable$date_exit[match(dogToFind, as.character(residencyTable$dog_id))])
  exitEvent[i]   <- residencyTable$exit_event[match(dogToFind, as.character(residencyTable$dog_id))]
  deathEvent[i]  <- residencyTable$exit_death[match(dogToFind, as.character(residencyTable$dog_id))]
  dateOfVacc[i]  <- as.character(vaccinationTable$vacc_start_date[match(dogToFind, as.character(vaccinationTable$dog_id))])
  standOfRes[i]  <- as.character(residencyTable$stand[match(dogToFind, as.character(residencyTable$dog_id))])
}

dateOfBirth <- as.numeric(as.Date(dateOfBirth, "%Y-%m-%d"))
dateOfEntry <- as.numeric(as.Date(dateOfEntry, "%Y-%m-%d"))
dateOfExit  <- as.numeric(as.Date(dateOfExit, "%Y-%m-%d"))
dateOfVacc  <- as.numeric(as.Date(dateOfVacc, "%Y-%m-%d"))

typeExit <- unique(exitEvent)
dateOfExitType1 <- ifelse(exitEvent == 1, dateOfExit, NA)
dateOfExitType2 <- ifelse(exitEvent == 2, dateOfExit, NA)
dateOfExitType3 <- ifelse(exitEvent == 3, dateOfExit, NA)
dateOfExitType4 <- ifelse(exitEvent == 4, dateOfExit, NA)
dateOfExitType5 <- ifelse(exitEvent == 5, dateOfExit, NA)
dateOfExitType7 <- ifelse(exitEvent == 7, dateOfExit, NA)
dateOfExitType8 <- ifelse(exitEvent == 8, dateOfExit, NA)
########################################################################################################################


########################################################################################################################
# create dataframe for mortality analysis

# set start and end dates of analysis
startDate <- as.numeric(as.Date("2012-1-1", "%Y-%m-%d"))
endDate <- max(max(dateOfEntry, na.rm=TRUE), max(dateOfExit, na.rm=TRUE))
# as.Date(endDate, origin = "1970-01-01")

# make a list of dogs in uniqueDogList that will be included in mortality analysis
# as.Date(as.character(endDate))
keep <- ifelse(is.na(dateOfBirth) |
                 is.na(sex) |
                 is.na(dateOfEntry) |
                 dateOfEntry < startDate |
                 dateOfBirth > dateOfEntry | 
                 sex == 88 | 
                 sex == 87 | 
                 sex==2 | 
                 !is.na(sterilized) |
                 (!is.na(dateOfExit) & dateOfEntry > dateOfExit), 
               FALSE, TRUE)

# above statement limits sample to female dogs who are unsterilized 

# remove dogs and data that are not included
dogs      <- uniqueDogList[keep]
gender    <- sex[keep]
entryDate <- dateOfEntry[keep]
birthDate <- dateOfBirth[keep]
exitDate  <- dateOfExit[keep]
exit1Date <- dateOfExitType1[keep]
exit2Date <- dateOfExitType2[keep]
exit3Date <- dateOfExitType3[keep]
exit4Date <- dateOfExitType4[keep]
exit5Date <- dateOfExitType5[keep]
exit7Date <- dateOfExitType7[keep]
exit8Date <- dateOfExitType8[keep]
dateOfVacc <- dateOfVacc[keep]
standOfRes <- standOfRes[keep]

# create a few data series we will use
ageAtEntry <- entryDate - birthDate
exitMarker <- !is.na(exitDate)  # equals zero if dog is still in population at end of Jan 1, 2016
exit1Marker <- !is.na(exit1Date)
exit2Marker <- !is.na(exit2Date)
exit3Marker <- !is.na(exit3Date)
exit4Marker <- !is.na(exit4Date)
exit5Marker <- !is.na(exit5Date)
exit7Marker <- !is.na(exit7Date)
exit8Marker <- !is.na(exit8Date)
vaccMarker  <- !is.na(dateOfVacc)

daysAlive  <- ifelse(is.na(exitDate), endDate - entryDate + 1, exitDate - entryDate + 1)

# create the series that will appear in the dataframe
dogID <- rep(dogs, daysAlive)
male  <- rep(gender, daysAlive)
day   <- rep(entryDate, daysAlive)
age   <- rep(birthDate, daysAlive)
exit  <- rep(0, length(dogID))
exit1 <- rep(0, length(dogID))
exit2 <- rep(0, length(dogID))
exit3 <- rep(0, length(dogID))
exit4 <- rep(0, length(dogID))
exit5 <- rep(0, length(dogID))
exit7 <- rep(0, length(dogID))
exit8 <- rep(0, length(dogID))
vacc  <- rep(0, length(dogID))
stand <- rep(0, length(dogID))


# combine above vectors into a dataframe
mortalityData <- data.frame(dogID, male, day, age, exit)

# incrament day for each dog 
unique <- unique(mortalityData$dogID)
mortalityDataPrime <- data.frame()
for (i in 1:length(unique)){
  currentDog <- mortalityData[mortalityData$dogID==unique[i], ]
  firstDay <- currentDog$day[1]
  currentDog$day <- seq(from=firstDay, to=firstDay+nrow(currentDog)-1)
  exitTest <- exitDate[i]
  currentDog$exit[currentDog$day==exitTest] <- 1
  mortalityDataPrime <- rbind(mortalityDataPrime, currentDog)
}

# replace original with correctly incremented data frame
mortalityData <- mortalityDataPrime

# reproduction
# this is a marker for whether or not an observation had a litter on its given day
litterTable$date_delivery <- as.numeric(as.Date(litterTable$date_delivery, "%Y-%m-%d"))
names(litterTable)[2] <- "day"
names(litterTable)[1] <- "dogID"
litterTable$dogID <- as.factor(litterTable$dogID)

mortLookUp <- data.frame(mortalityData[, c('dogID', 'day')])
combineMort <- paste(mortLookUp$dogID, mortLookUp$day, sep="_")
mortLookUp <- cbind(mortLookUp, combineMort)

litterLookUp <- data.frame(litterTable[, c('dogID', 'day', 'liveborn')])
combineLitter <- paste(litterLookUp$dogID, litterLookUp$day, sep="_")
litterLookUp <- cbind(litterLookUp, combineLitter)
birthMarker <- data.frame(rep(NA, nrow(mortalityData)))
birthMarker <- mortLookUp$combineMort %in% litterLookUp$combineLitter
mortalityData <- cbind(mortalityData, birthMarker)

# make this series look right
mortalityData$male <- as.numeric(mortalityData$male) - 1

# set the age
mortalityData$age <- mortalityData$day - mortalityData$age

# get the month and year that correspond to each day  
month <- as.numeric(format(as.Date(mortalityData$day, origin = "1970-01-01"), "%m"))
year  <- as.numeric(format(as.Date(mortalityData$day, origin = "1970-01-01"), "%Y"))

# create year dummies
yearMatrix <- matrix(NA, nrow=nrow(mortalityData), ncol=5)
colnames(yearMatrix) <- c('y2012', 'y2013', 'y2014', 'y2015', 'y2016')
yearMatrix[, 'y2012'] <- year == 2012
yearMatrix[, 'y2013'] <- year == 2013
yearMatrix[, 'y2014'] <- year == 2014
yearMatrix[, 'y2015'] <- year == 2015
yearMatrix[, 'y2016'] <- year == 2016

# create month dummies
monthMatrix <- matrix(NA, nrow=nrow(mortalityData), ncol=12)
colnames(monthMatrix) <- c('jan', 'feb', 'mar', 'apr', 'may', 'jun', 
                           'jul', 'aug', 'sep', 'oct', 'nov', 'dec')
monthMatrix[, 'jan'] <- month == 1
monthMatrix[, 'feb'] <- month == 2
monthMatrix[, 'mar'] <- month == 3
monthMatrix[, 'apr'] <- month == 4
monthMatrix[, 'may'] <- month == 5
monthMatrix[, 'jun'] <- month == 6
monthMatrix[, 'jul'] <- month == 7
monthMatrix[, 'aug'] <- month == 8
monthMatrix[, 'sep'] <- month == 9
monthMatrix[, 'oct'] <- month == 10
monthMatrix[, 'nov'] <- month == 11
monthMatrix[, 'dec'] <- month == 12

# combine all data
mortalityData <- cbind(mortalityData, yearMatrix, monthMatrix)

# limit to time frame 1-1-2012 -- 12-31-2015
litterData <- mortalityData[mortalityData$day >= 15340 & mortalityData$day <= 16800, ]

# limit to dogs age >= 300 days
litterData <- litterData[litterData$age >= 300, ]

## calculating average litter size

# limit sample to days where dogs had a litter
hadLitter <- litterData[litterData$birthMarker==1, ]
combine <- paste(hadLitter$dogID, hadLitter$day, sep="_")
hadLitter <- cbind(hadLitter, combine)

# Match dog and day to corresponding litter size
for (i in 1:nrow(hadLitter)){
  currentDog <- hadLitter$combine[i]
  hadLitter$litterSize[i] <- litterLookUp[match(currentDog, litterLookUp$combineLitter), "liveborn"]
}

# remove liveborn==88 (unknown)
hadLitter <- hadLitter[hadLitter$litterSize!=88, ]

# of dogs that had litters, calculating average litter size 

totalLitterSize <- sum(hadLitter$litterSize)
totalLitters <- nrow(hadLitter)

avgLitterSize <- totalLitterSize/totalLitters
print(avgLitterSize)

# calculating litter count by month (1-1-2012 -- 12-31-2015)

# jan
hadLitterJan   <- hadLitter[hadLitter$jan==1, ]
litterCountJan <- nrow(hadLitterJan)
print(litterCountJan)

# feb
hadLitterFeb   <- hadLitter[hadLitter$feb==1, ]
litterCountFeb <- nrow(hadLitterFeb)
print(litterCountFeb)

# mar
hadLitterMar   <- hadLitter[hadLitter$mar==1, ]
litterCountMar <- nrow(hadLitterMar)
print(litterCountMar)

# apr
hadLitterApr   <- hadLitter[hadLitter$apr==1, ]
litterCountApr <- nrow(hadLitterApr)
print(litterCountApr)

# may
hadLitterMay   <- hadLitter[hadLitter$may==1, ]
litterCountMay <- nrow(hadLitterMay)
print(litterCountMay)

# jun
hadLitterJun   <- hadLitter[hadLitter$jun==1, ]
litterCountJun <- nrow(hadLitterJun)
print(litterCountJun)

# jul
hadLitterJul   <- hadLitter[hadLitter$jul==1, ]
litterCountJul <- nrow(hadLitterJul)
print(litterCountJul)

# aug
hadLitterAug   <- hadLitter[hadLitter$aug==1, ]
litterCountAug <- nrow(hadLitterAug)
print(litterCountAug)

# sep
hadLitterSep   <- hadLitter[hadLitter$sep==1, ]
litterCountSep <- nrow(hadLitterSep)
print(litterCountSep)

# oct
hadLitterOct   <- hadLitter[hadLitter$oct==1, ]
litterCountOct <- nrow(hadLitterOct)
print(litterCountOct)

# nov
hadLitterNov   <- hadLitter[hadLitter$nov==1, ]
litterCountNov <- nrow(hadLitterNov)
print(litterCountNov)

# dec
hadLitterDec   <- hadLitter[hadLitter$dec==1, ]
litterCountDec <- nrow(hadLitterDec)
print(litterCountDec)