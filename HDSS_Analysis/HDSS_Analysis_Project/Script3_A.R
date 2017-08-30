
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
dateOfBirth <- rep(NA, length(uniqueDogList))
sex         <- rep(NA, length(uniqueDogList))
dateOfSter  <- rep(NA, length(uniqueDogList))
sterilized  <- rep(NA, length(uniqueDogList))
dateOfEntry <- rep(NA, length(uniqueDogList))
dateOfExit  <- rep(NA, length(uniqueDogList))
exitEvent   <- rep(NA, length(uniqueDogList))
deathEvent  <- rep(NA, length(uniqueDogList))
dateOfVacc  <- rep(NA, length(uniqueDogList))

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
  dateOfVacc[i]  <- as.character(vaccinationTable$vacc_start_date[match(dogToFind, 
                                                                        as.character(vaccinationTable$dog_id))])
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
               (!is.na(dateOfExit) & dateOfEntry > dateOfExit), 
               FALSE, TRUE)

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

# combine above vectors into a dataframe
mortalityData <- data.frame(dogID, male, day, age, exit)

# increment day and age for each dog 
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

# replace original with correctly incremented data
mortalityData <- mortalityDataPrime

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

# subset mortalityData to right time frame (1-1-2012 -- 12-31-2015)
mortalityData <- mortalityData[mortalityData$day >= 15340 & mortalityData$day <= 16800, ]

# create list of unique days
uniqueDay <- unique(mortalityData$day)

# create object to fill population for each day
totalPop <- rep(NA, length(uniqueDay))
juvPct <- rep(NA, length(uniqueDay))
femalePct <- rep(NA, length(uniqueDay))
puppyPct <- rep(NA, length(uniqueDay))

for (i in 1:length(uniqueDay)){
  currentDay <- mortalityData[mortalityData$day==uniqueDay[i], ]
  test <- unique(currentDay$dogID)
  totalPop[i] <- length(test)
  puppy    <- currentDay[currentDay$age <= 89, ]
  juvenile <- currentDay[currentDay$age < 300 & currentDay$age >= 90, ]
  adult    <- currentDay[currentDay$age >= 300, ]
  puppyCount <- nrow(puppy)
  juvCount <- nrow(juvenile)
  adultCount <- nrow(adult)
  puppyPct[i] <- puppyCount/totalPop[i]
  juvPct[i] <- juvCount/totalPop[i]
  female   <- currentDay[currentDay$male==0, ]
  male     <- currentDay[currentDay$male==1, ]
  femaleCount <- nrow(female)
  maleCount <- nrow(male)
  femalePct[i] <- femaleCount/totalPop[i]
}
########################################################################################################################


########################################################################################################################
# get results

# calculating total average abundance
avgPop <- ((sum(totalPop))/(length(totalPop)))
avgPop

# calculating max abundance
maxAbund <- max(totalPop)
maxAbund

# calculating average puppy fraction (puppy -> age < 90 days)
avgPuppyPct <- mean(puppyPct)
avgPuppyPct

# calculating average juvenile fraction (adult -> age >= 300 days)
avgJuvPct <- mean(juvPct)
avgJuvPct

# calculating average adult fraction
mean(1 - puppyPct - juvPct)

# calculating average fraction of non-adults that are puppies
mean(puppyPct / (puppyPct + juvPct))

# calculating average male/female sex ratio
avgSexPct <- mean(femalePct)
avgSexPct
