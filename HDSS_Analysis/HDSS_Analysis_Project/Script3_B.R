rm(list=ls())

##########################################################################################################################
# import data

# read the data
dogTable         <- read.delim("HDSS_r1-12_csv/1-Dogs.csv", sep=',', na.strings = c('', 'NA'))
litterTable      <- read.delim("HDSS_r1-12_csv/1-Litter.csv", sep=',', na.strings = c('', 'NA'))
residencyTable   <- read.delim("HDSS_r1-12_csv/1-Residency.csv", sep=',', na.strings = c('', 'NA'))
standsTable      <- read.delim("HDSS_r1-12_csv/1-Stands.csv", sep=',', na.strings = c('', 'NA'))
vaccinationTable <- read.delim("HDSS_r1-12_csv/1-Vaccination.csv", sep=',', na.strings = c('', 'NA'))

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

startDate <- as.numeric(as.Date("2012-1-1", "%Y-%m-%d"))
endDate <- max(max(dateOfEntry, na.rm=TRUE), max(dateOfExit, na.rm=TRUE))

# Calculating average immigrant dogs per year
residencyTable$date_entry <- as.numeric(as.Date(residencyTable$date_entry, "%Y-%m-%d"))
res2012 <- residencyTable[residencyTable$date_entry >= 15340, ]
resFinal <- res2012[res2012$entry_event != 1, ]

totalImmigrants <- nrow(resFinal)
totalDays <- endDate-startDate+1
totalYears <- totalDays/365

expectedImmigrants <- totalImmigrants/totalYears
print(expectedImmigrants)
