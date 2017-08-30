
rm(list=ls())

################################################################################
# import data

# read the data
dogTable         <- read.delim("dog_table.txt")
litterTable      <- read.delim("litter_table.txt")
residencyTable   <- read.delim("residency_table.txt")
standsTable      <- read.delim("stands_table.txt")
vaccinationTable <- read.delim("vaccination_table.txt")



# set start and end date of analysis
startDate <- as.numeric(as.Date("2012-1-1", "%Y-%m-%d"))
endDate   <- as.numeric(as.Date("2015-12-31", "%Y-%m-%d"))

# pull some data out of tables that correspond to rows in residencyTable
male      <- rep(0, nrow(residencyTable))
birthDate <- rep(0, nrow(residencyTable))  
sterDate  <- rep(0, nrow(residencyTable)) 
vaccDate  <- rep(0, nrow(residencyTable))
for(i in 1:nrow(residencyTable)) {
  male[i]      <- dogTable$sex[
                    match(residencyTable$dog_id[i], 
                          as.character(dogTable$dog_id))]
  birthDate[i] <- as.character(dogTable$date_birth[
                    match(residencyTable$dog_id[i], 
                          as.character(dogTable$dog_id))])
  sterDate[i]  <- as.character(dogTable$sterilization_year[
                    match(residencyTable$dog_id[i], 
                          as.character(dogTable$dog_id))])
  vaccDate[i]  <- as.character(vaccinationTable$vacc_start_date[
                    match(residencyTable$dog_id[i], 
                          as.character(vaccinationTable$dog_id))])
}

# remove rows with no birthDate or exitDate
missingInfo    <- male == '' | birthDate == ''
residencyTable <- subset(residencyTable, missingInfo == 0)
male           <- male[missingInfo == 0]
birthDate      <- birthDate[missingInfo == 0]
sterDate       <- sterDate[missingInfo == 0]
vaccDate       <- vaccDate[missingInfo == 0]

# pull some more data out of tables to match rows of residencyTable
dogID      <- as.character(residencyTable$dog_id)
stand      <- as.character(residencyTable$stand)
entryEvent <- residencyTable$entry_event
entryDate  <- as.character(residencyTable$date_entry)
exitEvent  <- residencyTable$exit_event
deathEvent <- residencyTable$exit_death
exitDate   <- as.character(residencyTable$date_exit)
deathDate  <- ifelse(exitEvent == 1, exitDate, NA)  # death
exit2Date  <- ifelse(exitEvent == 2, exitDate, NA)  # lost
exit3Date  <- ifelse(exitEvent == 3, exitDate, NA)  # abandoned
exit4Date  <- ifelse(exitEvent == 4, exitDate, NA)  # stolen
exit5Date  <- ifelse(exitEvent == 5, exitDate, NA)  # sold
exit6Date  <- ifelse(exitEvent == 6, exitDate, NA)  # exchange
exit7Date  <- ifelse(exitEvent == 7, exitDate, NA)  # gifted
exit8Date  <- ifelse(exitEvent == 8, exitDate, NA)  # moved
exit88Date <- ifelse(exitEvent == 88, exitDate, NA)  # unknown
death1Date  <- ifelse(deathEvent == 1, exitDate, NA)  # disease or parasite
death2Date  <- ifelse(deathEvent == 2, exitDate, NA)  # accident
death3Date  <- ifelse(deathEvent == 3, exitDate, NA)  # starved
death4Date  <- ifelse(deathEvent == 4, exitDate, NA)  # owner killed
death5Date  <- ifelse(deathEvent == 5, exitDate, NA)  # authorities killed
death6Date  <- ifelse(deathEvent == 6, exitDate, NA)  # someone killed
death7Date  <- ifelse(deathEvent == 7, exitDate, NA)  # other
death8Date  <- ifelse(deathEvent == 8, exitDate, NA)  # sudden death
death88Date <- ifelse(deathEvent == 88, exitDate, NA)  # unknown

# convert coding to binary
male[male == 88] <- NA
male <- male - 1

# convert dates to numeric
birthDate <- as.numeric(as.Date(birthDate, "%Y-%m-%d"))
sterDate  <- as.numeric(as.Date(sterDate, "%Y-%m-%d"))
vaccDate  <- as.numeric(as.Date(vaccDate, "%Y-%m-%d"))
entryDate <- as.numeric(as.Date(entryDate, "%Y-%m-%d"))
exitDate  <- as.numeric(as.Date(exitDate, "%Y-%m-%d"))

# calc age at entry and days in stand
entryAge  <- entryDate - birthDate
standDays <- pmin(exitDate, 
                  max(exitDate, na.rm=TRUE), na.rm=TRUE) - entryDate + 1

# list of variable that may be included in models
variableNames <- c('exit', 'exit2', 'exit3', 'exit4',
                   'exit5', 'exit6', 'exit7', 'exit8', 'exit88',
                   'death', 'death1', 'death2', 'death3', 'death4',
                   'death5', 'death6', 'death7', 'death8', 'death88',
                   'day', 'dogID', 'male', 'age',
                   'puppy', 'juvenile', 'adult',
                   'vaccinated', 'sterilized', 'stand',
                   'y2012', 'y2013', 'y2014', 'y2015',
                   'jan', 'feb', 'mar', 'apr', 'may', 'jun',
                   'jul', 'aug', 'sep', 'oct', 'nov', 'dec')

# construct the data frame
prelimMatrix            <- matrix(0, nrow=sum(standDays, na.rm=TRUE), 
                                  ncol=length(variableNames))
colnames(prelimMatrix)  <- variableNames
dataMatrix              <- data.frame(prelimMatrix)
class(dataMatrix$dogID) <- 'character'
class(dataMatrix$stand) <- 'character'

# fill in most of dataframe
end <- 0
for(i in 1:nrow(residencyTable)) {
  start <- end + 1
  end <- start + standDays[i] - 1
  dataMatrix[start:end, 'day']   <- seq(entryDate[i], length.out=standDays[i])
  dataMatrix[start:end, 'dogID'] <- rep(dogID[i], standDays[i])
  dataMatrix[start:end, 'male']  <- rep(male[i], standDays[i])
  dataMatrix[start:end, 'age']   <- seq(entryAge[i], length.out=standDays[i])
  dataMatrix[start:end, 'stand'] <- rep(stand[i], standDays[i])
  dataMatrix[end, 'exit']    <- !is.na(exitDate[i] == dataMatrix[end, 'day'])
  dataMatrix[end, 'exit2']   <- !is.na(exit2Date[i] == dataMatrix[end, 'day'])
  dataMatrix[end, 'exit3']   <- !is.na(exit3Date[i] == dataMatrix[end, 'day'])
  dataMatrix[end, 'exit4']   <- !is.na(exit4Date[i] == dataMatrix[end, 'day'])
  dataMatrix[end, 'exit5']   <- !is.na(exit5Date[i] == dataMatrix[end, 'day'])
  dataMatrix[end, 'exit6']   <- !is.na(exit6Date[i] == dataMatrix[end, 'day'])
  dataMatrix[end, 'exit7']   <- !is.na(exit7Date[i] == dataMatrix[end, 'day'])
  dataMatrix[end, 'exit8']   <- !is.na(exit8Date[i] == dataMatrix[end, 'day'])
  dataMatrix[end, 'exit88']  <- !is.na(exit88Date[i] == dataMatrix[end, 'day'])
  dataMatrix[end, 'death']   <- !is.na(deathDate[i] == dataMatrix[end, 'day'])
  dataMatrix[end, 'death1']  <- !is.na(death1Date[i] == dataMatrix[end, 'day'])
  dataMatrix[end, 'death2']  <- !is.na(death2Date[i] == dataMatrix[end, 'day'])
  dataMatrix[end, 'death3']  <- !is.na(death3Date[i] == dataMatrix[end, 'day'])
  dataMatrix[end, 'death4']  <- !is.na(death4Date[i] == dataMatrix[end, 'day'])
  dataMatrix[end, 'death5']  <- !is.na(death5Date[i] == dataMatrix[end, 'day'])
  dataMatrix[end, 'death6']  <- !is.na(death6Date[i] == dataMatrix[end, 'day'])
  dataMatrix[end, 'death7']  <- !is.na(death7Date[i] == dataMatrix[end, 'day'])
  dataMatrix[end, 'death8']  <- !is.na(death8Date[i] == dataMatrix[end, 'day'])
  dataMatrix[end, 'death88'] <- !is.na(death88Date[i] == dataMatrix[end, 'day'])
  dataMatrix[start:end, 'vaccinated'] <- dataMatrix[start:end, 'day'] >
                                         vaccDate[i]
  dataMatrix[start:end, 'sterilized'] <- dataMatrix[start:end, 'day'] >
                                         sterDate[i]                                   
}  # end for loop

# set age categories
dataMatrix[dataMatrix[, 'age'] < 90, 'puppy']     <- 1
dataMatrix[dataMatrix[, 'age'] > 89 &
           dataMatrix[, 'age'] < 300, 'juvenile'] <- 1
dataMatrix[dataMatrix[, 'age'] > 299, 'adult']    <- 1

# replace NA's with 0's
dataMatrix[is.na(dataMatrix[, 'vaccinated']), 'vaccinated'] <- 0
dataMatrix[is.na(dataMatrix[, 'sterilized']), 'sterilized'] <- 0

# get the month and year that correspond to each day  
month <- as.numeric(format(as.Date(dataMatrix$day, 
                                   origin = "1970-01-01"), "%m"))
year  <- as.numeric(format(as.Date(dataMatrix$day, 
                                   origin = "1970-01-01"), "%Y"))

# create year dummies
dataMatrix[, 'y2012'] <- year == 2012
dataMatrix[, 'y2013'] <- year == 2013
dataMatrix[, 'y2014'] <- year == 2014
dataMatrix[, 'y2015'] <- year == 2015
dataMatrix[, 'y2016'] <- year == 2016

# create month dummies
dataMatrix[, 'jan'] <- month == 1
dataMatrix[, 'feb'] <- month == 2
dataMatrix[, 'mar'] <- month == 3
dataMatrix[, 'apr'] <- month == 4
dataMatrix[, 'may'] <- month == 5
dataMatrix[, 'jun'] <- month == 6
dataMatrix[, 'jul'] <- month == 7
dataMatrix[, 'aug'] <- month == 8
dataMatrix[, 'sep'] <- month == 9
dataMatrix[, 'oct'] <- month == 10
dataMatrix[, 'nov'] <- month == 11
dataMatrix[, 'dec'] <- month == 12

# rows representing days before 2012 and after 2015
nrow(dataMatrix)
dataMatrix <- dataMatrix[(dataMatrix[, 'day'] >= startDate &
                          dataMatrix[, 'day'] <= endDate), ]
nrow(dataMatrix)


################################################################################
# estimate models


model1A <- glm(death1 ~ juvenile + adult + male + vaccinated, 
              data=dataMatrix, family=binomial(link='logit'))
summary(model1A)

otherDeath <- dataMatrix$death2 + dataMatrix$death3 + dataMatrix$death4 +
              dataMatrix$death5 + dataMatrix$death6 + dataMatrix$death7 +
              dataMatrix$death8
model1B <- glm(otherDeath ~ juvenile + adult + male + vaccinated, 
               data=dataMatrix, family=binomial(link='logit'))
summary(model1B)


