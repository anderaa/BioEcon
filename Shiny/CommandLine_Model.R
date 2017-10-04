rm(list=ls())

# inputs for simulation
simulationYears <- 5
simulationEnd   <- 365 * simulationYears
iterations      <- 5

# inputs for initial population
initialPopSize    <- 600
initialFracAdult  <- 0.49
initialFracPup    <- 0.5
initialFracFemale <- 0.3847
initialFracImmune <- 0.0
initialFracContra <- 0.0
initialFracVacc   <- 0.0
initialFracSter   <- 0.0

# inputs for mortality
maxJuvAge          <- 299
maxPuppyAge        <- 89
maxAge             <- 4000
carryingCap        <- 800
pupAnnMortProb     <- 0.75
juvAnnMortProb     <- 0.75
adultAnnMortProb   <- 0.3
emigrationProb     <- 0

# inputs for reproduction
immigrantDogs        <- 195
expectedLittersPerFemalePerYear <- 0.28
meanLitterSize       <- 4.52
femalePupProb        <- 0.3847
fractionBirthPulse   <- 0.5
birthPulseVector     <- rep(0, 12)
birthPulseVector[1]  <- 0 
birthPulseVector[2]  <- 0 
birthPulseVector[3]  <- 0
birthPulseVector[4]  <- 1 
birthPulseVector[5]  <- 1 
birthPulseVector[6]  <- 1 
birthPulseVector[7]  <- 0
birthPulseVector[8]  <- 0
birthPulseVector[9]  <- 0 
birthPulseVector[10] <- 0 
birthPulseVector[11] <- 0 
birthPulseVector[12] <- 0 

# inputs for disease
annualIntroProb    <- 0.0
timeLimitExposed   <- 25 
timeLimitInfective <- 5
survivalProb       <- 0
transmissionParam  <- 0.25

# inputs for benefits of management
bitesPerNonRabid     <- 0 
bitesPerRabid        <- 0  
PEPperNonRabidBite   <- 0
PEPperRabidBite      <- 0
costPerPEP           <- 0
lifeLossPerRabidBite <- 0

# inputs for treatment costs
vaccineCost             <- 1000
contraceptionCostFemale <- 0
contraceptionCostMale   <- 0
sterilizationCostFemale <- 0
sterilizationCostMale   <- 0
euthanasiaCost          <- 0

# inputs for effectiveness of contraception and vaccination
timeVaccineEffective       <- 2
timeBoosterEffective       <- 5
timeContraEffectiveMales   <- 2
timeContraEffectiveFemales <- 2

# inputs for contact costs
# note: 25, 50, 75, 100, mean 25% 50%, 75%, 100% of specified 
#       initial abundance
contactCost25  <- 0
contactCost50  <- 0
contactCost75  <- 0
contactCost100 <- 0

# input for budget years 1-5    
annualBudget     <- rep(0, 5)
annualBudget[1]  <- 0
annualBudget[2]  <- 4000
annualBudget[3]  <- 4000
annualBudget[4]  <- 4000
annualBudget[5]  <- 4000

# inputs for strategy
# note: model assumes already sterilized dogs are not re-sterilized. 
#       Within the same year dogs will not be vaccinated or contracepted 
#       twice. If dogs are re-contancted in a future year, they will be 
#       re-vaccinated or re-contracepted
# note: contraception and sterilization cannot both equal 1 for same 
#       demographic
# note: if euthanisia equal 1 for some demographic, 
#       all other treatments must equal zero  
vaccPuppyMale     <- 1
vaccPuppyFemale   <- 1
vaccAdultMale     <- 1
vaccAdultFemale   <- 1
vaccJuvMale       <- 1
vaccJuvFemale     <- 1
contraPuppyMale   <- 0
contraPuppyFemale <- 0
contraAdultMale   <- 0
contraAdultFemale <- 0
contraJuvMale     <- 0
contraJuvFemale   <- 0
sterPuppyMale     <- 0
sterPuppyFemale   <- 0
sterAdultMale     <- 0
sterAdultFemale   <- 0
sterJuvMale       <- 0
sterJuvFemale     <- 0
euthPuppyMale     <- 0
euthPuppyFemale   <- 0
euthAdultMale     <- 0
euthAdultFemale   <- 0
euthJuvMale       <- 0
euthJuvFemale     <- 0

boosterGiven <- TRUE


# inputs for management timing
mgtMonthVector     <- rep(0, 12)
mgtMonthVector[1]  <- 0 
mgtMonthVector[2]  <- 1 
mgtMonthVector[3]  <- 0
mgtMonthVector[4]  <- 0
mgtMonthVector[5]  <- 0
mgtMonthVector[6]  <- 0
mgtMonthVector[7]  <- 0
mgtMonthVector[8]  <- 0
mgtMonthVector[9]  <- 0
mgtMonthVector[10] <- 0
mgtMonthVector[11] <- 0
mgtMonthVector[12] <- 0



########################################
# Misc preliminary calculations and assignments:

# A vector of month number for use in seasonal timing:
monthSeries <- c(rep(1, 31), rep(2, 28), rep(3, 31), rep(4, 30), 
                 rep(5, 31), rep(6, 30), rep(7, 31), rep(8, 31),
                 rep(9, 30), rep(10, 31), rep(11, 30), rep(12, 31)) 

# Calculate demographics of initial population:
initialAdults     <- round(initialFracAdult * initialPopSize)
initialSubAdults  <- initialPopSize - initialAdults
initialPuppies    <- round(initialFracPup * initialSubAdults)
initialJuveniles  <- initialSubAdults - initialPuppies

# Calculate daily mortality probabilities:
pupMortalityProb   <- 1 - (1 - pupAnnMortProb) ^ (1/365)
juvMortalityProb   <- 1 - (1 - juvAnnMortProb) ^ (1/365)
adultMortalityProb <- 1 - (1 - adultAnnMortProb) ^ (1/365)

# Calculate daily litter probabilities:
monthDayCount <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
if(sum(birthPulseVector) != 12 & sum(birthPulseVector != 0)) {
  peakDays <- sum(birthPulseVector * monthDayCount)
  peakProb <- (fractionBirthPulse * expectedLittersPerFemalePerYear) / 
    peakDays
  offPeakDays <- sum((!birthPulseVector) * monthDayCount)
  offPeakProb <- ((1-fractionBirthPulse) * expectedLittersPerFemalePerYear) / 
    offPeakDays
  litterProbability <- rep(offPeakProb, 365)
  for(m in 1:12) {
    if(birthPulseVector[m] == 1) {
      litterProbability[monthSeries == m] <- peakProb
    }
  }
} else {
  litterProbability <- rep(expectedLittersPerFemalePerYear / 365, 365)
}

# Calculate daily probability of disease introduction:
exogenousIntroProb <- 1 - (1 - annualIntroProb) ^ (1/365)

# Calculate contact costs at carrying capacity:
contactCost25k <- contactCost25 * (carryingCap / initialPopSize)
contactCost50k <- contactCost50 * (carryingCap / initialPopSize)
contactCost75k <- contactCost75 * (carryingCap / initialPopSize)
contactCost100k <- contactCost100 * (carryingCap / initialPopSize)
contactCost25k  <- max(contactCost25k, 0) 
contactCost50k  <- max(contactCost50k, 0, contactCost25k)
contactCost75k  <- max(contactCost75k, 0, contactCost50k)
contactCost100k <- max(contactCost100k, 0, contactCost75k)

# List days that management will occur:
mgtDayVector <- rep(0, 365)
for(m in 1:12) {
  if(mgtMonthVector[m] == 1) {
    mgtDayVector[monthSeries == m] <- 1
  }
}
managementDays <- seq(1, 365)
managementDays <- managementDays[mgtDayVector == 1]

# A list of traits in the population matrix:
traitList <- c('age', 'puppy', 'adult','female',
               'sterilized', 'contracepted', 'timeContra',
               'vaccinated', 'timeVacc',
               'boosted',
               'exposed', 'timeExposed',
               'infective', 'timeInfective',
               'immune', 'month')

# A list of results that will be tracked:
censusSeries <- c('abundance', 'puppy', 'adult', 'females', 
                  'sterilized', 'femalesSterilized',
                  'contracepted', 'femalesContracepted', 
                  'vaccinated', 'immune', 'exposed', 'infective',
                  'PEPs', 'lifeLoss', 'newlyVaccinated') 
censusVector <- rep(0, length(censusSeries))
names(censusVector) <- censusSeries

# Create a 3d array to store results:
resultsMatrix <- array(data=NA, 
                       dim=c(simulationEnd, length(censusSeries), iterations))
colnames(resultsMatrix) <- censusSeries

# Create a vector of binary strategy indicators:
strategyNames <- c('vaccPuppyMale', 'vaccPuppyFemale',
                   'vaccAdultMale', 'vaccAdultFemale', 
                   'vaccJuvMale', 'vaccJuvFemale',
                   'contraPuppyMale', 'contraPuppyFemale',
                   'contraAdultMale', 'contraAdultFemale',
                   'contraJuvMale', 'contraJuvFemale',
                   'sterPuppyMale', 'sterPuppyFemale',
                   'sterAdultMale', 'sterAdultFemale', 
                   'sterJuvMale', 'sterJuvFemale',
                   'euthPuppyMale', 'euthPuppyFemale',
                   'euthAdultMale', 'euthAdultFemale', 
                   'euthJuvMale', 'euthJuvFemale') 
strategyVector <- c(vaccPuppyMale, vaccPuppyFemale,
                    vaccAdultMale, vaccAdultFemale, 
                    vaccJuvMale, vaccJuvFemale,
                    contraPuppyMale, contraPuppyFemale,
                    contraAdultMale, contraAdultFemale, 
                    contraJuvMale, contraJuvFemale,
                    sterPuppyMale, sterPuppyFemale,
                    sterAdultMale, sterAdultFemale, 
                    sterJuvMale, sterJuvFemale,
                    euthPuppyMale, euthPuppyFemale,
                    euthAdultMale, euthAdultFemale, 
                    euthJuvMale, euthJuvFemale)
names(strategyVector) <- strategyNames

# Create a cost vector to indicate unit cost of each strategy:
strategyCostVector <- c(rep(vaccineCost, 6),
                        contraceptionCostMale, contraceptionCostFemale,
                        contraceptionCostMale, contraceptionCostFemale, 
                        contraceptionCostMale, contraceptionCostFemale,
                        sterilizationCostMale, sterilizationCostFemale,
                        sterilizationCostMale, sterilizationCostFemale, 
                        sterilizationCostMale, sterilizationCostFemale,
                        rep(euthanasiaCost, 6))
names(strategyCostVector) <- strategyNames

# Create a mapping between cost and number of dogs contacted:
contactMapping <- c(seq(0.0, 0.25, length.out=min(10000, (contactCost25k + 1)*10000)),
                    seq(0.25, 0.5, length.out=min(10000, ((contactCost50k - contactCost25k + 1)*10000)))[-1],
                    seq(0.5, 0.75, length.out=min(10000, ((contactCost75k - contactCost50k + 1)*10000)))[-1],
                    seq(0.75, 1.0, length.out=min(10000, ((contactCost100k - contactCost75k + 1)*10000)))[-1])

costSequence <- c(seq(0.0, contactCost25k, length.out=min(10000, (contactCost25k + 1)*10000)),
                    seq(contactCost25k, contactCost50k, 
                        length.out=min(10000, ((contactCost50k - contactCost25k + 1)*10000)))[-1],
                    seq(contactCost50k, contactCost75k, 
                        length.out=min(10000, ((contactCost75k - contactCost50k + 1)*10000)))[-1],
                    seq(contactCost75k, contactCost100k, 
                        length.out=min(10000, ((contactCost100k - contactCost75k + 1)*10000)))[-1])
########################################

########################################
InitialPopulation <- function() {
  # Agruments: None.
  # Return:    The population matrix.
  # Purpose:   Construct the initial population matrix.
  
  popMatrix <- matrix(0, nrow=initialPopSize, ncol=length(traitList))
  colnames(popMatrix) <- traitList                 
  popMatrix[, 'age'] <- c(sample(seq(1, maxPuppyAge), 
                                 initialPuppies, replace=TRUE),
                          sample(seq(maxPuppyAge + 1, maxJuvAge), 
                                 initialJuveniles, replace=TRUE),
                          sample(seq(maxJuvAge + 1, maxAge), 
                                 initialAdults, replace=TRUE))
  popMatrix[, 'female'] <- sample(c(0, 1), initialPopSize, 
                                  replace=TRUE, 
                                  prob=c(1-initialFracFemale, 
                                         initialFracFemale))
  popMatrix[, 'contracepted'] <- sample(c(0, 1), initialPopSize, 
                                        replace=TRUE, 
                                        prob=c(1-initialFracContra, 
                                               initialFracContra))
  popMatrix[, 'sterilized'] <- sample(c(0, 1), initialPopSize, 
                                      replace=TRUE, 
                                      prob=c(1-initialFracSter, 
                                             initialFracSter))
  popMatrix[, 'vaccinated'] <- sample(c(0, 1), initialPopSize, 
                                      replace=TRUE, 
                                      prob=c(1-initialFracVacc, 
                                             initialFracVacc))
  popMatrix[, 'immune'] <- sample(c(0, 1), initialPopSize, 
                                  replace=TRUE, 
                                  prob=c(1-initialFracImmune, 
                                         initialFracImmune))
  popMatrix[popMatrix[, 'contracepted']==1, 'timeContra'] <- 
    vapply(popMatrix[popMatrix[, 'contracepted']==1, 'age'], sample, 
           size=1, FUN.VALUE=0) 
  popMatrix[popMatrix[, 'vaccinated']==1, 'timeVacc'] <- 
    vapply(popMatrix[popMatrix[, 'vaccinated']==1, 'age'], 
           sample, size=1, FUN.VALUE=0)
  popMatrix[popMatrix[, 'age'] > maxJuvAge, 'adult'] <- 1
  popMatrix[popMatrix[, 'age'] <= maxPuppyAge, 'puppy'] <- 1
  popMatrix[, 'month'] <- 1
  
  return(popMatrix)
}
########################################

########################################
TotalCost <- function(k, dogDemoForStrategies) {
  # Arguments: Money allocated to contact or capture,
  #            number of dogs available for each strategy.
  # Return:    The total cost including contact or capture and treatment 
  #            costs.
  # Purpose:   Calculates the total cost of management for a given year given
  #            the amount devoted to contact or capture and current 
  #            demographics.
  
  currentAbundance <- nrow(popMatrix)
  contactAllocation <- costSequence[k]
  
  # adjust contact cost for current population relative to k
  dogsContacted = contactMapping[k] * currentAbundance
  adjAllocation = contactAllocation * (currentAbundance / carryingCap)
  
  demoOfContacted <- dogsContacted * pmax((dogDemoForStrategies / nrow(popMatrix)), 0, na.rm=TRUE)
  totalCost <- adjAllocation + sum(demoOfContacted * strategyCostVector * strategyVector)
  
  return(totalCost)
}
########################################

########################################
AnnualStrategy <- function() {
  # Arguments: None.
  # Return:    A vector of the number of dogs in each demographic category
  #            that receive each treament.
  # Purpose:   At the start of each year, this function calculates the
  #            number of dogs that can be treated given costs, demographics,
  #            and the specified strategy.
  
  # Calc the expected demographics associated with each strategy choice:
  currentAbundance   <- nrow(popMatrix)
  pupFemalesUnster   <- sum(popMatrix[, 'female'] == 1 & 
                              popMatrix[, 'puppy'] == 1 & 
                              popMatrix[, 'sterilized'] == 0)
  juvFemalesUnster   <- sum(popMatrix[, 'female'] == 1 & 
                              popMatrix[, 'adult'] == 0 &
                              popMatrix[, 'puppy'] == 0 &
                              popMatrix[, 'sterilized'] == 0)
  adultFemalesUnster <- sum(popMatrix[, 'female'] == 1 & 
                              popMatrix[, 'adult'] == 1 & 
                              popMatrix[, 'sterilized'] == 0)
  pupMalesUnster     <- sum(popMatrix[, 'female'] == 0 & 
                              popMatrix[, 'puppy'] == 1 & 
                              popMatrix[, 'sterilized'] == 0)
  juvMalesUnster     <- sum(popMatrix[, 'female'] == 0 & 
                              popMatrix[, 'adult'] == 0 &
                              popMatrix[, 'puppy'] == 0 &
                              popMatrix[, 'sterilized'] == 0)
  adultMalesUnster   <- sum(popMatrix[, 'female'] == 0 & 
                              popMatrix[, 'adult'] == 1 & 
                              popMatrix[, 'sterilized'] == 0)
  pupFemalesSter     <- sum(popMatrix[, 'female'] == 1 & 
                              popMatrix[, 'puppy'] == 1 &
                              popMatrix[, 'sterilized'] == 1)
  juvFemalesSter     <- sum(popMatrix[, 'female'] == 1 & 
                              popMatrix[, 'adult'] == 0 &
                              popMatrix[, 'puppy'] == 0 &
                              popMatrix[, 'sterilized'] == 1)
  adultFemalesSter   <- sum(popMatrix[, 'female'] == 1 & 
                              popMatrix[, 'adult'] == 1 &
                              popMatrix[, 'sterilized'] == 1)
  pupMalesSter       <- sum(popMatrix[, 'female'] == 0 & 
                              popMatrix[, 'puppy'] == 1 & 
                              popMatrix[, 'sterilized'] == 1)
  juvMalesSter       <- sum(popMatrix[, 'female'] == 0 & 
                              popMatrix[, 'adult'] == 0 &
                              popMatrix[, 'puppy'] == 0 &
                              popMatrix[, 'sterilized'] == 1)
  adultMalesSter     <- sum(popMatrix[, 'female'] == 0 & 
                              popMatrix[, 'adult'] == 1 & 
                              popMatrix[, 'sterilized'] == 1)
  dogDemoForStrategies <- c(pupMalesUnster + pupMalesSter,
                            pupFemalesUnster + pupFemalesSter,
                            adultMalesUnster + adultMalesSter,
                            adultFemalesUnster + adultFemalesSter,
                            juvMalesUnster + juvMalesSter,
                            juvFemalesUnster + juvFemalesSter,
                            pupMalesUnster, pupFemalesUnster,
                            adultMalesUnster, adultFemalesUnster, 
                            juvMalesUnster, juvFemalesUnster,
                            pupMalesUnster, pupFemalesUnster,
                            adultMalesUnster, adultFemalesUnster, 
                            juvMalesUnster, juvFemalesUnster,
                            pupMalesUnster + pupMalesSter,
                            pupFemalesUnster + pupFemalesSter,
                            adultMalesUnster + adultMalesSter,
                            adultFemalesUnster + adultMalesSter,
                            juvMalesUnster + juvMalesSter,
                            juvFemalesUnster + juvFemalesSter)
  names(dogDemoForStrategies) <- strategyNames
  
  # An iterative approach to calculating the number of dogs that can be 
  # treated:
  #   Descrption: Incrementally increase funding allocation to contact/capture 
  #   until capture more dogs than manager can afford to treat. Then 
  #   incrementally decrease allocation to capture until total cost is less 
  #   than the budget. Continue to repeat, reversing incrementing direction 
  #   and reducing increment size by by half until the increment size reaches 
  #   specified tolerance.
  
  if(annualBudget[j] > 0) {
    k = length(costSequence)
    totalCost = annualBudget[j] + 1
    while(totalCost > annualBudget[j]) {
      allocation = costSequence[k]
      totalCost = TotalCost(k, dogDemoForStrategies)
      k = k - 1
    }
    contactAllocation = costSequence[k + 1]
    # adjust contact cost for current population relative to k
    dogsContacted = contactMapping[k + 1] * currentAbundance
    demoOfContacted <- dogsContacted * pmax((dogDemoForStrategies / nrow(popMatrix)), 0, na.rm=TRUE)
    strategyResults <- demoOfContacted * strategyVector
    
  } else {
    strategyResults = dogDemoForStrategies * 0
  }

  return(strategyResults) 
}
########################################

########################################
MortalityFunction <- function() {
  # Arguments: None.
  # Return:    An updated population matrix.
  # Purpose:   Induces out-migration, probabilistic mortality,
  #            old-age mortality, and censors population to carrying capacity.
  
  # Induce out-migration:
  emigDraw       <- runif(nrow(popMatrix))
  popMatrix <- popMatrix[emigDraw > emigrationProb, , drop=FALSE]
  
  # Induce mortality:
  n <- nrow(popMatrix)
  mortProbVector <- rep(adultMortalityProb, n)
  mortProbVector[popMatrix[, 'age'] <= maxJuvAge]  <- juvMortalityProb
  mortProbVector[popMatrix[, 'age'] <= maxPuppyAge] <- pupMortalityProb
  mortDraw <- runif(n)
  popMatrix <- popMatrix[mortDraw > mortProbVector, , drop=FALSE]
  popMatrix <- popMatrix[maxAge > popMatrix[, 'age'], , drop=FALSE]
  n <- nrow(popMatrix)
  popMatrix <- popMatrix[sample(seq(1, n), min(carryingCap, n), 
                                replace=FALSE), , drop=FALSE]
  
  return(popMatrix)
}
########################################

########################################
ReproductionFunction <- function(d) {
  # Arguments: Day of the year.
  # Return:    An updated population matrix.
  # Purpose:   Induces reproduction and adds the new puppies to
  #            the population.
  
  popMatrix[popMatrix[, 'age'] == (maxJuvAge + 1), 'adult'] <- 1
  popMatrix[popMatrix[, 'age'] == (maxPuppyAge +1), 'puppy'] <- 0
  fertFemales <- sum(popMatrix[, 'adult'] == 1 & 
                       popMatrix[, 'female'] == 1 &
                       popMatrix[, 'contracepted'] == 0 &
                       popMatrix[, 'sterilized'] == 0 &
                       (popMatrix[, 'exposed'] + popMatrix[, 'infective'] == 0 | 
                          popMatrix[, 'immune'] == 1))
  fertFemales <- max(0, fertFemales)
  litterDraw <- runif(fertFemales)
  puppies <- round(sum(litterDraw < litterProbability[d]) * meanLitterSize)
  newDogMatrix <- matrix(0, nrow=puppies, ncol=length(traitList))
  colnames(newDogMatrix) <- traitList
  newDogMatrix[, 'female'] <- sample(c(0, 1), puppies, 
                                     replace=TRUE, prob=c(1-femalePupProb, 
                                                          femalePupProb))
  newDogMatrix[, 'puppy'] <- 1
  popMatrix <- rbind(popMatrix, newDogMatrix)
  
  return(popMatrix)
}
########################################

########################################
ImmigrationFunction <- function() {
  # Arguments: None.
  # Return:    An updated population matrix.
  # Purpose:  Creates immigrant dogs and adds them to the population.
  
  # Calcuate number of new dogs to be added each day:
  newDogCount <- immigrantDogs%/%365
  probNewDog <- (immigrantDogs%%365) / 365
  newDogCount <- newDogCount + (runif(1) < probNewDog)
  
  # Create and add new dogs:
  newDogMatrix <- matrix(0, nrow=newDogCount, ncol=length(traitList))
  colnames(newDogMatrix) <- traitList
  newDogMatrix[, 'female'] <- sample(c(0, 1), newDogCount, 
                                     replace=TRUE, 
                                     prob=c(1-initialFracFemale, 
                                            initialFracFemale))
  newDogMatrix[, 'age'] <- sample(seq(0, maxAge), newDogCount, replace=TRUE)
  newDogMatrix[newDogMatrix[, 'age'] > maxJuvAge, 'adult'] <- 1
  newDogMatrix[newDogMatrix[, 'age'] <= maxPuppyAge, 'puppy'] <- 1
  popMatrix <- rbind(popMatrix, newDogMatrix)
  
  return(popMatrix)
}
########################################

########################################
DiseaseSpreadFunction <- function() {
  # Arguments: None.
  # Return:    An updated population matrix.
  # Purpose:   Induces disease transmission, exogenous disease introduction.
  
  # Exogenous transmission:
  n <- nrow(popMatrix)
  exogDraw   <- runif(n)
  newExposed <- exogDraw < exogenousIntroProb & 
    popMatrix[, 'infective'] == 0 &
    popMatrix[, 'exposed'] == 0 &
    popMatrix[, 'immune'] == 0 &
    popMatrix[, 'vaccinated'] == 0
  popMatrix[newExposed, 'exposed']     <- 1
  popMatrix[newExposed, 'timeExposed'] <- 0
  
  # Endogenous transmission:             
  transProb <- transmissionParam * 
    (1/carryingCap) * sum(popMatrix[, 'infective'])
  endoDraw  <- runif(n)
  newExposed <- endoDraw < transProb & 
    popMatrix[, 'infective'] == 0 &
    popMatrix[, 'exposed'] == 0 &
    popMatrix[, 'immune'] == 0 &
    popMatrix[, 'vaccinated'] == 0
  popMatrix[newExposed, 'exposed']     <- 1
  popMatrix[newExposed, 'timeExposed'] <- 0
  
  return(popMatrix)
} 
########################################

########################################
DiseaseProgressionFunction <- function() {
  # Arguments: None.
  # Return:    An updated population matrix.
  # Purpose:   Induces transition from exposed and infective states.
  
  # Transition exposed to infective:
  newInfective <- popMatrix[, 'exposed'] == 1 &
    popMatrix[, 'timeExposed'] > timeLimitExposed
  popMatrix[newInfective, 'exposed']       <- 0
  popMatrix[newInfective, 'infective']     <- 1
  popMatrix[newInfective, 'timeInfective'] <- 0
  
  # Transition infective to death or immune:
  newRecovered <- popMatrix[, 'infective'] == 1 &
    popMatrix[, 'timeInfective'] > timeLimitInfective
  recoverDraw <- runif(length(newRecovered))
  recover <- newRecovered[recoverDraw < survivalProb]
  death <- newRecovered[recoverDraw >= survivalProb]
  popMatrix[recover, 'infective'] <- 0
  popMatrix[recover, 'immune']    <- 1
  popMatrix <- popMatrix[!death, , drop=FALSE]
  
  return(popMatrix)
}
########################################

########################################
CensusFunction <- function() {
  # Arguments: None.
  # Return: A vector of results.
  # Purpose: Calculate results that are recorded daily.
  
  censusVector['abundance'] <- nrow(popMatrix)
  censusVector['puppy'] <- sum(popMatrix[, 'age'] <= maxPuppyAge)
  censusVector['adult'] <- sum(popMatrix[, 'age'] > maxJuvAge)
  censusVector['females'] <- sum(popMatrix[, 'female'])
  censusVector['sterilized'] <- sum(popMatrix[, 'sterilized'])
  censusVector['femalesSterilized'] <- sum(popMatrix[, 'sterilized'] == 1 &
                                             popMatrix[, 'female'] == 1)
  censusVector['contracepted'] <- sum(popMatrix[, 'sterilized'])
  censusVector['femalesContracepted'] <- 
    sum(popMatrix[, 'contracepted'] == 1 & popMatrix[, 'female'] == 1)
  censusVector['vaccinated'] <- sum(popMatrix[, 'vaccinated'])
  censusVector['immune'] <- sum(popMatrix[, 'immune'])
  censusVector['exposed'] <- sum(popMatrix[, 'exposed'])
  censusVector['infective'] <- sum(popMatrix[, 'infective'])
  bitesNonRabid <- bitesPerNonRabid * 
    (censusVector['abundance'] - censusVector['infective'])
  bitesRabid <- bitesPerRabid * (censusVector['infective'])
  censusVector['PEPs'] <- PEPperNonRabidBite * bitesNonRabid + 
    PEPperRabidBite * bitesRabid
  censusVector['lifeLoss'] <- lifeLossPerRabidBite * bitesRabid
  
  return(censusVector)
}
########################################

########################################
StrategySchedule <- function() {
  # Arguments: None.
  # Return: A vector of the counts of each demographic group that will
  #         receive each treatment each day.
  # Purpose: On the first day of the year, this function determines the
  #          number of dogs in each demographic category that will receive
  #          each treatment.
  
  treatmentCount <- matrix(0, nrow=365, ncol=24)
  colnames(treatmentCount) <- c('vaccPuppyMale', 'vaccPuppyFemale',
                                'vaccAdultMale', 'vaccAdultFemale', 
                                'vaccJuvMale', 'vaccJuvFemale',
                                'contraPuppyMale', 'contraPuppyFemale',
                                'contraAdultMale', 'contraAdultFemale',
                                'contraJuvMale', 'contraJuvFemale',
                                'sterPuppyMale', 'sterPuppyFemale',
                                'sterAdultMale', 'sterAdultFemale', 
                                'sterJuvMale', 'sterJuvFemale',
                                'euthPuppyMale', 'euthPuppyFemale',
                                'euthAdultMale', 'euthAdultFemale', 
                                'euthJuvMale', 'euthJuvFemale')
  daysVaccPuppyMale <- sample(managementDays, 
                              annualStrategy['vaccPuppyMale'], 
                              replace=TRUE)
  treatmentCount[sort(unique(daysVaccPuppyMale)), 1] <- 
    table(daysVaccPuppyMale)
  daysVaccPuppyFemale <- sample(managementDays, 
                                annualStrategy['vaccPuppyFemale'], 
                                replace=TRUE)
  treatmentCount[sort(unique(daysVaccPuppyFemale)), 2] <- 
    table(daysVaccPuppyFemale)
  daysVaccAdultMale <- sample(managementDays, 
                              annualStrategy['vaccAdultMale'], 
                              replace=TRUE)
  treatmentCount[sort(unique(daysVaccAdultMale)), 3] <- 
    table(daysVaccAdultMale)
  daysVaccAdultFemale <- sample(managementDays, 
                                annualStrategy['vaccAdultFemale'], 
                                replace=TRUE)
  treatmentCount[sort(unique(daysVaccAdultFemale)), 4] <- 
    table(daysVaccAdultFemale)
  daysVaccJuvMale <- sample(managementDays, 
                            annualStrategy['vaccJuvMale'], 
                            replace=TRUE)
  treatmentCount[sort(unique(daysVaccJuvMale)), 5] <- 
    table(daysVaccJuvMale)
  daysVaccJuvFemale <- sample(managementDays, 
                              annualStrategy['vaccJuvFemale'], 
                              replace=TRUE)
  treatmentCount[sort(unique(daysVaccJuvFemale)), 6] <- 
    table(daysVaccJuvFemale)
  daysContraPuppyMale <- sample(managementDays, 
                                annualStrategy['contraPuppyMale'], 
                                replace=TRUE)
  treatmentCount[sort(unique(daysContraPuppyMale)), 7] <- 
    table(daysContraPuppyMale)
  daysContraPuppyFemale <- sample(managementDays, 
                                  annualStrategy['contraPuppyFemale'], 
                                  replace=TRUE)
  treatmentCount[sort(unique(daysContraPuppyFemale)), 8] <- 
    table(daysContraPuppyFemale)
  daysContraAdultMale <- sample(managementDays, 
                                annualStrategy['contraAdultMale'], 
                                replace=TRUE)
  treatmentCount[sort(unique(daysContraAdultMale)), 9] <- 
    table(daysContraAdultMale)
  daysContraAdultFemale <- sample(managementDays, 
                                  annualStrategy['contraAdultFemale'], 
                                  replace=TRUE)
  treatmentCount[sort(unique(daysContraAdultFemale)), 10] <- 
    table(daysContraAdultFemale)
  daysContraJuvMale <- sample(managementDays, 
                              annualStrategy['contraJuvMale'], 
                              replace=TRUE)
  treatmentCount[sort(unique(daysContraJuvMale)), 11] <- 
    table(daysContraJuvMale)
  daysContraJuvFemale <- sample(managementDays, 
                                annualStrategy['contraJuvFemale'], 
                                replace=TRUE)
  treatmentCount[sort(unique(daysContraJuvFemale)), 12] <- 
    table(daysContraJuvFemale)
  daysSterPuppyMale <- sample(managementDays, 
                              annualStrategy['sterPuppyMale'], 
                              replace=TRUE)
  treatmentCount[sort(unique(daysSterPuppyMale)), 13] <- 
    table(daysSterPuppyMale)
  daysSterPuppyFemale <- sample(managementDays, 
                                annualStrategy['sterPuppyFemale'], 
                                replace=TRUE)
  treatmentCount[sort(unique(daysSterPuppyFemale)), 14] <- 
    table(daysSterPuppyFemale)
  daysSterAdultMale <- sample(managementDays, 
                              annualStrategy['sterAdultMale'], 
                              replace=TRUE)
  treatmentCount[sort(unique(daysSterAdultMale)), 15] <- 
    table(daysSterAdultMale)
  daysSterAdultFemale <- sample(managementDays, 
                                annualStrategy['sterAdultFemale'], 
                                replace=TRUE)
  treatmentCount[sort(unique(daysSterAdultFemale)), 16] <- 
    table(daysSterAdultFemale)
  daysSterJuvMale <- sample(managementDays, 
                            annualStrategy['sterJuvMale'], 
                            replace=TRUE)
  treatmentCount[sort(unique(daysSterJuvMale)), 17] <- 
    table(daysSterJuvMale)
  daysSterJuvFemale <- sample(managementDays, 
                              annualStrategy['sterJuvFemale'], 
                              replace=TRUE)
  treatmentCount[sort(unique(daysSterJuvFemale)), 18] <- 
    table(daysSterJuvFemale)
  daysEuthPuppyMale <- sample(managementDays, 
                              annualStrategy['euthPuppyMale'], 
                              replace=TRUE)
  treatmentCount[sort(unique(daysEuthPuppyMale)), 19] <- 
    table(daysEuthPuppyMale)
  daysEuthPuppyFemale <- sample(managementDays, 
                                annualStrategy['euthPuppyFemale'], 
                                replace=TRUE)
  treatmentCount[sort(unique(daysEuthPuppyFemale)), 20] <- 
    table(daysEuthPuppyFemale)
  daysEuthAdultMale <- sample(managementDays, 
                              annualStrategy['euthAdultMale'], 
                              replace=TRUE)
  treatmentCount[sort(unique(daysEuthAdultMale)), 21] <- 
    table(daysEuthAdultMale)
  daysEuthAdultFemale <- sample(managementDays, 
                                annualStrategy['euthAdultFemale'], 
                                replace=TRUE)
  treatmentCount[sort(unique(daysEuthAdultFemale)), 22] <- 
    table(daysEuthAdultFemale)
  daysEuthJuvMale <- sample(managementDays, 
                            annualStrategy['euthJuvMale'], 
                            replace=TRUE)
  treatmentCount[sort(unique(daysEuthJuvMale)), 23] <- 
    table(daysEuthJuvMale)
  daysEuthJuvFemale <- sample(managementDays, 
                              annualStrategy['euthJuvFemale'],
                              replace=TRUE)
  treatmentCount[sort(unique(daysEuthJuvFemale)), 24] <- 
    table(daysEuthJuvFemale)
  
  return(treatmentCount)
} 
########################################

########################################
if(boosterGiven == FALSE) {
  ManagementFunction <- function(d) {
    # Arguments: day of the year
    # Return:    An updated population matrix
    # Purpose:   Carries out management and adjusts population matrix 
    #            accordingly
    
    # Puppy male management:
    if(treatmentCount[d, 'euthPuppyMale'] > 0) {
      
      # Euthanize puppy males:
      currentAbundance  <- nrow(popMatrix)
      x <- rep(1, currentAbundance)
      abundanceSequence <- seq(1, currentAbundance)
      euth <- sample(seq(1, currentAbundance)[popMatrix[, 'puppy']==1 & 
                                                popMatrix[, 'female']==0], 
                     min(treatmentCount[d, 'euthPuppyMale'],  
                         length(seq(1,currentAbundance)[
                           popMatrix[, 'puppy']==1 & 
                             popMatrix[, 'female']==0])))
      if(length(euth) != 0) {
        popMatrix <- popMatrix[!abundanceSequence %in% euth, , drop=FALSE]
      }
    } else {
      idSeq <- seq(1, nrow(popMatrix))
      
      # Contracept puppy males:
      if(treatmentCount[d, 'contraPuppyMale'] > 0) {
        feasible <- idSeq[popMatrix[, 'puppy']==1 & 
                            popMatrix[, 'female']==0 &
                            popMatrix[, 'sterilized']==0 &
                            (popMatrix[, 'contracepted']==0 | 
                               popMatrix[, 'timeContra'] > d)]
        contra <- sample(feasible, min(length(feasible), 
                                       treatmentCount[d, 'contraPuppyMale']))
        popMatrix[contra, 'contracepted'] <- 1
        popMatrix[contra, 'timeContra']   <- 0
      } else {
        
        # Sterilize puppy males:
        feasible <- idSeq[popMatrix[, 'puppy']==1 & 
                            popMatrix[, 'female']==0 &
                            popMatrix[, 'sterilized']==0 &
                            (popMatrix[, 'contracepted']==0 | 
                               popMatrix[, 'timeContra'] > d)]
        ster <- sample(feasible, min(length(feasible), 
                                     treatmentCount[d, 'sterPuppyMale']))
        popMatrix[ster, 'sterilized'] <- 1
      }
      
      # Vaccinate puppy males:
      feasible <- idSeq[popMatrix[, 'puppy']==1 & 
                          popMatrix[, 'female']==0 &
                          (popMatrix[, 'vaccinated']==0 | 
                             popMatrix[, 'timeVacc'] > d)]
      vacc <- sample(feasible, min(length(feasible), 
                                   treatmentCount[d, 'vaccPuppyMale']))
      popMatrix[vacc, 'vaccinated'] <- 1
      popMatrix[vacc, 'timeVacc']   <- 0
    }
    
    # Puppy female management:
    if(treatmentCount[d, 'euthPuppyFemale'] > 0) {
      
      # Euthanize puppy females:
      currentAbundance  <- nrow(popMatrix)
      x <- rep(1, currentAbundance)
      abundanceSequence <- seq(1, currentAbundance)
      euth <- sample(seq(1,currentAbundance)[popMatrix[, 'puppy']==1 & 
                                               popMatrix[, 'female']==1], 
                     min(treatmentCount[d, 'euthPuppyFemale'],  
                         length(seq(
                           1,currentAbundance)[popMatrix[, 'puppy']==1 & 
                                                 popMatrix[, 'female']==1])))
      if(length(euth) != 0) {
        popMatrix <- popMatrix[!abundanceSequence %in% euth, , drop=FALSE]
      }
    } else {
      idSeq <- seq(1, nrow(popMatrix))
      
      # Contracept puppy females:
      if(treatmentCount[d, 'contraPuppyFemale'] > 0) {
        feasible <- idSeq[popMatrix[, 'puppy']==1 & 
                            popMatrix[, 'female']==1 &
                            popMatrix[, 'sterilized']==0 &
                            (popMatrix[, 'contracepted']==0 | 
                               popMatrix[, 'timeContra'] > d)]
        contra <- sample(feasible, 
                         min(length(feasible), 
                             treatmentCount[d, 'contraPuppyFemale']))
        popMatrix[contra, 'contracepted'] <- 1
        popMatrix[contra, 'timeContra']   <- 0
      } else {
        
        # Sterilize puppy females:
        feasible <- idSeq[popMatrix[, 'puppy']==1 & 
                            popMatrix[, 'female']==1 &
                            popMatrix[, 'sterilized']==0 &
                            (popMatrix[, 'contracepted']==0 | 
                               popMatrix[, 'timeContra'] > d)]
        ster <- sample(feasible, 
                       min(length(feasible), 
                           treatmentCount[d, 'sterPuppyFemale']))
        popMatrix[ster, 'sterilized'] <- 1
      }
      
      # Vaccinate puppy females:
      feasible <- idSeq[popMatrix[, 'puppy']==1 & 
                          popMatrix[, 'female']==1 &
                          (popMatrix[, 'vaccinated']==0 | 
                             popMatrix[, 'timeVacc'] > d)]
      vacc <- sample(feasible, 
                     min(length(feasible), 
                         treatmentCount[d, 'vaccPuppyFemale']))
      popMatrix[vacc, 'vaccinated'] <- 1
      popMatrix[vacc, 'timeVacc']   <- 0
    }
    
    # Adult male management:
    if(treatmentCount[d, 'euthAdultMale'] > 0) {
      
      # Euthanize adult males:
      currentAbundance  <- nrow(popMatrix)
      x <- rep(1, currentAbundance)
      abundanceSequence <- seq(1, currentAbundance)
      euth <- sample(seq(1, currentAbundance)[popMatrix[, 'adult']==1 & 
                                                popMatrix[, 'female']==0], 
                     min(treatmentCount[d, 'euthAdultMale'],  
                         length(seq(1,currentAbundance)[
                           popMatrix[, 'adult']==1 & 
                             popMatrix[, 'female']==0])))
      if(length(euth) != 0) {
        popMatrix <- popMatrix[!abundanceSequence %in% euth, , drop=FALSE]
      }
    } else {
      idSeq <- seq(1, nrow(popMatrix))
      
      # Contracept adult males:
      if(treatmentCount[d, 'contraAdultMale'] > 0) {
        feasible <- idSeq[popMatrix[, 'adult']==1 & 
                            popMatrix[, 'female']==0 &
                            popMatrix[, 'sterilized']==0 &
                            (popMatrix[, 'contracepted']==0 | 
                               popMatrix[, 'timeContra'] > d)]
        contra <- sample(feasible, min(length(feasible), 
                                       treatmentCount[d, 'contraAdultMale']))
        popMatrix[contra, 'contracepted'] <- 1
        popMatrix[contra, 'timeContra']   <- 0
      } else {
        
        # Sterilize adult males:
        feasible <- idSeq[popMatrix[, 'adult']==1 & 
                            popMatrix[, 'female']==0 &
                            popMatrix[, 'sterilized']==0 &
                            (popMatrix[, 'contracepted']==0 | 
                               popMatrix[, 'timeContra'] > d)]
        ster <- sample(feasible, min(length(feasible), 
                                     treatmentCount[d, 'sterAdultMale']))
        popMatrix[ster, 'sterilized'] <- 1
      }
      
      # Vaccinate adult males:
      feasible <- idSeq[popMatrix[, 'adult']==1 & 
                          popMatrix[, 'female']==0 &
                          (popMatrix[, 'vaccinated']==0 | 
                             popMatrix[, 'timeVacc'] > d)]
      vacc <- sample(feasible, min(length(feasible), 
                                   treatmentCount[d, 'vaccAdultMale']))
      popMatrix[vacc, 'vaccinated'] <- 1
      popMatrix[vacc, 'timeVacc']   <- 0
    }
    
    # Adult female management:
    if(treatmentCount[d, 'euthAdultFemale'] > 0) {
      
      # Euthanize adult females:
      currentAbundance  <- nrow(popMatrix)
      x <- rep(1, currentAbundance)
      abundanceSequence <- seq(1, currentAbundance)
      euth <- sample(seq(1,currentAbundance)[popMatrix[, 'adult']==1 & 
                                               popMatrix[, 'female']==1], 
                     min(treatmentCount[d, 'euthAdultFemale'],  
                         length(seq(
                           1,currentAbundance)[popMatrix[, 'adult']==1 & 
                                                 popMatrix[, 'female']==1])))
      if(length(euth) != 0) {
        popMatrix <- popMatrix[!abundanceSequence %in% euth, , drop=FALSE]
      }
    } else {
      idSeq <- seq(1, nrow(popMatrix))
      
      # Contracept adult females:
      if(treatmentCount[d, 'contraAdultFemale'] > 0) {
        feasible <- idSeq[popMatrix[, 'adult']==1 & 
                            popMatrix[, 'female']==1 &
                            popMatrix[, 'sterilized']==0 &
                            (popMatrix[, 'contracepted']==0 | 
                               popMatrix[, 'timeContra'] > d)]
        contra <- sample(feasible, 
                         min(length(feasible), 
                             treatmentCount[d, 'contraAdultFemale']))
        popMatrix[contra, 'contracepted'] <- 1
        popMatrix[contra, 'timeContra']   <- 0
      } else {
        
        # Sterilize adult females:
        feasible <- idSeq[popMatrix[, 'adult']==1 & 
                            popMatrix[, 'female']==1 &
                            popMatrix[, 'sterilized']==0 &
                            (popMatrix[, 'contracepted']==0 | 
                               popMatrix[, 'timeContra'] > d)]
        ster <- sample(feasible, 
                       min(length(feasible), 
                           treatmentCount[d, 'sterAdultFemale']))
        popMatrix[ster, 'sterilized'] <- 1
      }
      
      # Vaccinate adult females:
      feasible <- idSeq[popMatrix[, 'adult']==1 & 
                          popMatrix[, 'female']==1 &
                          (popMatrix[, 'vaccinated']==0 | 
                             popMatrix[, 'timeVacc'] > d)]
      vacc <- sample(feasible, 
                     min(length(feasible), 
                         treatmentCount[d, 'vaccAdultFemale']))
      popMatrix[vacc, 'vaccinated'] <- 1
      popMatrix[vacc, 'timeVacc']   <- 0
    }
    
    # Juvenile male management:
    if(treatmentCount[d, 'euthJuvMale'] > 0) {
      
      # Euthanize juvenile males:
      currentAbundance  <- nrow(popMatrix)
      x <- rep(1, currentAbundance)
      abundanceSequence <- seq(1, currentAbundance)
      euth <- sample(seq(1,currentAbundance)[popMatrix[, 'adult']==0 & 
                                               popMatrix[, 'puppy']==0 &
                                               popMatrix[, 'female']==0], 
                     min(treatmentCount[d, 'euthJuvMale'],  
                         length(seq(
                           1,currentAbundance)[popMatrix[, 'adult']==0 & 
                                                 popMatrix[, 'puppy']==0 &
                                                 popMatrix[, 'female']==0])))
      if(length(euth) != 0) {
        popMatrix <- popMatrix[!abundanceSequence %in% euth, , drop=FALSE]
      }
    } else {
      idSeq <- seq(1, nrow(popMatrix))
      
      # contra juvenile males
      if(treatmentCount[d, 'contraJuvMale'] > 0) {
        feasible <- idSeq[popMatrix[, 'adult']==0 & popMatrix[, 'puppy']==0 &
                            popMatrix[, 'female']==0 &
                            popMatrix[, 'sterilized']==0 &
                            (popMatrix[, 'contracepted']==0 | 
                               popMatrix[, 'timeContra'] > d)]
        contra <- sample(feasible, 
                         min(length(feasible), 
                             treatmentCount[d, 'contraJuvMale']))
        popMatrix[contra, 'contracepted'] <- 1
        popMatrix[contra, 'timeContra']   <- 0
      } else { 
        
        # Sterilize juvenile males:
        feasible <- idSeq[popMatrix[, 'adult']==0 & popMatrix[, 'puppy']==0 &
                            popMatrix[, 'female']==0 &
                            popMatrix[, 'sterilized']==0 &
                            (popMatrix[, 'contracepted']==0 | 
                               popMatrix[, 'timeContra'] > d)]
        ster <- sample(feasible, 
                       min(length(feasible), 
                           treatmentCount[d, 'sterJuvMale']))
        popMatrix[ster, 'sterilized'] <- 1
      }
      
      # Vaccinate juvenile males:
      feasible <- idSeq[popMatrix[, 'adult']==0 & popMatrix[, 'puppy']==0 &
                          popMatrix[, 'female']==0 &
                          (popMatrix[, 'vaccinated']==0 | 
                             popMatrix[, 'timeVacc'] > d)]
      vacc <- sample(feasible, min(length(feasible), 
                                   treatmentCount[d, 'vaccJuvMale']))
      popMatrix[vacc, 'vaccinated'] <- 1
      popMatrix[vacc, 'timeVacc']   <- 0
    }
    
    # Juvenile female management:
    if(treatmentCount[d, 'euthJuvFemale'] > 0) {
      
      # Euthanize juvenile females:
      currentAbundance  <- nrow(popMatrix)
      x <- rep(1, currentAbundance)
      abundanceSequence <- seq(1, currentAbundance)
      euth <- sample(seq(1,currentAbundance)[popMatrix[, 'adult']==0 & 
                                               popMatrix[, 'puppy']==0 &
                                               popMatrix[, 'female']==1], 
                     min(treatmentCount[d, 'euthJuvFemale'],  
                         length(seq(
                           1,currentAbundance)[popMatrix[, 'adult']==0 & 
                                                 popMatrix[, 'puppy']==0 &
                                                 popMatrix[, 'female']==1])))
      if(length(euth) != 0) {
        popMatrix <- popMatrix[!abundanceSequence %in% euth, , drop=FALSE]
      }
    } else {
      idSeq <- seq(1, nrow(popMatrix))
      
      # Contracept juvenile females:
      if(treatmentCount[d, 'contraJuvFemale']) {
        feasible <- idSeq[popMatrix[, 'adult']==0 & popMatrix[, 'puppy']==0 &
                            popMatrix[, 'female']==1 &
                            popMatrix[, 'sterilized']==0 &
                            (popMatrix[, 'contracepted']==0 | 
                               popMatrix[, 'timeContra'] > d)]
        contra <- sample(feasible, 
                         min(length(feasible), 
                             treatmentCount[d, 'contraJuvFemale']))
        popMatrix[contra, 'contracepted'] <- 1
        popMatrix[contra, 'timeContra']   <- 0
      } else {
        
        # Sterilize juvenile females:
        feasible <- idSeq[popMatrix[, 'adult']==0 & popMatrix[, 'puppy']==0 &
                            popMatrix[, 'female']==1 &
                            popMatrix[, 'sterilized']==0 &
                            (popMatrix[, 'contracepted']==0 | 
                               popMatrix[, 'timeContra'] > d)]
        ster <- sample(feasible, 
                       min(length(feasible), 
                           treatmentCount[d, 'sterJuvFemale']))
        popMatrix[ster, 'sterilized'] <- 1
        
      }
      
      # Vaccinate juvenile females:
      feasible <- idSeq[popMatrix[, 'adult']==0 & popMatrix[, 'puppy']==0 &
                          popMatrix[, 'female']==1 &
                          (popMatrix[, 'vaccinated']==0 | 
                             popMatrix[, 'timeVacc'] > d)]
      vacc <- sample(feasible, 
                     min(length(feasible), 
                         treatmentCount[d, 'vaccJuvFemale']))
      popMatrix[vacc, 'vaccinated'] <- 1
      popMatrix[vacc, 'timeVacc']   <- 0
      
    }
    
    # Turn off vaccinated and contracepted when time limit reached: 
    popMatrix[popMatrix[, 'timeVacc'] == timeVaccineEffective, 
              'vaccinated'] <- 0
    popMatrix[(popMatrix[, 'timeContra'] == timeContraEffectiveFemales &
                 popMatrix[, 'female'] == 1), 'contracepted'] <- 0
    popMatrix[(popMatrix[, 'timeContra'] == timeContraEffectiveMales &
                 popMatrix[, 'female'] == 0), 'contracepted'] <- 0
    
    return(popMatrix)
  } 
  
# end if statement
} else {
  ManagementFunction <- function(d) {
    # Arguments: day of the year
    # Return:    An updated population matrix
    # Purpose:   Carries out management and adjusts population matrix 
    #            accordingly
    
    # Puppy male management:
    if(treatmentCount[d, 'euthPuppyMale'] > 0) {
      
      # Euthanize puppy males:
      currentAbundance  <- nrow(popMatrix)
      x <- rep(1, currentAbundance)
      abundanceSequence <- seq(1, currentAbundance)
      euth <- sample(seq(1, currentAbundance)[popMatrix[, 'puppy']==1 & 
                                                popMatrix[, 'female']==0], 
                     min(treatmentCount[d, 'euthPuppyMale'],  
                         length(seq(1,currentAbundance)[
                           popMatrix[, 'puppy']==1 & 
                             popMatrix[, 'female']==0])))
      if(length(euth) != 0) {
        popMatrix <- popMatrix[!abundanceSequence %in% euth, , drop=FALSE]
      }
    } else {
      idSeq <- seq(1, nrow(popMatrix))
      
      # Contracept puppy males:
      if(treatmentCount[d, 'contraPuppyMale'] > 0) {
        feasible <- idSeq[popMatrix[, 'puppy']==1 & 
                            popMatrix[, 'female']==0 &
                            popMatrix[, 'sterilized']==0 &
                            (popMatrix[, 'contracepted']==0 | 
                               popMatrix[, 'timeContra'] > d)]
        contra <- sample(feasible, min(length(feasible), 
                                       treatmentCount[d, 'contraPuppyMale']))
        popMatrix[contra, 'contracepted'] <- 1
        popMatrix[contra, 'timeContra']   <- 0
      } else {
        
        # Sterilize puppy males:
        feasible <- idSeq[popMatrix[, 'puppy']==1 & 
                            popMatrix[, 'female']==0 &
                            popMatrix[, 'sterilized']==0 &
                            (popMatrix[, 'contracepted']==0 | 
                               popMatrix[, 'timeContra'] > d)]
        ster <- sample(feasible, min(length(feasible), 
                                     treatmentCount[d, 'sterPuppyMale']))
        popMatrix[ster, 'sterilized'] <- 1
      }
      
      # Vaccinate puppy males:
      feasible <- idSeq[popMatrix[, 'puppy']==1 & 
                          popMatrix[, 'female']==0 &
                          popMatrix[, 'boosted']==0]
      vacc <- sample(feasible, min(length(feasible), 
                                   treatmentCount[d, 'vaccPuppyMale']))
      popMatrix[vacc, 'boosted']  <- 1
      popMatrix[vacc, 'timeVacc'] <- 0
    }
    
    # Puppy female management:
    if(treatmentCount[d, 'euthPuppyFemale'] > 0) {
      
      # Euthanize puppy females:
      currentAbundance  <- nrow(popMatrix)
      x <- rep(1, currentAbundance)
      abundanceSequence <- seq(1, currentAbundance)
      euth <- sample(seq(1,currentAbundance)[popMatrix[, 'puppy']==1 & 
                                               popMatrix[, 'female']==1], 
                     min(treatmentCount[d, 'euthPuppyFemale'],  
                         length(seq(
                           1,currentAbundance)[popMatrix[, 'puppy']==1 & 
                                                 popMatrix[, 'female']==1])))
      if(length(euth) != 0) {
        popMatrix <- popMatrix[!abundanceSequence %in% euth, , drop=FALSE]
      }
    } else {
      idSeq <- seq(1, nrow(popMatrix))
      
      # Contracept puppy females:
      if(treatmentCount[d, 'contraPuppyFemale'] > 0) {
        feasible <- idSeq[popMatrix[, 'puppy']==1 & 
                            popMatrix[, 'female']==1 &
                            popMatrix[, 'sterilized']==0 &
                            (popMatrix[, 'contracepted']==0 | 
                               popMatrix[, 'timeContra'] > d)]
        contra <- sample(feasible, 
                         min(length(feasible), 
                             treatmentCount[d, 'contraPuppyFemale']))
        popMatrix[contra, 'contracepted'] <- 1
        popMatrix[contra, 'timeContra']   <- 0
      } else {
        
        # Sterilize puppy females:
        feasible <- idSeq[popMatrix[, 'puppy']==1 & 
                            popMatrix[, 'female']==1 &
                            popMatrix[, 'sterilized']==0 &
                            (popMatrix[, 'contracepted']==0 | 
                               popMatrix[, 'timeContra'] > d)]
        ster <- sample(feasible, 
                       min(length(feasible), 
                           treatmentCount[d, 'sterPuppyFemale']))
        popMatrix[ster, 'sterilized'] <- 1
      }
      
      # Vaccinate puppy females:
      feasible <- idSeq[popMatrix[, 'puppy']==1 & 
                          popMatrix[, 'female']==1 &
                          popMatrix[, 'boosted']==0]
      vacc <- sample(feasible, 
                     min(length(feasible), 
                         treatmentCount[d, 'vaccPuppyFemale']))
      popMatrix[vacc, 'boosted']  <- 1
      popMatrix[vacc, 'timeVacc'] <- 0
    }
    
    # Adult male management:
    if(treatmentCount[d, 'euthAdultMale'] > 0) {
      
      # Euthanize adult males:
      currentAbundance  <- nrow(popMatrix)
      x <- rep(1, currentAbundance)
      abundanceSequence <- seq(1, currentAbundance)
      euth <- sample(seq(1, currentAbundance)[popMatrix[, 'adult']==1 & 
                                                popMatrix[, 'female']==0], 
                     min(treatmentCount[d, 'euthAdultMale'],  
                         length(seq(1,currentAbundance)[
                           popMatrix[, 'adult']==1 & 
                             popMatrix[, 'female']==0])))
      if(length(euth) != 0) {
        popMatrix <- popMatrix[!abundanceSequence %in% euth, , drop=FALSE]
      }
    } else {
      idSeq <- seq(1, nrow(popMatrix))
      
      # Contracept adult males:
      if(treatmentCount[d, 'contraAdultMale'] > 0) {
        feasible <- idSeq[popMatrix[, 'adult']==1 & 
                            popMatrix[, 'female']==0 &
                            popMatrix[, 'sterilized']==0 &
                            (popMatrix[, 'contracepted']==0 | 
                               popMatrix[, 'timeContra'] > d)]
        contra <- sample(feasible, min(length(feasible), 
                                       treatmentCount[d, 'contraAdultMale']))
        popMatrix[contra, 'contracepted'] <- 1
        popMatrix[contra, 'timeContra']   <- 0
      } else {
        
        # Sterilize adult males:
        feasible <- idSeq[popMatrix[, 'adult']==1 & 
                            popMatrix[, 'female']==0 &
                            popMatrix[, 'sterilized']==0 &
                            (popMatrix[, 'contracepted']==0 | 
                               popMatrix[, 'timeContra'] > d)]
        ster <- sample(feasible, min(length(feasible), 
                                     treatmentCount[d, 'sterAdultMale']))
        popMatrix[ster, 'sterilized'] <- 1
      }
      
      # Vaccinate adult males:
      feasible <- idSeq[popMatrix[, 'adult']==1 & 
                          popMatrix[, 'female']==0 &
                          popMatrix[, 'boosted']==0]
      vacc <- sample(feasible, min(length(feasible), 
                                   treatmentCount[d, 'vaccAdultMale']))
      popMatrix[vacc, 'boosted']  <- 1
      popMatrix[vacc, 'timeVacc'] <- 0
    }
    
    # Adult female management:
    if(treatmentCount[d, 'euthAdultFemale'] > 0) {
      
      # Euthanize adult females:
      currentAbundance  <- nrow(popMatrix)
      x <- rep(1, currentAbundance)
      abundanceSequence <- seq(1, currentAbundance)
      euth <- sample(seq(1,currentAbundance)[popMatrix[, 'adult']==1 & 
                                               popMatrix[, 'female']==1], 
                     min(treatmentCount[d, 'euthAdultFemale'],  
                         length(seq(
                           1,currentAbundance)[popMatrix[, 'adult']==1 & 
                                                 popMatrix[, 'female']==1])))
      if(length(euth) != 0) {
        popMatrix <- popMatrix[!abundanceSequence %in% euth, , drop=FALSE]
      }
    } else {
      idSeq <- seq(1, nrow(popMatrix))
      
      # Contracept adult females:
      if(treatmentCount[d, 'contraAdultFemale'] > 0) {
        feasible <- idSeq[popMatrix[, 'adult']==1 & 
                            popMatrix[, 'female']==1 &
                            popMatrix[, 'sterilized']==0 &
                            (popMatrix[, 'contracepted']==0 | 
                               popMatrix[, 'timeContra'] > d)]
        contra <- sample(feasible, 
                         min(length(feasible), 
                             treatmentCount[d, 'contraAdultFemale']))
        popMatrix[contra, 'contracepted'] <- 1
        popMatrix[contra, 'timeContra']   <- 0
      } else {
        
        # Sterilize adult females:
        feasible <- idSeq[popMatrix[, 'adult']==1 & 
                            popMatrix[, 'female']==1 &
                            popMatrix[, 'sterilized']==0 &
                            (popMatrix[, 'contracepted']==0 | 
                               popMatrix[, 'timeContra'] > d)]
        ster <- sample(feasible, 
                       min(length(feasible), 
                           treatmentCount[d, 'sterAdultFemale']))
        popMatrix[ster, 'sterilized'] <- 1
      }
      
      # Vaccinate adult females:
      feasible <- idSeq[popMatrix[, 'adult']==1 & 
                          popMatrix[, 'female']==1 &
                          popMatrix[, 'boosted']==0]
      vacc <- sample(feasible, 
                     min(length(feasible), 
                         treatmentCount[d, 'vaccAdultFemale']))
      popMatrix[vacc, 'boosted']  <- 1
      popMatrix[vacc, 'timeVacc'] <- 0
    }
    
    # Juvenile male management:
    if(treatmentCount[d, 'euthJuvMale'] > 0) {
      
      # Euthanize juvenile males:
      currentAbundance  <- nrow(popMatrix)
      x <- rep(1, currentAbundance)
      abundanceSequence <- seq(1, currentAbundance)
      euth <- sample(seq(1,currentAbundance)[popMatrix[, 'adult']==0 & 
                                               popMatrix[, 'puppy']==0 &
                                               popMatrix[, 'female']==0], 
                     min(treatmentCount[d, 'euthJuvMale'],  
                         length(seq(
                           1,currentAbundance)[popMatrix[, 'adult']==0 & 
                                                 popMatrix[, 'puppy']==0 &
                                                 popMatrix[, 'female']==0])))
      if(length(euth) != 0) {
        popMatrix <- popMatrix[!abundanceSequence %in% euth, , drop=FALSE]
      }
    } else {
      idSeq <- seq(1, nrow(popMatrix))
      
      # contra juvenile males
      if(treatmentCount[d, 'contraJuvMale'] > 0) {
        feasible <- idSeq[popMatrix[, 'adult']==0 & popMatrix[, 'puppy']==0 &
                            popMatrix[, 'female']==0 &
                            popMatrix[, 'sterilized']==0 &
                            (popMatrix[, 'contracepted']==0 | 
                               popMatrix[, 'timeContra'] > d)]
        contra <- sample(feasible, 
                         min(length(feasible), 
                             treatmentCount[d, 'contraJuvMale']))
        popMatrix[contra, 'contracepted'] <- 1
        popMatrix[contra, 'timeContra']   <- 0
      } else { 
        
        # Sterilize juvenile males:
        feasible <- idSeq[popMatrix[, 'adult']==0 & popMatrix[, 'puppy']==0 &
                            popMatrix[, 'female']==0 &
                            popMatrix[, 'sterilized']==0 &
                            (popMatrix[, 'contracepted']==0 | 
                               popMatrix[, 'timeContra'] > d)]
        ster <- sample(feasible, 
                       min(length(feasible), 
                           treatmentCount[d, 'sterJuvMale']))
        popMatrix[ster, 'sterilized'] <- 1
      }
      
      # Vaccinate juvenile males:
      feasible <- idSeq[popMatrix[, 'adult']==0 & popMatrix[, 'puppy']==0 &
                          popMatrix[, 'female']==0 &
                          popMatrix[, 'boosted']==0]
      vacc <- sample(feasible, min(length(feasible), 
                                   treatmentCount[d, 'vaccJuvMale']))
      popMatrix[vacc, 'boosted']  <- 1
      popMatrix[vacc, 'timeVacc'] <- 0
    }
    
    # Juvenile female management:
    if(treatmentCount[d, 'euthJuvFemale'] > 0) {
      
      # Euthanize juvenile females:
      currentAbundance  <- nrow(popMatrix)
      x <- rep(1, currentAbundance)
      abundanceSequence <- seq(1, currentAbundance)
      euth <- sample(seq(1,currentAbundance)[popMatrix[, 'adult']==0 & 
                                               popMatrix[, 'puppy']==0 &
                                               popMatrix[, 'female']==1], 
                     min(treatmentCount[d, 'euthJuvFemale'],  
                         length(seq(
                           1,currentAbundance)[popMatrix[, 'adult']==0 & 
                                                 popMatrix[, 'puppy']==0 &
                                                 popMatrix[, 'female']==1])))
      if(length(euth) != 0) {
        popMatrix <- popMatrix[!abundanceSequence %in% euth, , drop=FALSE]
      }
    } else {
      idSeq <- seq(1, nrow(popMatrix))
      
      # Contracept juvenile females:
      if(treatmentCount[d, 'contraJuvFemale']) {
        feasible <- idSeq[popMatrix[, 'adult']==0 & popMatrix[, 'puppy']==0 &
                            popMatrix[, 'female']==1 &
                            popMatrix[, 'sterilized']==0 &
                            (popMatrix[, 'contracepted']==0 | 
                               popMatrix[, 'timeContra'] > d)]
        contra <- sample(feasible, 
                         min(length(feasible), 
                             treatmentCount[d, 'contraJuvFemale']))
        popMatrix[contra, 'contracepted'] <- 1
        popMatrix[contra, 'timeContra']   <- 0
      } else {
        
        # Sterilize juvenile females:
        feasible <- idSeq[popMatrix[, 'adult']==0 & popMatrix[, 'puppy']==0 &
                            popMatrix[, 'female']==1 &
                            popMatrix[, 'sterilized']==0 &
                            (popMatrix[, 'contracepted']==0 | 
                               popMatrix[, 'timeContra'] > d)]
        ster <- sample(feasible, 
                       min(length(feasible), 
                           treatmentCount[d, 'sterJuvFemale']))
        popMatrix[ster, 'sterilized'] <- 1
        
      }
      
      # Vaccinate juvenile females:
      feasible <- idSeq[popMatrix[, 'adult']==0 & popMatrix[, 'puppy']==0 &
                          popMatrix[, 'female']==1 &
                          popMatrix[, 'boosted']==0]
      vacc <- sample(feasible, 
                     min(length(feasible), 
                         treatmentCount[d, 'vaccJuvFemale']))
      popMatrix[vacc, 'boosted']  <- 1
      popMatrix[vacc, 'timeVacc'] <- 0
      
    }
    
    # Adjust to account for booster:
    idSeq <- seq(1, nrow(popMatrix))
    swap <- idSeq[popMatrix[, 'vaccinated'] == 0 & 
                    popMatrix[, 'boosted'] == 1]
    popMatrix[swap, 'vaccinated'] <- 1
    popMatrix[swap, 'boosted'] <- 0
    
    # Turn off vaccinated when time limit reached: 
    popMatrix[(popMatrix[, 'timeVacc'] == timeVaccineEffective &
                 popMatrix[, 'boosted'] == 0), 'vaccinated'] <- 0
    
    # Turn off boosted and vaccinated when time limit reached:
    popMatrix[(popMatrix[, 'timeVacc'] == timeBoosterEffective &
                 popMatrix[, 'boosted'] == 1), 'vaccinated'] <- 0
    popMatrix[(popMatrix[, 'timeVacc'] == timeBoosterEffective &
                 popMatrix[, 'boosted'] == 1), 'boosted'] <- 0
    
    # Turn off contracepted when time limit reached:
    popMatrix[(popMatrix[, 'timeContra'] == timeContraEffectiveFemales &
                 popMatrix[, 'female'] == 1), 'contracepted'] <- 0
    popMatrix[(popMatrix[, 'timeContra'] == timeContraEffectiveMales &
                 popMatrix[, 'female'] == 0), 'contracepted'] <- 0
    
    return(popMatrix)
  } 
  # end else statement
}
########################################

########################################
TimeFunction <- function() {
  # Arguments: None.
  # Return:    An updated population matrix.
  # Purpose:   Updates time-related columns in the population matrix.
  
  popMatrix[, 'age']           <- popMatrix[, 'age'] + 1
  popMatrix[, 'timeVacc']      <- popMatrix[, 'timeVacc'] + 1
  popMatrix[, 'timeContra']    <- popMatrix[, 'timeContra'] + 1
  popMatrix[, 'timeExposed']   <- popMatrix[, 'timeExposed'] + 1
  popMatrix[, 'timeInfective'] <- popMatrix[, 'timeInfective'] + 1
  
  return(popMatrix)
}
########################################

########################################
  # Loop through iterations:
for(i in 1:iterations) {
  popMatrix <- InitialPopulation()
  
  # Loop through years:
  for(j in 1:simulationYears) {
    annualStrategy <- round(AnnualStrategy())
    treatmentCount <- StrategySchedule()
      
    # Loop through days of the year
    for(d in 1:365) {
      popMatrix[, 'month'] <- monthSeries[d]
      resultsMatrix[(365 * (j-1) + d), ,i] <- CensusFunction()
      popMatrix <- MortalityFunction()
      popMatrix <- ReproductionFunction(d)
      popMatrix <- ImmigrationFunction()
      popMatrix <- DiseaseProgressionFunction()
      popMatrix <- DiseaseSpreadFunction()
      tempVacc <- sum(popMatrix[, 'vaccinated'])
      popMatrix <- ManagementFunction(d)
      # Record new vaccinations:
      resultsMatrix[(365 * (j-1) + d), 'newlyVaccinated', i] <- 
        sum(popMatrix[, 'vaccinated']) - tempVacc
      popMatrix <- TimeFunction()
    }  # close d for loop
  }  # close j for loop
}  # close i for loop
########################################

# to access resultsMatrix:
#   resultsMatrix[day, results series, iteration]

# getting means across iterations:
meanResults <- matrix(NA, nrow=simulationEnd, ncol=length(censusSeries))
for(i in 1:simulationEnd) {
  for(j in 1:length(censusSeries)) {
    meanResults[i, j] <- mean(resultsMatrix[i, j, ], na.rm=TRUE)
  }
}
names(resultsMatrix[1, , 1])    

