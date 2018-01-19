########################################################################################################################
# Primary author:     Aaron Anderson, National Wildlife Research Center
# Contact:            aaron.m.anderson@aphis.usda.gov
# Other contributors: Johan Kotze, Brody Hatch, Jordan Navin
# Description:        This code is the backend of a web app that can be used
#                     to forecast the results of canine rabies management
#                     in South Africa

# MIT License
# Copyright (c) 2017 Aaron Anderson
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.
########################################################################################################################


########################################################################################################################
# Note to user: install the following five packages:
#               shiny, ggplot2, gridExtra, gtable, grid
#               Do not install from with this script.

# Load required packages:
library(shiny)
library(ggplot2)
library(gridExtra)
library(gtable)
library(grid)
########################################################################################################################

########################################################################################################################
# Define server logic:
shinyServer(function(input, output, session) {

observe({
  # Move to graphical results tab when run button is pushed:
  if (input$run > 0) {
    updateTabsetPanel(session, 'mainTabs', selected = 'Model Output')
  } else {
    updateTabsetPanel(session, 'mainTabs', selected = 'Model Setup')
  }
})

# Run model:
getResultsMatrix <- eventReactive(input$run, {

  ########################################
  # Inputs:
  
  # Inputs for simulation:
  simulationYears <- 5
  iterations      <- input$iterations
  
  # Inputs for initial population:
  initialPopSize    <- input$initialPopSize
  initialFracAdult  <- input$initialFracAdult
  initialFracPup    <- input$initialFracPup
  initialFracFemale <- 0.38
  initialFracImmune <- 0.0
  initialFracContra <- 0.0
  initialFracVacc   <- 0.0
  initialFracSter   <- 0.0

  # Inputs for mortality:
  maxJuvAge          <- 299
  maxPuppyAge        <- 89
  maxAge             <- 4000
  carryingCap        <- input$carryingCap
  pupAnnMortProb     <- input$pupAnnMortProb
  juvAnnMortProb     <- input$juvAnnMortProb
  adultAnnMortProb   <- input$adultAnnMortProb
  emigrationProb     <- input$emigrationProb
  
  # Inputs for reproduction:
  immigrantDogs        <- input$immigrantDogs
  expLitterPer         <- input$expectedLittersPFY
  meanLitterSize       <- 4.4
  femalePupProb        <- 0.38
  fractionBirthPulse   <- input$fractionBirthPulse
  birthPulseVector     <- rep(0, 12)
  birthPulseVector[1]  <- !is.na(match('jan', input$birthPulseMonths)) 
  birthPulseVector[2]  <- !is.na(match('feb', input$birthPulseMonths)) 
  birthPulseVector[3]  <- !is.na(match('mar', input$birthPulseMonths)) 
  birthPulseVector[4]  <- !is.na(match('apr', input$birthPulseMonths)) 
  birthPulseVector[5]  <- !is.na(match('may', input$birthPulseMonths)) 
  birthPulseVector[6]  <- !is.na(match('jun', input$birthPulseMonths)) 
  birthPulseVector[7]  <- !is.na(match('jul', input$birthPulseMonths)) 
  birthPulseVector[8]  <- !is.na(match('aug', input$birthPulseMonths)) 
  birthPulseVector[9]  <- !is.na(match('sep', input$birthPulseMonths)) 
  birthPulseVector[10] <- !is.na(match('oct', input$birthPulseMonths)) 
  birthPulseVector[11] <- !is.na(match('nov', input$birthPulseMonths)) 
  birthPulseVector[12] <- !is.na(match('dec', input$birthPulseMonths)) 
  
  # Inputs for disease:
  monthsOfPressure      <- input$sequentialMonthsIntro
  dogsPerMonthExposed   <- input$dogsPerIntro
  monthInitIntroduction <- input$monthInitIntroduction
  timeLimitExposed      <- 22 
  timeLimitInfective    <- 3
  survivalProb          <- 0
  bitesPerRabidMean     <- input$transmissionParam
  bitesPerRabidShape    <- 1.33
  probInfectionFromBite <- 0.49

  # Inputs for benefits of management:
  bitesPerNonRabid     <- input$bitesPerNonRabid 
  bitesPerRabid        <- input$bitesPerRabid 
  PEPperNonRabidBite   <- input$PEPperNonRabidBite
  PEPperRabidBite      <- input$PEPperRabidBite
  costPerPEP           <- input$costPerPEP
  lifeLossPerRabidBite <- input$lifeLossPerRabidBite

  # Inputs for treatment costs:
  vaccineCost             <- input$vaccineCost
  contraceptionCostFemale <- input$contraceptionCostFemale
  contraceptionCostMale   <- input$contraceptionCostMale
  sterilizationCostFemale <- input$sterilizationCostFemale
  sterilizationCostMale   <- input$sterilizationCostMale
  euthanasiaCost          <- input$euthanasiaCost

  # Inputs for effectiveness of contraception and vaccination:
  timeVaccineEffective       <- input$timeVaccineEffective
  timeBoosterEffective       <- input$timeBoosterEffective
  timeContraEffectiveMales   <- input$timeContraEffectiveMales
  timeContraEffectiveFemales <- input$timeContraEffectiveFemales

  # Inputs for contact costs:
  contactCost25 <- input$contactCost25
  contactCost50 <- input$contactCost50
  contactCost75 <- input$contactCost75
  contactCost100 <- input$contactCost100

  # Input for budget years 1-5:    
  annualBudget     <- rep(0, 5)
  annualBudget[1]  <- input$annualBudget1
  annualBudget[2]  <- input$annualBudget2
  annualBudget[3]  <- input$annualBudget3
  annualBudget[4]  <- input$annualBudget4
  annualBudget[5]  <- input$annualBudget5
  
  # Inputs for strategy:
  vaccPuppyMale     <- !is.na(match('pm', input$vaccDemoInput))
  vaccPuppyFemale   <- !is.na(match('pf', input$vaccDemoInput))
  vaccAdultMale     <- !is.na(match('am', input$vaccDemoInput))  
  vaccAdultFemale   <- !is.na(match('af', input$vaccDemoInput))
  vaccJuvMale       <- !is.na(match('jm', input$vaccDemoInput))
  vaccJuvFemale     <- !is.na(match('jf', input$vaccDemoInput))
  contraPuppyMale   <- !is.na(match('pm', input$contraDemoInput))
  contraPuppyFemale <- !is.na(match('pf', input$contraDemoInput))
  contraAdultMale   <- !is.na(match('am', input$contraDemoInput))
  contraAdultFemale <- !is.na(match('af', input$contraDemoInput))
  contraJuvMale     <- !is.na(match('jm', input$contraDemoInput))
  contraJuvFemale   <- !is.na(match('jf', input$contraDemoInput))
  sterPuppyMale     <- !is.na(match('pm', input$sterDemoInput))
  sterPuppyFemale   <- !is.na(match('pf', input$sterDemoInput))
  sterAdultMale     <- !is.na(match('am', input$sterDemoInput))
  sterAdultFemale   <- !is.na(match('af', input$sterDemoInput))
  sterJuvMale       <- !is.na(match('jm', input$sterDemoInput))
  sterJuvFemale     <- !is.na(match('jf', input$sterDemoInput))
  euthPuppyMale     <- !is.na(match('pm', input$euthDemoInput))
  euthPuppyFemale   <- !is.na(match('pf', input$euthDemoInput))
  euthAdultMale     <- !is.na(match('am', input$euthDemoInput))
  euthAdultFemale   <- !is.na(match('af', input$euthDemoInput))
  euthJuvMale       <- !is.na(match('jm', input$euthDemoInput))
  euthJuvFemale     <- !is.na(match('jf', input$euthDemoInput))
  
  boosterGiven <- input$boosterGiven
  
  # Inputs for management timing:
  mgtMonthVector     <- rep(0, 12)
  mgtMonthVector[1]  <- !is.na(match('jan', input$managementMonths)) 
  mgtMonthVector[2]  <- !is.na(match('feb', input$managementMonths)) 
  mgtMonthVector[3]  <- !is.na(match('mar', input$managementMonths)) 
  mgtMonthVector[4]  <- !is.na(match('apr', input$managementMonths)) 
  mgtMonthVector[5]  <- !is.na(match('may', input$managementMonths)) 
  mgtMonthVector[6]  <- !is.na(match('jun', input$managementMonths)) 
  mgtMonthVector[7]  <- !is.na(match('jul', input$managementMonths)) 
  mgtMonthVector[8]  <- !is.na(match('aug', input$managementMonths)) 
  mgtMonthVector[9]  <- !is.na(match('sep', input$managementMonths)) 
  mgtMonthVector[10] <- !is.na(match('oct', input$managementMonths)) 
  mgtMonthVector[11] <- !is.na(match('nov', input$managementMonths)) 
  mgtMonthVector[12] <- !is.na(match('dec', input$managementMonths))
  ########################################

  ########################################
  # Misc preliminary calculations and assignments:
  
  # Get total number of days in simulation:
  simulationEnd   <- 365 * simulationYears
  
  # A vector of month number for use in seasonal timing:
  monthSeries <- c(rep(1, 31), rep(2, 28), rep(3, 31), rep(4, 30), 
                   rep(5, 31), rep(6, 30), rep(7, 31), rep(8, 31),
                   rep(9, 30), rep(10, 31), rep(11, 30), rep(12, 31)) 
  
  monthFirstDays <- rep(c(match(1, monthSeries), match(2, monthSeries), match(3, monthSeries), match(4, monthSeries),
                          match(5, monthSeries), match(6, monthSeries), match(7, monthSeries), match(8, monthSeries),
                          match(9, monthSeries), match(10, monthSeries), match(11, monthSeries), match(12, monthSeries)),
                        simulationYears)
  
  # Get days of each year that disease will be introduced:
  pressureMonths <- seq(monthInitIntroduction, monthInitIntroduction + monthsOfPressure - 1)  
  pressureYears <- (pressureMonths - 1) %/% 12 + 1
  pressureDays <- list()
  for (i in 1:simulationYears) {
    if (sum(pressureYears == i) > 0) {
      pressureDays[[i]] <- monthFirstDays[pressureMonths[pressureYears == i]]
    } else {
      pressureDays[[i]] <- 0
    }
  }
  flush.console()
  
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
    peakProb <- (fractionBirthPulse * expLitterPer) / peakDays
    offPeakDays <- sum((!birthPulseVector) * monthDayCount)
    offPeakProb <- ((1-fractionBirthPulse) * expLitterPer) / offPeakDays
    litterProbability <- rep(offPeakProb, 365)
    for(m in 1:12) {
      if(birthPulseVector[m] == 1) {
        litterProbability[monthSeries == m] <- peakProb
      }
    }
  } else {
    litterProbability <- rep(expLitterPer / 365, 365)
  }
  
  # Calculate marginal costs of contact:
  marginalCost1 <- contactCost25 / (initialPopSize * 0.25)
  marginalCost2 <- contactCost50 / (initialPopSize * 0.25)
  marginalCost3 <- contactCost75 / (initialPopSize * 0.25)
  marginalCost4 <- contactCost100 / (initialPopSize * 0.25)
  marginalCost <- c(marginalCost1, marginalCost2, marginalCost3, marginalCost4)
  
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
                 'boosted', 'contacted', 'contactCost',
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
  resultsMatrix <- array(data=NA, dim=c(simulationEnd, length(censusSeries), iterations))
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
  ########################################
  
  
  ########################################
  getDailyBudget <- function(j) {
    # Arguments: The year of the simulation (j)
    # Return:    The amount that can be spent each day given the management months specified and the annual budget
    # Purpose:   Called at the beginning of each year to allocate the budget across management campaigns during year
    dailyBudget <- mgtDayVector * (annualBudget[j] / sum(mgtDayVector))
    return(dailyBudget)
  }
  ########################################
  
  
  ########################################
  InitialPopulation <- function() {
    # Agruments: None.
    # Return:    The population matrix.
    # Purpose:   Construct the initial population matrix.
    
    popMatrix <- matrix(0, nrow=initialPopSize, ncol=length(traitList))
    colnames(popMatrix) <- traitList                 
    popMatrix[, 'age'] <- c(sample(seq(1, maxPuppyAge), initialPuppies, replace=TRUE),
                            sample(seq(maxPuppyAge + 1, maxJuvAge), initialJuveniles, replace=TRUE),
                            sample(seq(maxJuvAge + 1, maxAge), initialAdults, replace=TRUE))
    popMatrix[, 'female'] <- sample(c(0, 1), initialPopSize, replace=TRUE, 
                                    prob=c(1-initialFracFemale, initialFracFemale))
    popMatrix[, 'contracepted'] <- sample(c(0, 1), initialPopSize, replace=TRUE, 
                                          prob=c(1-initialFracContra, initialFracContra))
    popMatrix[, 'sterilized'] <- sample(c(0, 1), initialPopSize, replace=TRUE, 
                                        prob=c(1-initialFracSter, initialFracSter))
    popMatrix[, 'vaccinated'] <- sample(c(0, 1), initialPopSize, replace=TRUE, 
                                        prob=c(1-initialFracVacc, initialFracVacc))
    popMatrix[, 'immune'] <- sample(c(0, 1), initialPopSize, replace=TRUE, 
                                    prob=c(1-initialFracImmune, initialFracImmune))
    popMatrix[popMatrix[, 'contracepted']==1, 'timeContra'] <- 
      vapply(popMatrix[popMatrix[, 'contracepted']==1, 'age'], sample, size=1, FUN.VALUE=0) 
    popMatrix[popMatrix[, 'vaccinated']==1, 'timeVacc'] <- 
      vapply(popMatrix[popMatrix[, 'vaccinated']==1, 'age'], sample, size=1, FUN.VALUE=0)
    popMatrix[popMatrix[, 'age'] > maxJuvAge, 'adult'] <- 1
    popMatrix[popMatrix[, 'age'] <= maxPuppyAge, 'puppy'] <- 1
    popMatrix[, 'month'] <- 1
    popMatrix[, 'contactCost'] <- sample(marginalCost, nrow(popMatrix), replace=TRUE)
    
    return(popMatrix)
  }
  ########################################
  
  
  ########################################
  MortalityFunction <- function() {
    # Arguments: None.
    # Return:    An updated population matrix.
    # Purpose:   Induces out-migration, probabilistic mortality,
    #            old-age mortality, and censors population to carrying capacity.
    
    # Induce out-migration:
    emigDraw  <- runif(nrow(popMatrix))
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
    popMatrix <- popMatrix[sample(seq(1, n), min(carryingCap, n), replace=FALSE), , drop=FALSE]
    
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
                         (popMatrix[, 'exposed'] + popMatrix[, 'infective'] == 0 | popMatrix[, 'immune'] == 1))
    fertFemales <- max(0, fertFemales)
    litterDraw <- runif(fertFemales)
    puppies <- round(sum(litterDraw < litterProbability[d]) * meanLitterSize)
    newDogMatrix <- matrix(0, nrow=puppies, ncol=length(traitList))
    colnames(newDogMatrix) <- traitList
    newDogMatrix[, 'female'] <- sample(c(0, 1), puppies, 
                                       replace=TRUE, prob=c(1-femalePupProb, 
                                                            femalePupProb))
    newDogMatrix[, 'puppy'] <- 1
    newDogMatrix[, 'contactCost'] <- sample(marginalCost, nrow(newDogMatrix), replace=TRUE)
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
    newDogMatrix[, 'contactCost'] <- sample(marginalCost, nrow(newDogMatrix), replace=TRUE)
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
    if (d %in% pressureDays[[j]]) {
      temp <- rep(0, nrow(popMatrix))
      # Generate a vector of 0's and 1's that indicates which individuals are exposed:
      temp[sample(seq(1, nrow(popMatrix)), dogsPerMonthExposed)] <- 1
      # Change states if the individual can be moved to the exposed state:
      newExposed <- temp == 1 & popMatrix[, 'infective'] == 0 & popMatrix[, 'exposed'] == 0 &
        popMatrix[, 'immune'] == 0 & popMatrix[, 'vaccinated'] == 0
      popMatrix[newExposed, 'exposed']     <- 1
      popMatrix[newExposed, 'timeExposed'] <- 0
    }
    
    # Endogenous transmission:
    infectiveDogs <- sum(popMatrix[, 'infective'])
    dailyRabidBites <- sum(rnbinom(infectiveDogs, size=(bitesPerRabidShape/timeLimitInfective), 
                                   mu=bitesPerRabidMean/timeLimitInfective))
    # Now we draw dogs randomly from population to be bitten:
    rowsBitten <- unique(sample(seq(1:nrow(popMatrix)), dailyRabidBites, replace=TRUE))
    bitten <- rep(0, nrow(popMatrix))
    bitten[rowsBitten] <- 1
    infectionDraw <- runif(nrow(popMatrix))
    # Treat dog as unbitten if did not actually aquire infection from bite:
    bitten[infectionDraw > probInfectionFromBite] <- 0
    # Take the dogs that received rabid bites and moved to exposed state if appropriate:
    newExposed <- bitten == 1 & popMatrix[, 'infective'] == 0 & popMatrix[, 'exposed'] == 0 &
      popMatrix[, 'immune'] == 0 & popMatrix[, 'vaccinated'] == 0
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
    newInfective <- popMatrix[, 'exposed'] == 1 & popMatrix[, 'timeExposed'] > timeLimitExposed
    popMatrix[newInfective, 'exposed']       <- 0
    popMatrix[newInfective, 'infective']     <- 1
    popMatrix[newInfective, 'timeInfective'] <- 0
    
    # Transition infective to death or immune:
    newRecovered <- popMatrix[, 'infective'] == 1 & popMatrix[, 'timeInfective'] > timeLimitInfective
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
    censusVector['femalesSterilized'] <- sum(popMatrix[, 'sterilized'] == 1 & popMatrix[, 'female'] == 1)
    censusVector['contracepted'] <- sum(popMatrix[, 'sterilized'])
    censusVector['femalesContracepted'] <- sum(popMatrix[, 'contracepted'] == 1 & 
                                                 popMatrix[, 'female'] == 1)
    censusVector['vaccinated'] <- sum(popMatrix[, 'vaccinated'])
    censusVector['immune'] <- sum(popMatrix[, 'immune'])
    censusVector['exposed'] <- sum(popMatrix[, 'exposed'])
    censusVector['infective'] <- sum(popMatrix[, 'infective'])
    bitesNonRabid <- bitesPerNonRabid * (censusVector['abundance'] - censusVector['infective'])
    bitesRabid <- bitesPerRabid * (censusVector['infective'])
    censusVector['PEPs'] <- PEPperNonRabidBite * bitesNonRabid + PEPperRabidBite * bitesRabid
    censusVector['lifeLoss'] <- lifeLossPerRabidBite * ((1 - PEPperRabidBite) * bitesRabid)
    
    return(censusVector)
  }
  ########################################
  
  
  ########################################
  ManagementFunction <- function(d, marginalCost, dailyBudget, totalSpending, totalContacted) {
    dailySpending <- 0
    if (dailyBudget[d] > 0) {
      count <- 0
      while (dailySpending < dailyBudget[d] & min(popMatrix[, 'contacted']) == 0) {
        
        # if there are uncontacted dogs left in the lowest marginal cost category, contact them first
        if (sum(popMatrix[, 'contacted'] == 0 & popMatrix[, 'contactCost'] == marginalCost[1]) > 0) {
          dogNumber <- sample(rep(which(popMatrix[, 'contacted'] == 0 & 
                                        popMatrix[, 'contactCost'] == marginalCost[1]), 2), 1)
          # now check for uncontacted in 2nd lowest marginal cost category
        } else if (sum(popMatrix[, 'contacted'] == 0 & popMatrix[, 'contactCost'] == marginalCost[2]) > 0) {
          dogNumber <- sample(rep(which(popMatrix[, 'contacted'] == 0 & 
                                        popMatrix[, 'contactCost'] == marginalCost[2]), 2), 1)
          # and for 2nd highest marginal cost category
        } else if (sum(popMatrix[, 'contacted'] == 0 & popMatrix[, 'contactCost'] == marginalCost[3]) > 0) {
          dogNumber <- sample(rep(which(popMatrix[, 'contacted'] == 0 & 
                                        popMatrix[, 'contactCost'] == marginalCost[3]), 2), 1)
          # and for highest marginal cost category
        } else if (sum(popMatrix[, 'contacted'] == 0 & popMatrix[, 'contactCost'] == marginalCost[4]) > 0) {
          dogNumber <- sample(rep(which(popMatrix[, 'contacted'] == 0 & 
                                        popMatrix[, 'contactCost'] == marginalCost[4]), 2), 1)
        } else {
          break
        }
        
        popMatrix[dogNumber, 'contacted'] <- 1
        totalContacted <- totalContacted + 1
        dailySpending <- dailySpending + as.numeric(popMatrix[dogNumber, 'contactCost'])
        
        if (popMatrix[dogNumber, 'female'] == 1) {
          # FEMALE management starts here
          
          if (popMatrix[dogNumber, 'puppy'] == 1) {
            # female PUPPY management here
            if (strategyVector['euthPuppyFemale'] == 1) {
              popMatrix <- popMatrix[!dogNumber, , drop=FALSE]
              dailySpending <- dailySpending + as.numeric(strategyCostVector['euthPuppyFemale'])
            } else {
              if (strategyVector['sterPuppyFemale'] == 1) {
                if (popMatrix[dogNumber, 'sterilized'] == 0) {
                  popMatrix[dogNumber, 'sterilized'] <- 1
                  dailySpending <- dailySpending + as.numeric(strategyCostVector['sterPuppyFemale'])
                }
              } else if (strategyVector['contraPuppyFemale'] == 1) {
                if (popMatrix[dogNumber, 'sterilized'] == 0) {
                  # we won't contracept if dog has been sterilized, be we will even if already contracepted
                  popMatrix[dogNumber, 'contracepted'] <- 1
                  popMatrix[dogNumber, 'timeContra'] <- 0
                  dailySpending <- dailySpending + as.numeric(strategyCostVector['contraPuppyFemale'])
                }
              }
              if (strategyVector['vaccPuppyFemale'] == 1) {
                if (popMatrix[dogNumber, 'vaccinated'] == 0) {
                  popMatrix[dogNumber, 'vaccinated'] <- 1
                  popMatrix[dogNumber, 'timeVacc'] <- 0
                  dailySpending <- dailySpending + as.numeric(strategyCostVector['vaccPuppyFemale'])
                } else if (boosterGiven == TRUE) {
                  popMatrix[dogNumber, 'vaccinated'] <- 1
                  popMatrix[dogNumber, 'boosted'] <- 1
                  popMatrix[dogNumber, 'timeVacc'] <- 0
                  dailySpending <- dailySpending + as.numeric(strategyCostVector['vaccPuppyFemale'])
                }
              }
            }
          } else if (popMatrix[dogNumber, 'adult'] == 1) {
            # female ADULT management here
            if (strategyVector['euthAdultFemale'] == 1) {
              popMatrix <- popMatrix[!dogNumber, , drop=FALSE]
              dailySpending <- dailySpending + as.numeric(strategyCostVector['euthAdultFemale'])
            } else {
              if (strategyVector['sterAdultFemale'] == 1) {
                if (popMatrix[dogNumber, 'sterilized'] == 0) {
                  popMatrix[dogNumber, 'sterilized'] <- 1
                  dailySpending <- dailySpending + as.numeric(strategyCostVector['sterAdultFemale'])
                }
              } else if (strategyVector['contraAdultFemale'] == 1) {
                if (popMatrix[dogNumber, 'sterilized'] == 0) {
                  # we won't contracept if dog has been sterilized, be we will even if already contracepted
                  popMatrix[dogNumber, 'contracepted'] <- 1
                  popMatrix[dogNumber, 'timeContra'] <- 0
                  dailySpending <- dailySpending + as.numeric(strategyCostVector['contraAdultFemale'])
                }
              }
              if (strategyVector['vaccAdultFemale'] == 1) {
                if (popMatrix[dogNumber, 'vaccinated'] == 0) {
                  popMatrix[dogNumber, 'vaccinated'] <- 1
                  popMatrix[dogNumber, 'timeVacc'] <- 0
                  dailySpending <- dailySpending + as.numeric(strategyCostVector['vaccAdultFemale'])
                } else if (boosterGiven == TRUE) {
                  popMatrix[dogNumber, 'vaccinated'] <- 1
                  popMatrix[dogNumber, 'boosted'] <- 1
                  popMatrix[dogNumber, 'timeVacc'] <- 0
                  dailySpending <- dailySpending + as.numeric(strategyCostVector['vaccAdultFemale'])
                }
              }
            }
          } else {
            # female JUVENILE management here
            if (strategyVector['euthJuvFemale'] == 1) {
              popMatrix <- popMatrix[!dogNumber, , drop=FALSE]
              dailySpending <- dailySpending + as.numeric(strategyCostVector['euthJuvFemale'])
            } else {
              if (strategyVector['sterJuvFemale'] == 1) {
                if (popMatrix[dogNumber, 'sterilized'] == 0) {
                  popMatrix[dogNumber, 'sterilized'] <- 1
                  dailySpending <- dailySpending + as.numeric(strategyCostVector['sterJuvFemale'])
                }
              } else if (strategyVector['contraJuvFemale'] == 1) {
                if (popMatrix[dogNumber, 'sterilized'] == 0) {
                  # we won't contracept if dog has been sterilized, be we will even if already contracepted
                  popMatrix[dogNumber, 'contracepted'] <- 1
                  popMatrix[dogNumber, 'timeContra'] <- 0
                  dailySpending <- dailySpending + as.numeric(strategyCostVector['contraJuvFemale'])
                }
              }
              if (strategyVector['vaccJuvFemale'] == 1) {
                if (popMatrix[dogNumber, 'vaccinated'] == 0) {
                  popMatrix[dogNumber, 'vaccinated'] <- 1
                  popMatrix[dogNumber, 'timeVacc'] <- 0
                  dailySpending <- dailySpending + as.numeric(strategyCostVector['vaccJuvFemale'])
                } else if (boosterGiven == TRUE) {
                  popMatrix[dogNumber, 'vaccinated'] <- 1
                  popMatrix[dogNumber, 'boosted'] <- 1
                  popMatrix[dogNumber, 'timeVacc'] <- 0
                  dailySpending <- dailySpending + as.numeric(strategyCostVector['vaccJuvFemale'])
                }
              }
            }
          } 
          
        } else {
          # MALE management starts here
          if (popMatrix[dogNumber, 'puppy'] == 1) {
            # male PUPPY management here
            if (strategyVector['euthPuppyMale'] == 1) {
              popMatrix <- popMatrix[!dogNumber, , drop=FALSE]
              dailySpending <- dailySpending + as.numeric(strategyCostVector['euthPuppyMale'])
            } else {
              if (strategyVector['sterPuppyMale'] == 1) {
                if (popMatrix[dogNumber, 'sterilized'] == 0) {
                  popMatrix[dogNumber, 'sterilized'] <- 1
                  dailySpending <- dailySpending + as.numeric(strategyCostVector['sterPuppyMale'])
                }
              } else if (strategyVector['contraPuppyMale'] == 1) {
                if (popMatrix[dogNumber, 'sterilized'] == 0) {
                  # we won't contracept if dog has been sterilized, be we will even if already contracepted
                  popMatrix[dogNumber, 'contracepted'] <- 1
                  popMatrix[dogNumber, 'timeContra'] <- 0
                  dailySpending <- dailySpending + as.numeric(strategyCostVector['contraPuppyMale'])
                }
              }
              if (strategyVector['vaccPuppyMale'] == 1) {
                if (popMatrix[dogNumber, 'vaccinated'] == 0) {
                  popMatrix[dogNumber, 'vaccinated'] <- 1
                  popMatrix[dogNumber, 'timeVacc'] <- 0
                  dailySpending <- dailySpending + as.numeric(strategyCostVector['vaccPuppyMale'])
                } else if (boosterGiven == TRUE) {
                  popMatrix[dogNumber, 'vaccinated'] <- 1
                  popMatrix[dogNumber, 'boosted'] <- 1
                  popMatrix[dogNumber, 'timeVacc'] <- 0
                  dailySpending <- dailySpending + as.numeric(strategyCostVector['vaccPuppyMale'])
                }
              }
            }
            
          } else if (popMatrix[dogNumber, 'adult'] == 1) {
            # male ADULT management here
            if (strategyVector['euthAdultMale'] == 1) {
              popMatrix <- popMatrix[!dogNumber, , drop=FALSE]
              dailySpending <- dailySpending + as.numeric(strategyCostVector['euthAdultMale'])
            } else {
              if (strategyVector['sterAdultMale'] == 1) {
                if (popMatrix[dogNumber, 'sterilized'] == 0) {
                  popMatrix[dogNumber, 'sterilized'] <- 1
                  dailySpending <- dailySpending + as.numeric(strategyCostVector['sterAdultMale'])
                }
              } else if (strategyVector['contraAdultMale'] == 1) {
                if (popMatrix[dogNumber, 'sterilized'] == 0) {
                  # we won't contracept if dog has been sterilized, be we will even if already contracepted
                  popMatrix[dogNumber, 'contracepted'] <- 1
                  popMatrix[dogNumber, 'timeContra'] <- 0
                  dailySpending <- dailySpending + as.numeric(strategyCostVector['contraAdultMale'])
                }
              }
              if (strategyVector['vaccAdultMale'] == 1) {
                if (popMatrix[dogNumber, 'vaccinated'] == 0) {
                  popMatrix[dogNumber, 'vaccinated'] <- 1
                  popMatrix[dogNumber, 'timeVacc'] <- 0
                  dailySpending <- dailySpending + as.numeric(strategyCostVector['vaccAdultMale'])
                } else if (boosterGiven == TRUE) {
                  popMatrix[dogNumber, 'vaccinated'] <- 1
                  popMatrix[dogNumber, 'boosted'] <- 1
                  popMatrix[dogNumber, 'timeVacc'] <- 0
                  dailySpending <- dailySpending + as.numeric(strategyCostVector['vaccAdultMale'])
                }
              }
            }
            
          } else {
            # male JUVENILE management here
            if (strategyVector['euthJuvMale'] == 1) {
              popMatrix <- popMatrix[!dogNumber, , drop=FALSE]
              dailySpending <- dailySpending + as.numeric(strategyCostVector['euthJuvMale'])
            } else {
              if (strategyVector['sterJuvMale'] == 1) {
                if (popMatrix[dogNumber, 'sterilized'] == 0) {
                  popMatrix[dogNumber, 'sterilized'] <- 1
                  dailySpending <- dailySpending + as.numeric(strategyCostVector['sterJuvMale'])
                }
              } else if (strategyVector['contraJuvMale'] == 1) {
                if (popMatrix[dogNumber, 'sterilized'] == 0) {
                  # we won't contracept if dog has been sterilized, be we will even if already contracepted
                  popMatrix[dogNumber, 'contracepted'] <- 1
                  popMatrix[dogNumber, 'timeContra'] <- 0
                  dailySpending <- dailySpending + as.numeric(strategyCostVector['contraJuvMale'])
                }
              }
              if (strategyVector['vaccJuvMale'] == 1) {
                if (popMatrix[dogNumber, 'vaccinated'] == 0) {
                  popMatrix[dogNumber, 'vaccinated'] <- 1
                  popMatrix[dogNumber, 'timeVacc'] <- 0
                  dailySpending <- dailySpending + as.numeric(strategyCostVector['vaccJuvMale'])
                } else if (boosterGiven == TRUE) {
                  popMatrix[dogNumber, 'vaccinated'] <- 1
                  popMatrix[dogNumber, 'boosted'] <- 1
                  popMatrix[dogNumber, 'timeVacc'] <- 0
                  dailySpending <- dailySpending + as.numeric(strategyCostVector['vaccJuvMale'])
                }
              }
            }
          } 
        }
      }
    }
    
    return(list(popMatrix, totalContacted, dailySpending))
  }
  ########################################
  
  
  ########################################
  TimeFunction <- function() {
    # Arguments: None.
    # Return:    An updated population matrix.
    # Purpose:   Updates time-related columns in the population matrix.
    
    if (boosterGiven == FALSE) {
      # Turn off vaccinated and contracepted when time limit reached: 
      popMatrix[popMatrix[, 'timeVacc'] == timeVaccineEffective, 'vaccinated'] <- 0
      popMatrix[(popMatrix[, 'timeContra'] == timeContraEffectiveFemales & popMatrix[, 'female'] == 1), 
                'contracepted'] <- 0
      popMatrix[(popMatrix[, 'timeContra'] == timeContraEffectiveMales & popMatrix[, 'female'] == 0), 
                'contracepted'] <- 0
    } else {
      # Turn off vaccinated when time limit reached: 
      popMatrix[(popMatrix[, 'timeVacc'] == timeVaccineEffective & popMatrix[, 'boosted'] == 0), 'vaccinated'] <- 0
      
      # Turn off boosted and vaccinated when time limit reached:
      popMatrix[(popMatrix[, 'timeVacc'] == timeBoosterEffective & popMatrix[, 'boosted'] == 1), 'vaccinated'] <- 0
      popMatrix[(popMatrix[, 'timeVacc'] == timeBoosterEffective & popMatrix[, 'boosted'] == 1), 'boosted'] <- 0
      
      # Turn off contracepted when time limit reached:
      popMatrix[(popMatrix[, 'timeContra'] == timeContraEffectiveFemales & popMatrix[, 'female'] == 1), 
                'contracepted'] <- 0
      popMatrix[(popMatrix[, 'timeContra'] == timeContraEffectiveMales & popMatrix[, 'female'] == 0), 
                'contracepted'] <- 0
    }
    
    popMatrix[, 'age']           <- popMatrix[, 'age'] + 1
    popMatrix[, 'timeVacc']      <- popMatrix[, 'timeVacc'] + 1
    popMatrix[, 'timeContra']    <- popMatrix[, 'timeContra'] + 1
    popMatrix[, 'timeExposed']   <- popMatrix[, 'timeExposed'] + 1
    popMatrix[, 'timeInfective'] <- popMatrix[, 'timeInfective'] + 1
    
    return(popMatrix)
  }
  ########################################
  

  ########################################
  withProgress(min=0, max=100, value=0, message = '', detail = '', {
    # Loop through iterations:
    for(i in 1:iterations) {
      setProgress(round(100*(i/iterations)), message=paste('Running iteration', i, 'of', iterations))          
      popMatrix <- InitialPopulation()
      
      # Loop through years:
      # Loop through years:
      for(j in 1:simulationYears) {
        # reset total spending, number of dogs contacted, and contacted indicator at start of year
        totalSpending <- 0
        totalContacted <- 0
        popMatrix[, 'contacted'] <- 0
        # get the daily budget for each day of year
        dailyBudget <- getDailyBudget(j)
        
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
          if (totalSpending < annualBudget[j]) {
            mgtReturnList <- ManagementFunction(d, marginalCost, dailyBudget, totalSpending, totalContacted)
            popMatrix <- mgtReturnList[[1]]
            totalContacted <- mgtReturnList[[2]]
            totalSpending <- totalSpending + mgtReturnList[[3]]
          }
          # Record new vaccinations:
          resultsMatrix[(365 * (j-1) + d), 'newlyVaccinated', i] <- sum(popMatrix[, 'vaccinated']) - tempVacc
          popMatrix <- TimeFunction()
        }  # close d for loop
      }  # close j for loop
    }  # close i for loop
  })  # close withProgress
  ########################################

  # Return the result from eventReactive:
  resultsMatrix
})  # close eventReactive
########################################################################################################################

########################################################################################################################
getIterations <- eventReactive(input$run, {
  # Get iterations for use in results:
  input$iterations
})  # close eventReactive

getCostPerPEP <- eventReactive(input$run, {
  # Get costPerPEP for use in results:
  input$costPerPEP
})  # close eventReactive

getAnnualBudget <- eventReactive(input$run, {
  # Get budgets for use in results:
  c(input$annualBudget1, input$annualBudget2, input$annualBudget3, input$annualBudget4, input$annualBudget5)
})  # close eventReactive

getCarryingCapacity <- eventReactive(input$run, {
  # Get carrying capacity for use in results:
  input$carryingCap
})  # close eventReactive
########################################################################################################################

########################################################################################################################
# Plot used in the Graphical Results tab:
output$graphicalResults <- renderPlot({
  
  # Get some values that will be needed
  resultsMatrix   <- getResultsMatrix()
  iterations      <- getIterations()
  i               <- iterations
  simulationYears <- 5
  simulationEnd   <- 365 * simulationYears 
  carryingCap     <- getCarryingCapacity()
  abunMax         <- max(resultsMatrix[, 'abundance', ]) * 1.1
  prevMax         <- max(resultsMatrix[, 'infective', ]) * 1.1
  vaccMax         <- max(resultsMatrix[, 'vaccinated', ]) * 1.1
  daySeries       <- seq(1, simulationEnd)
  
  # Construct abundance plot:
  quant0abun   <- apply(resultsMatrix[, 'abundance', ], 1, quantile, 0.0)
  quant10abun  <- apply(resultsMatrix[, 'abundance', ], 1, quantile, 0.1)
  quant20abun  <- apply(resultsMatrix[, 'abundance', ], 1, quantile, 0.2)
  quant30abun  <- apply(resultsMatrix[, 'abundance', ], 1, quantile, 0.3)
  quant40abun  <- apply(resultsMatrix[, 'abundance', ], 1, quantile, 0.4)
  quant50abun  <- apply(resultsMatrix[, 'abundance', ], 1, quantile, 0.5)
  quant60abun  <- apply(resultsMatrix[, 'abundance', ], 1, quantile, 0.6)
  quant70abun  <- apply(resultsMatrix[, 'abundance', ], 1, quantile, 0.7)
  quant80abun  <- apply(resultsMatrix[, 'abundance', ], 1, quantile, 0.8)
  quant90abun  <- apply(resultsMatrix[, 'abundance', ], 1, quantile, 0.9)
  quant100abun <- apply(resultsMatrix[, 'abundance', ], 1, quantile, 1.0)
  meanAbun = apply(resultsMatrix[, 'abundance', ], 1, mean, na.rm=TRUE)
  
  abundPlot <- ggplot() +
    geom_ribbon(aes(x=daySeries, ymax=quant100abun, ymin=quant0abun, fill='full range  ')) +
    geom_ribbon(aes(x=daySeries, ymax=quant90abun, ymin=quant10abun, fill='percentile 10 to 90  ')) +
    geom_ribbon(aes(x=daySeries, ymax=quant80abun, ymin=quant20abun, fill='percentile 20 to 80  ')) +
    geom_ribbon(aes(x=daySeries, ymax=quant70abun, ymin=quant30abun, fill='percentile 30 to 70  ')) +
    geom_ribbon(aes(x=daySeries, ymax=quant60abun, ymin=quant40abun, fill='percentile 40 to 60  ')) +
    geom_line(aes(daySeries, quant50abun, colour = 'median  ')) +
    geom_line(aes(daySeries, meanAbun, colour = 'mean  '), size=1.0) +
    scale_x_continuous(limits=c(0, simulationEnd), expand = c(0, 25), breaks=c(365, 730, 1095, 1460, 1825),
                       labels = c('1', '2', '3', '4', '5')) +
    scale_y_continuous(limits=c(0, abunMax), expand = c(0, 0)) +
    ylab('abundance') +
    theme(axis.title.y=element_text(margin=margin(0,10,0,0))) +
    theme(axis.text=element_text(size=12, color='black'), 
          axis.title=element_text(size=14, face="bold", color='black')) +
    xlab('') +
    theme(axis.line = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_rect(fill=NA, colour='black'),
          panel.background = element_blank()) +
    scale_colour_manual(name=NULL, values=c('median  '='#1B4F72',
                                            'mean  '='black')) +
    scale_fill_manual(name=NULL, values=c('full range  '='#5DADE2', 
                                          'percentile 10 to 90  '='#3498DB', 
                                          'percentile 20 to 80  '='#2E86C1', 
                                          'percentile 30 to 70  '='#2874A6', 
                                          'percentile 40 to 60  '='#21618C')) +
    theme(legend.text.align=0) + 
    theme(legend.position='none') 

  # Construct the disease prevalence plot:
  quant0inf   <- apply(resultsMatrix[, 'infective', ], 1, quantile, 0.0)
  quant10inf  <- apply(resultsMatrix[, 'infective', ], 1, quantile, 0.1)
  quant20inf  <- apply(resultsMatrix[, 'infective', ], 1, quantile, 0.2)
  quant30inf  <- apply(resultsMatrix[, 'infective', ], 1, quantile, 0.3)
  quant40inf  <- apply(resultsMatrix[, 'infective', ], 1, quantile, 0.4)
  quant50inf  <- apply(resultsMatrix[, 'infective', ], 1, quantile, 0.5)
  quant60inf  <- apply(resultsMatrix[, 'infective', ], 1, quantile, 0.6)
  quant70inf  <- apply(resultsMatrix[, 'infective', ], 1, quantile, 0.7)
  quant80inf  <- apply(resultsMatrix[, 'infective', ], 1, quantile, 0.8)
  quant90inf  <- apply(resultsMatrix[, 'infective', ], 1, quantile, 0.9)
  quant100inf <- apply(resultsMatrix[, 'infective', ], 1, quantile, 1.0)
  meanInf = apply(resultsMatrix[, 'infective', ], 1, mean, na.rm=TRUE)
  
  infectPlot <- ggplot() +
    geom_ribbon(aes(x=daySeries, ymax=quant100inf, ymin=quant0inf, fill='full range  ')) +
    geom_ribbon(aes(x=daySeries, ymax=quant90inf, ymin=quant10inf, fill='percentile 10 to 90  ')) +
    geom_ribbon(aes(x=daySeries, ymax=quant80inf, ymin=quant20inf, fill='percentile 20 to 80  ')) +
    geom_ribbon(aes(x=daySeries, ymax=quant70inf, ymin=quant30inf, fill='percentile 30 to 70  ')) +
    geom_ribbon(aes(x=daySeries, ymax=quant60inf, ymin=quant40inf, fill='percentile 40 to 60  ')) +
    geom_line(aes(daySeries, quant50inf, colour = 'median  ')) +
    geom_line(aes(daySeries, meanInf, colour = 'mean  '), size=1.0) +
    scale_x_continuous(limits=c(0, simulationEnd), expand = c(0, 25), breaks=c(365, 730, 1095, 1460, 1825),
                       labels = c('1', '2', '3', '4', '5')) +
    scale_y_continuous(limits=c(0, prevMax), expand = c(0, 0)) +
    ylab('disease prevalence') +
    theme(axis.title.y=element_text(margin=margin(0,10,0,0))) +
    theme(axis.text=element_text(size=12, color='black'), 
          axis.title=element_text(size=14, face="bold", color='black')) +
    xlab('') +
    theme(axis.line = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_rect(fill=NA, colour='black'),
          panel.background = element_blank()) +
    scale_colour_manual(name=NULL, values=c('median  '='#1B4F72',
                                            'mean  '='black')) +
    scale_fill_manual(name=NULL, values=c('full range  '='#5DADE2', 
                                          'percentile 10 to 90  '='#3498DB', 
                                          'percentile 20 to 80  '='#2E86C1', 
                                          'percentile 30 to 70  '='#2874A6', 
                                          'percentile 40 to 60  '='#21618C')) +
    theme(legend.text.align=0) + 
    theme(legend.position='none') 
    
  # Construct the vaccination plot:
  quant0vac  <- apply(resultsMatrix[, 'vaccinated', ], 1, quantile, 0.0)
  quant10vac  <- apply(resultsMatrix[, 'vaccinated', ], 1, quantile, 0.1)
  quant20vac  <- apply(resultsMatrix[, 'vaccinated', ], 1, quantile, 0.2)
  quant30vac  <- apply(resultsMatrix[, 'vaccinated', ], 1, quantile, 0.3)
  quant40vac  <- apply(resultsMatrix[, 'vaccinated', ], 1, quantile, 0.4)
  quant50vac  <- apply(resultsMatrix[, 'vaccinated', ], 1, quantile, 0.5)
  quant60vac  <- apply(resultsMatrix[, 'vaccinated', ], 1, quantile, 0.6)
  quant70vac  <- apply(resultsMatrix[, 'vaccinated', ], 1, quantile, 0.7)
  quant80vac  <- apply(resultsMatrix[, 'vaccinated', ], 1, quantile, 0.8)
  quant90vac  <- apply(resultsMatrix[, 'vaccinated', ], 1, quantile, 0.9)
  quant100vac <- apply(resultsMatrix[, 'vaccinated', ], 1, quantile, 1.0)
  meanVac = apply(resultsMatrix[, 'vaccinated', ], 1, mean, na.rm=TRUE)
  
  vaccPlot <- ggplot() +
    geom_ribbon(aes(x=daySeries, ymax=quant100vac, ymin=quant0vac, fill='full range  ')) +
    geom_ribbon(aes(x=daySeries, ymax=quant90vac, ymin=quant10vac, fill='percentile 10 to 90  ')) +
    geom_ribbon(aes(x=daySeries, ymax=quant80vac, ymin=quant20vac, fill='percentile 20 to 80  ')) +
    geom_ribbon(aes(x=daySeries, ymax=quant70vac, ymin=quant30vac, fill='percentile 30 to 70  ')) +
    geom_ribbon(aes(x=daySeries, ymax=quant60vac, ymin=quant40vac, fill='percentile 40 to 60  ')) +
    geom_line(aes(daySeries, quant50vac, colour = 'median  ')) +
    geom_line(aes(daySeries, meanVac, colour = 'mean  '), size=1.0) +
    scale_x_continuous(limits=c(0, simulationEnd), expand = c(0, 25), breaks=c(365, 730, 1095, 1460, 1825),
                       labels = c('1', '2', '3', '4', '5')) +
    scale_y_continuous(limits=c(0, vaccMax), expand = c(0, 0)) +
    ylab('vacc dogs in pop') +
    theme(axis.title.y=element_text(margin=margin(0,10,0,0))) +
    theme(axis.text=element_text(size=12, color='black'), 
          axis.title=element_text(size=14, face="bold", color='black')) +
    xlab('year') +
    theme(axis.line = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_rect(fill=NA, colour='black'),
          panel.background = element_blank()) +
    scale_colour_manual(name=NULL, values=c('median  '='#1B4F72',
                                            'mean  '='black')) +
    scale_fill_manual(name=NULL, values=c('full range  '='#5DADE2', 
                                          'percentile 10 to 90  '='#3498DB', 
                                          'percentile 20 to 80  '='#2E86C1', 
                                          'percentile 30 to 70  '='#2874A6', 
                                          'percentile 40 to 60  '='#21618C')) +
    theme(legend.text.align=0) + 
    theme(legend.position='bottom') 
  
  # Put the three plots together:
  grid.draw(rbind(ggplotGrob(abundPlot), ggplotGrob(infectPlot), ggplotGrob(vaccPlot)))
  #grid.draw(abundPlot)
})  # close renderPlot
########################################################################################################################

########################################################################################################################
# Numerical results:
output$numericalResults <- renderPlot({
        
  resultsMatrix        <- getResultsMatrix()
  costPerPEP           <- getCostPerPEP()
  annualBudget         <- getAnnualBudget()
  simulationYears      <- 5                     
  simulationEnd        <- 365 * simulationYears
  carryingCap          <- getCarryingCapacity()
  dogDaysOfInfection   <- round(sum(apply((resultsMatrix[, 'infective', ]), 1, mean, na.rm=TRUE)))
  meanAbundance        <- round(mean(apply(resultsMatrix[, 'abundance', ], 1, mean, na.rm=TRUE)))
  totalCostOfInfection <- round(sum(apply(resultsMatrix[, 'PEPs', ], 1, mean, na.rm=TRUE))*costPerPEP)
  totalHumanDeaths     <- round(sum(apply(resultsMatrix[, 'lifeLoss', ], 1, mean, na.rm=TRUE)))
  totalBudget          <- round(sum(annualBudget[1:simulationYears]))
  totalVaccinations    <- round(sum(apply(resultsMatrix[, 'newlyVaccinated', ], 1, mean, na.rm=TRUE)))
  par(mar = c(0,0,0,0))
  plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
  text(0, 0.93, paste('Averages across all iterations:'), pos=4, cex=2)
  text(0.0, 0.86, paste('mean abundance =', meanAbundance), pos=4, cex=1.5, col='royalblue')
  text(0.0, 0.79, paste('dog-days of infection =', dogDaysOfInfection), pos=4, cex=1.5, col='royalblue')
  text(0.0, 0.72, paste('cost of infection =', totalCostOfInfection), pos=4, cex=1.5, col='royalblue')
  text(0.0, 0.65, paste('human deaths =', totalHumanDeaths), pos=4, cex=1.5, col='royalblue')
  text(0.0, 0.58, paste ('management cost =', totalBudget), pos=4, cex=1.5, col='royalblue')
  text(0.0, 0.51, paste('total vaccinations =', totalVaccinations), pos=4, cex=1.5, col='royalblue')
})  # close renderPlot
########################################################################################################################

})  # close shinyServer
################################################################################