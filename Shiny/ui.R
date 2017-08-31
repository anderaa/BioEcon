

########################################################################################################################
# Primary author:     Aaron Anderson, National Wildlife Research Center
# Contact:            aaron.m.anderson@aphis.usda.gov
# Other contributors: Johann Kotze, Brody Hatch, Jordan Navin
# Description:        This code is the frontend of a web app that can be used
#                     to forecast the results of canine rabies management
#                     in South Africa
########################################################################################################################

########################################################################################################################
# TO DEPLOY:
# library('rsconnect')
# from the token link on shiny.io copy and paste
#   rsconnect::....
# rsconnect::deployApp('<file path to shiney folder>')
########################################################################################################################

########################################################################################################################
library(shiny)


# Define UI for application:
shinyUI(fluidPage(tags$link(rel = 'stylesheet', type = 'text/css', href = 'styles.css'),
  img(src = "logo.png", height = 47, align='left'),      
  titlePanel(strong('BioEcon for Canine Rabies'), windowTitle='BioEcon'), 
    tabsetPanel(id='mainTabs', type='pills',
      tabPanel('Model Setup',
        navlistPanel('Model Setup', widths=c(3, 9),
                     
          tabPanel('Basic Inputs',
            sliderInput('iterations', label='number of iterations', min=5, max=100, value=5, step=1, ticks=FALSE), 
            br(), 
            numericInput('initialPopSize', label='initial abundance on January 1', value=404), 
            br(),
            sliderInput('initialFracAdult', label='fraction of initial population that are adult (age > 299 days)', 
                        min=0, max=1, value=0.49, step=0.01, ticks=FALSE), 
            br(),
            sliderInput('initialFracPup', label='fraction of juveniles (age < 300 days) that are puppies 
                        (age < 90 days)', min=0, max=1, value=0.39, step=0.01, ticks=FALSE), 
            br(),
            numericInput('carryingCap', label='carrying capacity', value=577)
          ),  # close Basic Inputs tabPanel

          tabPanel('Entry and Exit',
            fluidRow(
              column(4, 
                sliderInput('pupAnnMortProb', label='annual puppy mortality probability',
                            min=0.0, max=1.0, value=0.90, ticks=FALSE), 
                br(),
                sliderInput('juvAnnMortProb', label='annual juvenile mortality probability',
                            min=0.0, max=1.0, value=0.63, ticks=FALSE), 
                br(),
                sliderInput('adultAnnMortProb', label='annual adult mortality probability',
                            min=0.0, max=1.0, value=0.32, ticks=FALSE), 
                br(),
                sliderInput('emigrationProb', label='annual out-migration probability',
                            min=0.0, max=1.0, value=0, ticks=FALSE), 
                br(),
                numericInput('immigrantDogs', label='expected annual in-migration', value=189)
              ),  # close column
              column(1),
              column(4, 
                sliderInput('expectedLittersPerFemalePerYear', label='annual litters per fertile female', 
                            min=0, max=1, value=0.31, step=0.01, ticks=FALSE), 
                br(),
                tags$b('If there is a birth pulse, check the months of occurrence, otherwise ignore the inputs below.'),
                checkboxGroupInput('birthPulseMonths', label = '',
                                   c('January' = 'jan',
                                     'February' = 'feb',
                                     'March' = 'mar',
                                     'April' = 'apr',
                                     'May' = 'may',
                                     'June' = 'jun',
                                     'July' = 'jul',
                                     'August' = 'aug',
                                     'September' = 'sep',
                                     'October' = 'oct',
                                     'November' = 'nov',
                                     'December' = 'dec'),
                                     selected = c()),
                sliderInput('fractionBirthPulse', label='fraction of litters born during pulse', 
                            min=0, max=1, value=0.0, step=0.01, ticks=FALSE), 
                br()
              )  # close column
            )  # close fluid row
          ),  # close Entry and Exit tabPanel

          tabPanel('Disease Transmission',
            sliderInput('monthInitIntroduction', label='month of the initial introduction', 
                        min=1, max=48, value=1, step=1, ticks=FALSE, sep=''), 
            br(),
            sliderInput('sequentialMonthsIntro', label='number of sequential months of introduction ', 
                        min=1, max=12, value=1, step=1, ticks=FALSE, sep=''), 
            br(),
            sliderInput('dogsPerIntro', label='number of dogs exposed per month ', 
                        min=0, max=50, value=1, step=1, ticks=FALSE, sep=''), 
            br(),
            sliderInput('transmissionParam', label='mean bites per rabid dog while infectious', 
                        min=0.0, max=4.0, value=2.15, step=0.05, ticks=FALSE, sep='')
          ),  # close Disease Transmission tabPanel

          tabPanel('Disease Impacts',
            numericInput('bitesPerNonRabid', label='daily bites per non-rabid', value=0.003),
            numericInput('bitesPerRabid', label='daily bites per rabid', value=0.5), 
            br(),
            sliderInput('PEPperNonRabidBite', label='PEP per non-rabid bite',
                        min=0.0, max=1.0, value=0.05, ticks=FALSE),
            sliderInput('PEPperRabidBite', label='PEP per rabid bite', min=0.0, max=1.0, value=0.91, ticks=FALSE), 
            br(),
            numericInput('costPerPEP', label='cost per PEP', value=3), 
            br(),
            sliderInput('lifeLossPerRabidBite', label='probability of human death from rabid bite',
                        min=0.0, max=1.0, value=0.02, step=0.01, ticks=FALSE)
          ),  # close Disease Impacts tabPabel

          tabPanel('Management Costs',
            fluidRow(
              column(3, 
                numericInput('vaccineCost', label='cost per vaccination', value=2.426),
                numericInput('contraceptionCostFemale', label='cost per female contraception', value=150),
			          numericInput('contraceptionCostMale', label='cost per male contraception', value=150),
                numericInput('sterilizationCostFemale', label='cost per female sterilization', value=300),
			          numericInput('sterilizationCostMale', label='cost per male sterilization', value=200),
                numericInput('euthanasiaCost', label='cost per euthanized', value=150)
			        ),  # close column
              column(1),
              column(6,
                tags$b('The following inputs relate ONLY to the cost of contacting or capturing dogs. When answering, 
                        please ignore any costs that occur once dogs are contacted (e.g. vaccination). At the specified 
                        initial abundance of the population how much would it cost to contact or capture in a single 
                        year:'), 
                br(), br(),
                numericInput('contactCost25', label='25% of the population at initial abundance', value=0),
                numericInput('contactCost50', label='50% of the population at initial abundance', value=0),
                numericInput('contactCost75', label='75% of the population at initial abundance', value=0),
                numericInput('contactCost100', label='100% of the population at initial abundance', value=0),
                helpText('HINT: Per-dog capture or contact costs are likely to increase as more dogs are captured 
                          or contacted.')
              )  # close column
            )  # close fluidRow
          ),  # close Management Costs tabPanel
	   
          tabPanel('Management Strategy',
            fluidRow(
              column(12, 
                h4('Strategies to be applied to captured dogs:'))
              ),  # close column
            fluidRow(
              column(3,
                checkboxGroupInput('vaccDemoInput', label = 'Vaccination',
                                   c('puppy females' = 'pf',
                                     'puppy males' = 'pm',
                                     'juvenile females' = 'jf',
                                     'juvenile males' = 'jm',
                                     'adult females' = 'af',
                                     'adult males' = 'am'))
              ),  # close column  
              column(3, 
                checkboxGroupInput('sterDemoInput', label = 'Sterilization',
                                   c('puppy females' = 'pf',
                                     'puppy males' = 'pm',
                                     'juvenile females' = 'jf',
                                     'juvenile males' = 'jm',
                                     'adult females' = 'af',
                                     'adult males' = 'am'))
              ),  # close column
              column(3, 
                checkboxGroupInput('contraDemoInput', label = 'Contraception',
                                   c('puppy females' = 'pf',
                                     'puppy males' = 'pm',
                                     'juvenile females' = 'jf',
                                     'juvenile males' = 'jm',
                                     'adult females' = 'af',
                                     'adult males' = 'am'))
              ),  # close column
              column(3, 
                checkboxGroupInput('euthDemoInput', label = 'Euthanasia',
                                   c('puppy females' = 'pf',
                                     'puppy males' = 'pm',
                                     'juvenile females' = 'jf',
                                     'juvenile males' = 'jm',
                                     'adult females' = 'af',
                                     'adult males' = 'am'))
              )  # close column
            ),  # close fluidRow
            fluidRow(
              column(1), 
              column(10,
                helpText('WARNING: If euthanasia is selected for a demographic category, other selected options 
                          for the same group will be ignored. Addtionally, if sterilization and contraception
                          are both selected for the same demographic category, contraception will be ignored
                          and only sterilization will occur.')
              ) # close column
            ),  # close fluidRow
            fluidRow(
              column(12,
                     selectInput('boosterGiven', label='Is a booster vaccine given if an already-vaccinated dog is 
                                 re-contacted?', choices=list('no'=FALSE, 'yes'=TRUE), selected=TRUE)
              ) # close column
            ),  # close fluidRow
            fluidRow(
              column(5, 
                tags$b('Check months that managment occurs:'),
                checkboxGroupInput('managementMonths', label = '',
                                   c('January' = 'jan',
                                     'February' = 'feb',
                                     'March' = 'mar',
                                     'April' = 'apr',
                                     'May' = 'may',
                                     'June' = 'jun',
                                     'July' = 'jul',
                                     'August' = 'aug',
                                     'September' = 'sep',
                                     'October' = 'oct',
                                     'November' = 'nov',
                                     'December' = 'dec'),
                                    selected = c())
              ),  # close column 
              column(1),
              column(6,
                br(),
                tags$b('Please ignore the inputs below if irrelevant for your strategy.'),
                br(), br(),
                selectInput('timeVaccineEffective', label='vaccine duration',
                            choices=list('0.5 year'=183, '1 year'=365, 
                                         '1.5 years'=548, '2 years'=730,
                                         '3 years'=1095, '4 years'=1460, 
                                         '5 years'=1825, 
                                         'permanent'=999999), 
                            selected=730),
                selectInput('timeBoosterEffective', label='booster duration',
                            choices=list('0.5 year'=183, '1 year'=365, 
                                         '1.5 years'=548, '2 years'=730,
                                         '3 years'=1095, '4 years'=1460, 
                                         '5 years'=1825, 
                                         'permanent'=999999),
                            selected=1095),
                selectInput('timeContraEffectiveFemales', label='female contraception duration',
                            choices=list('0.5 year'=183, '1 year'=365, 
                                         '1.5 years'=548, '2 years'=730,
                                         '3 years'=1095, '4 years'=1460, 
                                         '5 years'=1825, 
                                         'permanent'=999999), 
                            selected=730),
                selectInput('timeContraEffectiveMales', label='male contraception duration',
                            choices=list('0.5 year'=183, '1 year'=365, 
                                         '1.5 years'=548, '2 years'=730,
                                         '3 years'=1095, '4 years'=1460, 
                                         '5 years'=1825, 
                                         'permanent'=999999), 
                            selected=730)
              )  # close column
            )  # close fluidRow
          ),  # close Management Strategy tabPanel

          tabPanel('Budget Information',
            fluidRow(
              column(4,
                numericInput('annualBudget1', label='year 1 budget', value=0),
                numericInput('annualBudget2', label='year 2 budget', value=0),
                numericInput('annualBudget3', label='year 3 budget', value=0),
                numericInput('annualBudget4', label='year 4 budget', value=0),
                numericInput('annualBudget5', label='year 5 budget', value=0)
              )  # close column
            )  # close fluidRow                   
          ),  # close Budget Information tabPanel

          tabPanel('Run', 
                   br(), br(), br(), br(), br(), br(), br(), br(),
                   tags$head(
                   tags$style(HTML('#run{background-color:orange}'))
                   ),
            actionButton('run', 'Run Simulation'),
            helpText('Progress through the iterations will be displayed inthe upper-right and results will be 
                      displayed after all iterations are complete.')
          )  # close Run tabPanel
        )  # close Model Setup navlistPanel
      ),  # close Model Setup tabPanel

      tabPanel('Model Output',
        navlistPanel('Model Output', widths=c(3, 9), 
          tabPanel('Graphical Output',
            plotOutput('graphicalResults', height='900px')),
          tabPanel('Numerical Output',
            plotOutput('numericalResults'))
        )  # close Model Output navlistPanel
      ),  # close Model Output tabPanel

      tabPanel('About', br(),
        tags$b('BioEcon for Canine Rabies is an individual-based, stochastic simulation model that forecasts the 
                economic and biological results of management.'), 
        br(), br(),
        'Built by Aaron Anderson, Johann Kotze, Brody Hatch, and Jordan Navin',
        br(), br(),
        'Inquiries to: Aaron.M.Anderson@aphis.usda.gov', 
        br(), br(),
        'Custom builds available.', 
        br(), br(),
        img(src = "nwrcLogo.png", height = 75, align='left')
      )  # close Help tabPanel
    )  # close mainTabs tabsetPanel
)  # close fluidPage
)  # close shiny UI
########################################################################################################################





