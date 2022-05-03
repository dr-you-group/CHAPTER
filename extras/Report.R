library(dplyr)

databaseIds <- c("Australia_LPD", "Japan_Claims")
outputFolder <- "./data"
source("./extras/FunctionsForReporting.R")

# dataFolderAus <- "./data/Results_Australia_LPD"
# dataFolderJp <- "./data/Results_Japan_Claims"

####Data load####
#load files into the environment
for (i in seq(length(databaseIds))){
  databaseId = databaseIds[i]
  dataFolder <- file.path(outputFolder, sprintf("Results_%s",databaseId))
  files <- list.files(dataFolder, pattern = "*.csv")
  #files <- c("incidence_rate.csv")
  if (i==1){
    
    # Remove data already in global environment:
    tableNames <- gsub("\\.csv", "", files) 
    camelCaseNames <- SqlRender::snakeCaseToCamelCase(tableNames)
    camelCaseNames <- unique(camelCaseNames)
    rm(list = camelCaseNames)
    
    #load files
    lapply(files, loadFile)
  }else {
    lapply(files, loadFile)
  }
}

#Subsetting the cohorts of interest
cohortIdsSub <- c(135,136, 138) #Hypertension

#Subsetting the cohorts of certain definition
cohortIdsInd <- 135 
personYearsMin <- 1e+5 #100,000
incidenceRateMin <- 1e-4
calendarYearsInt <- c(2010:2021)

incidenceRateSub <- incidenceRate %>% 
  filter(cohortId %in% cohortIdsInd) %>%
  filter(personYears >= personYearsMin) %>%
  filter(incidenceRate >= incidenceRateMin) %>%
  filter(calendarYear %in% calendarYearsInt)