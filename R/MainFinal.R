# Copyright 2025 Observational Health Data Sciences and Informatics
#
# This file is part of CHAPTER
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#' Execute the CHAPTER study
#' 
#' @details 
#' This function executes the incidence analyses of CHAPTER study. 
#' 
#' @param dbConnection         An object of type \code{dbConnection} as created using the
#'                             \code{\link[DBI]{dbConnect}} function in the
#'                             DBI package.
#' @param cdmDatabaseSchema    Schema name where your patient-level data in OMOP CDM format resides.
#'                             Note that for SQL Server, this should include both the database and
#'                             schema name, for example 'cdm_data.dbo'.
#' @param cohortDatabaseSchema Schema name where intermediate data can be stored. You will need to have
#'                             write privileges in this schema.
#' @param writePrefix.         Write prefix for the tables to be created in the write schema.
#' @param outputFolder         Name of local folder to place results; make sure to use forward slashes
#'                             (/). Do not use a folder on a network drive since this greatly impacts
#'                             performance.
#' @param databaseId           A short string for identifying the database (e.g.
#'                             'Synpuf').
#' @param readCohorts          A boolean to choose whether to read and instantiate the initial cohorts
#'                             from the available jsons.
#' @param minCellCount         Minimum number allowed to report for counts.
#'                     
#' @importFrom dplyr "%>%"
#' @export
executeIncidencePrevalenceFinal <- function(dbConnection,
                                            cdmDatabaseSchema,
                                            cohortDatabaseSchema,
                                            writePrefix,
                                            outputFolder,
                                            databaseId,
                                            readCohorts = TRUE,
                                            minCellCount = 5){
  
  # Create zip file for results
  zipName <- paste0(databaseId,"_ResultsFinal")
  resultsDir <- file.path(outputFolder, zipName)
  if (!file.exists(resultsDir))
    dir.create(resultsDir, recursive = TRUE)

  # Start log
  ParallelLogger::addDefaultFileLogger(file.path(outputFolder, "log.txt"))
  ParallelLogger::addDefaultErrorReportLogger(file.path(outputFolder, "errorReportR.txt"))
  on.exit(ParallelLogger::unregisterLogger("DEFAULT_FILE_LOGGER", silent = TRUE))
  on.exit(ParallelLogger::unregisterLogger("DEFAULT_ERRORREPORT_LOGGER", silent = TRUE), add = TRUE)
  
  cdm <- cdmFromCon(dbConnection, cdmDatabaseSchema,
                    writeSchema = cohortDatabaseSchema,
                    writePrefix = writePrefix,
                    cdmName = databaseId)
  
  # Get the cdm snapshot
  snapshot <- cdm |>
    OmopSketch::summariseOmopSnapshot()

  omopgenerics::exportSummarisedResult(snapshot,
                                       minCellCount = minCellCount,
                                       fileName = "snapshot_{cdm_name}_{date}.csv",
                                       path = resultsDir)
  
  ParallelLogger::logInfo("CDM snapshot retrieved")
  
  # Retrieve the cohorts from the json files
  if(readCohorts) {
    cohortsToCreate <- system.file("settings",
                                   "CohortsToCreateFinal.csv",
                                   package = "CHAPTER") %>% read.csv()
    
    tableNames <- cohortsToCreate %>%
      dplyr::pull("table_name") %>%
      unique()
    
    for(tn in tableNames) {
      folderName <- sub('^(\\w?)', '\\U\\1', tn, perl=T)
      folderName <- gsub('\\_(\\w?)', '\\U\\1', folderName, perl=T)
      cohortsToCreateWorking <- CDMConnector::readCohortSet(
        system.file("cohorts_final",folderName, package = "CHAPTER"))
      
      cdm <- CDMConnector::generateCohortSet(cdm, 
                                             cohortsToCreateWorking,
                                             name = tn,
                                             overwrite = TRUE)
    }
  } else {
    cdm <- cdmFromCon(dbConnection, cdmDatabaseSchema,
                      writeSchema = cohortDatabaseSchema,
                      cdmName = databaseId,
                      writePrefix = writePrefix,
                      cohortTables = c("allergy_cohorts",
                                       "cancer_cohorts",
                                       "cardio_cohorts",
                                       "general_cohorts",
                                       "pulmo_cohorts"))
  }
  ParallelLogger::logInfo("Initial cohorts read and instantiated")
  
  latestDataAvailability <- cdm$observation_period %>%
    dplyr::select(observation_period_end_date) %>%
    dplyr::filter(observation_period_end_date == max(observation_period_end_date)) %>%
    dplyr::pull() %>% unique() %>% as.Date()
  
  cohortNames <- c("allergy_cohorts",
                   "cancer_cohorts",
                   "cardio_cohorts",
                   "general_cohorts",
                   "pulmo_cohorts")
  
  # Retrieve incidence analyses to perform
  analysesToDo <- system.file("settings",
                              "AnalysesToPerformFinal.csv",
                              package = "CHAPTER") %>% read.csv()
  
  # Do the incidence calculations
  getIncidenceResults(cdm = cdm,
                      analyses = analysesToDo,
                      cohortNames = cohortNames,
                      latestDataAvailability = latestDataAvailability,
                      resultsDir = resultsDir,
                      minCellCount = minCellCount)
  ParallelLogger::logInfo("Incidence results calculated")
  
  # Zip the final results
  info(logger, 'EXPORT RESULTS')
  zip::zip(
    zipfile = here(
      resultsFolder, paste0("results-", cdmName(cdm), ".zip")
    ), 
    files = list.files(resultsFolder, recursive = TRUE, full.names = TRUE)
  )

  ParallelLogger::logInfo("Saved all results")
  
  print("Done!")
  print("If all has worked, there should now be a zip file with your results
         in the output folder to share")
  print("Thank you for running the study!")
}
