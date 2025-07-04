# Copyright 2024 Observational Health Data Sciences and Informatics
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

#' Get the incidence results of the CHAPTER study
#' @details 
#' This function performs the incidence calculations and characterisation
#' of the relevant cohorts of the CHAPTER study.
#' 
#' @param cdm                  A cdm object as created using the
#'                             \code{\link[CDMConnector]{cdmFromCon}} function 
#'                             in the CDMConnector package.
#' @param analyses             A tibble specifying the incidence analyses to
#'                             conduct, retrieved from the settings folder  
#' @param cohortNames          Names of all the cohorts stored in the cdm  
#' @param latestDataAvailability    Date of the latest data availability
#' @param resultsDir           Directory to save the results to
#' @param minCellCount         Minimum number allowed to report for counts.
#' 
#' @importFrom dplyr "%>%"
#' @export
#' 
getIncidenceResults <- function(cdm,
                                analyses,
                                cohortNames,
                                latestDataAvailability,
                                resultsDir,
                                minCellCount) {
  
  ######################################################
  ParallelLogger::logInfo("- Getting table ones")
  
  characteristics <- list()
  for(tn in cohortNames) {
    characteristics[[tn]] <- CohortCharacteristics::summariseCharacteristics(
      cohort = cdm[[tn]],
      ageGroup = list(c(0, 19), c(20, 39), c(40, 59), c(60, 79), c(80, 150)) 
    )
  }
  
  characteristics <- omopgenerics::bind(characteristics)
  
  ######################################################
  ParallelLogger::logInfo("- Getting incidence all population denominator")
  
  cdm <- IncidencePrevalence::generateDenominatorCohortSet(
    cdm =  cdm,
    name = "denominator",
    cohortDateRange = c(as.Date("2018-01-01"), as.Date(latestDataAvailability)),
    daysPriorObservation = 365,
    sex = c("Male", "Female", "Both"),
    ageGroup = list(c(0,18),c(19,39),c(40,65),c(66,150),c(0,150))
  )
  
  # Get tableOne all population denominator too
  characteristics_denom <- CohortCharacteristics::summariseCharacteristics(
    cohort = cdm[["denominator"]],
    ageGroup = list(c(0, 18), c(19, 39), c(40, 59), c(60, 79), c(80, 150)) 
  )

  # Incidence for all diagnoses in the whole population
  outcomesAllPop <- analyses %>%
    dplyr::filter(denominator_table_name == "all_population") %>%
    dplyr::select(outcome_id, outcome_table_name)
  
  inc <- list()
  for(tn in outcomesAllPop %>% dplyr::pull(outcome_table_name) %>% unique()) {
    
    outcomeIdsAllPop <- attr(cdm[[tn]], "cohort_set") |> 
      dplyr::pull(cohort_definition_id) |> 
      unique()
    
    inc[[tn]] <- IncidencePrevalence::estimateIncidence(
      cdm = cdm,
      denominatorTable = "denominator",
      outcomeTable = tn,
      outcomeCohortId = outcomeIdsAllPop,
      interval = c("years","months","overall"),
      completeDatabaseIntervals = FALSE)
    
  }
  inc <- omopgenerics::bind(inc)
  
  final_results <- omopgenerics::bind(
    characteristics,
    characteristics_denom,
    inc
  )
  
  omopgenerics::exportSummarisedResult(final_results,
                                       minCellCount = minCellCount,
                                       path = resultsDir)
}
