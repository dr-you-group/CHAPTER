# Sys.setlocale()
renv::restore()

library(CHAPTER)


# Optional: specify where the temporary files (used by the Andromeda package) will be created:
options(andromedaTempFolder = "C:/andromedaTemp")

# Maximum number of cores to be used:
maxCores <- parallel::detectCores()

# The folder where the study intermediate and result files will be written:
outputFolder <- "C:/Users/paul9/Rprojects/CHAPTERresultsfinal"

# Details for connecting to the server:
# See ?DatabaseConnector::createConnectionDetails for help
db <- DBI::dbConnect(odbc::odbc(),
                     Driver   = "ODBC Driver 18 for SQL Server",
                     Server   = "10.19.10.241",
                     Database = "YUHS_SC",
                     UID      = "paul9567",
                     PWD = "Euez3yz!@#",
                     TrustServerCertificate = "yes",
                     Port     = 1433)


# The name of the database schema where the CDM data can be found:
cdmDatabaseSchema <- "CDM_v531_YUHS.CDM"

# The name of the database schema and table where the study-specific cohorts will be instantiated:
cohortDatabaseSchema <- "cohortdb.changhoonhan"
cohortTable <- "CHAPTERfinalcompact"

# Some meta-information that will be used by the export function:
databaseId <- "YUHS"
databaseName <- "YUHS"
databaseDescription <- "YUHS"

# For some database platforms (e.g. Oracle): define a schema that can be used to emulate temp tables:
options(sqlRenderTempEmulationSchema = NULL)

CHAPTER::executeIncidencePrevalenceFinal(
  dbConnection = db,
  cdmDatabaseSchema,
  cohortDatabaseSchema,
  writePrefix = NULL,
  outputFolder,
  databaseId,
  readCohorts = TRUE,
  minCellCount = 5
)

CohortDiagnostics::preMergeDiagnosticsFiles(dataFolder = outputFolder)

CohortDiagnostics::launchDiagnosticsExplorer(dataFolder = outputFolder)


# Upload the results to the OHDSI SFTP server:
# privateKeyFileName <- ""
# userName <- ""
# CHAPTER::uploadResults(outputFolder, privateKeyFileName, userName)
