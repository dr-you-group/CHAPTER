# snake canse to camel case
snakeCaseToCamelCase <- function(string) {
  string <- tolower(string)
  for (letter in letters) {
    string <- gsub(paste("_", letter, sep = ""), toupper(letter), string)
  }
  string <- gsub("_([0-9])", "\\1", string)
  return(string)
}

# Load data from data folder:
loadFile <- function(file, tablesOfInterest = NULL) {
  # file = files[10]
  tableName <- gsub("\\.csv", "", file) 
  camelCaseName <- snakeCaseToCamelCase(tableName)
  newData <- read.csv(file.path(dataFolder, file))
  newData <- data.frame(lapply(newData, function(x){
    if(is.factor(x)) {
      as.character(x)
    } else {x}
  }), stringsAsFactors=FALSE)
  colnames(newData) <- snakeCaseToCamelCase(colnames(newData))
  if (exists(camelCaseName, envir = .GlobalEnv)) {
    existingData <- get(camelCaseName, envir = .GlobalEnv)
    newData <- rbind(existingData, newData)
  }
  assign(camelCaseName, newData, envir = .GlobalEnv)
  invisible(NULL)
}
