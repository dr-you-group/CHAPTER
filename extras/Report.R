# install.packages(c("foreign", "tsModel", "lmtest", "Epi", "splines", "vcd", "dplyr", "qcc"))
library(dplyr)
library(ggplot2)
# library(foreign)
# library(tsModel)
# library("lmtest")
# library("Epi")
# library("splines")
# library("vcd")

databaseIds <- c("Australia_LPD", "Japan_Claims")
rootDataFolder <- "./data"
outputFolder <- "./data/output"
source("./extras/FunctionsForReporting.R")

####Configuration####
personYearsMin <- 1e+5 #100,000
incidenceRateMin <- 1e-4
calendarYearsInt <- c(2010:2021)

####Data load####
#load files into the environment
for (i in seq(length(databaseIds))){
  databaseId = databaseIds[i]
  dataFolder <- file.path(rootDataFolder, sprintf("Results_%s",databaseId))
  # files <- list.files(dataFolder, pattern = "*.csv")
  files <- c("incidence_rate.csv")
  if (i==1){
    
    # Remove data already in global environment:
    tableNames <- gsub("\\.csv", "", files) 
    camelCaseNames <- snakeCaseToCamelCase(tableNames)
    camelCaseNames <- unique(camelCaseNames)
    rm(list = camelCaseNames)
    
    #load files
    lapply(files, loadFile)
  }else {
    lapply(files, loadFile)
  }
}

#Subsetting the cohorts of interest
cohortIdsSub <- c(88:92, 96:99, 135,136,138,141, 142:161) #Hypertension

for (cohortIdsInd in cohortIdsSub){
  # cohortIdsInd <- 138 
  
  incidenceRateSub <- incidenceRate %>% 
    filter(cohortId %in% cohortIdsInd) %>%
    filter(personYears >= personYearsMin) %>%
    filter(incidenceRate >= incidenceRateMin) %>%
    filter(calendarYear %in% calendarYearsInt)
  
  #make interruption column   
  incidenceRateSub <- incidenceRateSub %>%
    mutate(interruption = ifelse(calendarYear >= 2020,1,0)) #covid
  
  #### Interrupted time-series analysis ####
  for(databaseId in databaseIds){
    irSubInd <- incidenceRateSub %>% filter(databaseId == !!databaseId)
    irSubIndTotalPop <- irSubInd %>% filter(is.na(gender)) %>% filter(is.na(ageGroup))
    varData <- irSubIndTotalPop %>% select(cohortCount, personYears, incidenceRate, interruption, calendarYear)
    varData$time <- as.numeric(factor(varData$calendarYear))
    if(!(max(varData$time)-1) == max(varData$calendarYear)-min(varData$calendarYear)){
      next #make sure that the years are consecutive  ##should be recorded in the log
    } 
    if(nrow(varData)==0) next ##should be recorded in the log
    timespan = seq(max(varData$time))
    timespanInterrupted = (max(varData$time)-sum(varData$interruption==1)+1):max(varData$time)
    
    # fit poisson regression
    fitModel <- glm(cohortCount ~ interruption+time,
                    offset = log(personYears), 
                    family = "poisson", 
                    data = varData)
    
    summary(fitModel)$dispersion #check dispersion ##should be recorded in the log
    
    Epi::ci.lin(fitModel,Exp=T)["interruption",]
    Epi::ci.lin(fitModel,Exp=T)["interruption",4:7]
    
    #predict and plot the model
    datanew <- data.frame(personYears = mean(varData$personYears), 
                          interruption=rep(c(0,1),c(sum(varData$interruption==0),sum(varData$interruption==1))), 
                          time= varData$time)
    counterfactual <- data.frame(personYears = mean(varData$personYears), 
                                 interruption=0,
                                 time= varData$time)

    pred <- predict(fitModel,type="response",datanew) / mean(varData$personYears) * 1000
    predCounter <- predict(fitModel,type="response",counterfactual) / mean(varData$personYears) * 1000
    predCounter <- predCounter[c(min(timespanInterrupted)-1,timespanInterrupted)]
    counterSeq = c(min(timespanInterrupted)-1,timespanInterrupted)
    
    # If you want to change colors, modify this
    colors <- c("Fitted values" = "cyan3",
                "Observed values" = "black",
                "Counterfactual" = "red")
    
    g <- ggplot() + 
      geom_point(data = varData, aes(x = time, y = incidenceRate), shape = 1) + 
      geom_line(data = varData, aes(x = time, y = incidenceRate, color = "Observed values"), size = 0.2) +
      geom_line(aes(x = timespan, y = pred, color = "Fitted values"), size = 0.8) +
      geom_line(aes(x = counterSeq, y = predCounter, color = "Counterfactual"), lty = 2, size = 0.8) +
      scale_x_continuous(breaks = timespan, labels = varData$calendarYear) +
      labs(color="", x = "Year", y = "Incidence rate (/1000PYs)") +
      scale_color_manual(values = colors) +
      theme_bw() 
    g
    
    #Save the plot
    if(!file.exists(outputFolder)){
      dir.create(outputFolder)
    }
    ggsave(file.path(outputFolder, sprintf("incidence_rate_plot_cohort%s_db%s.png", cohortIdsInd, databaseId)), g)
  }
}

