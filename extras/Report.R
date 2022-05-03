library(dplyr)
library(foreign)
library(tsModel)
library("lmtest")
library("Epi")
library("splines")
library("vcd")
library(ggplot2)

databaseIds <- c("Australia_LPD", "Japan_Claims")
outputFolder <- "./data"
source("./extras/FunctionsForReporting.R")

#dataFolderAus <- "./data/Results_Australia_LPD"
#dataFolderJp <- "./data/Results_Japan_Claims"

####Data load####
#load files into the environment
for (i in seq(length(databaseIds))){
  databaseId = databaseIds[i]
  dataFolder <- file.path(outputFolder, sprintf("Results_%s",databaseId))
  #files <- list.files(dataFolder, pattern = "*.csv")
  files <- c("incidence_rate.csv")
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
cohortIdsSub <- c(135,136,138) #Hypertension

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

#make covid column   
incidenceRateSub <- incidenceRateSub %>%
  mutate(covid = ifelse(calendarYear >= 2020,1,0))


#### Interrupted time-series analysis ####

Australia <- incidenceRateSub %>% filter(databaseId == "Australia_LPD")
totalAustralia <- Australia %>% filter(is.na(gender)) %>% filter(is.na(ageGroup))

varData <- totalAustralia %>% select(cohortCount, covid)
varData$time <- c(1:nrow(varData))

#over-dispersion test
qcc::qcc.overdispersion.test(varData$cohortCount,type="poisson")

# fit poisson regression
fitModel <- glm(cohortCount ~ covid+time,family = quasipoisson, data = varData)

ci.lin(fitModel,Exp=T)["covid",4:7]

# # Autocorrelation test
# res3 <- residuals(fitModel,type="deviance")
# 
# #tiff(filename = "./plot/acf_total_selfharm.tif",
# #     width = 950, height = 550, units = "px", pointsize = 16)
# par(mfrow = c(1,2))
# acf(res3, main = "")
# pacf(res3, main = "")
# #dev.off()

#predict and plot the model
datanew <- data.frame(covid=rep(c(0,1),c(70,11)), time= 10:90/10)
counterfactual <- data.frame(covid=0,time= 10:90/10)

pred <- predict(fitModel,type="response",datanew)
predCounter <- predict(fitModel,type="response",counterfactual)
predCounter <- predCounter[71:81]

g <- ggplot() + 
  geom_point(data = varData, aes(x = time, y = cohortCount), shape = 1) + 
  geom_line(aes(x= 10:90/10, y = pred, color = "Fitted values"), size = 0.8) + 
  geom_line(aes(x= 80:90/10, y = predCounter, color = "Counterfactual"), lty = 2, size = 0.8) +
  scale_x_continuous(limits = c(1,9), breaks = c(1:9), labels = as.character(c(2013:2021))) +
  labs(color="", x = "year", y = "count") +
  theme_bw() 

g

