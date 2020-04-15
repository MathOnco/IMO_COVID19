library(ggplot2)
library(scales)
library(lubridate)
library(dplyr)

dateStart = as.Date("2020-03-17")
dateEnd = as.Date("2021-05-18")
daterange <- seq(dateStart, dateEnd, 1)

loadRData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}

readRData <- function(url, county) {
  out <- tryCatch(
    { loadRData(url) },
    error=function(cond) {
      message(county)
      return(NA)
    },
    warning=function(cond) {
      message(county)
      return(NA)
    }
  )    
  return(out)
}

# Prepare File of County Names
popData = read.csv('data/FL-census-data.csv',as.is = TRUE);
counties <- data.frame(matrix(NA, nrow=0, ncol=2))
for(i in 1:length(unique(popData$NAME))){
  county=unique(popData$NAME)[i]
  if(county!='Geographic Area Name'){
    retrievablecounty <- str_replace(str_replace(county, ' County, Florida', ''),' ','_')
    printablecounty <- str_replace(county, ' County, Florida', '')
    counties <- rbind(counties, data.frame(county, printablecounty, retrievablecounty))
  }
}
save(counties, file="../Florida_COVID19/data/FloridaCountyNames.rdata")

# Prep our age vectors
ageVector <- c("Under 5 years", "5 to 9 years", "10 to 14 years","15 to 19 years","20 to 24 years","25 to 29 years","30 to 34 years",
               "35 to 39 years","40 to 44 years","45 to 49 years","50 to 54 years","55 to 59 years","60 to 64 years","65 to 69 years",
               "70 to 74 years","75 years and over")
save(ageVector, file="../Florida_COVID19/data/AgeVector.rdata")

# Prepare Peak Dates
peakData <- data.frame(matrix(NA, nrow=0, ncol=3))
for(i in 1:length(unique(popData$NAME))){
  county=unique(popData$NAME)[i]
  if(county!='Geographic Area Name'){
    retrievablecounty <- str_replace(str_replace(county, ' County, Florida', ''),' ','_')
    printablecounty <- str_replace(county, ' County, Florida', '')
    
    countySEIRPeak <- readRData(paste0("./outputs/SEIR/",retrievablecounty,".peaktime_DurInf3.rdata"), retrievablecounty)
    countySEIcIscRPeak <- readRData(paste0("./outputs/SEIcIscR/",retrievablecounty,".peaktime_DurInf3.rdata"), retrievablecounty)
    if(is.na(countySEIRPeak)==FALSE & is.na(countySEIcIscRPeak)==FALSE ){
      peakData <- rbind(peakData, data.frame(printablecounty, daterange[median(countySEIRPeak[[1]]$peaktime)],daterange[median(countySEIcIscRPeak[[1]]$peaktime)], median(countySEIcIscRPeak[[1]]$peaksize), median(countySEIcIscRPeak[[1]]$peaksize)))
    }
  }
}
colnames(peakData) <- c("County", "SEIR.peak", "SEIcIscR.peak","SEIR.peakSize","SEIcIscR.peakSize")
save(peakData, file="../Florida_COVID19/data/peakData.rdata")

# Prepare Cumulative Cases Plot
incidenceTotals <- data.frame(matrix(NA, nrow=0, ncol=6))
for(i in 1:length(unique(popData$NAME))){
  county=unique(popData$NAME)[i]
  if(county!='Geographic Area Name'){
    retrievablecounty <- str_replace(str_replace(county, ' County, Florida', ''),' ','_')
    printablecounty <- str_replace(county, ' County, Florida', '')
    
    countyI <- readRData(paste0("./outputs/SEIR/",retrievablecounty,".covid_IDurInf3.rdata"), retrievablecounty)
    countyS <- readRData(paste0("./outputs/SEIR/",retrievablecounty,".covid_SDurInf3.rdata"), retrievablecounty)
    
    # countySEIcIscRPeak <- readRData(paste0("./outputs/SEIcIscR/",retrievablecounty,".peaktime_DurInf3.rdata"), retrievablecounty)
    if(is.na(countyI)==FALSE ){
      df <- data.frame(County=printablecounty, Time=daterange, Cases=countyI[[1]]$summary$median, LCI=countyI[[1]]$summary$lci, UCI=countyI[[1]]$summary$uci, CumSum=cumsum(countyI[[1]]$summary$median))
      incidenceTotals <- rbind(incidenceTotals, df)
    }
  }
}
allCounties <- as.data.frame(aggregate(incidenceTotals$Cases, by=list(incidenceTotals$Time), sum))
allCounties$County <- "All Counties"
allCounties$UCI <- 0
allCounties$LCI <- 0
allCounties$CumSum <- cumsum(allCounties$x)
colnames(allCounties) <- c("Time","Cases","County","UCI","LCI","CumSum")
incidenceTotals <- rbind(incidenceTotals, allCounties)
save(incidenceTotals, file="../Florida_COVID19/data/incidenceTotals.rdata")

ggplot(incidenceTotals, aes(x=Time, y=CumSum, color=County)) + geom_line() + guides(fill=F, color=F)








