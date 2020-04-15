library(stringr)
setwd("~/Desktop/COVID/covid19-clean/")

# load relevant the data files
source('codes/1_loadData.R')

# load the actual model data
source('codes/model_functions.R')

# load postprocessing functions
source('codes/2_functionsPostprocessing.R')

# Now Execute the models for each of the counties to be used for the Web App and comparisons
popData = read.csv('data/FL-census-data.csv',as.is = TRUE);

for(i in 1:length(unique(popData$NAME))){
  county=unique(popData$NAME)[i]
  if(county!='Geographic Area Name'){
    printablecounty <- str_replace(str_replace(county, ' County, Florida', ''),' ','_')
    indC<-which(popData$NAME == county)[[1]]
    ageVector <- c("Under 5 years", "5 to 9 years", "10 to 14 years","15 to 19 years","20 to 24 years","25 to 29 years","30 to 34 years",
                   "35 to 39 years","40 to 44 years","45 to 49 years","50 to 54 years","55 to 59 years","60 to 64 years","65 to 69 years",
                   "70 to 74 years","75 to 79 years","80 to 84 years","85 years and over")
    popC<-numeric(0)
    propC<-numeric(0)
    id<-numeric(0)
    for(a in 1:length(ageVector)){
      strValP<-paste('Estimate!!Total!!Total population!!AGE!!',ageVector[a], sep="")
      strValPr<-paste('Estimate!!Percent!!Total population!!AGE!!',ageVector[a], sep="")
      indAP<-which(popData[1,] == strValP)[[1]]
      indAPr<-which(popData[1,] == strValPr)[[1]]
      id[a]<-as.numeric(a)
      popC[a]<-as.numeric(popData[[indC,indAP]])
      propC[a]<-0.01*as.numeric(popData[[indC,indAPr]])
    }
    popC[16]<-popC[[16]]+popC[[17]]+popC[[18]]
    propC[16]<-propC[16]+propC[17]+propC[18]
    wuhanpop <- data.frame(id[1:16], popC[1:16], propC[1:16])
    colnames(wuhanpop) <- c("agegroup", "popage","propage")
    
    # load SEIR functions
    source('codes/3_simOutbreak_ncov_SEIR.R')
    source('codes/4_simOutbreak_ncov_SEIcIscR.R')
  }
}
