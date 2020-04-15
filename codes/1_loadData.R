require(data.table)
require(Matrix)
require(matrixcalc)

## load data: Wuhan's population age structure and Contact matrices

loadPopData = FALSE
loadFLData = TRUE
loadContactMatrices = TRUE
loadCaseData =TRUE
loadR0posterior =TRUE


## load data: Population age structure (currently using China's) and Contact matrices

# 1) population data
if(loadPopData) 
{ 
  wuhanpop = read.csv('data/wuhanpop.csv',as.is = TRUE)
}

# if(loadFLData)
# {
#   popData = read.csv('data/FL-census-data.csv',as.is = TRUE);
#   indC<-which(popData$NAME == 'Miami-Dade County, Florida')[[1]]
#   ageVector <- c("Under 5 years", "5 to 9 years", "10 to 14 years","15 to 19 years","20 to 24 years","25 to 29 years","30 to 34 years",
#                  "35 to 39 years","40 to 44 years","45 to 49 years","50 to 54 years","55 to 59 years","60 to 64 years","65 to 69 years",
#                  "70 to 74 years","75 to 79 years","80 to 84 years","85 years and over")
#   popC<-numeric(0)
#   propC<-numeric(0)
#   id<-numeric(0)
#   for(a in 1:length(ageVector)){
#     strValP<-paste('Estimate!!Total!!Total population!!AGE!!',ageVector[a], sep="")
#     strValPr<-paste('Estimate!!Percent!!Total population!!AGE!!',ageVector[a], sep="")
#     indAP<-which(popData[1,] == strValP)[[1]]
#     indAPr<-which(popData[1,] == strValPr)[[1]]
#     id[a]<-as.numeric(a)
#     popC[a]<-as.numeric(popData[[indC,indAP]])
#     propC[a]<-0.01*as.numeric(popData[[indC,indAPr]])
#   }
#   popC[16]<-popC[[16]]+popC[[17]]+popC[[18]]
#   propC[16]<-propC[16]+propC[17]+propC[18]
#   wuhanpop <- data.frame(id[1:16], popC[1:16], propC[1:16])
#   colnames(wuhanpop) <- c("agegroup", "popage","propage")
# }

# 2) (projected) contact matrices 
### Acknowlegments: codes from Petra Klepac (petrakle) 
normalize.contact.matrices <- function(C, popv, make.sym = F){
  # FUN normalize them so that
  # the matrix with all the contacts is normalized so that its dominant eigenvalue is 1 
  # and other matrices keep their contributions 
  if (make.sym){
    Csym <- lapply(C, function(x, popv) (x + t(x)*((popv)%*%t(1/popv)))/2, popv) # make sure contacts are reciprocal
  } else {
    Csym <- C # if make.sym = F leave it as is
  }
  eig1 <- Re(eigen(Csym["all"]$all)$value[1])  # save dominant eigenvalue of the matrix with all contacts
  
  # divide all matrices by the real part of the dominant matrix of all of the contacts
  # that way all the rescaled matrices still sum to C_all = C_work + C_home + C_school + C_other
  Cnorm <- lapply(Csym, function(x,eig1) x/eig1, eig1)
  
  return(Cnorm)
}

# Synthetic contact matrices for China from Prem, Cook and Jit (2017) https://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1005697   
if(loadContactMatrices)
{
  load(paste0('data/contacts_china.rdata'))
  contacts <- contacts_china # normalize.contact.matrices(contacts_china,wuhanpop$popage, make.sym=T)
  rm(contacts_china)
}

# case age distribution
if(loadCaseData)
{
  wuhancaseraw = read.csv('data/wuhan_pop_case_dist.csv',as.is = TRUE)
  caseage = rep(wuhancaseraw$wuhan,each=2)/2
  wuhancase = data.frame(agegroup = 1:16, caseage = c(caseage[1:15],sum(caseage[16:20])))
  rm(wuhancaseraw,caseage)
}

if(loadR0posterior)
{
  # --- read in R0 posterior
  R0_plot <-read.csv(paste0("data/out_R0.csv"))
  R0_dates <- read.csv(paste0('data/out_date.csv'))
  start_date <- as.Date(R0_dates[1,1]) # first case
  end_date <- as.Date(R0_dates[nrow(R0_dates),1]) # period to forecast ahead
  date_range <- seq.Date(start_date,end_date,1)
  
  # extract all estimates from 01.01.2020 - 23.01.2020
  R0_posterior <- R0_plot[which(date_range == as.Date("2020-01-01") ):which(date_range == as.Date("2020-01-23")),]
  range(R0_posterior)
  r0posterior = as.vector((unlist(R0_posterior)))
  par(mfrow=c(2,1))
  R0_dense = (density((r0posterior)))
  plot(x = R0_dense$x,y=R0_dense$y,type='l',xlab='R0',ylab='Density',lwd=2)
  R0_dense = (density(log(r0posterior)))
  plot(x = R0_dense$x,y=R0_dense$y,type='l',xlab='ln(R0)',ylab='Density',lwd=2)
  
  
  rm(R0_dense,R0_plot,R0_posterior,date_range,end_date,start_date)
  
}


rm(loadContactMatrices,loadPopData,loadR0posterior,loadCaseData)
