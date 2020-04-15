library(ggplot2)
library(reshape2)
library(RColorBrewer)
library(gmodels)
# plotting the results

options(scipen=12)

getDynamicsPlot <- function(df.I, df.S, daterange, Peak=NULL){
  dfI <- data.frame(Time=daterange, Median=df.I$median, UCI=df.I$uci, LCI=df.I$lci, Var="I")
  # dfS <- data.frame(Time=daterange, Median=df.S$median, UCI=df.S$uci, LCI=df.S$lci, Var="S")
  # df.plot <- rbind(dfI, dfS)
  p <- ggplot(dfI, aes(x=Time, y=Median, colour=Var)) + geom_line(size=1) + 
    theme(legend.title = element_blank(), axis.title.x = element_blank()) + 
    geom_ribbon(aes(ymin=LCI, ymax=UCI, fill=Var), alpha=0.4, color=F) + 
    theme_bw() + ylab("Individuals") +
    scale_color_brewer(palette="Set1") + scale_fill_brewer(palette="Set1") +
    geom_vline(aes(xintercept=daterange[median(Peak)]), linetype="dashed", size=1) +
    geom_vline(aes(xintercept=daterange[round(quantile(Peak)[2],0)] ), linetype="dotted", size=0.75) +
    geom_vline(aes(xintercept=daterange[round(quantile(Peak)[4],0)] ), linetype="dotted", size=0.75) +
    guides(fill=FALSE, colour=FALSE) + scale_x_date(date_minor_breaks = "1 month")
  return(p)
}

getOneSimPlot <- function(df, daterange, Peak=NULL, df.data=NULL, focusdata=F, title="Single Simulation"){
  df.plot <- data.frame(Time=daterange, S=rowSums(df$S), E=rowSums(df$E), I=rowSums(df$I), R=rowSums(df$R))
  df.plot <- melt(df.plot, id.vars=c("Time"))
  p <- ggplot(df.plot, aes(x=Time, y=value, colour=variable)) + geom_line(size=1) +
    theme_bw() + theme(legend.title = element_blank(), axis.title.x = element_blank()) + ylab("Individuals") + ggtitle(title) +
    scale_color_brewer(palette="Set1")
  if(focusdata){
    p <- p + geom_point(data=df.data, aes(x=Time, y=Cases), inherit.aes=F, size=1) + 
      scale_y_continuous(limits=c(0,max(df.data$Cases+500))) + scale_x_date(limits = c(min(daterange), max(df.data$Time)))
  }
  return(p)
}

getPeakPlot <- function(peakdata.3, daterange=daterange){
  df <- data.frame(InfDur="5", PeakTime=daterange[peakdata.3[[1]]$peaktime], PeakSize=peakdata.3[[1]]$peaksize)
  p <- ggplot(df, aes(x=PeakTime, y=PeakSize, color=InfDur, shape=InfDur)) + geom_point() + theme_bw() + 
    scale_color_brewer(palette="Dark2") + ylab("Peak Infections") + theme(legend.title = element_blank(), axis.title.x = element_blank())
  return(p)
}

getAgePlot <- function()
  
dateStart = as.Date("2020-03-17")
dateEnd = as.Date("2021-05-18")
daterange <- seq(dateStart, dateEnd, 1)

# Duration of Infection = 3 days
load("./outputs/SEIR/covid_SDurInf3.rdata")
load("./outputs/SEIR/covid_IDurInf3.rdata")
load("./outputs/SEIR/peaktime_DurInf3.rdata")
load("./outputs/SEIR/covid_DurInf3.rdata")
load("./outputs/SEIR/AGEcovid_IDurInf3.rdata")
load("./outputs/SEIR/epiFirstSimDurInf3.rdata")

# Load some actual data
hillsdata<-read.csv("~/Dropbox/Covid - modeling/ModelResults/HillsData/HillsData2.csv")
CaseData<-data.frame(hillsdata, Time=daterange[1:length(hillsdata$Cases)])

# Do nothing strategy plots
peakSummary <- ci(peaktime_DurInf3[[1]]$peaktime, confidence=0.95)
getPeakPlot(peaktime_DurInf3, daterange=daterange)

donothing.1plot <- getOneSimPlot(covid_IDurInf7[[1]]$Sim1, daterange)
donothing.1plot.with.data <- getOneSimPlot(covid_IDurInf7[[1]]$Sim1, daterange, df.data=CaseData, focusdata = T)


overall <- getDynamicsPlot(covid_IDurInf3[[1]]$summary, covid_SDurInf3[[1]]$summary,
                           daterange, Peak=peaktime_DurInf3[[1]]$peaktime)

layout_matrix= rbind(c(1,1,2),
                     c(3,3,NA))

grid.arrange(donothing.1plot, donothing.1plot.with.data, overall, layout_matrix=layout_matrix)



# 
# 
# 
# 
# 
# 
# # Looking at the do nothing results (Item 1 in each of the lists), 3 Day Infection Duration
# strategy0SEIR = data.frame(Time=daterange, Strategy="Do Nothing", Incidence=covid_SDurInf3[[1]]$summary$median*covid_IDurInf3[[1]]$summary$median/100000)
# strategy1SEIR = data.frame(Time=daterange, Strategy="Strategy 1", Incidence=covid_SDurInf3[[2]]$summary$median*covid_IDurInf3[[2]]$summary$median/100000)
# strategy2SEIR = data.frame(Time=daterange, Strategy="Strategy 2", Incidence=covid_SDurInf3[[3]]$summary$median*covid_IDurInf3[[3]]$summary$median/100000)
# strategy3SEIR = data.frame(Time=daterange, Strategy="Strategy 3", Incidence=covid_SDurInf3[[4]]$summary$median*covid_IDurInf3[[4]]$summary$median/100000)
# SEIR_DurInf3 = rbind(strategy0SEIR, strategy1SEIR, strategy2SEIR, strategy3SEIR) 
# 
# ggplot(SEIR_DurInf3, aes(x=Time, y=Incidence, color=Strategy)) +
#   annotate("rect", xmin=covid_SDurInf3[[1]]$Sim1$dateStartSchoolClosure, 
#            xmax=covid_SDurInf3[[1]]$Sim1$dateEndIntenseIntervention, ymin=0, ymax=Inf, 
#            fill="navy", alpha=0.3) +
#   annotate("rect", xmin=covid_SDurInf3[[1]]$Sim1$dateStartIntenseIntervention, 
#            xmax=covid_SDurInf3[[1]]$Sim1$dateEndIntenseIntervention, ymin=0, ymax=Inf, 
#            fill="navy", alpha=0.3) +
#   ylab("Incidence per 100,000") + geom_line(size=1) + theme_bw() + ggtitle("Total Incidence using SEIR by strategy; Duration of Infection = 3")
# 
# 
# # Do Nothing By Age
# median.by.age <- as.data.frame(AGEcovid_IDurInf3[[1]]$summary$median)
# uci.by.age <- melt(as.data.frame(AGEcovid_IDurInf3[[1]]$summary$uci))
# lci.by.age <- melt(as.data.frame(AGEcovid_IDurInf3[[1]]$summary$lci))
# colnames(median.by.age) <- seq(1,16,1)
# median.by.age <- cbind(data.frame(Time=daterange), median.by.age)
# incidence.by.age <- melt(median.by.age, id.vars=c("Time"))
# donothing.by.age <- cbind(incidence.by.age, uci.by.age$value, lci.by.age$value)
# colnames(donothing.by.age) <- c("Time","Age.Group","Median","U.CI","L.CI")
# 
# p.do.nothing.by.age <- ggplot(donothing.by.age, aes(x=Time,y=Median,colour=Age.Group)) + 
#   geom_ribbon(data=donothing.by.age, aes(x=Time, ymin=L.CI, ymax=U.CI, group=Age.Group), fill = "grey90", inherit.aes=F) +
#   geom_line() + theme_bw() + ylab("Incidence (per ?? or ??)") + facet_wrap(.~Age.Group, ncol=8) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#   guides(color=F) + ggtitle("Do Nothing Policy Incidence by Age, 3 Day Duration")
# p.do.nothing.by.age
#                                                                                                                                                                                                   
# # Looking at the do nothing results (Item 1 in each of the lists), 7!!! Day Infection Duration
# strategy0SEIR = data.frame(Time=daterange, Strategy="Do Nothing", Incidence=covid_SDurInf7[[1]]$summary$median*covid_IDurInf7[[1]]$summary$median/100000)
# strategy1SEIR = data.frame(Time=daterange, Strategy="Strategy 1", Incidence=covid_SDurInf7[[2]]$summary$median*covid_IDurInf7[[2]]$summary$median/100000)
# strategy2SEIR = data.frame(Time=daterange, Strategy="Strategy 2", Incidence=covid_SDurInf7[[3]]$summary$median*covid_IDurInf7[[3]]$summary$median/100000)
# strategy3SEIR = data.frame(Time=daterange, Strategy="Strategy 3", Incidence=covid_SDurInf7[[4]]$summary$median*covid_IDurInf7[[4]]$summary$median/100000)
# SEIR_DurInf7 = rbind(strategy0SEIR, strategy1SEIR, strategy2SEIR, strategy3SEIR) 
# 
# ggplot(SEIR_DurInf7, aes(x=Time, y=Incidence, color=Strategy)) +
#   annotate("rect", xmin=covid_SDurInf3[[1]]$Sim1$dateStartSchoolClosure, 
#            xmax=covid_SDurInf3[[1]]$Sim1$dateEndIntenseIntervention, ymin=0, ymax=Inf, 
#            fill="navy", alpha=0.3) +
#   annotate("rect", xmin=covid_SDurInf3[[1]]$Sim1$dateStartIntenseIntervention, 
#            xmax=covid_SDurInf3[[1]]$Sim1$dateEndIntenseIntervention, ymin=0, ymax=Inf, 
#            fill="navy", alpha=0.3) +
#   ylab("Incidence per 100,000") + geom_line(size=1) + theme_bw() + ggtitle("Total Incidence using SEIR by strategy; Duration of Infection = 7")
# 
# load("./outputs/SEIR/AGEcovid_IDurInf3.rdata")
# # Do Nothing By Age
# median.by.age <- as.data.frame(AGEcovid_IDurInf7[[1]]$summary$median)
# uci.by.age <- melt(as.data.frame(AGEcovid_IDurInf7[[1]]$summary$uci))
# lci.by.age <- melt(as.data.frame(AGEcovid_IDurInf7[[1]]$summary$lci))
# colnames(median.by.age) <- seq(1,16,1)
# median.by.age <- cbind(data.frame(Time=daterange), median.by.age)
# incidence.by.age <- melt(median.by.age, id.vars=c("Time"))
# donothing.by.age <- cbind(incidence.by.age, uci.by.age$value, lci.by.age$value)
# colnames(donothing.by.age) <- c("Time","Age.Group","Median","U.CI","L.CI")
# 
# p.do.nothing.by.age <- ggplot(donothing.by.age, aes(x=Time,y=Median,colour=Age.Group)) + 
#   geom_ribbon(data=donothing.by.age, aes(x=Time, ymin=L.CI, ymax=U.CI, group=Age.Group), fill = "grey90", inherit.aes=F) +
#   geom_line() + theme_bw() + ylab("Incidence (per ?? or ??)") + facet_wrap(.~Age.Group, ncol=8) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#   guides(color=F) + ggtitle("Do Nothing Policy Incidence by Age, 7 Day Duration")
# p.do.nothing.by.age
s

