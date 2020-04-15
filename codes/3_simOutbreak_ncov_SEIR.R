## To simulate n_simSEIR outbreaks

nsim = 200

set.seed(123)

r0postCrI = r0posterior
R0est = sample(x = r0postCrI,size = nsim)

## To simulate n_simSEIR outbreaks: duration of infection = 5 days, initial infected n=180 infected
epi_doNothingDurInf3 = vector('list',nsim)
epi_baseDurInf3 = vector('list',nsim)
epi_marchDurInf3 = vector('list',nsim)
epi_aprilDurInf3 = vector('list',nsim)
start = Sys.time()
durInfSim = 5
initialI = 180
for(sim in 1:nsim)
{
  epi_doNothingDurInf3[[sim]] = simulateOutbreakSEIR(R0t =R0est[sim] ,rho = rep(0.5,3660), dateStart=as.Date('2020-03-17') , dateStartSchoolClosure = as.Date('2020-03-16'),
                                                     dateStartIntenseIntervention = as.Date('2020-04-03'), dateEndIntenseIntervention = as.Date('2020-04-30'),
                                                     pWorkOpen = c(1,1,1,1),numWeekStagger = c(0,0,0),pInfected=initialI,durInf = durInfSim)
  # epi_baseDurInf3[[sim]] = simulateOutbreakSEIR(R0t =R0est[sim] ,rho = rep(0.5,3660), dateStart=as.Date('2020-03-17') ,dateStartSchoolClosure = as.Date('2020-03-16'),
  #                                               dateStartIntenseIntervention = as.Date('2020-04-03'), dateEndIntenseIntervention = as.Date('2020-04-30'),pWorkOpen = c(0.1,0.75,1,1),
  #                                               numWeekStagger = c(10/7,10/7,10/7),pInfected=initialI,durInf = durInfSim)
  # epi_marchDurInf3[[sim]] = simulateOutbreakSEIR(R0t =R0est[sim] ,rho = rep(0.5,3660), dateStart=as.Date('2020-03-17') ,dateStartSchoolClosure = as.Date('2020-03-16'),
  #                                                dateStartIntenseIntervention = as.Date('2020-04-03'), dateEndIntenseIntervention = as.Date('2020-04-30'),
  #                                                pInfected=initialI,durInf = durInfSim)
  # epi_aprilDurInf3[[sim]] = simulateOutbreakSEIR(R0t =R0est[sim] ,rho = rep(0.5,3660), dateStart=as.Date('2020-03-17') ,dateStartSchoolClosure = as.Date('2020-03-16'),
  #                                                dateStartIntenseIntervention = as.Date('2020-04-03'), dateEndIntenseIntervention = as.Date('2020-04-30'),
  #                                                pInfected=initialI,durInf = durInfSim)
  if(sim%%10==0) print(paste0('Done with simulation ',sim))
}
end = Sys.time()
print(end-start)

covid_SDurInf3 = list() 
covid_SDurInf3[[1]] = summariseSimulations(VAR = 'S',CI = 50,SIMS = epi_doNothingDurInf3)
# covid_SDurInf3[[2]] = summariseSimulations(VAR = 'S',CI = 50,SIMS = epi_baseDurInf3)
# covid_SDurInf3[[3]] = summariseSimulations(VAR = 'S',CI = 50,SIMS = epi_marchDurInf3)
# covid_SDurInf3[[4]] = summariseSimulations(VAR = 'S',CI = 50,SIMS = epi_aprilDurInf3)

covid_IDurInf3 = list() 
covid_IDurInf3[[1]] = summariseSimulations(VAR = 'incidence',CI = 50,SIMS = epi_doNothingDurInf3)
# covid_IDurInf3[[2]] = summariseSimulations(VAR = 'incidence',CI = 50,SIMS = epi_baseDurInf3)
# covid_IDurInf3[[3]] = summariseSimulations(VAR = 'incidence',CI = 50,SIMS = epi_marchDurInf3)
# covid_IDurInf3[[4]] = summariseSimulations(VAR = 'incidence',CI = 50,SIMS = epi_aprilDurInf3)

peaktime_DurInf3 = list()
peaktime_DurInf3[[1]] = summarisePeakTimePeakSize(SIMS = epi_doNothingDurInf3)
# peaktime_DurInf3[[2]] = summarisePeakTimePeakSize(SIMS = epi_baseDurInf3)
# peaktime_DurInf3[[3]] = summarisePeakTimePeakSize(SIMS = epi_marchDurInf3)
# peaktime_DurInf3[[4]] = summarisePeakTimePeakSize(SIMS = epi_aprilDurInf3)

covid_DurInf3 = list() 
covid_DurInf3[[1]] = summariseSimulations_mid(CI = 50,SIMS = epi_doNothingDurInf3)
# covid_DurInf3[[2]] = summariseSimulations_mid(CI = 50,SIMS = epi_baseDurInf3)
# covid_DurInf3[[3]] = summariseSimulations_mid(CI = 50,SIMS = epi_marchDurInf3)
# covid_DurInf3[[4]] = summariseSimulations_mid(CI = 50,SIMS = epi_aprilDurInf3)

AGEcovid_IDurInf3 = list()
AGEcovid_IDurInf3[[1]] = summariseSimulationsAGE(VAR = 'incidence',CI = 50,SIMS = epi_doNothingDurInf3)
# AGEcovid_IDurInf3[[2]] = summariseSimulationsAGE(VAR = 'incidence',CI = 50,SIMS = epi_baseDurInf3)
# AGEcovid_IDurInf3[[3]] = summariseSimulationsAGE(VAR = 'incidence',CI = 50,SIMS = epi_marchDurInf3)
# AGEcovid_IDurInf3[[4]] = summariseSimulationsAGE(VAR = 'incidence',CI = 50,SIMS = epi_aprilDurInf3)

epiFirstSimDurInf3 = list(epi_doNothingDurInf3 = epi_doNothingDurInf3[[1]],
                          epi_baseDurInf3= epi_baseDurInf3[[1]],
                          epi_marchDurInf3 = epi_marchDurInf3[[1]],
                          epi_aprilDurInf3 = epi_aprilDurInf3[[1]])

save(covid_IDurInf3,file = paste('./ShinyApp/data/SEIcIscR/',printablecounty,'.covid_IDurInf3.rdata', sep=""))
save(covid_SDurInf3,file = paste('./ShinyApp/data/SEIcIscR/',printablecounty,'.covid_SDurInf3.rdata', sep=""))

save(peaktime_DurInf3,file = paste('./ShinyApp/data/SEIcIscR/',printablecounty,'.peaktime_DurInf3.rdata', sep=""))

save(covid_DurInf3,file =paste('./ShinyApp/data/SEIcIscR/',printablecounty,'.covid_DurInf3.rdata', sep=""))

save(AGEcovid_IDurInf3,file =paste('./ShinyApp/data/SEIcIscR/',printablecounty,'.AGEcovid_IDurInf3.rdata', sep=""))

save(epiFirstSimDurInf3,file =paste('./ShinyApp/data/SEIcIscR/',printablecounty,'.epiFirstSimDurInf3.rdata', sep=""))

rm(epi_doNothingDurInf3,epi_baseDurInf3,epi_marchDurInf3,epi_aprilDurInf3)




