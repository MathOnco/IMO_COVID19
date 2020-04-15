## To simulate n_simSEIcIscR outbreaks

nsim = 200

set.seed(123)
r0postCrI = r0posterior
R0est = sample(x = r0postCrI,size = nsim)

## To simulate n_sim SEIcIscR outbreaks: duration of infection = 3 days, initial infected  n=180 infected
epi_doNothingDurInf3 = vector('list',nsim)
epi_baseDurInf3 = vector('list',nsim)
epi_marchDurInf3 = vector('list',nsim)
epi_aprilDurInf3 = vector('list',nsim)
start = Sys.time()
durInfSim = 5
initialI = 180
for(sim in 1:nsim)
{
  epi_doNothingDurInf3[[sim]] = simulateOutbreakSEIcIscR(R0t =R0est[sim] ,dateStart=as.Date('2020-03-17') , dateStartSchoolClosure = as.Date('2020-03-16'),
                                                         dateStartIntenseIntervention = as.Date('2020-04-03'), dateEndIntenseIntervention = as.Date('2020-04-30'),
                                                         pWorkOpen = c(1,1,1,1),numWeekStagger = c(0,0,0),pInfected=initialI,durInf = durInfSim)
  # epi_baseDurInf3[[sim]] = simulateOutbreakSEIcIscR(R0t =R0est[sim] ,dateStart=as.Date('2020-03-17') , dateStartSchoolClosure = as.Date('2020-03-16'),
  #                                                   dateStartIntenseIntervention = as.Date('2020-04-03'), dateEndIntenseIntervention = as.Date('2020-04-30'),pWorkOpen = c(0.1,0.75,1,1),
  #                                                   numWeekStagger = c(10/7,10/7,10/7),pInfected=initialI,durInf = durInfSim)
  # epi_marchDurInf3[[sim]] = simulateOutbreakSEIcIscR(R0t =R0est[sim] ,dateStart=as.Date('2020-03-17') , dateStartSchoolClosure = as.Date('2020-03-16'),
  #                                                    dateStartIntenseIntervention = as.Date('2020-04-03'), dateEndIntenseIntervention = as.Date('2020-04-30'),
  #                                                    pInfected=initialI,durInf = durInfSim)
  # epi_aprilDurInf3[[sim]] = simulateOutbreakSEIcIscR(R0t =R0est[sim] ,dateStart=as.Date('2020-03-17') , dateStartSchoolClosure = as.Date('2020-03-16'),
  #                                                    dateStartIntenseIntervention = as.Date('2020-04-03'), dateEndIntenseIntervention = as.Date('2020-04-30'),
  #                                                    pInfected=initialI,durInf = durInfSim)
  if(sim%%10==0) print(paste0('Done with simulation ',sim))
}
end = Sys.time()
print(end-start)

covid_SDurInf3sc = list() 
covid_SDurInf3sc[[1]] = summariseSimulations(VAR = 'S',CI = 50,SIMS = epi_doNothingDurInf3)
# covid_SDurInf3sc[[2]] = summariseSimulations(VAR = 'S',CI = 50,SIMS = epi_baseDurInf3)
# covid_SDurInf3sc[[3]] = summariseSimulations(VAR = 'S',CI = 50,SIMS = epi_marchDurInf3)
# covid_SDurInf3sc[[4]] = summariseSimulations(VAR = 'S',CI = 50,SIMS = epi_aprilDurInf3)

covid_IDurInf3sc = list() 
covid_IDurInf3sc[[1]] = summariseSimulations(VAR = 'incidence',CI = 50,SIMS = epi_doNothingDurInf3)
# covid_IDurInf3sc[[2]] = summariseSimulations(VAR = 'incidence',CI = 50,SIMS = epi_baseDurInf3)
# covid_IDurInf3sc[[3]] = summariseSimulations(VAR = 'incidence',CI = 50,SIMS = epi_marchDurInf3)
# covid_IDurInf3sc[[4]] = summariseSimulations(VAR = 'incidence',CI = 50,SIMS = epi_aprilDurInf3)

peaktime_DurInf3sc = list()
peaktime_DurInf3sc[[1]] = summarisePeakTimePeakSize(SIMS = epi_doNothingDurInf3)
# peaktime_DurInf3sc[[2]] = summarisePeakTimePeakSize(SIMS = epi_baseDurInf3)
# peaktime_DurInf3sc[[3]] = summarisePeakTimePeakSize(SIMS = epi_marchDurInf3)
# peaktime_DurInf3sc[[4]] = summarisePeakTimePeakSize(SIMS = epi_aprilDurInf3)

covid_DurInf3sc = list() 
covid_DurInf3sc[[1]] = summariseSimulations_mid(CI = 50,SIMS = epi_doNothingDurInf3)
# covid_DurInf3sc[[2]] = summariseSimulations_mid(CI = 50,SIMS = epi_baseDurInf3)
# covid_DurInf3sc[[3]] = summariseSimulations_mid(CI = 50,SIMS = epi_marchDurInf3)
# covid_DurInf3sc[[4]] = summariseSimulations_mid(CI = 50,SIMS = epi_aprilDurInf3)

AGEcovid_IDurInf3sc = list()
AGEcovid_IDurInf3sc[[1]] = summariseSimulationsAGE(VAR = 'incidence',CI = 50,SIMS = epi_doNothingDurInf3)
# AGEcovid_IDurInf3sc[[2]] = summariseSimulationsAGE(VAR = 'incidence',CI = 50,SIMS = epi_baseDurInf3)
# AGEcovid_IDurInf3sc[[3]] = summariseSimulationsAGE(VAR = 'incidence',CI = 50,SIMS = epi_marchDurInf3)
# AGEcovid_IDurInf3sc[[4]] = summariseSimulationsAGE(VAR = 'incidence',CI = 50,SIMS = epi_aprilDurInf3)

epiFirstSimDurInf3sc = list(epi_doNothingDurInf3 = epi_doNothingDurInf3[[1]],
                            epi_baseDurInf3= epi_baseDurInf3[[1]],
                            epi_marchDurInf3 = epi_marchDurInf3[[1]],
                            epi_aprilDurInf3 = epi_aprilDurInf3[[1]])

save(covid_IDurInf3sc,file = 'outputs/SEIcIscR/covid_IDurInf3.rdata')
save(covid_SDurInf3sc,file = 'outputs/SEIcIscR/covid_SDurInf3.rdata')

save(peaktime_DurInf3sc,file = 'outputs/SEIcIscR/peaktime_DurInf3.rdata')

save(covid_DurInf3sc,file ='outputs/SEIcIscR/covid_DurInf3.rdata')

save(AGEcovid_IDurInf3sc,file ='outputs/SEIcIscR/AGEcovid_IDurInf3.rdata')

save(epiFirstSimDurInf3sc,file ='outputs/SEIcIscR/epiFirstSimDurInf3.rdata')

rm(epi_doNothingDurInf3,epi_baseDurInf3,epi_marchDurInf3,epi_aprilDurInf3)

