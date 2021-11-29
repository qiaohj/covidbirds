library(ggplot2)
library(raster)
library(data.table)
library(DBI)
library(mgcv)
library(pastecs)
library(dplyr)
library(ggpubr)

setwd("/media/huijieqiao/WD12T/eBird/Script/ebird_migration")
folder_t<-"/media/huijieqiao/WD12T/eBird/Tables/Locality_202105"
dirs<-list.dirs(folder_t, full.names = F)
d<-dirs[2]
countries<-c()
for (d in dirs){
  ts<-strsplit(d, "/")[[1]]
  if (length(ts)==1){
    countries<-c(countries, ts)
  }
}
i=1
country_list<-list()
state_list<-list()
sp_list_country_list<-list()
sp_list_state_list<-list()

country_list_year<-list()
state_list_year<-list()
sp_list_country_list_year<-list()
sp_list_state_list_year<-list()

for (i in c(1:length(countries))){
  country<-countries[i]
  states<-list.dirs(sprintf("%s/%s", folder_t, country), full.names=F)
  j=2
  all_df<-list()
  for (j in c(1:length(states))){
    item<-strsplit(states[j], "/")[[1]]
    if (length(item)==2){
      print(paste(i, length(countries), country, j, length(states), item[1], item[2]))
      rda<-sprintf("%s/%s/%s/%s/raw.rda", folder_t, country, item[1], item[2])
      if (!file.exists(rda)){
        next()
      }
      
      df<-readRDS(rda)
      df<-data.table(df)
      df<-df[(APPROVED==1)|(REVIEWED==1)]
      if (nrow(df)==0){
        next()
      }
      
      all_df[[states[j]]]<-df
    }
  }
  all_df<-rbindlist(all_df)
  sp_list_country<-all_df[, .(N_EVENTS=.N,
                              N_OBSERVERS=length(unique(OBSERVER_ID)),
                      N_OBSERVATION=sum(OBSERVATION_COUNT),
                      MEAN_OBSERVATION_PER_EVENT=mean(OBSERVATION_COUNT, na.rm=T),
                      SD_OBSERVATION_PER_EVENT=sd(OBSERVATION_COUNT, na.rm=T),
                      EFFORT_DISTANCE=mean(EFFORT_DISTANCE_KM, na.rm=T)),
                  by=list(COUNTRY, YEAR, MONTH, SCIENTIFIC_NAME)]
  
  sp_list_state<-all_df[, .(N_EVENTS=.N,
                            N_OBSERVERS=length(unique(OBSERVER_ID)),
                              N_OBSERVATION=sum(OBSERVATION_COUNT),
                              MEAN_OBSERVATION_PER_EVENT=mean(OBSERVATION_COUNT, na.rm=T),
                              SD_OBSERVATION_PER_EVENT=sd(OBSERVATION_COUNT, na.rm=T),
                              EFFORT_DISTANCE=mean(EFFORT_DISTANCE_KM, na.rm=T)),
                          by=list(COUNTRY, STATE, YEAR, MONTH, SCIENTIFIC_NAME)]
  
  all_df_se<-all_df[, .(N_SP=length(unique(SCIENTIFIC_NAME)),
                    N_EVENTS=.N,
                    N_OBSERVERS=length(unique(OBSERVER_ID)),
                    N_OBSERVATION=sum(OBSERVATION_COUNT),
                    MEAN_OBSERVATION_PER_EVENT=mean(OBSERVATION_COUNT, na.rm=T),
                    SD_OBSERVATION_PER_EVENT=sd(OBSERVATION_COUNT, na.rm=T),
                    EFFORT_DISTANCE=mean(EFFORT_DISTANCE_KM, na.rm=T)),
                    by=list(COUNTRY, YEAR, MONTH)]
  
  all_df_state_se<-all_df[, .(N_SP=length(unique(SCIENTIFIC_NAME)),
                        N_EVENTS=.N,
                        N_OBSERVERS=length(unique(OBSERVER_ID)),
                        N_OBSERVATION=sum(OBSERVATION_COUNT),
                        MEAN_OBSERVATION_PER_EVENT=mean(OBSERVATION_COUNT, na.rm=T),
                        SD_OBSERVATION_PER_EVENT=sd(OBSERVATION_COUNT, na.rm=T),
                        EFFORT_DISTANCE=mean(EFFORT_DISTANCE_KM, na.rm=T)),
                    by=list(COUNTRY, STATE, YEAR, MONTH)]
  
  sp_list_country_year<-all_df[, .(N_EVENTS=.N,
                              N_OBSERVERS=length(unique(OBSERVER_ID)),
                              N_OBSERVATION=sum(OBSERVATION_COUNT),
                              MEAN_OBSERVATION_PER_EVENT=mean(OBSERVATION_COUNT, na.rm=T),
                              SD_OBSERVATION_PER_EVENT=sd(OBSERVATION_COUNT, na.rm=T),
                              EFFORT_DISTANCE=mean(EFFORT_DISTANCE_KM, na.rm=T)),
                          by=list(COUNTRY, YEAR, SCIENTIFIC_NAME)]
  
  sp_list_state_year<-all_df[, .(N_EVENTS=.N,
                            N_OBSERVERS=length(unique(OBSERVER_ID)),
                            N_OBSERVATION=sum(OBSERVATION_COUNT),
                            MEAN_OBSERVATION_PER_EVENT=mean(OBSERVATION_COUNT, na.rm=T),
                            SD_OBSERVATION_PER_EVENT=sd(OBSERVATION_COUNT, na.rm=T),
                            EFFORT_DISTANCE=mean(EFFORT_DISTANCE_KM, na.rm=T)),
                        by=list(COUNTRY, STATE, YEAR, SCIENTIFIC_NAME)]
  
  all_df_se_year<-all_df[, .(N_SP=length(unique(SCIENTIFIC_NAME)),
                        N_EVENTS=.N,
                        N_OBSERVERS=length(unique(OBSERVER_ID)),
                        N_OBSERVATION=sum(OBSERVATION_COUNT),
                        MEAN_OBSERVATION_PER_EVENT=mean(OBSERVATION_COUNT, na.rm=T),
                        SD_OBSERVATION_PER_EVENT=sd(OBSERVATION_COUNT, na.rm=T),
                        EFFORT_DISTANCE=mean(EFFORT_DISTANCE_KM, na.rm=T)),
                    by=list(COUNTRY, YEAR)]
  
  all_df_state_se_year<-all_df[, .(N_SP=length(unique(SCIENTIFIC_NAME)),
                              N_EVENTS=.N,
                              N_OBSERVERS=length(unique(OBSERVER_ID)),
                              N_OBSERVATION=sum(OBSERVATION_COUNT),
                              MEAN_OBSERVATION_PER_EVENT=mean(OBSERVATION_COUNT, na.rm=T),
                              SD_OBSERVATION_PER_EVENT=sd(OBSERVATION_COUNT, na.rm=T),
                              EFFORT_DISTANCE=mean(EFFORT_DISTANCE_KM, na.rm=T)),
                          by=list(COUNTRY, STATE, YEAR)]
  
  country_list[[country]]<-all_df_se
  state_list[[country]]<-all_df_state_se
  sp_list_country_list[[country]]<-sp_list_country
  sp_list_state_list[[country]]<-sp_list_state
  
  country_list_year[[country]]<-all_df_se_year
  state_list_year[[country]]<-all_df_state_se_year
  sp_list_country_list_year[[country]]<-sp_list_country_year
  sp_list_state_list_year[[country]]<-sp_list_state_year
}
country_list<-rbindlist(country_list)
state_list<-rbindlist(state_list)
sp_list_country_list<-rbindlist(sp_list_country_list)
sp_list_state_list<-rbindlist(sp_list_state_list)
saveRDS(country_list, "../../eBird_Pendemic_2021/Objects/N_SP_Observation/country_list.rda")
saveRDS(state_list, "../../eBird_Pendemic_2021/Objects/N_SP_Observation/state_list.rda")
saveRDS(sp_list_country_list, "../../eBird_Pendemic_2021/Objects/N_SP_Observation/sp_list_country_list.rda")
saveRDS(sp_list_state_list, "../../eBird_Pendemic_2021/Objects/N_SP_Observation/sp_list_state_list.rda")

country_list_year<-rbindlist(country_list_year)
state_list_year<-rbindlist(state_list_year)
sp_list_country_list_year<-rbindlist(sp_list_country_list_year)
sp_list_state_list_year<-rbindlist(sp_list_state_list_year)
saveRDS(country_list_year, "../../eBird_Pendemic_2021/Objects/N_SP_Observation/country_list_year.rda")
saveRDS(state_list_year, "../../eBird_Pendemic_2021/Objects/N_SP_Observation/state_list_year.rda")
saveRDS(sp_list_country_list_year, "../../eBird_Pendemic_2021/Objects/N_SP_Observation/sp_list_country_list_year.rda")
saveRDS(sp_list_state_list_year, "../../eBird_Pendemic_2021/Objects/N_SP_Observation/sp_list_state_list_year.rda")

