library(RMySQL)
library(data.table)
library(ggplot2)
setwd("/media/huijieqiao/WD12T/eBird/Script/ebird_migration")


if (F){
  #Using database, doesn't work
  con<-dbConnect(MySQL(), user="root", password="mikania", 
                 dbname="eBird", host="172.16.120.11")
  
  #rs<-dbSendQuery(con, sprintf(SQL_template, Loc_IDs))
  SQL<-"SELECT COUNT(1) COUNT, OBSERVER_ID FROM eBird202105 GROUP BY OBSERVER_ID"
  #print(SQL)
  rs<-dbSendQuery(con, SQL)
  df<-dbFetch(rs, n=-1)
  dbClearResult(rs)
  dbDisconnect(con)
  saveRDS(df, "../../Data_eBird_2020/Tables/observers.rda")
  ggplot(df)+geom_histogram(aes(x=COUNT), bins=50)+scale_y_log10()
  quantile(df$COUNT, c(0.99, 0.9, 0.8, seq(0, 1, by=0.25)))
  sum(df$COUNT)
  
  #Number of unique observers per country per year
  con<-dbConnect(MySQL(), user="root", password="mikania", 
                 dbname="eBird", host="172.16.120.11")
  
  #rs<-dbSendQuery(con, sprintf(SQL_template, Loc_IDs))
  SQL<-"SELECT COUNT(1) N_EVENTS, COUNT(DISTINCT OBSERVER_ID) OBSERVER_COUNT, COUNTRY, `YEAR` FROM eBird202105 GROUP BY COUNTRY, `YEAR`"
  #print(SQL)
  rs<-dbSendQuery(con, SQL)
  df<-dbFetch(rs, n=-1)
  dbClearResult(rs)
  dbDisconnect(con)
  saveRDS(df, "../../Data_eBird_2020/Tables/observers_country_year.rda")
  
  #Number of unique observers per country per year and month
  con<-dbConnect(MySQL(), user="root", password="mikania", 
                 dbname="eBird", host="172.16.120.11")
  
  #rs<-dbSendQuery(con, sprintf(SQL_template, Loc_IDs))
  SQL<-"SELECT COUNT(1) N_EVENTS, COUNT(DISTINCT OBSERVER_ID) OBSERVER_COUNT, COUNTRY, `YEAR`, `MONTH` FROM eBird202105 GROUP BY COUNTRY, `YEAR`, `MONTH`"
  #print(SQL)
  rs<-dbSendQuery(con, SQL)
  df<-dbFetch(rs, n=-1)
  dbClearResult(rs)
  dbDisconnect(con)
  saveRDS(df, "../../Data_eBird_2020/Tables/observers_country_year_month.rda")
  
  #Number of unique observers per country and state per year
  con<-dbConnect(MySQL(), user="root", password="mikania", 
                 dbname="eBird", host="172.16.120.11")
  
  #rs<-dbSendQuery(con, sprintf(SQL_template, Loc_IDs))
  SQL<-"SELECT COUNT(1) N_EVENTS, COUNT(DISTINCT OBSERVER_ID) OBSERVER_COUNT, COUNTRY, STATE, `YEAR` FROM eBird202105 GROUP BY COUNTRY, STATE, `YEAR`"
  #print(SQL)
  rs<-dbSendQuery(con, SQL)
  df<-dbFetch(rs, n=-1)
  dbClearResult(rs)
  dbDisconnect(con)
  saveRDS(df, "../../Data_eBird_2020/Tables/observers_state_year.rda")
  
  #Number of unique observers per country and state per year and month
  con<-dbConnect(MySQL(), user="root", password="mikania", 
                 dbname="eBird", host="172.16.120.11")
  
  #rs<-dbSendQuery(con, sprintf(SQL_template, Loc_IDs))
  SQL<-"SELECT COUNT(1) N_EVENTS, COUNT(DISTINCT OBSERVER_ID) OBSERVER_COUNT, COUNTRY, STATE, `YEAR`, `MONTH` FROM eBird202105 GROUP BY COUNTRY, STATE, `YEAR`, `MONTH`"
  #print(SQL)
  rs<-dbSendQuery(con, SQL)
  df<-dbFetch(rs, n=-1)
  dbClearResult(rs)
  dbDisconnect(con)
  saveRDS(df, "../../Data_eBird_2020/Tables/observers_state_year_month.rda")
  
}
observers<-readRDS("../../Data_eBird_2020/Tables/observers.rda")
target_observers<-observers[which(observers$COUNT>=100),]
if (T){
  countries<-list.dirs("../../Tables/Locality_202105", full.names = F, recursive=F)
  countries<-countries[sample(length(countries), length(countries))]
  country<-"American Samoa"
  for (country in countries){
    target<-sprintf("../../Tables/Observers/%s", country)
    if (dir.exists(target)){
      next()
    }
    dir.create(target)
    states<-list.dirs(sprintf("../../Tables/Locality_202105/%s", country), full.names=F, recursive = F)
    state<-states[1]
    df_states<-list()
    for (state in states){
      print(paste(country, state, sep="/"))
      counties<-list.dirs(sprintf("../../Tables/Locality_202105/%s/%s", country, state), full.names=F, recursive = F)
      county<-counties[1]
      for (county in counties){
        if (!file.exists(sprintf("../../Tables/Locality_202105/%s/%s/%s/raw.rda", country, state, county))){
          next()
        }
        item<-readRDS(sprintf("../../Tables/Locality_202105/%s/%s/%s/raw.rda", country, state, county))
        df_states[[paste(state, county)]]<-item
      }
    }
    df_states<-rbindlist(df_states)
    df_states_target<-df_states[OBSERVER_ID %in% target_observers$OBSERVER_ID]
    df_states_year<-df_states[, .(N_EVENT=.N, N_OBSERVATION=sum(OBSERVATION_COUNT),
                                  N_LOCALITY=length(unique(LOCALITY_ID)),
                                  N_OBSERVER=length(unique(OBSERVER_ID)),
                                  N_SPECIES=length(unique(SCIENTIFIC_NAME))
                                  ), by=list(COUNTRY, STATE, YEAR)]
    
    df_states_year_month<-df_states[, .(N_EVENT=.N, N_OBSERVATION=sum(OBSERVATION_COUNT),
                                  N_LOCALITY=length(unique(LOCALITY_ID)),
                                  N_OBSERVER=length(unique(OBSERVER_ID)),
                                  N_SPECIES=length(unique(SCIENTIFIC_NAME))
    ), by=list(COUNTRY, STATE, YEAR, MONTH)]
    
    df_states_month_2015_2019<-df_states[between(YEAR, 2015, 2019), 
                                              .(N_EVENT=.N, N_OBSERVATION=sum(OBSERVATION_COUNT),
                                        N_LOCALITY=length(unique(LOCALITY_ID)),
                                        N_OBSERVER=length(unique(OBSERVER_ID)),
                                        N_SPECIES=length(unique(SCIENTIFIC_NAME))
    ), by=list(COUNTRY, STATE, MONTH)]
    
    df_states_month_2020<-df_states[YEAR==2020, 
                                         .(N_EVENT=.N, N_OBSERVATION=sum(OBSERVATION_COUNT),
                                           N_LOCALITY=length(unique(LOCALITY_ID)),
                                           N_OBSERVER=length(unique(OBSERVER_ID)),
                                           N_SPECIES=length(unique(SCIENTIFIC_NAME))
                                         ), by=list(COUNTRY, STATE, MONTH)]
    df_states_month_2021<-df_states[YEAR==2021, 
                                    .(N_EVENT=.N, N_OBSERVATION=sum(OBSERVATION_COUNT),
                                      N_LOCALITY=length(unique(LOCALITY_ID)),
                                      N_OBSERVER=length(unique(OBSERVER_ID)),
                                      N_SPECIES=length(unique(SCIENTIFIC_NAME))
                                    ), by=list(COUNTRY, STATE, MONTH)]
    
    df_states_month_2015_2019$YEAR<-"BEFORE"
    df_states_month_2020$YEAR<-"2020"
    df_states_month_2021$YEAR<-"2021"
    df_states_month_time_bins<-rbindlist(list(df_states_month_2015_2019, df_states_month_2020, df_states_month_2021))
    
    saveRDS(df_states_year, sprintf("../../Tables/Observers/%s/df_states_year.rda", country))
    saveRDS(df_states_year_month, sprintf("../../Tables/Observers/%s/df_states_year_month.rda", country))
    saveRDS(df_states_month_time_bins, sprintf("../../Tables/Observers/%s/df_states_month_time_bins.rda", country))
    
    df_country_year<-df_states[, .(N_EVENT=.N, N_OBSERVATION=sum(OBSERVATION_COUNT),
                                  N_LOCALITY=length(unique(LOCALITY_ID)),
                                  N_OBSERVER=length(unique(OBSERVER_ID)),
                                  N_SPECIES=length(unique(SCIENTIFIC_NAME))
    ), by=list(COUNTRY, YEAR)]
    
    df_country_year_month<-df_states[, .(N_EVENT=.N, N_OBSERVATION=sum(OBSERVATION_COUNT),
                                        N_LOCALITY=length(unique(LOCALITY_ID)),
                                        N_OBSERVER=length(unique(OBSERVER_ID)),
                                        N_SPECIES=length(unique(SCIENTIFIC_NAME))
    ), by=list(COUNTRY, YEAR, MONTH)]
    
    df_country_month_2015_2019<-df_states[between(YEAR, 2015, 2019), 
                                         .(N_EVENT=.N, N_OBSERVATION=sum(OBSERVATION_COUNT),
                                           N_LOCALITY=length(unique(LOCALITY_ID)),
                                           N_OBSERVER=length(unique(OBSERVER_ID)),
                                           N_SPECIES=length(unique(SCIENTIFIC_NAME))
                                         ), by=list(COUNTRY, MONTH)]
    
    df_country_month_2020<-df_states[YEAR==2020, 
                                    .(N_EVENT=.N, N_OBSERVATION=sum(OBSERVATION_COUNT),
                                      N_LOCALITY=length(unique(LOCALITY_ID)),
                                      N_OBSERVER=length(unique(OBSERVER_ID)),
                                      N_SPECIES=length(unique(SCIENTIFIC_NAME))
                                    ), by=list(COUNTRY, MONTH)]
    df_country_month_2021<-df_states[YEAR==2021, 
                                    .(N_EVENT=.N, N_OBSERVATION=sum(OBSERVATION_COUNT),
                                      N_LOCALITY=length(unique(LOCALITY_ID)),
                                      N_OBSERVER=length(unique(OBSERVER_ID)),
                                      N_SPECIES=length(unique(SCIENTIFIC_NAME))
                                    ), by=list(COUNTRY, MONTH)]
    
    df_country_month_2015_2019$YEAR<-"BEFORE"
    df_country_month_2020$YEAR<-"2020"
    df_country_month_2021$YEAR<-"2021"
    df_country_month_time_bins<-rbindlist(list(df_country_month_2015_2019, df_country_month_2020, df_country_month_2021))
    
    saveRDS(df_country_year, sprintf("../../Tables/Observers/%s/df_country_year.rda", country))
    saveRDS(df_country_year_month, sprintf("../../Tables/Observers/%s/df_country_year_month.rda", country))
    saveRDS(df_country_month_time_bins, sprintf("../../Tables/Observers/%s/df_country_month_time_bins.rda", country))
    
    df_states<-df_states_target
    df_states_year<-df_states[, .(N_EVENT=.N, N_OBSERVATION=sum(OBSERVATION_COUNT),
                                  N_LOCALITY=length(unique(LOCALITY_ID)),
                                  N_OBSERVER=length(unique(OBSERVER_ID)),
                                  N_SPECIES=length(unique(SCIENTIFIC_NAME))
    ), by=list(COUNTRY, STATE, YEAR)]
    
    df_states_year_month<-df_states[, .(N_EVENT=.N, N_OBSERVATION=sum(OBSERVATION_COUNT),
                                        N_LOCALITY=length(unique(LOCALITY_ID)),
                                        N_OBSERVER=length(unique(OBSERVER_ID)),
                                        N_SPECIES=length(unique(SCIENTIFIC_NAME))
    ), by=list(COUNTRY, STATE, YEAR, MONTH)]
    
    df_states_month_2015_2019<-df_states[between(YEAR, 2015, 2019), 
                                         .(N_EVENT=.N, N_OBSERVATION=sum(OBSERVATION_COUNT),
                                           N_LOCALITY=length(unique(LOCALITY_ID)),
                                           N_OBSERVER=length(unique(OBSERVER_ID)),
                                           N_SPECIES=length(unique(SCIENTIFIC_NAME))
                                         ), by=list(COUNTRY, STATE, MONTH)]
    
    df_states_month_2020<-df_states[YEAR==2020, 
                                    .(N_EVENT=.N, N_OBSERVATION=sum(OBSERVATION_COUNT),
                                      N_LOCALITY=length(unique(LOCALITY_ID)),
                                      N_OBSERVER=length(unique(OBSERVER_ID)),
                                      N_SPECIES=length(unique(SCIENTIFIC_NAME))
                                    ), by=list(COUNTRY, STATE, MONTH)]
    df_states_month_2021<-df_states[YEAR==2021, 
                                    .(N_EVENT=.N, N_OBSERVATION=sum(OBSERVATION_COUNT),
                                      N_LOCALITY=length(unique(LOCALITY_ID)),
                                      N_OBSERVER=length(unique(OBSERVER_ID)),
                                      N_SPECIES=length(unique(SCIENTIFIC_NAME))
                                    ), by=list(COUNTRY, STATE, MONTH)]
    
    df_states_month_2015_2019$YEAR<-"BEFORE"
    df_states_month_2020$YEAR<-"2020"
    df_states_month_2021$YEAR<-"2021"
    df_states_month_time_bins<-rbindlist(list(df_states_month_2015_2019, df_states_month_2020, df_states_month_2021))
    
    saveRDS(df_states_year, sprintf("../../Tables/Observers/%s/df_states_year_threshold_100.rda", country))
    saveRDS(df_states_year_month, sprintf("../../Tables/Observers/%s/df_states_year_month_threshold_100.rda", country))
    saveRDS(df_states_month_time_bins, sprintf("../../Tables/Observers/%s/df_states_month_time_bins_threshold_100.rda", country))
    
    df_country_year<-df_states[, .(N_EVENT=.N, N_OBSERVATION=sum(OBSERVATION_COUNT),
                                   N_LOCALITY=length(unique(LOCALITY_ID)),
                                   N_OBSERVER=length(unique(OBSERVER_ID)),
                                   N_SPECIES=length(unique(SCIENTIFIC_NAME))
    ), by=list(COUNTRY, YEAR)]
    
    df_country_year_month<-df_states[, .(N_EVENT=.N, N_OBSERVATION=sum(OBSERVATION_COUNT),
                                         N_LOCALITY=length(unique(LOCALITY_ID)),
                                         N_OBSERVER=length(unique(OBSERVER_ID)),
                                         N_SPECIES=length(unique(SCIENTIFIC_NAME))
    ), by=list(COUNTRY, YEAR, MONTH)]
    
    df_country_month_2015_2019<-df_states[between(YEAR, 2015, 2019), 
                                          .(N_EVENT=.N, N_OBSERVATION=sum(OBSERVATION_COUNT),
                                            N_LOCALITY=length(unique(LOCALITY_ID)),
                                            N_OBSERVER=length(unique(OBSERVER_ID)),
                                            N_SPECIES=length(unique(SCIENTIFIC_NAME))
                                          ), by=list(COUNTRY, MONTH)]
    
    df_country_month_2020<-df_states[YEAR==2020, 
                                     .(N_EVENT=.N, N_OBSERVATION=sum(OBSERVATION_COUNT),
                                       N_LOCALITY=length(unique(LOCALITY_ID)),
                                       N_OBSERVER=length(unique(OBSERVER_ID)),
                                       N_SPECIES=length(unique(SCIENTIFIC_NAME))
                                     ), by=list(COUNTRY, MONTH)]
    df_country_month_2021<-df_states[YEAR==2021, 
                                     .(N_EVENT=.N, N_OBSERVATION=sum(OBSERVATION_COUNT),
                                       N_LOCALITY=length(unique(LOCALITY_ID)),
                                       N_OBSERVER=length(unique(OBSERVER_ID)),
                                       N_SPECIES=length(unique(SCIENTIFIC_NAME))
                                     ), by=list(COUNTRY, MONTH)]
    
    df_country_month_2015_2019$YEAR<-"BEFORE"
    df_country_month_2020$YEAR<-"2020"
    df_country_month_2021$YEAR<-"2021"
    df_country_month_time_bins<-rbindlist(list(df_country_month_2015_2019, df_country_month_2020, df_country_month_2021))
    
    saveRDS(df_country_year, sprintf("../../Tables/Observers/%s/df_country_year_threshold_100.rda", country))
    saveRDS(df_country_year_month, sprintf("../../Tables/Observers/%s/df_country_year_month_threshold_100.rda", country))
    saveRDS(df_country_month_time_bins, sprintf("../../Tables/Observers/%s/df_country_month_time_bins_threshold_100.rda", country))
  }
  
}



observers[which(observers$COUNT==max(observers$COUNT)),]

df_sub<-df[which(df$COUNT>=34940),]
sum(df_sub$COUNT)
sum(df_sub$COUNT)/sum(df$COUNT)


df<-readRDS("/media/huijieqiao/WD12T/eBird/Tables/Locality_202105/Afghanistan/Badakhshan/Unnamed/raw.rda")
