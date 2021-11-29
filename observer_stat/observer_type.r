library(RMySQL)
library(data.table)
library(ggplot2)
setwd("/media/huijieqiao/WD12T/eBird/Script/ebird_migration")

observers<-readRDS("../../Data_eBird_2020/Tables/observers.rda")


if (F){
  
  i=1
  SQL_template<-"SELECT LOCALITY_ID,\
OBSERVATION_DATE,\
OBSERVATION_COUNT,\
EFFORT_DISTANCE_KM,\
APPROVED, REVIEWED, COUNTRY, STATE, COUNTY, LATITUDE, LONGITUDE,\
SCIENTIFIC_NAME,\
OBSERVER_ID FROM eBird202105 \
WHERE OBSERVER_ID='%s'"
  locality<-readRDS("../../eBird_Pendemic_2021/Objects/Observers/Overall/localities.rda")
  
  
  for (i in c(1:nrow(observers))){
    
    print(paste(i, nrow(observers)))
    observer<-observers[i,]
    if (file.exists(sprintf("../../eBird_Pendemic_2021/Objects/Observers/Observers_by_id/%d/%s.rda", floor(i / 1000), observer$OBSERVER_ID))){
      next()
    }
    
    con<-dbConnect(MySQL(), user="root", password="mikania", 
                   dbname="eBird", host="172.16.120.11")
    SQL<-sprintf(SQL_template, observer$OBSERVER_ID)
    #rs<-dbSendQuery(con, sprintf(SQL_template, Loc_IDs))
    
    #print(SQL)
    rs<-dbSendQuery(con, SQL)
    df<-dbFetch(rs, n=-1)
    dbClearResult(rs)
    dbDisconnect(con)
    df$SCHENGEN<-ifelse(df$COUNTRY %in% Schengen, "Schengen", df$COUNTRY)
    df$OBSERVATION_DATE<-as.POSIXct(df$OBSERVATION_DATE)
    df$YEAR<-as.numeric(format(df$OBSERVATION_DATE, "%Y"))
    df$MONTH<-as.numeric(format(df$OBSERVATION_DATE, "%m"))
    df$DAY<-as.numeric(format(df$OBSERVATION_DATE, "%d"))
    df$DAYS<-as.numeric(abs(difftime(strptime("1980-01-01", format = "%Y-%m-%d"),
                                     strptime(df$OBSERVATION_DATE,
                                              format = "%Y-%m-%d"), 
                                     units="days")))
    df$OBSERVATION_COUNT<-as.numeric(df$OBSERVATION_COUNT)
    df$DAYS_OF_YEAR<-as.numeric(strftime(df$OBSERVATION_DATE, format = "%j"))
    df[is.na(df$OBSERVATION_COUNT), "OBSERVATION_COUNT"]<-1
    
    df$LATITUDE<-as.numeric(df$LATITUDE)
    df$LONGITUDE<-as.numeric(df$LONGITUDE)
    df$APPROVED<-as.numeric(df$APPROVED)
    df$REVIEWED<-as.numeric(df$REVIEWED)
    df$EFFORT_DISTANCE_KM<-as.numeric(df$EFFORT_DISTANCE_KM)
    #df<-merge(df, locality, 
    #               by=c("COUNTRY", "STATE", "COUNTY", "LOCALITY_ID", "LONGITUDE", "LATITUDE"))
    
    df<-data.table(df)
    folder<-dir.create(sprintf("../../eBird_Pendemic_2021/Objects/Observers/Observers_by_id/%d", floor(i / 1000)))
    saveRDS(df, sprintf("../../eBird_Pendemic_2021/Objects/Observers/Observers_by_id/%d/%s.rda", floor(i / 1000), observer$OBSERVER_ID))
    
  }
}



if (T){
  Schengen<-c("Germany", "Austria", "Belgium", "Czech Republic", "Denmark", 
              "Estonia", "Finland", "France", "Greece", "Hungary", "Iceland", "Italy",
              "Latvia", "Liechtenstein", "Lithuania", "Luxembourg", "Malta", 
              "Netherlands", "Norway", "Poland", "Portugal", "Slovakia", 
              "Slovenia", "Spain", "Sweden", "Switzerland", "United Kingdom")
  sss<-Schengen[1]
  
  i=105204
  observer<-data.table(observers)[COUNT==quantile(COUNT, 0.95)]
  observers[(observers$COUNT==5316),]
  observer_by_years<-list()
  observer_by_countries<-list()
  observer_by_year_countries<-list()
  for (i in c(1:nrow(observers))){
    
    print(paste(i, nrow(observers)))
    observer<-observers[i,]
    item<-readRDS(sprintf("../../eBird_Pendemic_2021/Objects/Observers/Observers_by_id/%d/%s.rda", floor(i / 1000), observer$OBSERVER_ID))
    if (nrow(item)!=observer$COUNT){
      asdf
    }
    item$SCHENGEN<-ifelse(item$COUNTRY %in% Schengen, "Schengen", item$COUNTRY)
    table(item$SCHENGEN)
    observer_by_year<-item[, .(N_EVENT=.N, 
                          N_COUNTRY=length(unique(SCHENGEN)),
                          N_STATE=length(unique(STATE)),
                          N_LOCALITY=length(unique(LOCALITY_ID)),
                          N_SPECIES=length(unique(SCIENTIFIC_NAME)),
                          N_OBSERVATION=sum(OBSERVATION_COUNT)
                          ), by=list(YEAR, OBSERVER_ID)]
    observer_by_years[[observer$OBSERVER_ID]]<-observer_by_year
    observer_by_country<-item[, .(N_EVENT=.N, 
                              MIN_YEAR=min(YEAR),
                              MAX_YEAR=max(YEAR),
                              N_STATE=length(unique(STATE)),
                              N_LOCALITY=length(unique(LOCALITY_ID)),
                              N_SPECIES=length(unique(SCIENTIFIC_NAME)),
                              N_OBSERVATION=sum(OBSERVATION_COUNT)
    ), by=list(SCHENGEN, OBSERVER_ID)]
    observer_by_countries[[observer$OBSERVER_ID]]<-observer_by_country
    observer_by_year_country<-item[, .(N_EVENT=.N, 
                                  N_STATE=length(unique(STATE)),
                                  N_LOCALITY=length(unique(LOCALITY_ID)),
                                  N_SPECIES=length(unique(SCIENTIFIC_NAME)),
                                  N_OBSERVATION=sum(OBSERVATION_COUNT)
    ), by=list(SCHENGEN, OBSERVER_ID, YEAR)]
    observer_by_year_countries[[observer$OBSERVER_ID]]<-observer_by_year_country
    
  }
  
  observer_by_years<-rbindlist(observer_by_years)
  saveRDS(observer_by_years, "../../eBird_Pendemic_2021/Objects/Observers/Observers_Traits/observer_by_years.rda")
  
  observer_by_countries<-rbindlist(observer_by_countries)
  saveRDS(observer_by_countries, "../../eBird_Pendemic_2021/Objects/Observers/Observers_Traits/observer_by_countries.rda")
  
  observer_by_year_countries<-rbindlist(observer_by_year_countries)
  saveRDS(observer_by_year_countries, "../../eBird_Pendemic_2021/Objects/Observers/Observers_Traits/observer_by_year_countries.rda")
}