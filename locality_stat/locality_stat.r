library(dplyr)
library(RMySQL)
library(raster)
library(rgdal)
library(ggplot2)
library(data.table)
setwd("/media/huijieqiao/WD12T/eBird/Script/ebird_migration")
if (F){
  
  mask<-raster("../../Data_eBird_2020/mask.tif")
  not_na<-!is.na(values(mask))
  values(mask)[not_na]<-c(1:length(not_na[not_na]))
  writeRaster(mask, "../../Data_eBird_2020/mask_500m.tif", dataType="INT4U", overwrite=T)
  
  mask<-raster("../../Data_eBird_2020/mask_500m.tif")
  
  mask_10km<-aggregate(mask, 20)
  not_na<-!is.na(values(mask_10km))
  values(mask_10km)[not_na]<-c(1:length(not_na[not_na]))
  writeRaster(mask_10km, "../../Data_eBird_2020/mask_10km.tif", dataType="INT4U", overwrite=T)
  
  mask_5km<-aggregate(mask, 10)
  not_na<-!is.na(values(mask_5km))
  values(mask_5km)[not_na]<-c(1:length(not_na[not_na]))
  writeRaster(mask_5km, "../../Data_eBird_2020/mask_5km.tif", dataType="INT4U", overwrite=T)
  
  mask_2500m<-aggregate(mask, 5)
  not_na<-!is.na(values(mask_2500m))
  values(mask_2500m)[not_na]<-c(1:length(not_na[not_na]))
  writeRaster(mask_2500m, "../../Data_eBird_2020/mask_2.5km.tif", dataType="INT4U", overwrite=T)
  
  mask_5km<-raster("../../Data_eBird_2020/mask_5km.tif")
  mask_10km<-raster("../../Data_eBird_2020/mask_10km.tif")
  mask_2.5km<-raster("../../Data_eBird_2020/mask_2.5km.tif")
  if (F){
    tail(values(mask_5km)[!is.na(values(mask_5km))])
    tail(values(mask_10km)[!is.na(values(mask_10km))])
    tail(values(mask_2.5km)[!is.na(values(mask_2.5km))])
  }
  localities<-readRDS("../../Tables/Overall/localities.rda")
  localities[11304598,]
  localities$LONGITUDE<-as.numeric(localities$LONGITUDE)
  localities$LATITUDE<-as.numeric(localities$LATITUDE)
  dim(localities[which(is.na(localities$LONGITUDE)),])
  localities<-localities[which(!is.na(localities$LONGITUDE)),]
  points<-SpatialPointsDataFrame(localities[, c("LONGITUDE", "LATITUDE")], 
                                 proj4string=crs("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"),
                                 localities)
  points_sinu <- spTransform(points, crs(mask_2.5km))
  
  localities$X<-points_sinu@coords[,1]
  localities$Y<-points_sinu@coords[,2]
  
  localities$INDEX_2500m<-extract(mask_2.5km, localities[, c("X", "Y")])
  localities$INDEX_5km<-extract(mask_5km, localities[, c("X", "Y")])
  localities$INDEX_10km<-extract(mask_10km, localities[, c("X", "Y")])
  localities[which(localities$COUNTY==""), "COUNTY"]<-"Unnamed"
  saveRDS(localities, "../../Tables/Overall/localities.rda")
  
  
  
  states<-localities%>%dplyr::group_by(COUNTRY, STATE, COUNTY)%>%
    dplyr::summarise(N_SUM=sum(N))
  saveRDS(states, "../../Tables/Overall/states.rda")
  localities<-data.table(localities)
  localities_count<-localities[, .(N = .N), by = INDEX_10km]
  localities_count<-localities_count[!is.na(INDEX_10km)]
  mask_10km_p<-data.table(rasterToPoints(mask_10km))
  colnames(mask_10km_p)[3]<-"INDEX_10km"
  locality_10km_p<-merge(mask_10km_p, localities_count, by="INDEX_10km", all=T)
  r<-mask_10km
  values(r)[!is.na(values(r))]<-locality_10km_p$N
  locality_10km_p[N==max(locality_10km_p$N, na.rm=T)]
  
  ggplot(locality_10km_p)+geom_histogram(aes(x=N))+
    scale_x_log10()
  writeRaster(r, "../../Data_eBird_2020/TIF/Localities/localities_10km.tif", overwrite=T)
  
  points<-SpatialPointsDataFrame(coords = localities[, c("LONGITUDE", "LATITUDE")], 
                                 data = localities,
                                 proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  writeOGR(points, "../../Data_eBird_2020/Shape/Localities", layer="Localities", driver="ESRI Shapefile")
  
  localities_details<-readRDS("../../Data_eBird_2020/N_SP/N_SP_Details_Merged_ALL.rda")
  length(unique(localities_details$LOCALITY_ID))
}

print("Getting all localities ...")
locality<-readRDS("../../Tables/Overall/localities.rda")
print(paste(nrow(locality), "found"))
#colnames(locality)[which(colnames(locality)=="LOCALITY ID")]<-"LOCALITY_ID"
states<-readRDS("../../Tables/Overall/states.rda")
states[which(states$STATE==""),]
states<-states[sample(nrow(states), nrow(states)),]

#SQL_template<-"SELECT `LOCALITY ID` as LOCALITY_ID,\
#`OBSERVATION DATE` as OBSERVATION_DATE,\
#`OBSERVATION COUNT` as OBSERVATION_COUNT,\
#APPROVED, REVIEWED, COUNTRY, STATE, COUNTY, LATITUDE, LONGITUDE,\
#`OBSERVER ID` as OBSERVER_ID,\
#`SCIENTIFIC NAME` as SCIENTIFIC_NAME\ FROM eBird202012 WHERE `LOCALITY ID` in (%s)"

SQL_template<-"SELECT LOCALITY_ID,\
OBSERVATION_DATE,\
OBSERVATION_COUNT,\
EFFORT_DISTANCE_KM,\
APPROVED, REVIEWED, COUNTRY, STATE, COUNTY, LATITUDE, LONGITUDE,\
SCIENTIFIC_NAME,\
OBSERVER_ID FROM eBird202105 \
WHERE COUNTRY='%s' AND STATE='%s' AND COUNTY='%s'"

i<-1

#exception
#Algeria Laghouat Unnamed 
item<-states%>%dplyr::filter((COUNTRY=="Libya")&(STATE=="Al Marj")&(COUNTY=="Unnamed"))
item<-states%>%dplyr::filter(COUNTRY=="United States")
item<-item[5,]
states<-states[sample(nrow(states), nrow(states)),]
#mask_25<-raster("../../Data_eBird_2020/mask_2.5km.tif")
#mask_5km<-raster("../../Data_eBird_2020/mask_5km.tif")
#mask_10km<-raster("../../Data_eBird_2020/mask_10km.tif")
#states<-states%>%dplyr::filter(COUNTRY=="Colombia")

#Canada Ontario Renfrew

for (i in c(1:nrow(states))){
  
  item<-states[i,]
  target<-sprintf("../../Tables/Locality_202105/%s/%s/%s", item$COUNTRY, item$STATE, item$COUNTY)
  if (dir.exists(target)){
    #if (file.exists(sprintf("%s/raw.rda", target))){
    #print("Skip")
    #next()
    if (file.exists(sprintf("%s/N_SP.rda", target))){
      #next()
      N_SP<-readRDS(sprintf("%s/N_SP.rda", target))
      N_SP<-sum(N_SP$N_EVENTS)
      if (N_SP==item$N_SUM){
        next()
      }
    }
  }
  
  print(paste(i, nrow(states), item$COUNTRY, item$STATE, item$COUNTY, item$N_SUM))
  
  dir.create(target, recursive = T, showWarnings = F)
  #gsub("'","''",string)
  #county<-ifelse(item$COUNTY=="Unnamed", "",item$COUNTY)
  loc_items<-locality%>%dplyr::filter((COUNTRY==item$COUNTRY)&(STATE==item$STATE)&
                                        (COUNTY==item$COUNTY)&(LOCALITY_ID!='P')&(LOCALITY_ID!='H'))
  #Loc_IDs<-paste("'", loc_items$LOCALITY_ID, "'", sep="", collapse=",")
  
  con<-dbConnect(MySQL(), user="root", password="mikania", 
                 dbname="eBird", host="172.16.120.11")
  
  #rs<-dbSendQuery(con, sprintf(SQL_template, Loc_IDs))
  SQL<-sprintf(SQL_template, gsub("'", "''", item$COUNTRY), gsub("'", "''", item$STATE), 
               ifelse(item$COUNTY=="Unnamed", "", gsub("'", "''", item$COUNTY)))
  #print(SQL)
  rs<-dbSendQuery(con, SQL)
  df<-dbFetch(rs, n=-1)
  dbClearResult(rs)
  dbDisconnect(con)
  
  if (nrow(df)!=item$N_SUM){
    print(paste("Error", nrow(df)))
    next()
  }
  print(paste("N Record:", nrow(df)))
  
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
  df$COUNTY<-item$COUNTY
  df<-inner_join(df, loc_items, 
                 by=c("COUNTRY", "STATE", "COUNTY", "LOCALITY_ID", "LONGITUDE", "LATITUDE"))
  
  saveRDS(df, sprintf("%s/raw.rda", target))
  
  N_SP_Details_2.5km<-df%>%
    dplyr::group_by(SCIENTIFIC_NAME, COUNTRY, STATE, COUNTY, 
                    INDEX_2500m, YEAR, MONTH)%>%
    dplyr::summarise(N_SP=n_distinct(SCIENTIFIC_NAME),
                     N_EVENTS=n(),
                     N_OBSERVATION=sum(OBSERVATION_COUNT),
                     N_OBSERVER=n_distinct(OBSERVER_ID),
                     MEAN_OBSERVATION_PER_EVENT=mean(OBSERVATION_COUNT, na.rm=T),
                     SD_OBSERVATION_PER_EVENT=sd(OBSERVATION_COUNT, na.rm=T),
                     EFFORT_DISTANCE=mean(EFFORT_DISTANCE_KM, na.rm=T))
  saveRDS(N_SP_Details_2.5km, sprintf("%s/N_SP_Details_2.5km.rda", target))
  
  N_SP_Details_5km<-df%>%
    dplyr::group_by(SCIENTIFIC_NAME, COUNTRY, STATE, COUNTY, 
                    INDEX_5km, YEAR, MONTH)%>%
    dplyr::summarise(N_SP=n_distinct(SCIENTIFIC_NAME),
                     N_EVENTS=n(),
                     N_OBSERVATION=sum(OBSERVATION_COUNT),
                     N_OBSERVER=n_distinct(OBSERVER_ID),
                     MEAN_OBSERVATION_PER_EVENT=mean(OBSERVATION_COUNT, na.rm=T),
                     SD_OBSERVATION_PER_EVENT=sd(OBSERVATION_COUNT, na.rm=T),
                     EFFORT_DISTANCE=mean(EFFORT_DISTANCE_KM, na.rm=T))
  saveRDS(N_SP_Details_5km, sprintf("%s/N_SP_Details_5km.rda", target))
  
  N_SP_Details_10km<-df%>%
    dplyr::group_by(SCIENTIFIC_NAME, COUNTRY, STATE, COUNTY, 
                    INDEX_10km, YEAR, MONTH)%>%
    dplyr::summarise(N_SP=n_distinct(SCIENTIFIC_NAME),
                     N_EVENTS=n(),
                     N_OBSERVATION=sum(OBSERVATION_COUNT),
                     N_OBSERVER=n_distinct(OBSERVER_ID),
                     MEAN_OBSERVATION_PER_EVENT=mean(OBSERVATION_COUNT, na.rm=T),
                     SD_OBSERVATION_PER_EVENT=sd(OBSERVATION_COUNT, na.rm=T),
                     EFFORT_DISTANCE=mean(EFFORT_DISTANCE_KM, na.rm=T))
  saveRDS(N_SP_Details_10km, sprintf("%s/N_SP_Details_10km.rda", target))
  
  
  N_SP_2.5km<-df%>%
    dplyr::group_by(COUNTRY, STATE, COUNTY, 
                    INDEX_2500m, YEAR, MONTH)%>%
    dplyr::summarise(N_SP=n_distinct(SCIENTIFIC_NAME),
                     N_EVENTS=n(),
                     N_OBSERVATION=sum(OBSERVATION_COUNT),
                     N_OBSERVER=n_distinct(OBSERVER_ID),
                     MEAN_OBSERVATION_PER_EVENT=mean(OBSERVATION_COUNT, na.rm=T),
                     SD_OBSERVATION_PER_EVENT=sd(OBSERVATION_COUNT, na.rm=T),
                     EFFORT_DISTANCE=mean(EFFORT_DISTANCE_KM, na.rm=T))
  saveRDS(N_SP_2.5km, sprintf("%s/N_SP_2.5km.rda", target))
  
  N_OBSERVER_2.5km<-df%>%
    dplyr::group_by(COUNTRY, STATE, COUNTY, 
                    INDEX_2500m, YEAR, MONTH, OBSERVER_ID)%>%
    dplyr::summarise(N_SP=n_distinct(SCIENTIFIC_NAME),
                     N_EVENTS=n(),
                     N_OBSERVATION=sum(OBSERVATION_COUNT),
                     MEAN_OBSERVATION_PER_EVENT=mean(OBSERVATION_COUNT, na.rm=T),
                     SD_OBSERVATION_PER_EVENT=sd(OBSERVATION_COUNT, na.rm=T),
                     EFFORT_DISTANCE=mean(EFFORT_DISTANCE_KM, na.rm=T))
  saveRDS(N_OBSERVER_2.5km, sprintf("%s/N_OBSERVER_2.5km.rda", target))
  
  N_SP_5km<-df%>%
    dplyr::group_by(COUNTRY, STATE, COUNTY, 
                    INDEX_5km, YEAR, MONTH)%>%
    dplyr::summarise(N_SP=n_distinct(SCIENTIFIC_NAME),
                     N_EVENTS=n(),
                     N_OBSERVATION=sum(OBSERVATION_COUNT),
                     N_OBSERVER=n_distinct(OBSERVER_ID),
                     MEAN_OBSERVATION_PER_EVENT=mean(OBSERVATION_COUNT, na.rm=T),
                     SD_OBSERVATION_PER_EVENT=sd(OBSERVATION_COUNT, na.rm=T),
                     EFFORT_DISTANCE=mean(EFFORT_DISTANCE_KM, na.rm=T))
  saveRDS(N_SP_5km, sprintf("%s/N_SP_5km.rda", target))
  
  N_OBSERVER_5km<-df%>%
    dplyr::group_by(COUNTRY, STATE, COUNTY, 
                    INDEX_5km, YEAR, MONTH, OBSERVER_ID)%>%
    dplyr::summarise(N_SP=n_distinct(SCIENTIFIC_NAME),
                     N_EVENTS=n(),
                     N_OBSERVATION=sum(OBSERVATION_COUNT),
                     MEAN_OBSERVATION_PER_EVENT=mean(OBSERVATION_COUNT, na.rm=T),
                     SD_OBSERVATION_PER_EVENT=sd(OBSERVATION_COUNT, na.rm=T),
                     EFFORT_DISTANCE=mean(EFFORT_DISTANCE_KM, na.rm=T))
  saveRDS(N_OBSERVER_5km, sprintf("%s/N_OBSERVER_5km.rda", target))
  
  
  N_SP_10km<-df%>%
    dplyr::group_by(COUNTRY, STATE, COUNTY, 
                    INDEX_10km, YEAR, MONTH)%>%
    dplyr::summarise(N_SP=n_distinct(SCIENTIFIC_NAME),
                     N_EVENTS=n(),
                     N_OBSERVATION=sum(OBSERVATION_COUNT),
                     N_OBSERVER=n_distinct(OBSERVER_ID),
                     MEAN_OBSERVATION_PER_EVENT=mean(OBSERVATION_COUNT, na.rm=T),
                     SD_OBSERVATION_PER_EVENT=sd(OBSERVATION_COUNT, na.rm=T),
                     EFFORT_DISTANCE=mean(EFFORT_DISTANCE_KM, na.rm=T))
  saveRDS(N_SP_10km, sprintf("%s/N_SP_10km.rda", target))
  
  N_OBSERVER_10km<-df%>%
    dplyr::group_by(COUNTRY, STATE, COUNTY, 
                    INDEX_10km, YEAR, MONTH, OBSERVER_ID)%>%
    dplyr::summarise(N_SP=n_distinct(SCIENTIFIC_NAME),
                     N_EVENTS=n(),
                     N_OBSERVATION=sum(OBSERVATION_COUNT),
                     MEAN_OBSERVATION_PER_EVENT=mean(OBSERVATION_COUNT, na.rm=T),
                     SD_OBSERVATION_PER_EVENT=sd(OBSERVATION_COUNT, na.rm=T),
                     EFFORT_DISTANCE=mean(EFFORT_DISTANCE_KM, na.rm=T))
  saveRDS(N_OBSERVER_10km, sprintf("%s/N_OBSERVER_10km.rda", target))
  
  
  N_SP_Details<-df%>%
    dplyr::group_by(SCIENTIFIC_NAME, COUNTRY, STATE, COUNTY, 
                    LOCALITY_ID, LATITUDE, LONGITUDE, YEAR, MONTH)%>%
    dplyr::summarise(N_SP=n_distinct(SCIENTIFIC_NAME),
                     N_EVENTS=n(),
                     N_OBSERVATION=sum(OBSERVATION_COUNT),
                     N_OBSERVER=n_distinct(OBSERVER_ID),
                     MEAN_OBSERVATION_PER_EVENT=mean(OBSERVATION_COUNT, na.rm=T),
                     SD_OBSERVATION_PER_EVENT=sd(OBSERVATION_COUNT, na.rm=T),
                     EFFORT_DISTANCE=mean(EFFORT_DISTANCE_KM, na.rm=T))
  saveRDS(N_SP_Details, sprintf("%s/N_SP_Details.rda", target))
  
  N_SP<-df%>%
    dplyr::group_by(COUNTRY, STATE, COUNTY, LOCALITY_ID, 
                    LATITUDE, LONGITUDE, YEAR, MONTH)%>%
    dplyr::summarise(N_SP=n_distinct(SCIENTIFIC_NAME),
                     N_EVENTS=n(),
                     N_OBSERVATION=sum(OBSERVATION_COUNT),
                     N_OBSERVER=n_distinct(OBSERVER_ID),
                     MEAN_OBSERVATION_PER_EVENT=mean(OBSERVATION_COUNT, na.rm=T),
                     SD_OBSERVATION_PER_EVENT=sd(OBSERVATION_COUNT, na.rm=T),
                     EFFORT_DISTANCE=mean(EFFORT_DISTANCE_KM, na.rm=T))
  saveRDS(N_SP, sprintf("%s/N_SP.rda", target))
  
  N_OBSERVER<-df%>%
    dplyr::group_by(COUNTRY, STATE, COUNTY, LOCALITY_ID, 
                    LATITUDE, LONGITUDE, YEAR, MONTH, OBSERVER_ID)%>%
    dplyr::summarise(N_SP=n_distinct(SCIENTIFIC_NAME),
                     N_EVENTS=n(),
                     N_OBSERVATION=sum(OBSERVATION_COUNT),
                     MEAN_OBSERVATION_PER_EVENT=mean(OBSERVATION_COUNT, na.rm=T),
                     SD_OBSERVATION_PER_EVENT=sd(OBSERVATION_COUNT, na.rm=T),
                     EFFORT_DISTANCE=mean(EFFORT_DISTANCE_KM, na.rm=T))
  saveRDS(N_OBSERVER, sprintf("%s/N_OBSERVER.rda", target))
}


dbDisconnectAll <- function(){
  ile <- length(dbListConnections(MySQL())  )
  lapply( dbListConnections(MySQL()), function(x) dbDisconnect(x) )
  cat(sprintf("%s connection(s) closed.\n", ile))
}
dbDisconnectAll()

