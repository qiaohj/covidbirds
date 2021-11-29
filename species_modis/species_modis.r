setwd("/media/huijieqiao/WD12T/eBird/Script/ebird_migration")
library("RMySQL")
library("data.table")
library("rgdal")
library("raster")
library("sp")
setDTthreads(8)
print(sprintf("%d CPUs are using", getDTthreads()))

print("Reading species")
if (F){
  sql<-"SELECT count(1) N_RECORDS, `SCIENTIFIC NAME` as SCIENTIFIC_NAME FROM eBird202012 GROUP BY `SCIENTIFIC NAME`"
  con<-dbConnect(MySQL(), user="root", password="mikania", 
                 dbname="eBird", host="172.16.120.50")
  rs<-dbSendQuery(con, sql)
  sp_list<-fetch(rs, n=-1)
  dbClearResult(rs)
  dbDisconnect(con)
  saveRDS(sp_list, "../../Data_eBird_2020/Tables/species_list.rda")
}
sp_list<-readRDS("../../Data_eBird_2020/Tables/species_list.rda")

SQL_template<-"SELECT \
`LOCALITY ID` as LOCALITY_ID,\
`OBSERVATION DATE` as OBSERVATION_DATE,\
`OBSERVATION COUNT` as OBSERVATION_COUNT,\
APPROVED, COUNTRY, STATE, COUNTY, LATITUDE, LONGITUDE,\
`SCIENTIFIC NAME` as SCIENTIFIC_NAME\
FROM eBird202012 WHERE `SCIENTIFIC NAME`='%s'"


modis<-stack(sprintf("/media/huijieqiao/Speciation_Extin/land_sparing_sharing/Data/MCD12Q1/TIF/%d_LC_Type5.tif",
                     c(2009:2019)))
sp_list<-sp_list[sample(nrow(sp_list), nrow(sp_list)),]
i=1
for (i in c(1:nrow(sp_list))){
  sp<-sp_list[i,]
  if (grepl("/", sp$SCIENTIFIC_NAME)){
    next()
  }
  if (grepl("\\.", sp$SCIENTIFIC_NAME)){
    next()
  }
  print(paste(i, nrow(sp_list), sp$SCIENTIFIC_NAME, sp$N_RECORDS))
  target<-sprintf("../../Data_eBird_2020/Tables/Species/%s.rda", sp$SCIENTIFIC_NAME)
  if (file.exists(target)){
    next()
  }
  saveRDS(NULL, target)
  
  con<-dbConnect(MySQL(), user="root", password="mikania", 
                 dbname="eBird", host="172.16.120.50")
  SQL<-sprintf(SQL_template, sp$SCIENTIFIC_NAME)
  rs<-dbSendQuery(con, SQL)
  records<-fetch(rs, n=-1)
  dbClearResult(rs)
  dbDisconnect(con)
  if (nrow(records)!=sp$N_RECORDS){
    print("error, skip")
    next()
  }
  records<-data.table(records)
  records$LONGITUDE<-as.numeric(records$LONGITUDE)
  records$LATITUDE<-as.numeric(records$LATITUDE)
  records$OBS_DATE<-as.Date(records$OBSERVATION_DATE, format="%Y-%m-%d")
  records<-records[OBS_DATE>="2009-1-1"]
  records<-records[APPROVED==1]
  records$YEAR<-as.numeric(format(records$OBS_DATE,"%Y"))
  records$MONTH<-as.numeric(format(records$OBS_DATE,"%m"))
  records$OBSERVATION_COUNT<-as.numeric(records$OBSERVATION_COUNT)
  records[is.na(OBSERVATION_COUNT)]$OBSERVATION_COUNT<-1
  if (nrow(records)==0){
    next()
  }
  p<-SpatialPointsDataFrame(records[, c("LONGITUDE", "LATITUDE")], records,
                            proj4string=CRS("+proj=longlat"))
  
  p_sinu<-spTransform(p, CRS(proj4string(modis)))
  records$X<-p_sinu@coords[,1]
  records$Y<-p_sinu@coords[,2]
  records$MODIS<-255
  y=2020
  for (y in unique(records$YEAR)){
    print(y)
    item<-records[YEAR==y]
    
    modis_index<-y-2008
    if (modis_index>11){
      modis_index<-11
    }
    modis_v<-raster::extract(modis[[modis_index]], item[, c("X", "Y")])
    records[YEAR==y]$MODIS<-modis_v
  }
  saveRDS(records, target)
}

