library(ggplot2)
library(raster)
library(dplyr)
library(data.table)
setwd("/media/huijieqiao/WD12T/eBird/Script/ebird_migration")
if (F){
  
  
  mask_2500m<-raster("../../Data_eBird_2020/mask_2.5km.tif")
  mask_5km<-raster("../../Data_eBird_2020/mask_5km.tif")
  mask_10km<-raster("../../Data_eBird_2020/mask_10km.tif")
  
  
  mask_p<-data.frame(rasterToPoints(mask_2500m))
  mask_p$mask_5km<-extract(mask_5km, mask_p[, c("x", "y")])
  mask_p$mask_10km<-extract(mask_10km, mask_p[, c("x", "y")])
  saveRDS(mask_p, "../../Data_eBird_2020/mask_points.rda")
}

if (F){
  mask_25<-raster("../../Data_eBird_2020/mask_2.5km.tif")
  mask_5km<-raster("../../Data_eBird_2020/mask_5km.tif")
  mask_10km<-raster("../../Data_eBird_2020/mask_10km.tif")
  
  print("Getting all localities ...")
  
  states<-readRDS("../../Tables/Overall/states.rda")
  #states<-states[which(states$COUNTRY=="United Kingdom"), ]
  item<-states[which(states$COUNTRY=="United Kingdom"), ]
  item<-item[5,]
  target_years<-c(2010:2021)
  i=200
  N_SP_Merged_ALL<-list()
  N_OBSERVER_Merged_ALL<-list()
  N_SP_2.5km_Merged_ALL<-list()
  N_OBSERVER_2.5km_Merged_ALL<-list()
  N_SP_5km_Merged_ALL<-list()
  N_OBSERVER_5km_Merged_ALL<-list()
  N_SP_10km_Merged_ALL<-list()
  N_OBSERVER_10km_Merged_ALL<-list()
  
  N_SP_Details_Merged_ALL<-list()
  N_SP_2.5km_Details_Merged_ALL<-list()
  N_SP_5km_Details_Merged_ALL<-list()
  N_SP_10km_Details_Merged_ALL<-list()
  
  for (i in c(1:nrow(states))){
    item<-states[i,]
    print(paste(i, nrow(states), item$COUNTRY, item$STATE, item$COUNTY, item$N_SUM))
    target<-sprintf("../../Tables/Locality_202105/%s/%s/%s", item$COUNTRY, item$STATE, item$COUNTY)
    if (!file.exists(sprintf("%s/N_SP_Details.rda", target))){
    	next()
    }
    N_SP_Details<-readRDS(sprintf("%s/N_SP_Details.rda", target))
    N_SP_Details<-N_SP_Details%>%dplyr::filter(YEAR %in% target_years)
    if (nrow(N_SP_Details)==0){
      print("NO DATA, SKIP!")
      next()
    }
    
    N_SP<-readRDS(sprintf("%s/N_SP.rda", target))
    N_OBSERVER<-readRDS(sprintf("%s/N_OBSERVER.rda", target))
    N_SP_2.5km<-readRDS(sprintf("%s/N_SP_2.5km.rda", target))
    N_OBSERVER_2.5km<-readRDS(sprintf("%s/N_OBSERVER_2.5km.rda", target))
    N_SP_5km<-readRDS(sprintf("%s/N_SP_5km.rda", target))
    N_OBSERVER_5km<-readRDS(sprintf("%s/N_OBSERVER_5km.rda", target))
    N_SP_10km<-readRDS(sprintf("%s/N_SP_10km.rda", target))
    N_OBSERVER_10km<-readRDS(sprintf("%s/N_OBSERVER_10km.rda", target))
                              
    N_SP_2.5km_Details<-readRDS(sprintf("%s/N_SP_Details_2.5km.rda", target))
    N_SP_5km_Details<-readRDS(sprintf("%s/N_SP_Details_5km.rda", target))
    N_SP_10km_Details<-readRDS(sprintf("%s/N_SP_Details_10km.rda", target))
    
    
    #y=2020
    for (y in target_years[2:length(target_years)]){
      N_SP_CURRENT<-N_SP%>%dplyr::filter(YEAR==y)
      colnames(N_SP_CURRENT)[9:15]<-paste(c("N_SP", "N_EVENTS", "N_OBSERVATIONS", 
                                            "N_OBSERVERS", "MEAN_OBSERVATIONS_PER_EVENT", 
                                            "SD_OBSERVATION_PER_EVENT",
                                            "EFFORT_DISTANCE"), "CURRENT", sep="_")
      N_SP_CURRENT<-N_SP_CURRENT%>%ungroup()%>%dplyr::select(-c(YEAR))
      N_SP_LAST<-N_SP%>%dplyr::filter(YEAR==(y-1))
      colnames(N_SP_LAST)[9:15]<-paste(c("N_SP", "N_EVENTS", "N_OBSERVATIONS", 
                                         "N_OBSERVERS", "MEAN_OBSERVATIONS_PER_EVENT", 
                                         "SD_OBSERVATION_PER_EVENT",
                                         "EFFORT_DISTANCE"), "PREVIOUS", sep="_")
      N_SP_LAST<-N_SP_LAST%>%ungroup()%>%dplyr::select(-c(YEAR))
      N_SP_Merged<-full_join(N_SP_LAST, N_SP_CURRENT, 
                             by=c("COUNTRY", "STATE", "COUNTY", "LOCALITY_ID", 
                                  "LATITUDE", "LONGITUDE", "MONTH"))
      N_SP_Merged<-N_SP_Merged %>% 
        mutate(EFFORT_DISTANCE_CURRENT=replace(EFFORT_DISTANCE_CURRENT, 
                                               is.na(EFFORT_DISTANCE_CURRENT), -9999))
      N_SP_Merged<-N_SP_Merged %>% 
        mutate(EFFORT_DISTANCE_PREVIOUS=replace(EFFORT_DISTANCE_PREVIOUS, 
                                               is.na(EFFORT_DISTANCE_PREVIOUS), -9999))
      
      N_SP_Merged[is.na(N_SP_Merged)]<-0
      
      N_SP_Merged<-N_SP_Merged %>% 
        mutate(EFFORT_DISTANCE_CURRENT=replace(EFFORT_DISTANCE_CURRENT, 
                                               EFFORT_DISTANCE_CURRENT==-9999, NA))
      N_SP_Merged<-N_SP_Merged %>% 
        mutate(EFFORT_DISTANCE_PREVIOUS=replace(EFFORT_DISTANCE_PREVIOUS, 
                                                EFFORT_DISTANCE_PREVIOUS==-9999, NA))
      
      N_SP_Merged$YEAR<-y
      N_SP_Merged_ALL[[sprintf("%s-%s-%s-%d", 
                               item$COUNTRY, item$STATE, item$COUNTY, y)]]<-N_SP_Merged
      
      N_OBSERVER_CURRENT<-N_OBSERVER%>%dplyr::filter(YEAR==y)
      colnames(N_OBSERVER_CURRENT)[10:15]<-paste(c("N_SP", "N_EVENTS", "N_OBSERVATIONS", 
                                            "MEAN_OBSERVATIONS_PER_EVENT", 
                                            "SD_OBSERVATION_PER_EVENT",
                                            "EFFORT_DISTANCE"), "CURRENT", sep="_")
      N_OBSERVER_CURRENT<-N_OBSERVER_CURRENT%>%ungroup()%>%dplyr::select(-c(YEAR))
      N_OBSERVER_LAST<-N_OBSERVER%>%dplyr::filter(YEAR==(y-1))
      colnames(N_OBSERVER_LAST)[10:15]<-paste(c("N_SP", "N_EVENTS", "N_OBSERVATIONS", 
                                                "MEAN_OBSERVATIONS_PER_EVENT", 
                                                "SD_OBSERVATION_PER_EVENT",
                                                "EFFORT_DISTANCE"), "PREVIOUS", sep="_")
      N_OBSERVER_LAST<-N_OBSERVER_LAST%>%ungroup()%>%dplyr::select(-c(YEAR))
      N_OBSERVER_Merged<-full_join(N_OBSERVER_LAST, N_OBSERVER_CURRENT, 
                             by=c("COUNTRY", "STATE", "COUNTY", "LOCALITY_ID", 
                                  "LATITUDE", "LONGITUDE", "MONTH", "OBSERVER_ID"))
      N_OBSERVER_Merged<-N_OBSERVER_Merged %>% 
        mutate(EFFORT_DISTANCE_CURRENT=replace(EFFORT_DISTANCE_CURRENT, 
                                               is.na(EFFORT_DISTANCE_CURRENT), -9999))
      N_OBSERVER_Merged<-N_OBSERVER_Merged %>% 
        mutate(EFFORT_DISTANCE_PREVIOUS=replace(EFFORT_DISTANCE_PREVIOUS, 
                                                is.na(EFFORT_DISTANCE_PREVIOUS), -9999))
      
      N_OBSERVER_Merged[is.na(N_OBSERVER_Merged)]<-0
      
      N_OBSERVER_Merged<-N_OBSERVER_Merged %>% 
        mutate(EFFORT_DISTANCE_CURRENT=replace(EFFORT_DISTANCE_CURRENT, 
                                               EFFORT_DISTANCE_CURRENT==-9999, NA))
      N_OBSERVER_Merged<-N_OBSERVER_Merged %>% 
        mutate(EFFORT_DISTANCE_PREVIOUS=replace(EFFORT_DISTANCE_PREVIOUS, 
                                                EFFORT_DISTANCE_PREVIOUS==-9999, NA))
      
      N_OBSERVER_Merged$YEAR<-y
      N_OBSERVER_Merged_ALL[[sprintf("%s-%s-%s-%d", 
                               item$COUNTRY, item$STATE, item$COUNTY, y)]]<-N_OBSERVER_Merged
      
      N_SP_Details_CURRENT<-N_SP_Details%>%dplyr::filter(YEAR==y)
      colnames(N_SP_Details_CURRENT)[10:16]<-paste(c("N_SP", "N_EVENTS", 
                                                    "N_OBSERVATIONS", 
                                                    "N_OBSERVERS",
                                                    "MEAN_OBSERVATIONS_PER_EVENT",
                                                    "SD_OBSERVATION_PER_EVENT",
                                                    "EFFORT_DISTANCE"), 
                                                  "CURRENT", sep="_")
      N_SP_Details_CURRENT<-N_SP_Details_CURRENT%>%ungroup()%>%dplyr::select(-c(YEAR))
      N_SP_Details_LAST<-N_SP_Details%>%dplyr::filter(YEAR==(y-1))
      colnames(N_SP_Details_LAST)[10:16]<-paste(c("N_SP", "N_EVENTS", 
                                                  "N_OBSERVATIONS", 
                                                  "N_OBSERVERS",
                                                  "MEAN_OBSERVATIONS_PER_EVENT",
                                                  "SD_OBSERVATION_PER_EVENT",
                                                  "EFFORT_DISTANCE"), 
                                                "PREVIOUS", sep="_")
      N_SP_Details_LAST<-N_SP_Details_LAST%>%ungroup()%>%dplyr::select(-c(YEAR))
      N_SP_Details_Merged<-full_join(N_SP_Details_LAST, N_SP_Details_CURRENT, 
                             by=c("SCIENTIFIC_NAME", "COUNTRY", "STATE", "COUNTY", "LOCALITY_ID", 
                                  "LATITUDE", "LONGITUDE", "MONTH"))
      N_SP_Details_Merged<-N_SP_Details_Merged %>% 
        mutate(EFFORT_DISTANCE_CURRENT=replace(EFFORT_DISTANCE_CURRENT, 
                                               is.na(EFFORT_DISTANCE_CURRENT), -9999))
      N_SP_Details_Merged<-N_SP_Details_Merged %>% 
        mutate(EFFORT_DISTANCE_PREVIOUS=replace(EFFORT_DISTANCE_PREVIOUS, 
                                                is.na(EFFORT_DISTANCE_PREVIOUS), -9999))
      
      N_SP_Details_Merged[is.na(N_SP_Details_Merged)]<-0
      
      N_SP_Details_Merged<-N_SP_Details_Merged %>% 
        mutate(EFFORT_DISTANCE_CURRENT=replace(EFFORT_DISTANCE_CURRENT, 
                                               EFFORT_DISTANCE_CURRENT==-9999, NA))
      N_SP_Details_Merged<-N_SP_Details_Merged %>% 
        mutate(EFFORT_DISTANCE_PREVIOUS=replace(EFFORT_DISTANCE_PREVIOUS, 
                                                EFFORT_DISTANCE_PREVIOUS==-9999, NA))
      N_SP_Details_Merged$YEAR<-y
      N_SP_Details_Merged_ALL[[sprintf("%s-%s-%s-%d", 
                               item$COUNTRY, item$STATE, item$COUNTY, y)]]<-N_SP_Details_Merged
      
      ##2.5km
      
      N_SP_2.5km_CURRENT<-N_SP_2.5km%>%dplyr::filter(YEAR==y)
      colnames(N_SP_2.5km_CURRENT)[7:13]<-paste(c("N_SP", "N_EVENTS", 
                                                  "N_OBSERVATIONS", 
                                                  "N_OBSERVERS",
                                                  "MEAN_OBSERVATIONS_PER_EVENT",
                                                  "SD_OBSERVATION_PER_EVENT",
                                                  "EFFORT_DISTANCE"), "CURRENT", sep="_")
      N_SP_2.5km_CURRENT<-N_SP_2.5km_CURRENT%>%ungroup()%>%dplyr::select(-c(YEAR))
      N_SP_2.5km_LAST<-N_SP_2.5km%>%dplyr::filter(YEAR==(y-1))
      colnames(N_SP_2.5km_LAST)[7:13]<-paste(c("N_SP", "N_EVENTS", 
                                               "N_OBSERVATIONS", 
                                               "N_OBSERVERS",
                                               "MEAN_OBSERVATIONS_PER_EVENT",
                                               "SD_OBSERVATION_PER_EVENT",
                                               "EFFORT_DISTANCE"), "PREVIOUS", sep="_")
      N_SP_2.5km_LAST<-N_SP_2.5km_LAST%>%ungroup()%>%dplyr::select(-c(YEAR))
      N_SP_2.5km_Merged<-full_join(N_SP_2.5km_LAST, N_SP_2.5km_CURRENT, 
                             by=c("COUNTRY", "STATE", "COUNTY", "INDEX_2500m", 
                                  "MONTH"))
      N_SP_2.5km_Merged<-N_SP_2.5km_Merged %>% 
        mutate(EFFORT_DISTANCE_CURRENT=replace(EFFORT_DISTANCE_CURRENT, 
                                               is.na(EFFORT_DISTANCE_CURRENT), -9999))
      N_SP_2.5km_Merged<-N_SP_2.5km_Merged %>% 
        mutate(EFFORT_DISTANCE_PREVIOUS=replace(EFFORT_DISTANCE_PREVIOUS, 
                                                is.na(EFFORT_DISTANCE_PREVIOUS), -9999))
      
      N_SP_2.5km_Merged[is.na(N_SP_2.5km_Merged)]<-0
      
      N_SP_2.5km_Merged<-N_SP_2.5km_Merged %>% 
        mutate(EFFORT_DISTANCE_CURRENT=replace(EFFORT_DISTANCE_CURRENT, 
                                               EFFORT_DISTANCE_CURRENT==-9999, NA))
      N_SP_2.5km_Merged<-N_SP_2.5km_Merged %>% 
        mutate(EFFORT_DISTANCE_PREVIOUS=replace(EFFORT_DISTANCE_PREVIOUS, 
                                                EFFORT_DISTANCE_PREVIOUS==-9999, NA))
      
      N_SP_2.5km_Merged$YEAR<-y
      N_SP_2.5km_Merged_ALL[[sprintf("%s-%s-%s-%d", 
                               item$COUNTRY, item$STATE, item$COUNTY, y)]]<-N_SP_2.5km_Merged
      
      N_OBSERVER_2.5km_CURRENT<-N_OBSERVER_2.5km%>%dplyr::filter(YEAR==y)
      colnames(N_OBSERVER_2.5km_CURRENT)[8:13]<-paste(c("N_SP", "N_EVENTS", "N_OBSERVATIONS", 
                                                   "MEAN_OBSERVATIONS_PER_EVENT", 
                                                   "SD_OBSERVATION_PER_EVENT",
                                                   "EFFORT_DISTANCE"), "CURRENT", sep="_")
      N_OBSERVER_2.5km_CURRENT<-N_OBSERVER_2.5km_CURRENT%>%ungroup()%>%dplyr::select(-c(YEAR))
      N_OBSERVER_2.5km_LAST<-N_OBSERVER_2.5km%>%dplyr::filter(YEAR==(y-1))
      colnames(N_OBSERVER_2.5km_LAST)[8:13]<-paste(c("N_SP", "N_EVENTS", "N_OBSERVATIONS", 
                                                "MEAN_OBSERVATIONS_PER_EVENT", 
                                                "SD_OBSERVATION_PER_EVENT",
                                                "EFFORT_DISTANCE"), "PREVIOUS", sep="_")
      N_OBSERVER_2.5km_LAST<-N_OBSERVER_2.5km_LAST%>%ungroup()%>%dplyr::select(-c(YEAR))
      N_OBSERVER_2.5km_Merged<-full_join(N_OBSERVER_2.5km_LAST, N_OBSERVER_2.5km_CURRENT, 
                                   by=c("COUNTRY", "STATE", "COUNTY", "INDEX_2500m", 
                                        "MONTH", "OBSERVER_ID"))
      N_OBSERVER_2.5km_Merged<-N_OBSERVER_2.5km_Merged %>% 
        mutate(EFFORT_DISTANCE_CURRENT=replace(EFFORT_DISTANCE_CURRENT, 
                                               is.na(EFFORT_DISTANCE_CURRENT), -9999))
      N_OBSERVER_2.5km_Merged<-N_OBSERVER_2.5km_Merged %>% 
        mutate(EFFORT_DISTANCE_PREVIOUS=replace(EFFORT_DISTANCE_PREVIOUS, 
                                                is.na(EFFORT_DISTANCE_PREVIOUS), -9999))
      
      N_OBSERVER_2.5km_Merged[is.na(N_OBSERVER_2.5km_Merged)]<-0
      
      N_OBSERVER_2.5km_Merged<-N_OBSERVER_2.5km_Merged %>% 
        mutate(EFFORT_DISTANCE_CURRENT=replace(EFFORT_DISTANCE_CURRENT, 
                                               EFFORT_DISTANCE_CURRENT==-9999, NA))
      N_OBSERVER_2.5km_Merged<-N_OBSERVER_2.5km_Merged %>% 
        mutate(EFFORT_DISTANCE_PREVIOUS=replace(EFFORT_DISTANCE_PREVIOUS, 
                                                EFFORT_DISTANCE_PREVIOUS==-9999, NA))
      
      N_OBSERVER_2.5km_Merged$YEAR<-y
      N_OBSERVER_2.5km_Merged_ALL[[sprintf("%s-%s-%s-%d", 
                                     item$COUNTRY, item$STATE, item$COUNTY, y)]]<-N_OBSERVER_2.5km_Merged
      
      N_SP_2.5km_Details_CURRENT<-N_SP_2.5km_Details%>%dplyr::filter(YEAR==y)
      colnames(N_SP_2.5km_Details_CURRENT)[8:14]<-paste(c("N_SP", "N_EVENTS", 
                                                          "N_OBSERVATIONS", 
                                                          "N_OBSERVERS",
                                                          "MEAN_OBSERVATIONS_PER_EVENT",
                                                          "SD_OBSERVATION_PER_EVENT",
                                                          "EFFORT_DISTANCE"), 
                                                   "CURRENT", sep="_")
      N_SP_2.5km_Details_CURRENT<-N_SP_2.5km_Details_CURRENT%>%ungroup()%>%dplyr::select(-c(YEAR))
      N_SP_2.5km_Details_LAST<-N_SP_2.5km_Details%>%dplyr::filter(YEAR==(y-1))
      colnames(N_SP_2.5km_Details_LAST)[8:14]<-paste(c("N_SP", "N_EVENTS", 
                                                       "N_OBSERVATIONS", 
                                                       "N_OBSERVERS",
                                                       "MEAN_OBSERVATIONS_PER_EVENT",
                                                       "SD_OBSERVATION_PER_EVENT",
                                                       "EFFORT_DISTANCE"), 
                                                "PREVIOUS", sep="_")
      N_SP_2.5km_Details_LAST<-N_SP_2.5km_Details_LAST%>%ungroup()%>%dplyr::select(-c(YEAR))
      N_SP_2.5km_Details_Merged<-full_join(N_SP_2.5km_Details_LAST, N_SP_2.5km_Details_CURRENT, 
                                     by=c("SCIENTIFIC_NAME", "COUNTRY", "STATE", "COUNTY", 
                                          "INDEX_2500m", "MONTH"))
      N_SP_2.5km_Details_Merged<-N_SP_2.5km_Details_Merged %>% 
        mutate(EFFORT_DISTANCE_CURRENT=replace(EFFORT_DISTANCE_CURRENT, 
                                               is.na(EFFORT_DISTANCE_CURRENT), -9999))
      N_SP_2.5km_Details_Merged<-N_SP_2.5km_Details_Merged %>% 
        mutate(EFFORT_DISTANCE_PREVIOUS=replace(EFFORT_DISTANCE_PREVIOUS, 
                                                is.na(EFFORT_DISTANCE_PREVIOUS), -9999))
      
      N_SP_2.5km_Details_Merged[is.na(N_SP_2.5km_Details_Merged)]<-0
      
      N_SP_2.5km_Details_Merged<-N_SP_2.5km_Details_Merged %>% 
        mutate(EFFORT_DISTANCE_CURRENT=replace(EFFORT_DISTANCE_CURRENT, 
                                               EFFORT_DISTANCE_CURRENT==-9999, NA))
      N_SP_2.5km_Details_Merged<-N_SP_2.5km_Details_Merged %>% 
        mutate(EFFORT_DISTANCE_PREVIOUS=replace(EFFORT_DISTANCE_PREVIOUS, 
                                                EFFORT_DISTANCE_PREVIOUS==-9999, NA))
      N_SP_2.5km_Details_Merged$YEAR<-y
      N_SP_2.5km_Details_Merged_ALL[[sprintf("%s-%s-%s-%d", 
                                       item$COUNTRY, item$STATE, item$COUNTY, y)]]<-N_SP_2.5km_Details_Merged
      
      ##5km
      
      N_SP_5km_CURRENT<-N_SP_5km%>%dplyr::filter(YEAR==y)
      colnames(N_SP_5km_CURRENT)[7:13]<-paste(c("N_SP", "N_EVENTS", 
                                                "N_OBSERVATIONS", 
                                                "N_OBSERVERS",
                                                "MEAN_OBSERVATIONS_PER_EVENT",
                                                "SD_OBSERVATION_PER_EVENT",
                                                "EFFORT_DISTANCE"), "CURRENT", sep="_")
      N_SP_5km_CURRENT<-N_SP_5km_CURRENT%>%ungroup()%>%dplyr::select(-c(YEAR))
      N_SP_5km_LAST<-N_SP_5km%>%dplyr::filter(YEAR==(y-1))
      colnames(N_SP_5km_LAST)[7:13]<-paste(c("N_SP", "N_EVENTS", 
                                             "N_OBSERVATIONS", 
                                             "N_OBSERVERS",
                                             "MEAN_OBSERVATIONS_PER_EVENT",
                                             "SD_OBSERVATION_PER_EVENT",
                                             "EFFORT_DISTANCE"), "PREVIOUS", sep="_")
      N_SP_5km_LAST<-N_SP_5km_LAST%>%ungroup()%>%dplyr::select(-c(YEAR))
      N_SP_5km_Merged<-full_join(N_SP_5km_LAST, N_SP_5km_CURRENT, 
                                   by=c("COUNTRY", "STATE", "COUNTY", "INDEX_5km", 
                                        "MONTH"))
      N_SP_5km_Merged<-N_SP_5km_Merged %>% 
        mutate(EFFORT_DISTANCE_CURRENT=replace(EFFORT_DISTANCE_CURRENT, 
                                               is.na(EFFORT_DISTANCE_CURRENT), -9999))
      N_SP_5km_Merged<-N_SP_5km_Merged %>% 
        mutate(EFFORT_DISTANCE_PREVIOUS=replace(EFFORT_DISTANCE_PREVIOUS, 
                                                is.na(EFFORT_DISTANCE_PREVIOUS), -9999))
      
      N_SP_5km_Merged[is.na(N_SP_5km_Merged)]<-0
      
      N_SP_5km_Merged<-N_SP_5km_Merged %>% 
        mutate(EFFORT_DISTANCE_CURRENT=replace(EFFORT_DISTANCE_CURRENT, 
                                               EFFORT_DISTANCE_CURRENT==-9999, NA))
      N_SP_5km_Merged<-N_SP_5km_Merged %>% 
        mutate(EFFORT_DISTANCE_PREVIOUS=replace(EFFORT_DISTANCE_PREVIOUS, 
                                                EFFORT_DISTANCE_PREVIOUS==-9999, NA))
      
      N_SP_5km_Merged$YEAR<-y
      N_SP_5km_Merged_ALL[[sprintf("%s-%s-%s-%d", 
                                     item$COUNTRY, item$STATE, item$COUNTY, y)]]<-N_SP_5km_Merged
      
      
      N_OBSERVER_5km_CURRENT<-N_OBSERVER_5km%>%dplyr::filter(YEAR==y)
      colnames(N_OBSERVER_5km_CURRENT)[8:13]<-paste(c("N_SP", "N_EVENTS", "N_OBSERVATIONS", 
                                                        "MEAN_OBSERVATIONS_PER_EVENT", 
                                                        "SD_OBSERVATION_PER_EVENT",
                                                        "EFFORT_DISTANCE"), "CURRENT", sep="_")
      N_OBSERVER_5km_CURRENT<-N_OBSERVER_5km_CURRENT%>%ungroup()%>%dplyr::select(-c(YEAR))
      N_OBSERVER_5km_LAST<-N_OBSERVER_5km%>%dplyr::filter(YEAR==(y-1))
      colnames(N_OBSERVER_5km_LAST)[8:13]<-paste(c("N_SP", "N_EVENTS", "N_OBSERVATIONS", 
                                                     "MEAN_OBSERVATIONS_PER_EVENT", 
                                                     "SD_OBSERVATION_PER_EVENT",
                                                     "EFFORT_DISTANCE"), "PREVIOUS", sep="_")
      N_OBSERVER_5km_LAST<-N_OBSERVER_5km_LAST%>%ungroup()%>%dplyr::select(-c(YEAR))
      N_OBSERVER_5km_Merged<-full_join(N_OBSERVER_5km_LAST, N_OBSERVER_5km_CURRENT, 
                                         by=c("COUNTRY", "STATE", "COUNTY", "INDEX_5km", 
                                              "MONTH", "OBSERVER_ID"))
      N_OBSERVER_5km_Merged<-N_OBSERVER_5km_Merged %>% 
        mutate(EFFORT_DISTANCE_CURRENT=replace(EFFORT_DISTANCE_CURRENT, 
                                               is.na(EFFORT_DISTANCE_CURRENT), -9999))
      N_OBSERVER_5km_Merged<-N_OBSERVER_5km_Merged %>% 
        mutate(EFFORT_DISTANCE_PREVIOUS=replace(EFFORT_DISTANCE_PREVIOUS, 
                                                is.na(EFFORT_DISTANCE_PREVIOUS), -9999))
      
      N_OBSERVER_5km_Merged[is.na(N_OBSERVER_5km_Merged)]<-0
      
      N_OBSERVER_5km_Merged<-N_OBSERVER_5km_Merged %>% 
        mutate(EFFORT_DISTANCE_CURRENT=replace(EFFORT_DISTANCE_CURRENT, 
                                               EFFORT_DISTANCE_CURRENT==-9999, NA))
      N_OBSERVER_5km_Merged<-N_OBSERVER_5km_Merged %>% 
        mutate(EFFORT_DISTANCE_PREVIOUS=replace(EFFORT_DISTANCE_PREVIOUS, 
                                                EFFORT_DISTANCE_PREVIOUS==-9999, NA))
      
      N_OBSERVER_5km_Merged$YEAR<-y
      N_OBSERVER_5km_Merged_ALL[[sprintf("%s-%s-%s-%d", 
                                           item$COUNTRY, item$STATE, item$COUNTY, y)]]<-N_OBSERVER_5km_Merged
      
      N_SP_5km_Details_CURRENT<-N_SP_5km_Details%>%dplyr::filter(YEAR==y)
      colnames(N_SP_5km_Details_CURRENT)[8:14]<-paste(c("N_SP", "N_EVENTS", 
                                                        "N_OBSERVATIONS", 
                                                        "N_OBSERVERS",
                                                        "MEAN_OBSERVATIONS_PER_EVENT",
                                                        "SD_OBSERVATION_PER_EVENT",
                                                        "EFFORT_DISTANCE"), 
                                                        "CURRENT", sep="_")
      N_SP_5km_Details_CURRENT<-N_SP_5km_Details_CURRENT%>%ungroup()%>%dplyr::select(-c(YEAR))
      N_SP_5km_Details_LAST<-N_SP_5km_Details%>%dplyr::filter(YEAR==(y-1))
      colnames(N_SP_5km_Details_LAST)[8:14]<-paste(c("N_SP", "N_EVENTS", 
                                                     "N_OBSERVATIONS", 
                                                     "N_OBSERVERS",
                                                     "MEAN_OBSERVATIONS_PER_EVENT",
                                                     "SD_OBSERVATION_PER_EVENT",
                                                     "EFFORT_DISTANCE"), 
                                                     "PREVIOUS", sep="_")
      N_SP_5km_Details_LAST<-N_SP_5km_Details_LAST%>%ungroup()%>%dplyr::select(-c(YEAR))
      N_SP_5km_Details_Merged<-full_join(N_SP_5km_Details_LAST, N_SP_5km_Details_CURRENT, 
                                           by=c("SCIENTIFIC_NAME", "COUNTRY", "STATE", "COUNTY", 
                                                "INDEX_5km", "MONTH"))
      N_SP_5km_Details_Merged<-N_SP_5km_Details_Merged %>% 
        mutate(EFFORT_DISTANCE_CURRENT=replace(EFFORT_DISTANCE_CURRENT, 
                                               is.na(EFFORT_DISTANCE_CURRENT), -9999))
      N_SP_5km_Details_Merged<-N_SP_5km_Details_Merged %>% 
        mutate(EFFORT_DISTANCE_PREVIOUS=replace(EFFORT_DISTANCE_PREVIOUS, 
                                                is.na(EFFORT_DISTANCE_PREVIOUS), -9999))
      
      N_SP_5km_Details_Merged[is.na(N_SP_5km_Details_Merged)]<-0
      
      N_SP_5km_Details_Merged<-N_SP_5km_Details_Merged %>% 
        mutate(EFFORT_DISTANCE_CURRENT=replace(EFFORT_DISTANCE_CURRENT, 
                                               EFFORT_DISTANCE_CURRENT==-9999, NA))
      N_SP_5km_Details_Merged<-N_SP_5km_Details_Merged %>% 
        mutate(EFFORT_DISTANCE_PREVIOUS=replace(EFFORT_DISTANCE_PREVIOUS, 
                                                EFFORT_DISTANCE_PREVIOUS==-9999, NA))
      N_SP_5km_Details_Merged$YEAR<-y
      N_SP_5km_Details_Merged_ALL[[sprintf("%s-%s-%s-%d", 
                                             item$COUNTRY, item$STATE, item$COUNTY, y)]]<-N_SP_5km_Details_Merged
      
      ##10km
      
      N_SP_10km_CURRENT<-N_SP_10km%>%dplyr::filter(YEAR==y)
      colnames(N_SP_10km_CURRENT)[7:13]<-paste(c("N_SP", "N_EVENTS", 
                                                 "N_OBSERVATIONS", 
                                                 "N_OBSERVERS",
                                                 "MEAN_OBSERVATIONS_PER_EVENT",
                                                 "SD_OBSERVATION_PER_EVENT",
                                                 "EFFORT_DISTANCE"), "CURRENT", sep="_")
      N_SP_10km_CURRENT<-N_SP_10km_CURRENT%>%ungroup()%>%dplyr::select(-c(YEAR))
      N_SP_10km_LAST<-N_SP_10km%>%dplyr::filter(YEAR==(y-1))
      colnames(N_SP_10km_LAST)[7:13]<-paste(c("N_SP", "N_EVENTS", 
                                              "N_OBSERVATIONS", 
                                              "N_OBSERVERS",
                                              "MEAN_OBSERVATIONS_PER_EVENT",
                                              "SD_OBSERVATION_PER_EVENT",
                                              "EFFORT_DISTANCE"), "PREVIOUS", sep="_")
      N_SP_10km_LAST<-N_SP_10km_LAST%>%ungroup()%>%dplyr::select(-c(YEAR))
      N_SP_10km_Merged<-full_join(N_SP_10km_LAST, N_SP_10km_CURRENT, 
                                 by=c("COUNTRY", "STATE", "COUNTY", "INDEX_10km", 
                                      "MONTH"))
      N_SP_10km_Merged<-N_SP_10km_Merged %>% 
        mutate(EFFORT_DISTANCE_CURRENT=replace(EFFORT_DISTANCE_CURRENT, 
                                               is.na(EFFORT_DISTANCE_CURRENT), -9999))
      N_SP_10km_Merged<-N_SP_10km_Merged %>% 
        mutate(EFFORT_DISTANCE_PREVIOUS=replace(EFFORT_DISTANCE_PREVIOUS, 
                                                is.na(EFFORT_DISTANCE_PREVIOUS), -9999))
      
      N_SP_10km_Merged[is.na(N_SP_10km_Merged)]<-0
      
      N_SP_10km_Merged<-N_SP_10km_Merged %>% 
        mutate(EFFORT_DISTANCE_CURRENT=replace(EFFORT_DISTANCE_CURRENT, 
                                               EFFORT_DISTANCE_CURRENT==-9999, NA))
      N_SP_10km_Merged<-N_SP_10km_Merged %>% 
        mutate(EFFORT_DISTANCE_PREVIOUS=replace(EFFORT_DISTANCE_PREVIOUS, 
                                                EFFORT_DISTANCE_PREVIOUS==-9999, NA))
      
      N_SP_10km_Merged$YEAR<-y
      N_SP_10km_Merged_ALL[[sprintf("%s-%s-%s-%d", 
                                   item$COUNTRY, item$STATE, item$COUNTY, y)]]<-N_SP_10km_Merged
      
      
      N_OBSERVER_10km_CURRENT<-N_OBSERVER_10km%>%dplyr::filter(YEAR==y)
      colnames(N_OBSERVER_10km_CURRENT)[8:13]<-paste(c("N_SP", "N_EVENTS", "N_OBSERVATIONS", 
                                                        "MEAN_OBSERVATIONS_PER_EVENT", 
                                                        "SD_OBSERVATION_PER_EVENT",
                                                        "EFFORT_DISTANCE"), "CURRENT", sep="_")
      N_OBSERVER_10km_CURRENT<-N_OBSERVER_10km_CURRENT%>%ungroup()%>%dplyr::select(-c(YEAR))
      N_OBSERVER_10km_LAST<-N_OBSERVER_10km%>%dplyr::filter(YEAR==(y-1))
      colnames(N_OBSERVER_10km_LAST)[8:13]<-paste(c("N_SP", "N_EVENTS", "N_OBSERVATIONS", 
                                                     "MEAN_OBSERVATIONS_PER_EVENT", 
                                                     "SD_OBSERVATION_PER_EVENT",
                                                     "EFFORT_DISTANCE"), "PREVIOUS", sep="_")
      N_OBSERVER_10km_LAST<-N_OBSERVER_10km_LAST%>%ungroup()%>%dplyr::select(-c(YEAR))
      N_OBSERVER_10km_Merged<-full_join(N_OBSERVER_10km_LAST, N_OBSERVER_10km_CURRENT, 
                                         by=c("COUNTRY", "STATE", "COUNTY", "INDEX_10km", 
                                              "MONTH", "OBSERVER_ID"))
      N_OBSERVER_10km_Merged<-N_OBSERVER_10km_Merged %>% 
        mutate(EFFORT_DISTANCE_CURRENT=replace(EFFORT_DISTANCE_CURRENT, 
                                               is.na(EFFORT_DISTANCE_CURRENT), -9999))
      N_OBSERVER_10km_Merged<-N_OBSERVER_10km_Merged %>% 
        mutate(EFFORT_DISTANCE_PREVIOUS=replace(EFFORT_DISTANCE_PREVIOUS, 
                                                is.na(EFFORT_DISTANCE_PREVIOUS), -9999))
      
      N_OBSERVER_10km_Merged[is.na(N_OBSERVER_10km_Merged)]<-0
      
      N_OBSERVER_10km_Merged<-N_OBSERVER_10km_Merged %>% 
        mutate(EFFORT_DISTANCE_CURRENT=replace(EFFORT_DISTANCE_CURRENT, 
                                               EFFORT_DISTANCE_CURRENT==-9999, NA))
      N_OBSERVER_10km_Merged<-N_OBSERVER_10km_Merged %>% 
        mutate(EFFORT_DISTANCE_PREVIOUS=replace(EFFORT_DISTANCE_PREVIOUS, 
                                                EFFORT_DISTANCE_PREVIOUS==-9999, NA))
      
      N_OBSERVER_10km_Merged$YEAR<-y
      N_OBSERVER_10km_Merged_ALL[[sprintf("%s-%s-%s-%d", 
                                           item$COUNTRY, item$STATE, item$COUNTY, y)]]<-N_OBSERVER_10km_Merged
      
      
      N_SP_10km_Details_CURRENT<-N_SP_10km_Details%>%dplyr::filter(YEAR==y)
      colnames(N_SP_10km_Details_CURRENT)[8:14]<-paste(c("N_SP", "N_EVENTS", 
                                                         "N_OBSERVATIONS", 
                                                         "N_OBSERVERS",
                                                         "MEAN_OBSERVATIONS_PER_EVENT",
                                                         "SD_OBSERVATION_PER_EVENT",
                                                         "EFFORT_DISTANCE"), 
                                                      "CURRENT", sep="_")
      N_SP_10km_Details_CURRENT<-N_SP_10km_Details_CURRENT%>%ungroup()%>%dplyr::select(-c(YEAR))
      N_SP_10km_Details_LAST<-N_SP_10km_Details%>%dplyr::filter(YEAR==(y-1))
      colnames(N_SP_10km_Details_LAST)[8:14]<-paste(c("N_SP", "N_EVENTS", 
                                                      "N_OBSERVATIONS", 
                                                      "N_OBSERVERS",
                                                      "MEAN_OBSERVATIONS_PER_EVENT",
                                                      "SD_OBSERVATION_PER_EVENT",
                                                      "EFFORT_DISTANCE"), 
                                                   "PREVIOUS", sep="_")
      N_SP_10km_Details_LAST<-N_SP_10km_Details_LAST%>%ungroup()%>%dplyr::select(-c(YEAR))
      N_SP_10km_Details_Merged<-full_join(N_SP_10km_Details_LAST, N_SP_10km_Details_CURRENT, 
                                         by=c("SCIENTIFIC_NAME", "COUNTRY", "STATE", "COUNTY", 
                                              "INDEX_10km", "MONTH"))
      N_SP_10km_Details_Merged<-N_SP_10km_Details_Merged %>% 
        mutate(EFFORT_DISTANCE_CURRENT=replace(EFFORT_DISTANCE_CURRENT, 
                                               is.na(EFFORT_DISTANCE_CURRENT), -9999))
      N_SP_10km_Details_Merged<-N_SP_10km_Details_Merged %>% 
        mutate(EFFORT_DISTANCE_PREVIOUS=replace(EFFORT_DISTANCE_PREVIOUS, 
                                                is.na(EFFORT_DISTANCE_PREVIOUS), -9999))
      
      N_SP_10km_Details_Merged[is.na(N_SP_10km_Details_Merged)]<-0
      
      N_SP_10km_Details_Merged<-N_SP_10km_Details_Merged %>% 
        mutate(EFFORT_DISTANCE_CURRENT=replace(EFFORT_DISTANCE_CURRENT, 
                                               EFFORT_DISTANCE_CURRENT==-9999, NA))
      N_SP_10km_Details_Merged<-N_SP_10km_Details_Merged %>% 
        mutate(EFFORT_DISTANCE_PREVIOUS=replace(EFFORT_DISTANCE_PREVIOUS, 
                                                EFFORT_DISTANCE_PREVIOUS==-9999, NA))
      N_SP_10km_Details_Merged$YEAR<-y
      N_SP_10km_Details_Merged_ALL[[sprintf("%s-%s-%s-%d", 
                                           item$COUNTRY, item$STATE, item$COUNTY, y)]]<-N_SP_10km_Details_Merged
      
    }
  }
  N_SP_Merged_ALL<-rbindlist(N_SP_Merged_ALL)
  N_SP_2.5km_Merged_ALL<-rbindlist(N_SP_2.5km_Merged_ALL)
  N_SP_5km_Merged_ALL<-rbindlist(N_SP_5km_Merged_ALL)
  N_SP_10km_Merged_ALL<-rbindlist(N_SP_10km_Merged_ALL)
  
  N_OBSERVER_Merged_ALL<-rbindlist(N_OBSERVER_Merged_ALL)
  N_OBSERVER_2.5km_Merged_ALL<-rbindlist(N_OBSERVER_2.5km_Merged_ALL)
  N_OBSERVER_5km_Merged_ALL<-rbindlist(N_OBSERVER_5km_Merged_ALL)
  N_OBSERVER_10km_Merged_ALL<-rbindlist(N_OBSERVER_10km_Merged_ALL)
  
  N_SP_Details_Merged_ALL<-rbindlist(N_SP_Details_Merged_ALL)
  N_SP_2.5km_Details_Merged_ALL<-rbindlist(N_SP_2.5km_Details_Merged_ALL)
  N_SP_5km_Details_Merged_ALL<-rbindlist(N_SP_5km_Details_Merged_ALL)
  N_SP_10km_Details_Merged_ALL<-rbindlist(N_SP_10km_Details_Merged_ALL)
  
  saveRDS(N_SP_Merged_ALL, "../../Data_eBird_2020/N_SP/N_SP_Merged_ALL.rda")
  saveRDS(N_SP_2.5km_Merged_ALL, "../../Data_eBird_2020/N_SP/N_SP_2.5km_Merged_ALL.rda")
  saveRDS(N_SP_5km_Merged_ALL, "../../Data_eBird_2020/N_SP/N_SP_5km_Merged_ALL.rda")
  saveRDS(N_SP_10km_Merged_ALL, "../../Data_eBird_2020/N_SP/N_SP_10km_Merged_ALL.rda")
  
  saveRDS(N_OBSERVER_Merged_ALL, "../../Data_eBird_2020/N_SP/N_OBSERVER_Merged_ALL.rda")
  saveRDS(N_OBSERVER_2.5km_Merged_ALL, "../../Data_eBird_2020/N_SP/N_OBSERVER_2.5km_Merged_ALL.rda")
  saveRDS(N_OBSERVER_5km_Merged_ALL, "../../Data_eBird_2020/N_SP/N_OBSERVER_5km_Merged_ALL.rda")
  saveRDS(N_OBSERVER_10km_Merged_ALL, "../../Data_eBird_2020/N_SP/N_OBSERVER_10km_Merged_ALL.rda")
  
  saveRDS(N_SP_Details_Merged_ALL, "../../Data_eBird_2020/N_SP/N_SP_Details_Merged_ALL.rda")
  saveRDS(N_SP_2.5km_Details_Merged_ALL, "../../Data_eBird_2020/N_SP/N_SP_2.5km_Details_Merged_ALL.rda")
  saveRDS(N_SP_5km_Details_Merged_ALL, "../../Data_eBird_2020/N_SP/N_SP_5km_Details_Merged_ALL.rda")
  saveRDS(N_SP_10km_Details_Merged_ALL, "../../Data_eBird_2020/N_SP/N_SP_10km_Details_Merged_ALL.rda")
}


if (F){
  
  N_SP_2.5km_Merged_ALL<-readRDS("../../Data_eBird_2020/N_SP/N_SP_2.5km_Merged_ALL.rda")
  
  N_SP_2.5km_Details_Merged_ALL<-readRDS("../../Data_eBird_2020/N_SP/N_SP_2.5km_Details_Merged_ALL.rda")
  
  mask<-raster("../../Data_eBird_2020/mask_2.5km.tif")
  mask_p<-data.frame(rasterToPoints(mask))
  colnames(mask_p)[3]<-"INDEX_2500m"
  mask_p<-data.table(mask_p)
  
  setkey(N_SP_2.5km_Merged_ALL, INDEX_2500m)
  setkey(N_SP_2.5km_Details_Merged_ALL, INDEX_2500m)
  setkey(mask_p, INDEX_2500m)
  
  
  N_SP_2.5km_Merged_ALL_With_XY<-N_SP_2.5km_Merged_ALL[mask_p, on="INDEX_2500m", nomatch=0]
  N_SP_2.5km_Details_Merged_ALL_With_XY<-N_SP_2.5km_Details_Merged_ALL[mask_p, 
                                                                       on="INDEX_2500m", nomatch=0,
                                                                       allow.cartesian=TRUE]
  dim(N_SP_2.5km_Details_Merged_ALL)
  dim(N_SP_2.5km_Details_Merged_ALL_With_XY)
  
  saveRDS(N_SP_2.5km_Merged_ALL_With_XY, "../../Data_eBird_2020/N_SP/N_SP_2.5km_Merged_ALL_With_XY.rda")
  saveRDS(N_SP_2.5km_Details_Merged_ALL_With_XY, "../../Data_eBird_2020/N_SP/N_SP_2.5km_Details_Merged_ALL_With_XY.rda")
  
  N_OBSERVER_2.5km_Merged_ALL<-readRDS("../../Data_eBird_2020/N_SP/N_OBSERVER_2.5km_Merged_ALL.rda")
  
  setkey(N_OBSERVER_2.5km_Merged_ALL, INDEX_2500m)
  N_OBSERVER_2.5km_Merged_ALL_With_XY<-N_OBSERVER_2.5km_Merged_ALL[mask_p, on="INDEX_2500m", nomatch=0]
  saveRDS(N_OBSERVER_2.5km_Merged_ALL_With_XY, "../../Data_eBird_2020/N_SP/N_OBSERVER_2.5km_Merged_ALL_With_XY.rda")
  
  
  N_SP_5km_Merged_ALL<-readRDS("../../Data_eBird_2020/N_SP/N_SP_5km_Merged_ALL.rda")
  N_SP_5km_Details_Merged_ALL<-readRDS("../../Data_eBird_2020/N_SP/N_SP_5km_Details_Merged_ALL.rda")
  
  mask<-raster("../../Data_eBird_2020/mask_5km.tif")
  mask_p<-data.frame(rasterToPoints(mask))
  colnames(mask_p)[3]<-"INDEX_5km"
  mask_p<-data.table(mask_p)
  
  
  setkey(N_SP_5km_Merged_ALL, INDEX_5km)
  setkey(N_SP_5km_Details_Merged_ALL, INDEX_5km)
  setkey(mask_p, INDEX_5km)
  
  N_SP_5km_Merged_ALL_With_XY<-N_SP_5km_Merged_ALL[mask_p, on="INDEX_5km", nomatch=0]
  N_SP_5km_Details_Merged_ALL_With_XY<-N_SP_5km_Details_Merged_ALL[mask_p, 
                                                                   on="INDEX_5km", nomatch=0,
                                                                   allow.cartesian=TRUE]
  
  saveRDS(N_SP_5km_Merged_ALL_With_XY, "../../Data_eBird_2020/N_SP/N_SP_5km_Merged_ALL_With_XY.rda")
  saveRDS(N_SP_5km_Details_Merged_ALL_With_XY, "../../Data_eBird_2020/N_SP/N_SP_5km_Details_Merged_ALL_With_XY.rda")
  N_OBSERVER_5km_Merged_ALL<-readRDS("../../Data_eBird_2020/N_SP/N_OBSERVER_5km_Merged_ALL.rda")
  setkey(N_OBSERVER_5km_Merged_ALL, INDEX_5km)
  N_OBSERVER_5km_Merged_ALL_With_XY<-N_OBSERVER_5km_Merged_ALL[mask_p, on="INDEX_5km", nomatch=0]
  saveRDS(N_OBSERVER_5km_Merged_ALL_With_XY, "../../Data_eBird_2020/N_OBSERVER_5km_Merged_ALL_With_XY.rda")
  
  
  N_SP_10km_Merged_ALL<-readRDS("../../Data_eBird_2020/N_SP/N_SP_10km_Merged_ALL.rda")
  N_SP_10km_Details_Merged_ALL<-readRDS("../../Data_eBird_2020/N_SP/N_SP_10km_Details_Merged_ALL.rda")
  
  mask<-raster("../../Data_eBird_2020/mask_10km.tif")
  mask_p<-data.frame(rasterToPoints(mask))
  colnames(mask_p)[3]<-"INDEX_10km"
  mask_p<-data.table(mask_p)
  
  
  setkey(N_SP_10km_Merged_ALL, INDEX_10km)
  setkey(N_SP_10km_Details_Merged_ALL, INDEX_10km)
  setkey(mask_p, INDEX_10km)
  
  N_SP_10km_Merged_ALL_With_XY<-N_SP_10km_Merged_ALL[mask_p, on="INDEX_10km", nomatch=0]
  N_SP_10km_Details_Merged_ALL_With_XY<-N_SP_10km_Details_Merged_ALL[mask_p, 
                                                                     on="INDEX_10km", nomatch=0,
                                                                     allow.cartesian=TRUE]
  
  saveRDS(N_SP_10km_Merged_ALL_With_XY, "../../Data_eBird_2020/N_SP/N_SP_10km_Merged_ALL_With_XY.rda")
  saveRDS(N_SP_10km_Details_Merged_ALL_With_XY, "../../Data_eBird_2020/N_SP/N_SP_10km_Details_Merged_ALL_With_XY.rda")
  N_OBSERVER_10km_Merged_ALL<-readRDS("../../Data_eBird_2020/N_SP/N_OBSERVER_10km_Merged_ALL.rda")
  setkey(N_OBSERVER_10km_Merged_ALL, INDEX_10km)
  N_OBSERVER_10km_Merged_ALL_With_XY<-N_OBSERVER_10km_Merged_ALL[mask_p, on="INDEX_10km", nomatch=0]
  saveRDS(N_OBSERVER_10km_Merged_ALL_With_XY, "../../Data_eBird_2020/N_OBSERVER_10km_Merged_ALL_With_XY.rda")
  
  
  View(tail(N_SP_2.5km_Merged_ALL[INDEX_2500m>0], 1000))
  View(tail(N_SP_2.5km_Merged_ALL_With_XY[INDEX_2500m>0], 1000))
}
year<-2020
i=1

N_SP_10km_Merged_ALL_With_XY<-readRDS("../../Data_eBird_2020/N_SP/N_SP_10km_Merged_ALL_With_XY.rda")
N_SP_10km_Merged_ALL_With_XY$N_SP_INCREASE_PERCENT<-
  (N_SP_10km_Merged_ALL_With_XY$N_SP_CURRENT-N_SP_10km_Merged_ALL_With_XY$N_EVENTS_PREVIOUS)/
  (N_SP_10km_Merged_ALL_With_XY$N_SP_CURRENT+N_SP_10km_Merged_ALL_With_XY$N_EVENTS_PREVIOUS)

for (year in c(2018:2021)){
  for (i in c(1:12)){
    print(i)
    item<-N_SP_10km_Merged_ALL_With_XY%>%dplyr::filter((MONTH==i)&(YEAR==year))
    p<-ggplot(item)+
      geom_tile(aes(x=x, y=y, fill=N_SP_INCREASE_PERCENT))+
      theme_bw()+
      ggtitle(sprintf("%d %s", year, month.abb[i]))+
      scale_fill_gradient2(
        low = "blue",
        mid = "green",
        high = "red",
        midpoint = 0
      )+
      xlim(range(N_SP_10km_Merged_ALL_With_XY$x))+
      ylim(range(N_SP_10km_Merged_ALL_With_XY$y))
    p
    ggsave(p, filename=sprintf("../../Data_eBird_2020/Figures/N_SP_Differ_by_Month/%d_%d.png", year, i))
  }
}

N_SP_Merged_Profile<-N_SP_10km_Merged_ALL_With_XY%>%dplyr::group_by(YEAR, MONTH)%>%
  dplyr::summarise(N_SP_INCREASE_PERCENT_MEAN=mean(N_SP_INCREASE_PERCENT),
                   N_SP_INCREASE_PERCENT_SD=sd(N_SP_INCREASE_PERCENT))

ggplot(N_SP_Merged_Profile)+
  geom_line(aes(x=MONTH, y=N_SP_INCREASE_PERCENT_MEAN, color=factor(YEAR)))

N_SP_Merged_Profile_Country<-N_SP_10km_Merged_ALL_With_XY%>%dplyr::group_by(YEAR, MONTH, COUNTRY)%>%
  dplyr::summarise(N_SP_INCREASE_PERCENT_MEAN=mean(N_SP_INCREASE_PERCENT),
                   N_SP_INCREASE_PERCENT_SD=sd(N_SP_INCREASE_PERCENT))
unique(N_SP_Merged_Profile_Country$COUNTRY)
ggplot(N_SP_Merged_Profile_Country%>%
         dplyr::filter((YEAR %in% c(2019:2021)) &
                         (COUNTRY %in% c("United States", "United Kingdom"))))+
  geom_line(aes(x=MONTH, y=N_SP_INCREASE_PERCENT_MEAN, color=factor(COUNTRY)))+
  facet_wrap(~YEAR, nrow = 3)

N_SP_Merged_Profile_States<-N_SP_10km_Merged_ALL_With_XY%>%
  dplyr::group_by(COUNTRY, STATE, YEAR, MONTH)%>%
  dplyr::summarise(N_SP_INCREASE_PERCENT_MEAN=mean(N_SP_INCREASE_PERCENT),
                   N_SP_INCREASE_PERCENT_SD=sd(N_SP_INCREASE_PERCENT))

ggplot(N_SP_Merged_Profile_States%>%
         dplyr::filter((COUNTRY=="United States")&(YEAR==2020)))+
  geom_line(aes(x=MONTH, y=N_SP_INCREASE_PERCENT_MEAN, 
                color=factor(STATE)))+
  facet_wrap(~YEAR)
