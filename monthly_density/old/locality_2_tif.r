library(ggplot2)
library(raster)
library(dplyr)
library(data.table)
setwd("/media/huijieqiao/WD12T/eBird/Script/ebird_migration")
reses<-c("2.5km", "5km", "10km")
res<-"10km"
for (res in reses[3]){
  if (T){
    print(paste("Reading data...... res=", res))
    N_SP_Merged_ALL_With_XY<-
      readRDS(sprintf("../../Data_eBird_2020/N_SP/N_SP_%s_Merged_ALL_With_XY.rda", res))
    N_OBSERVER_Merged_ALL_With_XY<-
      readRDS(sprintf("../../Data_eBird_2020/N_SP/N_OBSERVER_%s_Merged_ALL.rda", res))
    
    #N_SP_Details_Merged_ALL_With_XY<-
    #  readRDS(sprintf("../../Data_eBird_2020/N_SP/N_SP_%s_Details_Merged_ALL_With_XY.rda", res))
    
    mask<-raster(sprintf("../../Data_eBird_2020/mask_%s.tif", res))
    no_na<-!is.na(values(mask))
    mask_p<-data.table(rasterToPoints(mask))
    label<-sprintf("INDEX_%s", res)
    if (res=="2.5km"){
      label<-sprintf("INDEX_%s", "2500m")
    }
    colnames(mask_p)[3]<-label
    
  }
  
  yy<-2010
  mo<-1
  columns<-c(label, "MONTH", "YEAR")
  
  for (yy in c(2015:2020)){
    stacked_N_SP<-NULL
    stacked_N_EVENTS<-NULL
    stacked_N_OBSERVERS<-NULL
    stacked_N_OBSERVATIONS<-NULL
    r_names<-c()
    for (mo in c(1:12)){
      print(paste(yy, mo))
      
      if (yy==2010){
        item<-N_SP_Merged_ALL_With_XY[(YEAR==(yy+1))&(MONTH==mo)]
        
        item_se<-item%>%ungroup()%>%
          dplyr::group_by_at(vars(one_of(columns)))%>%
          dplyr::summarise(N_SP=mean(N_SP_PREVIOUS),
                           N_EVENTS=mean(N_EVENTS_PREVIOUS),
                           N_OBSERVATIONS=mean(N_OBSERVATIONS_PREVIOUS),
                           N_OBSERVERS=mean(N_OBSERVERS_PREVIOUS),
                           EFFORT_DISTANCE=mean(EFFORT_DISTANCE_PREVIOUS, na.rm=T))
      }else{
        item<-N_SP_Merged_ALL_With_XY[(YEAR==yy)&(MONTH==mo)]
        
        item_se<-item%>%ungroup()%>%
          dplyr::group_by_at(vars(one_of(columns)))%>%
          dplyr::summarise(N_SP=mean(N_SP_CURRENT),
                           N_EVENTS=mean(N_EVENTS_CURRENT),
                           N_OBSERVATIONS=mean(N_OBSERVATIONS_CURRENT),
                           N_OBSERVERS=mean(N_OBSERVERS_CURRENT),
                           EFFORT_DISTANCE=mean(EFFORT_DISTANCE_CURRENT, na.rm=T))
      }
      Sum_df<-left_join(mask_p, item_se, by=c(label))
      
      r<-mask
      values(r)[no_na]<-Sum_df$N_SP
      r_names<-c(r_names, sprintf("%s_%d_%d", "N_SP", yy, mo))
      if (is.null(stacked_N_SP)){
        stacked_N_SP<-r
      }else{
        stacked_N_SP<-stack(stacked_N_SP, r)
      }
      
      r<-mask
      values(r)[no_na]<-Sum_df$N_EVENTS
      r_names<-c(r_names, sprintf("%s_%d_%d", "N_EVENTS", yy, mo))
      if (is.null(stacked_N_EVENTS)){
        stacked_N_EVENTS<-r
      }else{
        stacked_N_EVENTS<-stack(stacked_N_EVENTS, r)
      }
      
      r<-mask
      values(r)[no_na]<-Sum_df$N_OBSERVATIONS
      r_names<-c(r_names, sprintf("%s_%d_%d", "N_OBSERVATIONS", yy, mo))
      if (is.null(stacked_N_OBSERVATIONS)){
        stacked_N_OBSERVATIONS<-r
      }else{
        stacked_N_OBSERVATIONS<-stack(stacked_N_OBSERVATIONS, r)
      }
      
      r<-mask
      values(r)[no_na]<-Sum_df$N_OBSERVERS
      r_names<-c(r_names, sprintf("%s_%d_%d", "N_OBSERVERS", yy, mo))
      if (is.null(stacked_N_OBSERVERS)){
        stacked_N_OBSERVERS<-r
      }else{
        stacked_N_OBSERVERS<-stack(stacked_N_OBSERVERS, r)
      }
    }
    print("writing raster.....")
    writeRaster(stacked_N_SP, sprintf("../../Data_eBird_2020/TIF/Overall/%s/%d_N_SP_%s.tif", res, yy, res),
                datatype="INT4S")
    writeRaster(stacked_N_EVENTS, sprintf("../../Data_eBird_2020/TIF/Overall/%s/%d_N_EVENTS_%s.tif", res, yy, res),
                datatype="INT4S")
    writeRaster(stacked_N_OBSERVERS, 
                sprintf("../../Data_eBird_2020/TIF/Overall/%s/%d_N_OBSERVERS_%s.tif", res, yy, res),
                datatype="INT4S")
    writeRaster(stacked_N_OBSERVATIONS, 
                sprintf("../../Data_eBird_2020/TIF/Overall/%s/%d_N_OBSERVATIONS_%s.tif", res, yy, res),
                datatype="INT4S")
  }
}


year<-2019
print(year)
r<-stack(sprintf("../../Data_eBird_2020/TIF/Overall/%s/%d.tif", "10km", year))
m=2
N_SP_ALL<-list()
for (m in c(1:12)){
  print(m)
  N_SP<-data.table(rasterToPoints(r[[(m-1)*3+1]]))
  colnames(N_SP)[3]<-"V"
  N_SP$MONTH<-m
  N_SP_ALL[[m]]<-N_SP
}
N_SP_ALL<-rbindlist(N_SP_ALL)
N_SP_ALL_se<-N_SP_ALL%>%dplyr::group_by(MONTH)%>%
  dplyr::summarize(mean_V=mean(V, na.rm=T),
                   sd_V=sd(V, na.rm=T))

ggplot(N_SP_ALL_se)+geom_line(aes(x=MONTH, y=mean_V))
N_SP_ALL_2020<-N_SP_ALL_2020%>%dplyr::group_by(MONTH)%>%
  dplyr::summarize(YEAR=2020,
                   mean_V_2020=mean(V, na.rm=T),
                   sd_V_2020=sd(V, na.rm=T))

N_SP_ALL_2019<-N_SP_ALL_2019%>%dplyr::group_by(MONTH)%>%
  dplyr::summarize(YEAR=2019,
                   mean_V_2019=mean(V, na.rm=T),
                   sd_V_2019=sd(V, na.rm=T))

N_SP_ALL<-inner_join(N_SP_ALL_2020, N_SP_ALL_2019, by="MONTH")
N_SP_ALL$DIFFER<-N_SP_ALL$mean_V_2020-N_SP_ALL$mean_V_2019
plot(N_SP_ALL$MONTH, N_SP_ALL$DIFFER, type="l")


View(head(N_SP_Details_Merged_ALL_With_XY[, c(1,2,3,4,5, 10, 11, 12, 13, 14, 15, 16)]))
