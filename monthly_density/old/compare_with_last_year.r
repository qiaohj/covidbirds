library(ggplot2)
library(raster)
library(data.table)
library(DBI)
library(mgcv)
library(pastecs)

setwd("/media/huijieqiao/WD12T/eBird/Script/ebird_migration")
N_SP_10km_Merged_ALL_With_XY<-readRDS("../../Data_eBird_2020/N_SP/N_SP_10km_Merged_ALL_With_XY.rda")
N_SP_10km_Merged_ALL_With_XY$N_SP_INCREASE_PERCENT<-
  (N_SP_10km_Merged_ALL_With_XY$N_SP_CURRENT-N_SP_10km_Merged_ALL_With_XY$N_EVENTS_PREVIOUS)/
  (N_SP_10km_Merged_ALL_With_XY$N_SP_CURRENT+N_SP_10km_Merged_ALL_With_XY$N_EVENTS_PREVIOUS)

N_SP_BY_COUNTRY<-N_SP_10km_Merged_ALL_With_XY[, .(MEAN_N_SP_CURRENT=mean(N_SP_CURRENT),
                                                  SD_N_SP_CURRENT=sd(N_SP_CURRENT),
                                                  MEAN_N_EVENTS_CURRENT=mean(N_EVENTS_CURRENT),
                                                  SD_N_EVENTS_CURRENT=sd(N_EVENTS_CURRENT),
                                                  MEAN_N_OBSERVATIONS_CURRENT=mean(N_OBSERVATIONS_CURRENT),
                                                  SD_N_OBSERVATIONS_CURRENT=sd(N_OBSERVATIONS_CURRENT),
                                                  MEAN_N_SP_PREVIOUS=mean(N_SP_PREVIOUS),
                                                  SD_N_SP_PREVIOUS=sd(N_SP_PREVIOUS),
                                                  MEAN_N_EVENTS_PREVIOUS=mean(N_EVENTS_PREVIOUS),
                                                  SD_N_EVENTS_PREVIOUS=sd(N_EVENTS_PREVIOUS),
                                                  MEAN_N_OBSERVATIONS_PREVIOUS=mean(N_OBSERVATIONS_PREVIOUS),
                                                  SD_N_OBSERVATIONS_PREVIOUS=sd(N_OBSERVATIONS_PREVIOUS),
                                                  MEAN_N_SP_INCREASE_PERCENT=mean(N_SP_INCREASE_PERCENT),
                                                  SD_N_SP_INCREASE_PERCENT=sd(N_SP_INCREASE_PERCENT)),
                                              by=list(COUNTRY, YEAR, MONTH)]



