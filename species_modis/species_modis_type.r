setwd("/media/huijieqiao/WD12T/eBird/Script/ebird_migration")
library("RMySQL")
library("data.table")
library("rgdal")
library("raster")
library("sp")
setDTthreads(1)
print(sprintf("%d CPUs are using", getDTthreads()))

print("Reading species")

sp_list<-readRDS("../../Data_eBird_2020/Tables/species_list.rda")

crop_ids<-c(7, 8)
build_up_ids<-c(9)
pristine_ids<-c(1:6, 10, 11)


if (F){
  #sp_list$MODIS_TYPE<-"000"
  i=281
  sp_list$PRISTINE<-0
  sp_list$PRISTINE_P<-0
  sp_list$BUILD_UP<-0
  sp_list$BUILD_UP_P<-0
  sp_list$CROPLAND<-0
  sp_list$CROPLAND_P<-0
  sp_list$URBAN<-0
  sp_list$URBAN_P<-0
  sp_list$RURAL<-0
  sp_list$RURAL_P<-0
  sp_list$URBAN_TYPE<-"00"
  sp_list$N_RECORDS_BEFORE_2019<-0
  
  sp_list$MODIS_TYPE_2020<-"000"
  sp_list$PRISTINE_2020<-0
  sp_list$PRISTINE_P_2020<-0
  sp_list$BUILD_UP_2020<-0
  sp_list$BUILD_UP_P_2020<-0
  sp_list$CROPLAND_2020<-0
  sp_list$CROPLAND_P_2020<-0
  sp_list$URBAN_2020<-0
  sp_list$URBAN_P_2020<-0
  sp_list$RURAL_2020<-0
  sp_list$RURAL_P_2020<-0
  sp_list$URBAN_TYPE_2020<-"00"
  sp_list$N_RECORDS_2020<-0
  
  for (i in c(1:nrow(sp_list))){
    sp<-sp_list[i,]
    if (sp$MODIS_TYPE!="000"){
      next()
    }
    print(paste(i, nrow(sp_list), sp$SCIENTIFIC_NAME))
    target<-sprintf("../../Data_eBird_2020/Tables/Species_With_Urban/%s.rda", sp$SCIENTIFIC_NAME)
    if (!file.exists(target)){
      next()
    }
    records_raw<-readRDS(target)
    if (is.null(records_raw)){
      next()
    }
    records<-records_raw[YEAR<2020]
    if (nrow(records)>0){
      records$MODIS_TYPE<-"UNKNOWN"
      records[MODIS %in% crop_ids]$MODIS_TYPE<-"CROPLAND"
      records[MODIS %in% build_up_ids]$MODIS_TYPE<-"BUILD_UP"
      records[MODIS %in% pristine_ids]$MODIS_TYPE<-"PRISTINE"
      
      count_t<-records[, .(COUNT=.N), by=list(MODIS_TYPE)]
      sum_t<-sum(count_t[MODIS_TYPE!="UNKNOWN"]$COUNT)
      if (sum_t!=0){
        PRISTINE<-count_t[MODIS_TYPE=="PRISTINE"]$COUNT
        PRISTINE<-ifelse(length(PRISTINE)==1, PRISTINE, 0)
        PRISTINE_P<-PRISTINE/sum_t
        BUILD_UP<-count_t[MODIS_TYPE=="BUILD_UP"]$COUNT
        BUILD_UP<-ifelse(length(BUILD_UP)==1, BUILD_UP, 0)
        BUILD_UP_P<-BUILD_UP/sum_t
        CROPLAND<-count_t[MODIS_TYPE=="CROPLAND"]$COUNT
        CROPLAND<-ifelse(length(CROPLAND)==1, CROPLAND, 0)
        CROPLAND_P<-CROPLAND/sum_t
        
        MODIS_TYPE<-"000";
        if (PRISTINE_P>(1/3)){
          MODIS_TYPE<-sprintf("1%s", substr(MODIS_TYPE, 2, 3))
        }
        if (CROPLAND_P>(1/3)){
          MODIS_TYPE<-sprintf("%s1%s", substr(MODIS_TYPE, 1, 1), substr(MODIS_TYPE, 3, 3))
        }
        if (BUILD_UP_P>(1/3)){
          MODIS_TYPE<-sprintf("%s1", substr(MODIS_TYPE, 1, 2))
        }
        sp_list[i,]$N_RECORDS_BEFORE_2019<-nrow(records)
        sp_list[i,]$PRISTINE<-PRISTINE
        sp_list[i,]$PRISTINE_P<-PRISTINE_P
        sp_list[i,]$BUILD_UP<-BUILD_UP
        sp_list[i,]$BUILD_UP_P<-BUILD_UP_P
        sp_list[i,]$CROPLAND<-CROPLAND
        sp_list[i,]$CROPLAND_P<-CROPLAND_P
        
        sp_list[i,]$MODIS_TYPE<-MODIS_TYPE
      }
      
      
      records$URBAN_TYPE<-"UNKNOWN"
      records[URBAN_BUFFER==0]$URBAN_TYPE<-"RURAL"
      records[URBAN_BUFFER==1]$URBAN_TYPE<-"URBAN"
      
      count_t<-records[, .(COUNT=.N), by=list(URBAN_TYPE)]
      sum_t<-sum(count_t[URBAN_TYPE!="UNKNOWN"]$COUNT)
      if (sum_t!=0){
        URBAN<-count_t[URBAN_TYPE=="URBAN"]$COUNT
        URBAN<-ifelse(length(URBAN)==1, URBAN, 0)
        URBAN_P<-URBAN/sum_t
        RURAL<-count_t[URBAN_TYPE=="RURAL"]$COUNT
        RURAL<-ifelse(length(RURAL)==1, RURAL, 0)
        RURAL_P<-RURAL/sum_t
        sp_list[i,]$URBAN<-URBAN
        sp_list[i,]$URBAN_P<-URBAN_P
        sp_list[i,]$RURAL<-RURAL
        sp_list[i,]$RURAL_P<-RURAL_P
        
        URBAN_TYPE<-"11";
        if (URBAN_P>(2/3)){
          URBAN_TYPE<-"10"
        }
        if (URBAN_P<(1/3)){
          URBAN_TYPE<-"01"
        }
        sp_list[i,]$URBAN_TYPE<-URBAN_TYPE
      }
    }
    
    records<-records_raw[YEAR==2020]
    if (nrow(records)>0){
      records$MODIS_TYPE_2020<-"UNKNOWN"
      records[MODIS %in% crop_ids]$MODIS_TYPE_2020<-"CROPLAND"
      records[MODIS %in% build_up_ids]$MODIS_TYPE_2020<-"BUILD_UP"
      records[MODIS %in% pristine_ids]$MODIS_TYPE_2020<-"PRISTINE"
      
      count_t<-records[, .(COUNT=.N), by=list(MODIS_TYPE_2020)]
      sum_t<-sum(count_t[MODIS_TYPE_2020!="UNKNOWN"]$COUNT)
      if (sum_t!=0){
        PRISTINE<-count_t[MODIS_TYPE_2020=="PRISTINE"]$COUNT
        PRISTINE<-ifelse(length(PRISTINE)==1, PRISTINE, 0)
        PRISTINE_P<-PRISTINE/sum_t
        BUILD_UP<-count_t[MODIS_TYPE_2020=="BUILD_UP"]$COUNT
        BUILD_UP<-ifelse(length(BUILD_UP)==1, BUILD_UP, 0)
        BUILD_UP_P<-BUILD_UP/sum_t
        CROPLAND<-count_t[MODIS_TYPE_2020=="CROPLAND"]$COUNT
        CROPLAND<-ifelse(length(CROPLAND)==1, CROPLAND, 0)
        CROPLAND_P<-CROPLAND/sum_t
        
        MODIS_TYPE<-"000";
        if (PRISTINE_P>(1/3)){
          MODIS_TYPE<-sprintf("1%s", substr(MODIS_TYPE, 2, 3))
        }
        if (CROPLAND_P>(1/3)){
          MODIS_TYPE<-sprintf("%s1%s", substr(MODIS_TYPE, 1, 1), substr(MODIS_TYPE, 3, 3))
        }
        if (BUILD_UP_P>(1/3)){
          MODIS_TYPE<-sprintf("%s1", substr(MODIS_TYPE, 1, 2))
        }
        sp_list[i,]$N_RECORDS_2020<-nrow(records)
        sp_list[i,]$PRISTINE_2020<-PRISTINE
        sp_list[i,]$PRISTINE_P_2020<-PRISTINE_P
        sp_list[i,]$BUILD_UP_2020<-BUILD_UP
        sp_list[i,]$BUILD_UP_P_2020<-BUILD_UP_P
        sp_list[i,]$CROPLAND_2020<-CROPLAND
        sp_list[i,]$CROPLAND_P_2020<-CROPLAND_P
        
        sp_list[i,]$MODIS_TYPE_2020<-MODIS_TYPE
      }
      
      
      records$URBAN_TYPE_2020<-"UNKNOWN"
      records[URBAN_BUFFER==0]$URBAN_TYPE_2020<-"RURAL"
      records[URBAN_BUFFER==1]$URBAN_TYPE_2020<-"URBAN"
      
      count_t<-records[, .(COUNT=.N), by=list(URBAN_TYPE_2020)]
      sum_t<-sum(count_t[URBAN_TYPE_2020!="UNKNOWN"]$COUNT)
      
      if (sum_t!=0){
        URBAN<-count_t[URBAN_TYPE_2020=="URBAN"]$COUNT
        URBAN<-ifelse(length(URBAN)==1, URBAN, 0)
        URBAN_P<-URBAN/sum_t
        RURAL<-count_t[URBAN_TYPE_2020=="RURAL"]$COUNT
        RURAL<-ifelse(length(RURAL)==1, RURAL, 0)
        RURAL_P<-RURAL/sum_t
        sp_list[i,]$URBAN_2020<-URBAN
        sp_list[i,]$URBAN_P_2020<-URBAN_P
        sp_list[i,]$RURAL_2020<-RURAL
        sp_list[i,]$RURAL_P_2020<-RURAL_P
        
        URBAN_TYPE<-"11";
        if (URBAN_P>(2/3)){
          URBAN_TYPE<-"10"
        }
        if (URBAN_P<(1/3)){
          URBAN_TYPE<-"01"
        }
        sp_list[i,]$URBAN_TYPE_2020<-URBAN_TYPE
      }
    }
    
    
    
  }
  endanger<-read.csv("../../Data_eBird_2020/Tables/assessments.csv", 
                     head=T, sep=",", stringsAsFactors = F)
  
  sp_list<-merge(sp_list, endanger[, c("scientificName", "redlistCategory")], 
                 by.x="SCIENTIFIC_NAME", by.y="scientificName", all.x=T, all.y=F)
  sp_list$IS_ENDANGER<-ifelse(is.na(sp_list$redlistCategory), "COMMON", "ENDANGER")
  saveRDS(sp_list, "../../Data_eBird_2020/Tables/species_list.rda")
}
sp_list<-readRDS("../../Data_eBird_2020/Tables/species_list.rda")

unique(sp_list$MODIS_TYPE)
library(ggplot2)
sp_list<-data.table(sp_list)
sp_list<-sp_list[(MODIS_TYPE!="000")|(MODIS_TYPE_2020!="000")]
ggplot(sp_list)+geom_density(aes(x=URBAN_P, color=factor(IS_ENDANGER), linetype="2009-2019"))+
  geom_density(aes(x=URBAN_P_2020, color=factor(IS_ENDANGER), linetype="2020"))
  
write.csv(sp_list, "../../Data_eBird_2020/Tables/species_list.csv", row.names = F)
#"SCIENTIFIC_NAME"       "N_RECORDS"             "MODIS_TYPE"            "PRISTINE"             
#[5] "PRISTINE_P"            "BUILD_UP"              "BUILD_UP_P"            "CROPLAND"             
#[9] "CROPLAND_P"            "URBAN"                 "URBAN_P"               "RURAL"                
#[13] "RURAL_P"               "URBAN_TYPE"            "N_RECORDS_BEFORE_2019" "MODIS_TYPE_2020"      
#[17] "PRISTINE_2020"         "PRISTINE_P_2020"       "BUILD_UP_2020"         "BUILD_UP_P_2020"      
#[21] "CROPLAND_2020"         "CROPLAND_P_2020"       "URBAN_2020"            "URBAN_P_2020"         
#[25] "RURAL_2020"            "RURAL_P_2020"          "URBAN_TYPE_2020"       "N_RECORDS_2020"       
#[29] "redlistCategory"       "IS_ENDANGER"          
item<-sp_list[,c("SCIENTIFIC_NAME", "IS_ENDANGER", "URBAN_P", 
                 "PRISTINE_P", "BUILD_UP_P", "CROPLAND_P", 
                 "MODIS_TYPE", "URBAN_TYPE")]
item2<-sp_list[,c("SCIENTIFIC_NAME", "IS_ENDANGER", "URBAN_P_2020", 
                  "PRISTINE_P_2020", "BUILD_UP_P_2020", "CROPLAND_P_2020",
                  "MODIS_TYPE_2020", "URBAN_TYPE_2020")]
item$YEAR<-"BEFORE 2020"
item2$YEAR<-"2020"
colnames(item2)[c(3, 4, 5, 6, 7, 8)]<-c("URBAN_P", "PRISTINE_P", "BUILD_UP_P", "CROPLAND_P",
                                        "MODIS_TYPE", "URBAN_TYPE")
df<-rbind(item, item2)
ggplot(df)+geom_histogram(aes(x=URBAN_P, fill=factor(YEAR)), 
                               position="dodge", bins=20)+
  facet_wrap(~IS_ENDANGER, scale="free")
  

ggplot(df)+geom_histogram(aes(x=PRISTINE_P, fill=factor(YEAR)), 
                          position="dodge", bins=20)+
  facet_wrap(~IS_ENDANGER, scale="free")

ggplot(df)+geom_histogram(aes(x=BUILD_UP_P, fill=factor(YEAR)), 
                          position="dodge", bins=20)+
  facet_wrap(~IS_ENDANGER, scale="free")

ggplot(df)+geom_histogram(aes(x=CROPLAND_P, fill=factor(YEAR)), 
                          position="dodge", bins=20)+
  facet_wrap(~IS_ENDANGER, scale="free")
library(Hmisc)
df$PRISTINE_CUT<-cut2(df$PRISTINE_P, cuts=seq(0, 1, 0.1))
df$URBAN_CUT<-cut2(df$URBAN_P, cuts=seq(0, 1, 0.1))

df_se<-df%>%dplyr::group_by(IS_ENDANGER, PRISTINE_CUT, URBAN_CUT, YEAR)%>%
  dplyr::summarise(mean_URBAN_P=mean(URBAN_P),
                   sd_URBAN_P=sd(URBAN_P),
                   mean_PRISTINE_P=mean(PRISTINE_P),
                   sd_PRISTINE_P=sd(PRISTINE_P),
                   mean_BUILD_UP_P=mean(BUILD_UP_P),
                   sd_BUILD_UP_P=sd(BUILD_UP_P),
                   mean_CROPLAND_P=mean(CROPLAND_P),
                   sd_CROPLAND_P=sd(CROPLAND_P))
df_se[is.na(df_se)]<-0
#df_se$LABEL<-paste(df_se$MODIS_TYPE, df_se$URBAN_TYPE)
ggplot(df_se)+geom_point(aes(x=mean_URBAN_P, y=mean_PRISTINE_P,
                             color=factor(YEAR)), size=2)+
  geom_errorbar(aes(x=mean_URBAN_P, ymin=mean_PRISTINE_P-sd_PRISTINE_P, 
                    ymax=mean_PRISTINE_P+sd_PRISTINE_P, color=factor(YEAR)))+
  geom_errorbarh(aes(xmin=mean_URBAN_P-sd_URBAN_P, xmax=mean_URBAN_P+sd_URBAN_P, 
                    y=mean_PRISTINE_P, color=factor(YEAR)))+
  facet_wrap(~IS_ENDANGER, scale="free")


