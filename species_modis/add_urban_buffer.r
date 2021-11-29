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
urban_buffer<-raster("../../Data_eBird_2020/mask_urban_buffer.tif")
if (T){
  sp_list<-sp_list[sample(nrow(sp_list), nrow(sp_list)),]
  for (i in c(1:nrow(sp_list))){
    sp<-sp_list[i,]
    print(paste(i, nrow(sp_list), sp$SCIENTIFIC_NAME))
    source<-sprintf("../../Data_eBird_2020/Tables/Species/%s.rda", sp$SCIENTIFIC_NAME)
    target<-sprintf("../../Data_eBird_2020/Tables/Species_With_Urban/%s.rda", sp$SCIENTIFIC_NAME)
    if (!file.exists(source)){
      next()
    }
    if (file.exists(target)){
      next()
    }
    saveRDS(NULL, target)
    records<-readRDS(source)
    if (is.null(records)){
      next()
    }
    values<-raster::extract(urban_buffer, records[, c("X", "Y")])
    records$URBAN_BUFFER<-ifelse(is.na(values), 0, 1)
    saveRDS(records, target)
  }
  
  
}
