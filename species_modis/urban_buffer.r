library(rgdal)
library(raster)
library(sf)
setwd("/media/huijieqiao/WD12T/eBird/Script/ebird_migration")
urban_buffer<-readOGR("../../Data_eBird_2020/Shape/buffer", "buffer_cities")
mask<-raster("../../Data_eBird_2020/mask_500m.tif")
urban_buffer_sinu<-spTransform(urban_buffer, CRS(proj4string(mask)))
writeOGR(urban_buffer_sinu, "../../Data_eBird_2020/Shape/buffer",
         "buffer_cities_sinu", driver="ESRI Shapefile")

urban_buffer_sinu<-readOGR("../../Data_eBird_2020/Shape/buffer", "buffer_cities_sinu")

mask_crop<-crop(mask, extent(urban_buffer_sinu))
mask_mask<-mask(mask_crop, urban_buffer_sinu)
plot(mask_mask)
writeRaster(mask_mask, "../../Data_eBird_2020/mask_urban_buffer.tif", 
            datetype="INT4U", overwrite=T)
