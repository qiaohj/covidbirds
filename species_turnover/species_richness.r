library(sf)
library(data.table)
setwd("/media/huijieqiao/WD12T/eBird/Script/ebird_migration")
countries<-st_read("/media/huijieqiao/WD12T/customs/Shape/TM_WORLD_BORDERS-0.3/TM_WORLD_BORDERS-0.3.shp")
#countries<-st_read("X:/customs/Shape/TM_WORLD_BORDERS-0.3/TM_WORLD_BORDERS-0.3.shp")
species_loss_gain<-readRDS("../../eBird_Pendemic_2021/Figures/LOSS_GAIN/species_loss_gain.rda")
species_loss_gain[!(COUNTRY %in% countries$NAME)]$COUNTRY
missing_countries_kay_ebird<-species_loss_gain[!(COUNTRY %in% countries$NAME)]$COUNTRY
missing_countries_kay_map<-c(NA, "Brunei Darussalam", "Ivoire", NA, NA, "Netherlands Antilles", 
                             "Democratic Republic of the Congo",
                             "Swaziland", NA, "Iran", NA,
                             "Lao People's Democratic Republic", "Libyan Arab Jamahiriya",
                             "Micronesia, Federated States of", "Republic of Moldova", "Burma",
                             "Korea, Democratic People's Republic of", "The former Yugoslav Republic of Macedonia",
                             "Palestine", "Reunion", "Sao Tome and Principe", "Saint Barthelemy","Saint Helena",
                             "Saint Martin", NA, "South Georgia South Sandwich Islands", "Korea, Republic of",
                             NA, "Syrian Arab Republic", "United Republic of Tanzania", "Holy See",
                             "Viet Nam", "British Virgin Islands", "United States Virgin Islands",
                             "Wallis and Futuna Islands")
missing_countries<-data.table(ebird=missing_countries_kay_ebird, map=missing_countries_kay_map)

countries[which(grepl("Brunei", countries$NAME)), ]$NAME

i=1
for (i in c(1:nrow(missing_countries))){
  item<-missing_countries[i]
  if (is.na(item$map)){
    next()
  }
  species_loss_gain[COUNTRY==item$ebird]$COUNTRY<-countries[which(grepl(item$map, countries$NAME)), ]$NAME
}

species_loss_gain[!(COUNTRY %in% countries$NAME)]$COUNTRY
country<-"Brunei Darussalam"
countries$N_GAIN_2020<--9999
countries$N_LOSS_2020<--9999
countries$N_ALL_SPECIES<--9999


for (country in unique(countries$NAME)){
  item<-species_loss_gain[COUNTRY==country]
  if (nrow(item)==0){
    next()
  }
  countries[which(countries$NAME==country), "N_GAIN_2020"]<-item$N_GAIN_2020
  countries[which(countries$NAME==country), "N_LOSS_2020"]<-item$N_LOSS_2020
  countries[which(countries$NAME==country), "N_ALL_SPECIES"]<-item$N_ALL_SPECIES
  
}
countries<-countries[which(countries$N_ALL_SPECIES>0),]
st_write(countries, "../../eBird_Pendemic_2021/Shape/Species_Loss_gain/Species_Loss_gain.shp",
         delete_layer=T)
                  