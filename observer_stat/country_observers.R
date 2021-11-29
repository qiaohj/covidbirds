library(sf)
library(ggplot2)
library(data.table)
countries<-st_read("../../eBird_Pendemic_2021/Shape/Country_Observers/Country_Observers.shp")
View(countries[, c("ISO3", "NAME")])
target_countries<-readRDS("../../eBird_Pendemic_2021/Tables/target_countries.rda")

"Iran (Islamic Republic of)"
"Korea, Republic of"
"United Republic of Tanzania"


countries[which(countries$NAME=="Iran (Islamic Republic of)"), "NAME"]<-"Iran"
countries[which(countries$NAME=="Korea, Republic of"), "NAME"]<-"South Korea"
countries[which(countries$NAME=="United Republic of Tanzania"), "NAME"]<-"Tanzania"

unique(countries$ISO3)

target_countries[!(target_countries %in% countries$NAME)]

countries<-countries[which(countries$NAME %in% target_countries),]
plot(st_geometry(countries))

p<-ggplot(countries)+geom_point(aes(x=D_N_PREV, y=D_N), color="red")+
  geom_point(aes(x=I_N_PREV, y=I_N), color="blue")+
  scale_x_log10()+
  scale_y_log10()+
  geom_abline()+
  xlab("Number of observers before the pandemic")+
  ylab("Number of observers after the pandemic")+
  theme_bw()
p
ggsave(p, filename="../../eBird_Pendemic_2021/Figures/Observers/Observers_type_by_country.png", 
       width=5, height=4)

observers_details<-read.csv("../../eBird_Pendemic_2021/Figures/Observers/countries_observers_type_all.csv", stringsAsFactors = F)
observers_details<-observers_details[which(observers_details$SCHENGEN %in% c(target_countries, "Schengen")),]

observers_details$International_Proportion<-observers_details$International_N_OBSERVER/(observers_details$Domestic_N_OBSERVER+observers_details$International_N_OBSERVER)
observers_details$COUNTRY_TYPE<-ifelse(observers_details$International_Proportion>0.5, 
                                       1, 0)
table(observers_details$COUNTRY_TYPE)
table(observers_details[, c("SCHENGEN", "YEAR", "COUNTRY_TYPE")])



observers_details_item1<-observers_details[, c("SCHENGEN", "YEAR", "COUNTRY_TYPE", 
                                               "Domestic_OBSERVER_TYPE", 
                                               "Domestic_N_OBSERVER", "Domestic_N_OBSERVER_PREV", 
                                               "Domestic_DIFFER", "Domestic_DIFFER_RATIO")]
colnames(observers_details_item1)<-c("SCHENGEN", "YEAR", "COUNTRY_TYPE", "OBSERVER_TYPE", 
                                     "N_OBSERVER", "N_OBSERVER_PREV", 
                                     "DIFFER", "DIFFER_RATIO")
observers_details_item2<-observers_details[, c("SCHENGEN", "YEAR", "COUNTRY_TYPE", 
                                               "International_OBSERVER_TYPE", 
                                               "International_N_OBSERVER", "International_N_OBSERVER_PREV", 
                                               "International_DIFFER", "International_DIFFER_RATIO")]
colnames(observers_details_item2)<-c("SCHENGEN", "YEAR", "COUNTRY_TYPE", 
                                     "OBSERVER_TYPE", 
                                     "N_OBSERVER", "N_OBSERVER_PREV", 
                                     "DIFFER", "DIFFER_RATIO")
observers_details_all<-rbind(observers_details_item1, observers_details_item2)
observers_details_all$group<-paste(observers_details_all$SCHENGEN, observers_details_all$OBSERVER_TYPE)

p<-ggplot(observers_details_all)+
  geom_line(aes(x=YEAR, y=N_OBSERVER, group=group, color=OBSERVER_TYPE))+
  geom_point(aes(x=YEAR, y=N_OBSERVER, color=OBSERVER_TYPE))+
  scale_y_log10()+
  theme_bw()

p

observers_details_all$Pandemic<-ifelse(observers_details_all$YEAR>2019, "After pandemic", "Before pandemic")
observers_details_all_se<-data.table(observers_details_all)
observers_details_all_se<-observers_details_all_se[, .(COUNTRY_TYPE=mean(COUNTRY_TYPE)),
                                                   by=list(SCHENGEN, Pandemic)]

country_types<-list()
for (country in unique(observers_details_all_se$SCHENGEN)){
  item<-observers_details_all_se[SCHENGEN==country]
  COUNTRY_TYPE_1<-"Mixed"
  COUNTRY_TYPE_2<-"Mixed"
  if (item[Pandemic=="Before pandemic"]$COUNTRY_TYPE==1){
    COUNTRY_TYPE_1<-"International"  
  }
  if (item[Pandemic=="Before pandemic"]$COUNTRY_TYPE==0){
    COUNTRY_TYPE_1<-"Domestic"  
  }
  if (item[Pandemic=="After pandemic"]$COUNTRY_TYPE==1){
    COUNTRY_TYPE_2<-"International"  
  }
  if (item[Pandemic=="After pandemic"]$COUNTRY_TYPE==0){
    COUNTRY_TYPE_2<-"Domestic"  
  }
  COUNTRY_TYPE<-sprintf("%s - %s", COUNTRY_TYPE_1, COUNTRY_TYPE_2)
  
  country_types[[country]]<-data.frame(SCHENGEN=country, COUNTRY_TYPE_STR=COUNTRY_TYPE)
}

country_types<-rbindlist(country_types)

observers_details_all<-merge(observers_details_all, country_types, by="SCHENGEN")
observers_details_all_se<-data.table(observers_details_all)
observers_details_all_se<-observers_details_all_se[, .(N_OBSERVER=mean(N_OBSERVER),
                                                       SD_N_OBSERVER=sd(N_OBSERVER)),
                                                   by=list(OBSERVER_TYPE, YEAR, COUNTRY_TYPE_STR)]
p<-ggplot(observers_details_all_se[YEAR<=2020])+
  geom_line(aes(x=YEAR, y=N_OBSERVER, linetype=COUNTRY_TYPE_STR, color=OBSERVER_TYPE))+
  geom_point(aes(x=YEAR, y=N_OBSERVER, shape=COUNTRY_TYPE_STR, color=OBSERVER_TYPE))+
  scale_y_log10()+
  theme_bw()

p

Schengen<-c("Germany", "Austria", "Belgium", "Czech Republic", "Denmark", 
            "Estonia", "Finland", "France", "Greece", "Hungary", "Iceland", "Italy",
            "Latvia", "Liechtenstein", "Lithuania", "Luxembourg", "Malta", 
            "Netherlands", "Norway", "Poland", "Portugal", "Slovakia", 
            "Slovenia", "Spain", "Sweden", "Switzerland", "United Kingdom")
GDPS<-read.csv("../../eBird_Pendemic_2021/Tables/COVID-19/GDPS.csv", stringsAsFactors = F)
gdpPerCapita<-mean(GDPS[which(GDPS$country %in% Schengen), "gdpPerCapita"])

observers_details_all
observers_details_all2<-merge(unique(observers_details_all[, c("SCHENGEN", "COUNTRY_TYPE_STR")]), 
                                     GDPS, by.x="SCHENGEN", by.y="country",all.x = T)

observers_details_all2[is.na(observers_details_all2$gdpPerCapita), "gdpPerCapita"]<-gdpPerCapita
table(observers_details_all2$COUNTRY_TYPE_STR)
write.csv(observers_details_all2, "../../eBird_Pendemic_2021/Figures/Observers/Country_type_GDP.csv")
p<-ggplot(observers_details_all2)+geom_violin(aes(x=COUNTRY_TYPE_STR, y=gdpPerCapita))+
  geom_point(aes(x=COUNTRY_TYPE_STR, y=gdpPerCapita))+
  geom_text(aes(x=COUNTRY_TYPE_STR, y=gdpPerCapita, label=SCHENGEN))+
  scale_y_log10()+
  xlab("Country type")+
  theme_bw()
p
ggsave(p, 
       filename="../../eBird_Pendemic_2021/Figures/Observers/Country_type_GDP_2021.png",
       width=9, height=5)

