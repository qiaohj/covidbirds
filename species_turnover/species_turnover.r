library(ggplot2)
library(raster)
library(data.table)
library(fpp2)
library(tsoutliers)
library(scales)
library(ggpubr)
if (F){
  sp_list_country_list<-readRDS("../../eBird_Pendemic_2021/Objects/N_SP_Observation/sp_list_country_list.rda")
  sp_list_country_list<-sp_list_country_list[!(grepl("/", SCIENTIFIC_NAME))]
  sp_list_country_list<-sp_list_country_list[!(grepl("sp\\.", SCIENTIFIC_NAME))]
  sp_list_country_list<-sp_list_country_list[!(grepl(" x ", SCIENTIFIC_NAME))]
  sp_list_country_list<-sp_list_country_list[!(grepl("\\(", SCIENTIFIC_NAME))]
  
  
  countries<-unique(sp_list_country_list$COUNTRY)
  country<-"United States"
  species_details<-list()
  species_loss_gain<-list()
  for (country in countries){
    item<-sp_list_country_list[COUNTRY==country]
    
    item<-item[YEAR>=2015]
    if (nrow(item)==0){
      next()
    }
    print(country)
    
    item_se<-item[, .(N_EVENTS=sum(N_EVENTS),
                      N_OBSERVATION=sum(N_OBSERVATION),
                      N_OBSERVERS=sum(N_OBSERVERS)),
                  by=list(SCIENTIFIC_NAME)]
    sp_list_2018_2019<-unique(item[(YEAR==2018)|((YEAR==2019)&(MONTH<=5))]$SCIENTIFIC_NAME)
    sp_list_2019<-unique(item[YEAR==2019]$SCIENTIFIC_NAME)
    sp_list_2020<-unique(item[YEAR==2020]$SCIENTIFIC_NAME)
    sp_list_2020_2021<-unique(item[YEAR>=2020]$SCIENTIFIC_NAME)
    
    GAIN_2020<-sp_list_2020[!(sp_list_2020 %in% sp_list_2019)]
    item_se$LOSS_GAIN_2020<-ifelse((item_se$SCIENTIFIC_NAME %in% GAIN_2020), "GAIN", "STABLE")
    LOSS_2020<-sp_list_2019[!(sp_list_2019 %in% sp_list_2020)]
    item_se[(SCIENTIFIC_NAME %in% LOSS_2020)]$LOSS_GAIN_2020<-"LOSS"
    
    N_GAIN_2020<-length(GAIN_2020)
    N_LOSS_2020<-length(LOSS_2020)
    
    
    GAIN_2020_2021<-sp_list_2020_2021[!(sp_list_2020_2021 %in% sp_list_2018_2019)]
    item_se$LOSS_GAIN_2020_2021<-ifelse((item_se$SCIENTIFIC_NAME %in% GAIN_2020_2021), "GAIN", "STABLE")
    LOSS_2020_2021<-sp_list_2018_2019[!(sp_list_2018_2019 %in% sp_list_2020_2021)]
    item_se[(SCIENTIFIC_NAME %in% LOSS_2020_2021)]$LOSS_GAIN_2020_2021<-"LOSS"
    table(item_se$LOSS_GAIN_2020_2021)
    
    N_GAIN_2020_2021<-length(GAIN_2020_2021)
    N_LOSS_2020_2021<-length(LOSS_2020_2021)
    
    N_2019<-length(sp_list_2019)
    N_2020<-length(sp_list_2020)
    N_2018_2019<-length(sp_list_2018_2019)
    N_2020_2021<-length(sp_list_2020_2021)
    N_ALL_SPECIES<-length(unique(item_se$SCIENTIFIC_NAME))
    
    if (T){
      cols<-c("SCIENTIFIC_NAME", "N_EVENTS", "LOSS_GAIN_2020_2021", "LOSS_GAIN_2020")
      item_df1<-item_se[, ..cols]
      colnames(item_df1)[2]<-"Value"
      item_df1$COUNTRY<-country
      item_df1$TYPE<-"Number of events"
      
      cols<-c("SCIENTIFIC_NAME", "N_OBSERVATION", "LOSS_GAIN_2020_2021", "LOSS_GAIN_2020")
      item_df2<-item_se[, ..cols]
      colnames(item_df2)[2]<-"Value"
      item_df2$COUNTRY<-country
      item_df2$TYPE<-"Number of observations"
      
      cols<-c("SCIENTIFIC_NAME", "N_OBSERVERS", "LOSS_GAIN_2020_2021", "LOSS_GAIN_2020")
      item_df3<-item_se[, ..cols]
      colnames(item_df3)[2]<-"Value"
      item_df3$COUNTRY<-country
      item_df3$TYPE<-"Number of observers"
      
      item_df<-rbindlist(list(item_df1, item_df2, item_df3))
      species_details[[country]]<-item_df
      if ((nrow(item_df[LOSS_GAIN_2020_2021!="STABLE"])>0)&
          (nrow(item_df[LOSS_GAIN_2020!="STABLE"])>0)){
        p1<-ggplot(item_df)+geom_density(aes(x=Value, color=LOSS_GAIN_2020_2021))+
          scale_x_log10()+ggtitle(country)+theme_bw()+facet_wrap(~TYPE, scale="free")+
          xlab("Loss and gain of 2020.1 - 2021.5 vs 2018.1 - 2019.5")
        p2<-ggplot(item_df[LOSS_GAIN_2020_2021!="STABLE"])+geom_histogram(aes(x=Value, fill=LOSS_GAIN_2020_2021), bins=10)+
          scale_x_log10()+ggtitle(country)+theme_bw()+facet_wrap(~TYPE, scale="free")+
          xlab("Loss and gain of 2020.1 - 2021.5 vs 2018.1 - 2019.5")
        
        p3<-ggplot(item_df)+geom_density(aes(x=Value, color=LOSS_GAIN_2020))+
          scale_x_log10()+ggtitle(country)+theme_bw()+facet_wrap(~TYPE, scale="free")+
          xlab("Loss and gain of 2020 vs 2019")
        p4<-ggplot(item_df[LOSS_GAIN_2020!="STABLE"])+geom_histogram(aes(x=Value, fill=LOSS_GAIN_2020), bins=10)+
          scale_x_log10()+ggtitle(country)+theme_bw()+facet_wrap(~TYPE, scale="free")+
          xlab("Loss and gain of 2020 vs 2019")
        
        p<-ggarrange(plotlist=list(p1, p2, p3, p4), nrow=4, ncol=1)
        p
        ggsave(p, filename=sprintf("../../eBird_Pendemic_2021/Figures/LOSS_GAIN/By_COUNTRY/%s.png", country), width=12, height=12)
      }
    }
    df_item<-data.table(COUNTRY=country,
                        N_2019=N_2019,
                        N_2020=N_2020,
                        N_2018_2019=N_2018_2019,
                        N_2020_2021=N_2020_2021,
                        N_GAIN_2020=N_GAIN_2020,
                        N_LOSS_2020=N_LOSS_2020,
                        N_GAIN_2020_2021=N_GAIN_2020_2021,
                        N_LOSS_2020_2021=N_LOSS_2020_2021,
                        N_ALL_SPECIES=N_ALL_SPECIES)
    species_loss_gain[[country]]<-df_item
  }
  species_loss_gain<-rbindlist(species_loss_gain)
  species_details<-rbindlist(species_details)
  saveRDS(species_loss_gain, "../../eBird_Pendemic_2021/Figures/LOSS_GAIN/species_loss_gain.rda")
  saveRDS(species_details, "../../eBird_Pendemic_2021/Figures/LOSS_GAIN/species_details.rda")
  write.csv(species_loss_gain, "../../eBird_Pendemic_2021/Figures/LOSS_GAIN/species_loss_gain.csv", row.names = F)
  write.csv(species_details, "../../eBird_Pendemic_2021/Figures/LOSS_GAIN/species_details.csv", row.names = F)
}

species_details<-readRDS("../../eBird_Pendemic_2021/Figures/LOSS_GAIN/species_details.rda")


species_loss_gain<-readRDS("../../eBird_Pendemic_2021/Figures/LOSS_GAIN/species_loss_gain.rda")

ggplot(species_loss_gain)+geom_boxplot(aes(x="LOSS", y=N_LOSS_2020))+
  geom_boxplot(aes(x="GAIN", y=N_GAIN_2020))

ggplot(species_details[LOSS_GAIN_2020!="STABLE"])+geom_boxplot(aes(x=LOSS_GAIN_2020, y=Value ))+
  scale_y_log10()

lockdown_data<-readRDS("../../eBird_Pendemic_2021/Tables/COVID-19/stay-at-home-covid1.rda")
lockdown_se<-lockdown_data[, .(N_LOCKDOWN=sum(stay_home_requirements),
                               N_DAYS=.N),
                           by=list(Entity)]
GDPS<-read.csv("../../eBird_Pendemic_2021/Tables/COVID-19/GDPS.csv", stringsAsFactors = F)

species_loss_gain<-merge(species_loss_gain, lockdown_se, by.x="COUNTRY", by.y="Entity")
species_loss_gain$LOCKDOWN_RESTRICT<-species_loss_gain$N_LOCKDOWN/species_loss_gain$N_DAYS
species_loss_gain<-merge(species_loss_gain, GDPS, by.x="COUNTRY", by.y="country")

ggplot(species_loss_gain)+
  geom_point(aes(x=gdpPerCapita, y=N_LOSS_2020, color=COUNTRY))+
  geom_smooth(aes(x=gdpPerCapita, y=N_LOSS_2020))+
  scale_y_log10()+
  theme_bw()+theme(legend.position="none")

ggplot(species_loss_gain)+
  geom_point(aes(x=gdpPerCapita, y=N_GAIN_2020, color="red"))+
  geom_smooth(aes(x=gdpPerCapita, y=N_GAIN_2020))+
  geom_point(aes(x=gdpPerCapita, y=N_LOSS_2020, color="blue"))+
  geom_smooth(aes(x=gdpPerCapita, y=N_LOSS_2020))+
  scale_y_log10()+
  theme_bw()+theme(legend.position="none")

ggplot(species_loss_gain)+
  geom_point(aes(x=LOCKDOWN_RESTRICT, y=N_GAIN_2020, color="red"))+
  geom_smooth(aes(x=LOCKDOWN_RESTRICT, y=N_GAIN_2020))+
  geom_point(aes(x=LOCKDOWN_RESTRICT, y=N_LOSS_2020, color="blue"))+
  geom_smooth(aes(x=LOCKDOWN_RESTRICT, y=N_LOSS_2020))+
  scale_y_log10()+
  theme_bw()+theme(legend.position="none")
