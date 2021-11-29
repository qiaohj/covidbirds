library(ggplot2)
library(data.table)
library(ggthemes)
setwd("/media/huijieqiao/WD12T/eBird/Script/ebird_migration")
observer_by_years<-readRDS("../../eBird_Pendemic_2021/Objects/Observers/Observers_Traits/observer_by_years.rda")

observer_by_years_sub<-observer_by_years[YEAR>=2015]

observer_N_Event<-observer_by_years_sub[, .(N_EVENT=sum(N_EVENT)), by=list(OBSERVER_ID)]

#observer_by_years_sub<-observer_by_years_sub[N_EVENT>=100]

#N Observers per year
observers_per_year<-observer_by_years_sub[, .(N_Observers=unique(length(OBSERVER_ID))),
                                          by=list(YEAR)]

p<-ggplot(observers_per_year, aes(x=YEAR, y=N_Observers))+geom_line()+
  geom_point()+theme_bw()
p
ggsave(p, filename="../../eBird_Pendemic_2021/Figures/Observers/observers_per_year.png", 
       width=5, height=3)

#Observers' N Event vs N Country
observer_by_years_sub$N_COUNTRY_LABEL<-as.character(observer_by_years_sub$N_COUNTRY)
observer_by_years_sub[N_COUNTRY>=10]$N_COUNTRY_LABEL<-">=10"
p<-ggplot(observer_by_years_sub)+geom_point(aes(x=N_COUNTRY,  y=N_EVENT, color=N_COUNTRY_LABEL))+
  facet_wrap(~YEAR)+theme_bw()+
  scale_x_log10()+scale_y_log10()
p
ggsave(p, filename="../../eBird_Pendemic_2021/Figures/Observers/n_event_by_country_by_year.png", 
       width=10, height=8)

#N Observers per year by N country
observers_per_year<-observer_by_years_sub[, .(N_Observers=unique(length(OBSERVER_ID))),
                                          by=list(YEAR, N_COUNTRY_LABEL)]
colors<-colorblind_pal()(8)
colors<-c(colors, "blue", "red")
colors<-c("#888888","#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499", 
          "#44AA99", "#999933", "#882255", "#661100", "#6699CC")
p1<-ggplot(observers_per_year, aes(x=YEAR, y=N_Observers, color=N_COUNTRY_LABEL))+
  geom_line()+geom_point()+
  geom_text(data=observers_per_year[YEAR==2021], 
            aes(x=2021.3, y=N_Observers, label=N_COUNTRY_LABEL))+
  scale_x_continuous(breaks=c(2015:2021), labels=c(2015:2021))+
  scale_y_log10()+
  scale_color_manual(values=colors)+
  ylab("Number of observers (log transformed)")+
  xlab("Year")+
  labs(color="Number of countries")+
  theme_bw()+
  theme(legend.position = "none")
p1
ggsave(p1, filename="../../eBird_Pendemic_2021/Figures/Observers/observers_per_year_by_N_country.png", 
       width=6, height=4)

observers_per_year$N_COUNTRY_LABEL_2<-observers_per_year$N_COUNTRY_LABEL
observers_per_year[N_COUNTRY_LABEL!="1"]$N_COUNTRY_LABEL_2<-">1"
observers_per_year<-data.table(observers_per_year)
observers_per_year_se<-observers_per_year[, .(N_Observers=sum(N_Observers)),
                                              by=list(YEAR, N_COUNTRY_LABEL_2)]
                                          ]
p2<-ggplot(observers_per_year_se, aes(x=YEAR, y=N_Observers, 
                                     color=N_COUNTRY_LABEL_2))+
  geom_line()+geom_point()+theme_bw()+scale_y_log10()
p2
ggsave(p2, filename="../../eBird_Pendemic_2021/Figures/Observers/observers_per_year_by_1_country.png", 
       width=5, height=3)
ggarrange(p1, p2)

observer_by_year_countries<-readRDS("../../eBird_Pendemic_2021/Objects/Observers/Observers_Traits/observer_by_year_countries.rda")
observer_by_year_countries_sub<-observer_by_year_countries[YEAR>=2015]

sel_cols<-c("YEAR", "OBSERVER_ID", "N_COUNTRY_LABEL")
observer_by_year_countries_with_label<-merge(observer_by_year_countries_sub, observer_by_years_sub[, ..sel_cols], 
                                             by=c("OBSERVER_ID", "YEAR"))
countries_observers_all<-observer_by_year_countries_with_label[,.(N_OBSERVER=length(unique(OBSERVER_ID))),
                                                           by=list(SCHENGEN)]

quantile(countries_observers_all$N_OBSERVER, seq(0, 1, 0.1))
target_countries<-countries_observers_all[N_OBSERVER>=1000]$SCHENGEN
countries_observers<-observer_by_year_countries_with_label[,.(N_OBSERVER=length(unique(OBSERVER_ID))),
                                                    by=list(SCHENGEN, YEAR, N_COUNTRY_LABEL)]
countries_observers$N_COUNTRY_LABEL<-factor(countries_observers$N_COUNTRY_LABEL, levels=c(as.character(c(1:9)), ">=10"))
countries_observers_prev<-countries_observers
colnames(countries_observers_prev)[4]<-"N_OBSERVER_PREV"
countries_observers_prev$YEAR<-countries_observers_prev$YEAR+1

countries_observers<-merge(countries_observers, countries_observers_prev, by=c("SCHENGEN", "YEAR", "N_COUNTRY_LABEL"))
countries_observers$DIFFER<-countries_observers$N_OBSERVER - countries_observers$N_OBSERVER_PREV
countries_observers$DIFFER_RATIO<-countries_observers$DIFFER/(countries_observers$N_OBSERVER + countries_observers$N_OBSERVER_PREV)
write.csv(countries_observers, "../../eBird_Pendemic_2021/Figures/Observers/countries_observers.csv", row.names = F)
#top 10 1-countri increase
countries_observers_filted<-countries_observers[SCHENGEN %in% target_countries]
quantile(countries_observers_filted$DIFFER_RATIO, seq(0, 1, 0.1))

p<-ggplot(countries_observers_filted)+
  geom_point(aes(x=N_COUNTRY_LABEL, y=DIFFER_RATIO))+
  facet_wrap(~YEAR)+theme_bw()
p
ggsave(p, filename="../../eBird_Pendemic_2021/Figures/Observers/observers_label_per_year_per_country.png", 
       width=10, height=5)

observer_by_year_countries_with_label$OBSERVER_TYPE<-ifelse(observer_by_year_countries_with_label$N_COUNTRY_LABEL=="1", 
                                                            "Domestic", "International")

countries_observers_type<-observer_by_year_countries_with_label[,.(N_OBSERVER=length(unique(OBSERVER_ID))),
                                                           by=list(SCHENGEN, YEAR, OBSERVER_TYPE)]
countries_observers_type_prev<-countries_observers_type
colnames(countries_observers_type_prev)[4]<-"N_OBSERVER_PREV"
countries_observers_type_prev$YEAR<-countries_observers_type_prev$YEAR+1
countries_observers_type<-merge(countries_observers_type, countries_observers_type_prev, 
                                by=c("SCHENGEN", "YEAR", "OBSERVER_TYPE"))
countries_observers_type$DIFFER<-countries_observers_type$N_OBSERVER - countries_observers_type$N_OBSERVER_PREV
countries_observers_type$DIFFER_RATIO<-countries_observers_type$DIFFER/(countries_observers_type$N_OBSERVER + countries_observers_type$N_OBSERVER_PREV)
write.csv(countries_observers_type, "../../eBird_Pendemic_2021/Figures/Observers/countries_observers_type.csv", row.names = F)

#top 10 1-countri increase
countries_observers_type_filted<-countries_observers_type[SCHENGEN %in% target_countries]
quantile(countries_observers_type_filted$DIFFER_RATIO, seq(0, 1, 0.1))

p<-ggplot(countries_observers_type_filted)+
  geom_point(aes(x=OBSERVER_TYPE, y=DIFFER_RATIO))+
  facet_wrap(~YEAR)+theme_bw()
p
ggsave(p, filename="../../eBird_Pendemic_2021/Figures/Observers/observers_type_per_year_per_country.png", 
       width=10, height=5)

df_domestic<-countries_observers_type[OBSERVER_TYPE=="Domestic"]
colnames(df_domestic)[3:7]<-paste("Domestic", colnames(df_domestic)[3:7], sep = "_")
df_International<-countries_observers_type[OBSERVER_TYPE=="International"]
colnames(df_International)[3:7]<-paste("International", colnames(df_International)[3:7], sep = "_")
df_all<-merge(df_domestic, df_International, by=c("SCHENGEN", "YEAR"))
df_all$Label_Domestic<-"STABLE"
df_all[Domestic_DIFFER_RATIO<(-0.1)]$Label_Domestic<-"DECREASE"
df_all[Domestic_DIFFER_RATIO>0.1]$Label_Domestic<-"INCREASE"

df_all$Label_International<-"STABLE"
df_all[International_DIFFER_RATIO<(-0.1)]$Label_International<-"DECREASE"
df_all[International_DIFFER_RATIO>0.1]$Label_International<-"INCREASE"

df_all$Label<-paste("D:", df_all$Label_Domestic, " I:", df_all$Label_International, sep="")
df_all_se<-df_all[, .(N_Country=length(unique(SCHENGEN))),
                  by=list(YEAR, Label_Domestic, Label_International)]

p<-ggplot(df_all_se)+
  geom_line(aes(x=YEAR, y=N_Country, color=Label_Domestic, linetype=Label_International))+
  theme_bw()
p
ggsave(p, filename="../../eBird_Pendemic_2021/Figures/Observers/N_country_by_observers_type.png", 
       width=5, height=3)

df_all_se[YEAR==2020]

df_all[(Label_Domestic=="INCREASE")&(Label_International=="DECREASE")&(YEAR==2020)]$SCHENGEN
df_all[(Label_Domestic!="INCREASE")&(Label_International=="DECREASE")&(YEAR==2020)]$SCHENGEN

write.csv(df_all, "../../eBird_Pendemic_2021/Figures/Observers/countries_observers_type_all.csv", row.names = F)

library(RMySQL)
library(sf)
con<-dbConnect(MySQL(), user="root", password="mikania", 
               dbname="eBird", host="172.16.120.11")
rs<-dbSendQuery(con, "SELECT DISTINCT COUNTRY FROM eBird202105")
countries_list<-dbFetch(rs, n=-1)
dbClearResult(rs)
dbDisconnect(con)
country<-countries_list$COUNTRY[1]
countries_ebird<-list()
for (country in countries_list$COUNTRY){
  con<-dbConnect(MySQL(), user="root", password="mikania", 
                 dbname="eBird", host="172.16.120.11")
  
  #rs<-dbSendQuery(con, sprintf(SQL_template, Loc_IDs))
  
  #print(SQL)
  rs<-dbSendQuery(con, 
                  sprintf('SELECT COUNTRY, COUNTRY_CODE FROM eBird202105 WHERE COUNTRY="%s" limit 0,1', country))
  item<-dbFetch(rs, n=-1)
  dbClearResult(rs)
  dbDisconnect(con)
  countries_ebird[[country]]<-item
}
countries_ebird<-rbindlist(countries_ebird)
x <- countries_ebird[43, ]
Encoding(x$COUNTRY) <- "UTF-8"

x$COUNTRY
Schengen<-c("Germany", "Austria", "Belgium", "Czech Republic", "Denmark", 
            "Estonia", "Finland", "France", "Greece", "Hungary", "Iceland", "Italy",
            "Latvia", "Liechtenstein", "Lithuania", "Luxembourg", "Malta", 
            "Netherlands", "Norway", "Poland", "Portugal", "Slovakia", 
            "Slovenia", "Spain", "Sweden", "Switzerland", "United Kingdom")
countries_ebird$SCHENGEN_CODE<-countries_ebird$COUNTRY_CODE
countries_ebird[COUNTRY %in% Schengen]$SCHENGEN_CODE<-"SCHENGEN"
countries_ebird$SCHENGEN<-countries_ebird$COUNTRY
countries_ebird[COUNTRY %in% Schengen]$SCHENGEN<-"Schengen"
countries_ebird[COUNTRY=="Curaçao"]
cols<-c("SCHENGEN_CODE", "SCHENGEN")
df_all_with_code<-merge(df_all, unique(countries_ebird[, ..cols]), by="SCHENGEN")

df_all_with_code_2020<-df_all_with_code[YEAR==2020]
countries<-st_read("/media/huijieqiao/WD12T/customs/Shape/TM_WORLD_BORDERS-0.3/TM_WORLD_BORDERS-0.3.shp")
i=1
cols<-colnames(df_all_with_code_2020)[c(4:7,9:12)]
for (col in cols){
  countries[, col]<-NA
}

for (i in c(1:nrow(countries))){
  item<-data.frame(df_all_with_code_2020[SCHENGEN_CODE==countries[i, ]$ISO2])
  if (countries[i, ]$NAME %in% Schengen){
    item<-data.frame(df_all_with_code_2020[SCHENGEN_CODE=="SCHENGEN"])
  }
  if (nrow(item)>0){
    for (col in cols){
      countries[i, col]<-item[1, col]
    }
    
  }
}
colnames(countries)[13:20]<-c("D_N", "D_N_PREV", "D_DIFF", "D_DIFF_R", "I_N", "I_N_PREV", "I_DIFF", "I_DIFF_R")
st_write(countries, "../../eBird_Pendemic_2021/Shape/Country_Observers/Country_Observers.shp", delete_dsn=T)

