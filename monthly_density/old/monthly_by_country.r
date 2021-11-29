library(ggplot2)
library(raster)
library(data.table)
library(DBI)
library(mgcv)
library(pastecs)
library(dplyr)
library(ggpubr)

setwd("/media/huijieqiao/WD12T/eBird/Script/ebird_migration")
N_SP_10km_Merged_ALL_With_XY<-readRDS("../../Data_eBird_2020/N_SP/N_SP_10km_Merged_ALL_With_XY.rda")
cols<-c("COUNTRY", "STATE", "COUNTY", "INDEX_10km", "MONTH",
        "N_SP_CURRENT", "N_EVENTS_CURRENT", "N_OBSERVATIONS_CURRENT", "N_OBSERVERS_CURRENT")
N_2019<-N_SP_10km_Merged_ALL_With_XY[YEAR==2019, ..cols]
colnames(N_2019)[6:9]<-c("N_SP_2019", "N_EVENTS_2019", "N_OBSERVATIONS_2019", "N_OBSERVERS_2019")
N_2020<-N_SP_10km_Merged_ALL_With_XY[YEAR==2020, ..cols]
N_2021<-N_SP_10km_Merged_ALL_With_XY[YEAR==2021, ..cols]
N_2020$YEAR<-2020
N_2020<-merge(N_2020, N_2019, by=c("COUNTRY", "STATE", "COUNTY", "INDEX_10km", "MONTH"),
              all.x=T, all.y=F)
N_2021$YEAR<-2021
N_2021<-merge(N_2021, N_2019, by=c("COUNTRY", "STATE", "COUNTY", "INDEX_10km", "MONTH"),
              all.x=T, all.y=F)

cols_others<-c("COUNTRY", "STATE", "COUNTY", "INDEX_10km", "MONTH",
                "N_SP_CURRENT", "N_EVENTS_CURRENT", "N_OBSERVATIONS_CURRENT", "N_OBSERVERS_CURRENT", "YEAR",
                "N_SP_PREVIOUS", "N_EVENTS_PREVIOUS", "N_OBSERVATIONS_PREVIOUS", "N_OBSERVERS_PREVIOUS")
N_Others<-N_SP_10km_Merged_ALL_With_XY[YEAR<=2019, ..cols_others]
colnames(N_Others)<-colnames(N_2020)
N_ALL<-rbindlist(list(N_2020, N_2021, N_Others))
N_ALL<-data.frame(N_ALL)
N_ALL[is.na(N_ALL)]<-0
N_ALL<-data.table(N_ALL)
N_ALL<-N_ALL[(N_SP_CURRENT+N_SP_2019)>0]

N_ALL$N_SP_INCREASE_PERCENT<-
  (N_ALL$N_SP_CURRENT-N_ALL$N_SP_2019)/
  (N_ALL$N_SP_CURRENT+N_ALL$N_SP_2019)

N_ALL$N_EVENTS_INCREASE_PERCENT<-
  (N_ALL$N_EVENTS_CURRENT-N_ALL$N_EVENTS_2019)/
  (N_ALL$N_EVENTS_CURRENT+N_ALL$N_EVENTS_2019)

N_ALL$N_OBSERVATIONS_INCREASE_PERCENT<-
  (N_ALL$N_OBSERVATIONS_CURRENT-N_ALL$N_OBSERVATIONS_2019)/
  (N_ALL$N_OBSERVATIONS_CURRENT+N_ALL$N_OBSERVATIONS_2019)

N_SP_BY_COUNTRY<-N_ALL[, .(MEAN_N_SP_CURRENT=mean(N_SP_CURRENT),
                           SD_N_SP_CURRENT=sd(N_SP_CURRENT),
                           MEAN_N_EVENTS_CURRENT=mean(N_EVENTS_CURRENT),
                           SD_N_EVENTS_CURRENT=sd(N_EVENTS_CURRENT),
                           MEAN_N_OBSERVATIONS_CURRENT=mean(N_OBSERVATIONS_CURRENT),
                           SD_N_OBSERVATIONS_CURRENT=sd(N_OBSERVATIONS_CURRENT),
                           MEAN_N_SP_2019=mean(N_SP_2019),
                           SD_N_SP_2019=sd(N_SP_2019),
                           MEAN_N_EVENTS_2019=mean(N_EVENTS_2019),
                           SD_N_EVENTS_2019=sd(N_EVENTS_2019),
                           MEAN_N_OBSERVATIONS_2019=mean(N_OBSERVATIONS_2019),
                           SD_N_OBSERVATIONS_2019=sd(N_OBSERVATIONS_2019),
                           MEAN_N_SP_INCREASE_PERCENT=mean(N_SP_INCREASE_PERCENT),
                           SD_N_SP_INCREASE_PERCENT=sd(N_SP_INCREASE_PERCENT),
                           MEAN_N_EVENTS_INCREASE_PERCENT=mean(N_EVENTS_INCREASE_PERCENT),
                           SD_N_EVENTS_INCREASE_PERCENT=sd(N_SP_INCREASE_PERCENT),
                           MEAN_N_OBSERVATIONS_INCREASE_PERCENT=mean(N_OBSERVATIONS_INCREASE_PERCENT),
                           SD_N_OBSERVATIONS_INCREASE_PERCENT=sd(N_OBSERVATIONS_INCREASE_PERCENT)),
                       by=list(COUNTRY, YEAR, MONTH)]

#target_country<-"United States"
target_country<-"Mexico"
df1<-N_SP_BY_COUNTRY[COUNTRY==target_country]
df1<-df1[MEAN_N_OBSERVATIONS_CURRENT>0]
df1$MONTH_V<-df1$YEAR*12+df1$MONTH
setorder(df1, MONTH_V)
ggplot(df1)+
  geom_line(aes(x=MONTH_V, y=MEAN_N_OBSERVATIONS_INCREASE_PERCENT, color=factor(YEAR)))

k=1
var<-"MEAN_N_OBSERVATIONS_INCREASE_PERCENT"
df_set<-df1
bandwidth=3
get_peak_pit<-function(df_set, var, bandwidth=3){
  #model_gam<-gam(MEAN_N_OBSERVATIONS~s(MONTH_V, k=k), data=df)
  model<-ksmooth(df_set$MONTH_V, pull(df_set[, ..var]), 'normal',bandwidth=bandwidth)
  #plot(model)
  #df$predicted<-predict(model, df)
  predicted<-model$y
  #plot(df$MONTH_V, df$MEAN_N_OBSERVATIONS, type="l")
  #lines(df$MONTH_V, df$predicted_MEAN_N_OBSERVATIONS, type="l", col="red")
  
  v<-predicted
  tp<-turnpoints(v)
  #plot(tp)
  #plot(v, type = "l")
  #lines(tp)
  item<-data.table(MONTH_V=pull(df_set[, "MONTH_V"]),
                   YEAR=pull(df_set[, "YEAR"]),
                   MONTH=pull(df_set[, "MONTH"]),
                   v=pull(df_set[, ..var]),
                   predicted=predicted,
                   peak=tp$peaks,
                   pit=tp$pits,
                   direction=0,
                   group=0,
                   length=0)

  direction<-0
  length<-0
  g<-0
  month_v<-24140
  for (month_v in unique(item$MONTH_V)){
    if (pull(item[MONTH_V==month_v, "peak"])){
      item[group==g, "length"]<-length
      direction<- -1
      length<-0
      g<-g+1
    }
    if (pull(item[MONTH_V==month_v, "pit"])){
      item[group==g, "length"]<-length
      direction<- 1
      length<-0
      g<-g+1
    }
    length<-length+1
    item[MONTH_V==month_v, "direction"]<- direction
    item[MONTH_V==month_v, "group"]<- g
  }
  
  item[group==g, "length"]<-length
  #unique(df[, c("group", "length")])
  item
}
result1<-get_peak_pit(df1, "MEAN_N_OBSERVATIONS_CURRENT",1)

ggplot(result1, aes(y=predicted, x=MONTH_V))+
  geom_line(aes(y=v), linetype=2, alpha=0.3)+
  geom_line()+
  geom_point(data=result1[pit==T], color="blue")+
  geom_point(data=result1[peak==T], color="red")+
  scale_x_continuous(breaks=result1$MONTH_V, 
                     labels=paste(result1$YEAR, result1$MONTH, sep="/"))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

result2<-get_peak_pit(df1, "MEAN_N_SP_CURRENT")

ggplot(result2, aes(y=predicted, x=MONTH_V))+
  geom_line(aes(y=v), linetype=2, alpha=0.3)+
  geom_line()+
  geom_point(data=result2[pit==T], color="blue")+
  geom_point(data=result2[peak==T], color="red")+
  scale_x_continuous(breaks=result2$MONTH_V, 
                     labels=paste(result2$YEAR, result2$MONTH, sep="/"))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

result1$TYPE<-"N_OBSERVATIONS"
result2$TYPE<-"N_SP"
result<-rbindlist(list(result1, result2))

ggplot(result, aes(y=predicted, x=MONTH_V, color=TYPE))+
  geom_line(aes(y=v), linetype=2, alpha=0.3)+
  geom_line()+
  geom_point(data=result[pit==T], color="blue")+
  geom_point(data=result[peak==T], color="red")+
  scale_x_continuous(breaks=result$MONTH_V, 
                     labels=paste(result$YEAR, result$MONTH, sep="/"))+
  scale_y_log10()+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



#COVID 19
if (F){
  df<-read.csv("../../Data_eBird_2020/Tables/COVID-19/global_cases.csv")
  df_newcases<-data.table(read.csv("../../Data_eBird_2020/Tables/COVID-19/global_newcases.csv"))
  df_deaths<-data.table(read.csv("../../Data_eBird_2020/Tables/COVID-19/global_deaths.csv"))
  df_newdeaths<-data.table(read.csv("../../Data_eBird_2020/Tables/COVID-19/global_newdeaths.csv"))
  
  date_list<-colnames(df)[5:ncol(df)]
  date_list_date<-gsub("X", "", date_list)
  date_list_date<-as.POSIXct(date_list_date, format="%Y.%m.%d")
  years<-as.numeric(format(date_list_date, "%Y"))
  months<-as.numeric(format(date_list_date, "%m"))
  days<-as.numeric(format(date_list_date, "%d"))
  date_df<-data.table(date_str=date_list, date=date_list_date, year=years, month=months, day=days)
  df<-data.table(df)
  df_all<-list()
  i=20
  for (i in c(1:nrow(date_df))){
    print(paste(i, nrow(date_df)))
    item<-date_df[i,]
    cols<-c("Province.State", "Country.Region", "Lat", "Long", item$date_str)
    df_item<-df[, ..cols]
    colnames(df_item)[5]<-"N"
    df_item$Type<-"N cases"
    if (item$date_str %in% colnames(df_newcases)){
      df_item_newcases<-df_newcases[, ..cols]
      colnames(df_item_newcases)[5]<-"N"
      df_item_newcases$Type<-"N new cases"
    }
    if (item$date_str %in% colnames(df_deaths)){
      df_item_deaths<-df_deaths[, ..cols]
      colnames(df_item_deaths)[5]<-"N"
      df_item_deaths$Type<-"N deaths"
    }
    if (item$date_str %in% colnames(df_newdeaths)){
      df_item_newdeaths<-df_newdeaths[, ..cols]
      colnames(df_item_newdeaths)[5]<-"N"
      df_item_newdeaths$Type<-"N new deaths"
    }
    all_items<-rbindlist(list(df_item, df_item_newcases, df_item_deaths, df_item_newdeaths))
    all_items$date_str<-item$date_str<-item$date_str
    all_items$date<-item$date
    all_items$year<-item$year
    all_items$month<-item$month
    all_items$day<-item$day
    
    df_all[[item$date_str]]<-all_items
  }
  df_all<-rbindlist(df_all)
  df_all[is.na(Province.State)]$Province.State<-""
  saveRDS(df_all, "../../Data_eBird_2020/Tables/COVID-19/all_data_raw.rda")
  df_all_country<-df_all[, .(N=sum(N)), by=list(Country.Region, Type, date_str, date, year, month, day)]
  saveRDS(df_all_country, "../../Data_eBird_2020/Tables/COVID-19/all_data_by_country_raw.rda")
  df_all_country_month<-df_all[, .(N=sum(N)), by=list(Country.Region, Type, year, month)]
  saveRDS(df_all_country_month, "../../Data_eBird_2020/Tables/COVID-19/all_data_by_country_by_month.rda")
  
  covid19_visitors<-data.table(read.csv("../../Data_eBird_2020/Tables/COVID-19/changes-visitors-covid.csv"))
  covid19_visitors$Date_str<-covid19_visitors$Day
  covid19_visitors$Date<-as.POSIXct(covid19_visitors$Date_str, , format="%Y-%m-%d")
  covid19_visitors$Year<-as.numeric(format(covid19_visitors$Date, "%Y"))
  covid19_visitors$Month<-as.numeric(format(covid19_visitors$Date, "%m"))
  covid19_visitors$Day<-as.numeric(format(covid19_visitors$Date, "%d"))
  
  cols<-c("retail_and_recreation", "grocery_and_pharmacy", "residential",
          "transit_stations", "parks", "workplaces")
  col<-cols[1]
  df_all<-list()
  for (col in cols){
    selcols<-c("Entity", "Code", "Date_str", "Date", "Year", "Month", "Day", col)
    item<-covid19_visitors[, ..selcols]
    colnames(item)[8]<-"Value"
    item$Type<-col
    df_all[[col]]<-item
  }
  df_all<-rbindlist(df_all)
  saveRDS(df_all, "../../Data_eBird_2020/Tables/COVID-19/covid19_visitors.rda")
  
}

result2<-get_peak_pit(df1, "MEAN_N_SP_INCREASE_PERCENT", bandwidth = 5)
result2_item<-result2[YEAR>=2020]
p1<-ggplot(result2_item, aes(y=predicted, x=MONTH_V))+
  geom_line(aes(y=v), linetype=2, alpha=0.3)+
  geom_line()+
  geom_point(data=result2_item[pit==T], color="blue")+
  geom_point(data=result2_item[peak==T], color="red")+
  scale_x_continuous(breaks=result2_item$MONTH_V, 
                     labels=paste(result2_item$YEAR, result2_item$MONTH, sep="/"))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
p1
df_all_country_month<-readRDS("../../Data_eBird_2020/Tables/COVID-19/all_data_by_country_by_month.rda")
covid19_country_month<-df_all_country_month[Country.Region==target_country]
covid19_country_month$MONTH_V<-covid19_country_month$year * 12 + covid19_country_month$month
p2<-ggplot(covid19_country_month, aes(y=N, x=MONTH_V))+
  #geom_line(aes(y=N), linetype=2, alpha=0.3)+
  geom_line()+
  #geom_point(data=result2_item[pit==T], color="blue")+
  #geom_point(data=result2_item[peak==T], color="red")+
  scale_x_continuous(breaks=covid19_country_month$MONTH_V, 
                     labels=paste(covid19_country_month$year, covid19_country_month$month, sep="/"))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  facet_wrap(~Type, scale="free")
p2

covid19_visitors<-readRDS("../../Data_eBird_2020/Tables/COVID-19/covid19_visitors.rda")
covid19_visitors_month<-covid19_visitors[, .(MEAN_Value=mean(Value, na.rm=T),
                                             SD_Value=sd(Value, na.rm=T)),
                                         by=list(Entity, Code, Year, Month, Type)]
covid19_visitors_month$MONTH_V<-covid19_visitors_month$Year * 12 + covid19_visitors_month$Month
covid19_visitors_country_month<-covid19_visitors_month[covid19_visitors_month$Entity==target_country]

p3<-ggplot(covid19_visitors_country_month, aes(y=MEAN_Value, x=MONTH_V, color=Type))+
  #geom_line(aes(y=N), linetype=2, alpha=0.3)+
  geom_line()+
  #geom_point(data=result2_item[pit==T], color="blue")+
  #geom_point(data=result2_item[peak==T], color="red")+
  scale_x_continuous(breaks=covid19_visitors_country_month$MONTH_V, 
                     labels=paste(covid19_visitors_country_month$Year, covid19_visitors_country_month$Month, sep="/"))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
p3

ggarrange(p1, p2, p3, nrow=3)
