library(ggplot2)
library(raster)
library(data.table)
library(fpp2)
library(tsoutliers)
library(scales)
library(ggpubr)

setwd("/media/huijieqiao/WD12T/eBird/Script/ebird_migration")
if (F){
  country_list<-readRDS("../../eBird_Pendemic_2021/Objects/N_SP_Observation/country_list.rda")
  state_list<-readRDS("../../eBird_Pendemic_2021/Objects/N_SP_Observation/state_list.rda")
  
  sp_list_state_list<-readRDS("../../eBird_Pendemic_2021/Objects/N_SP_Observation/sp_list_state_list.rda")
  # (we could grade this as 0 for none, 1 for some restrictions on movement etc and 2 on hard lockdown, 
  #based on the data sent on recreation etc, we need to define the dates of change for each) 
  
  
  if (F){
    lockdown_data<-read.csv("../../eBird_Pendemic_2021/Tables/COVID-19/stay-at-home-covid1.csv", stringsAsFactors = F)
    lockdown_data$Data<-as.Date(lockdown_data$Day, format="%m/%d/%Y")
    lockdown_data$YEAR<-as.numeric(format(lockdown_data$Data, format="%Y"))
    lockdown_data$MONTH<-as.numeric(format(lockdown_data$Data, format="%m"))
    lockdown_data$DAY<-as.numeric(format(lockdown_data$Data, format="%d"))
    lockdown_data$DAYS<-as.numeric(format(lockdown_data$Data, format="%j"))
    lockdown_data$DAYS<-(lockdown_data$YEAR-2020) * 366 + lockdown_data$DAYS
    lockdown_data<-data.table(lockdown_data)
    unique(lockdown_data[!(Entity %in% country_list$COUNTRY)]$Entity)
    saveRDS(lockdown_data, "../../eBird_Pendemic_2021/Tables/COVID-19/stay-at-home-covid1.rda")
  }
  lockdown_data<-readRDS("../../eBird_Pendemic_2021/Tables/COVID-19/stay-at-home-covid1.rda")
  
  target_country<-"United States"
  target_country<-"Mexico"
  target_country<-"China"
  target_country<-"Kenya"
  target_country<-"United Kingdom"
  target_country<-"India"
  target_country<-"Italy"
  target_country<-"Australia"
  target_country<-"Spain"
  
  countries<-unique(lockdown_data$Entity)
  full_results<-list()
  predicted_result<-list()
  for (target_country in countries){
    lockdown_item<-lockdown_data[Entity==target_country]
    #ggplot(lockdown_item)+geom_point(aes(x=DAYS, y=stay_home_requirements))
    lockdown_se<-lockdown_item[, .(N_LOCKDOWN=sum(stay_home_requirements),
                                   N_DAYS=.N),
                               by=list(YEAR, MONTH)]
    df1<-country_list[COUNTRY==target_country]
    df1<-df1[YEAR>=2015]
    df1$MONTH_V<-df1$YEAR*12+df1$MONTH
    df1$DATE<-as.Date(sprintf("%d/%d/1", df1$YEAR, df1$MONTH))
    setorderv(df1, c("DATE"))
    start<-2015
    item<-data.frame(df1[between(YEAR, start, 2021)])
    if (nrow(item)!=77){
      next()
    }
    print(target_country)
    col<-"N_SP"
    cols<-c("N_SP", "N_EVENTS", "N_OBSERVATION", "N_OBSERVERS")
    result_item<-list()
    for (col in cols){
      
      ts_data<-ts(item[, col], start=c(start,1), frequency = 12)
      different<-diff(ts_data)
      
      ts_train<-ts(item[which(item$YEAR<=2019), col], start=c(start, 1), frequency = 12)
      different_train<-diff(ts_train)
      fit_ets<-ets(ts_train, model="AAA")
      result_item[[col]]<-fit_ets
      item[, sprintf("FITTED_%s", col)]<-NA
      item[, sprintf("FITTED_UPPER_80_%s", col)]<-NA
      item[, sprintf("FITTED_UPPER_95_%s", col)]<-NA
      item[, sprintf("FITTED_LOWER_80_%s", col)]<-NA
      item[, sprintf("FITTED_LOWER_95_%s", col)]<-NA
      item[, sprintf("IS_FITTED_OUTLIER_80_%s", col)]<-NA
      item[, sprintf("IS_FITTED_OUTLIER_95_%s", col)]<-NA
      
      item[c(1:length(ts_train)), sprintf("FITTED_%s", col)]<-fitted.values(fit_ets)
      item[c(1:length(ts_train)), sprintf("FITTED_UPPER_80_%s", col)]<-fitted.values(fit_ets)
      item[c(1:length(ts_train)), sprintf("FITTED_UPPER_95_%s", col)]<-fitted.values(fit_ets)
      item[c(1:length(ts_train)), sprintf("FITTED_LOWER_80_%s", col)]<-fitted.values(fit_ets)
      item[c(1:length(ts_train)), sprintf("FITTED_LOWER_95_%s", col)]<-fitted.values(fit_ets)
      
      #forcase
      fcst<-forecast(fit_ets, h=17)
      item[c((length(ts_train)+1):nrow(item)), sprintf("FITTED_%s", col)]<-fcst$mean
      item[c((length(ts_train)+1):nrow(item)), sprintf("FITTED_UPPER_80_%s", col)]<-fcst$upper[,1]
      item[c((length(ts_train)+1):nrow(item)), sprintf("FITTED_UPPER_95_%s", col)]<-fcst$upper[,2]
      item[c((length(ts_train)+1):nrow(item)), sprintf("FITTED_LOWER_80_%s", col)]<-fcst$lower[,1]
      item[c((length(ts_train)+1):nrow(item)), sprintf("FITTED_LOWER_95_%s", col)]<-fcst$lower[,2]
      
      item[which(item[, col]<item[, sprintf("FITTED_LOWER_80_%s", col)]), 
           sprintf("IS_FITTED_OUTLIER_80_%s", col)]<-"LOWER"
      item[which(item[, col]>item[, sprintf("FITTED_UPPER_80_%s", col)]), 
           sprintf("IS_FITTED_OUTLIER_80_%s", col)]<-"UPPER"
      
      item[which(item[, col]<item[, sprintf("FITTED_LOWER_95_%s", col)]), 
           sprintf("IS_FITTED_OUTLIER_95_%s", col)]<-"LOWER"
      item[which(item[, col]>item[, sprintf("FITTED_UPPER_95_%s", col)]), 
           sprintf("IS_FITTED_OUTLIER_95_%s", col)]<-"UPPER"
      
      
      outliers_number<-tsoutliers(ts_data, iterate=1)
      outliers_different<-tsoutliers(different, iterate=3)
      #tso(ts_data, types = c("TS"), maxit.iloop=10, cval=3)
      item[,sprintf("IS_OUTLIERS_NUMBER_%s", col)]<-F
      item[outliers_number$index, sprintf("IS_OUTLIERS_NUMBER_%s", col)]<-T
      item[,sprintf("OUTLIERS_NUMBER_%s", col)]<-item[, col]
      item[outliers_number$index, sprintf("OUTLIERS_NUMBER_%s", col)]<-outliers_number$replacements
      item[,sprintf("IS_OUTLIERS_DIFF_%s", col)]<-F
      item[outliers_different$index+1, sprintf("IS_OUTLIERS_DIFF_%s", col)]<-T
      item[,sprintf("OUTLIERS_DIFF_%s", col)]<-c(NA,as.numeric(different))
      item[outliers_different$index+1, sprintf("OUTLIERS_DIFF_%s", col)]<-outliers_different$replacements
      
      upper_80<-mean(fcst$upper[,1] - fcst$mean, na.rm=T)
      upper_95<-mean(fcst$upper[,2] - fcst$mean, na.rm=T)
      lower_80<-mean(fcst$lower[,1] - fcst$mean, na.rm=T)
      lower_95<-mean(fcst$lower[,2] - fcst$mean, na.rm=T)
      mean_pred<-mean(fcst$mean, na.rm=T)
      diff_per_80<-upper_80/mean_pred
      diff_per_95<-upper_95/mean_pred
      
      mitem<-data.frame(accuracy(fit_ets))
      mitem$loglik<-fit_ets$loglik
      mitem$aic<-fit_ets$aic
      mitem$bic<-fit_ets$bic
      mitem$aicc<-fit_ets$aicc
      mitem$mse<-fit_ets$mse
      mitem$amse<-fit_ets$amse
      mitem$sigma2<-fit_ets$sigma2
      mitem$upper_80<-upper_80
      mitem$upper_95<-upper_95
      mitem$lower_80<-lower_80
      mitem$lower_95<-lower_95
      mitem$mean_pred<-mean_pred
      mitem$diff_per_80<-diff_per_80
      mitem$diff_per_95<-diff_per_95
      mitem$col<-col
      mitem$country<-target_country
      
      full_results[[paste(target_country, col)]]<-mitem
      
      
    }
    saveRDS(result_item, sprintf("../../eBird_Pendemic_2021/Objects/TS/Country/%s.rda", target_country))
    
    item<-merge(item, lockdown_se, by=c("YEAR", "MONTH"), all.x=T, all.y=F)
    item<-data.table(item)
    
    item$LOCKDOWN_RATIO<-item$N_LOCKDOWN/item$N_DAYS
    item$LOCKDOWN_TYPE<-"NO"
    item[is.na(LOCKDOWN_RATIO)]$LOCKDOWN_RATIO<-0
    item[LOCKDOWN_RATIO<0.2]$LOCKDOWN_TYPE<-"NO"
    item[between(LOCKDOWN_RATIO, 0.2, 1.2)]$LOCKDOWN_TYPE<-"LOSS"
    item[LOCKDOWN_RATIO>1.2]$LOCKDOWN_TYPE<-"HARD"
    item<-data.frame(item)
    col<-"N_EVENTS"
    ps<-list()
    for (col in cols){
      outlier_item<-item[which((item$YEAR>2019)&
                                 (!is.na(item[,sprintf("IS_FITTED_OUTLIER_95_%s", col)]))),]
      p<-ggplot(item)+
        geom_bar(aes_string(x="DATE", y=max(c(item[, col], item[, sprintf("FITTED_%s", col)])), 
                            fill="LOCKDOWN_TYPE"), 
                 stat='identity', alpha=0.1)+
        geom_ribbon(aes_string(x="DATE", 
                               ymin=sprintf("FITTED_LOWER_95_%s", col),
                               ymax=sprintf("FITTED_UPPER_95_%s", col)), 
                    linetype=2, fill="red", alpha=0.3)+
        geom_line(aes_string(x="DATE", y=col))+
        geom_line(aes_string(x="DATE", y=sprintf("FITTED_%s", col)), linetype=2, color="red")+
        geom_point(data=outlier_item, 
                   aes_string(x="DATE", y=col, color=sprintf("IS_FITTED_OUTLIER_95_%s", col)))+
        geom_text(data=outlier_item,
                  aes_string(x="DATE", y=col, label="MONTH"),hjust=0.5, vjust=-0.5)+
        scale_y_continuous(limits=c(
          min(c(item[, col], item[, sprintf("FITTED_%s", col)])),
          max(c(item[, col], item[, sprintf("FITTED_%s", col)]))),
          oob = rescale_none)+
        ggtitle(paste(target_country, col))+
        theme_bw()#+
      #theme(legend.position = "none")
      p
      ps[[col]]<-p
    }
    pp<-ggarrange(plotlist=ps, ncol=1)
    ggsave(pp, filename=sprintf("../../eBird_Pendemic_2021/Figures/Country_outliers/%s.png", target_country),
           width=8, height=12)
    predicted_result[[target_country]]<-item
  }
  full_results<-rbindlist(full_results)
  saveRDS(full_results, "../../eBird_Pendemic_2021/Objects/TS/models.rda")
  saveRDS(predicted_result, "../../eBird_Pendemic_2021/Objects/TS/prediction.rda")
  
}
predicted_result<-readRDS("../../eBird_Pendemic_2021/Objects/TS/prediction.rda")
full_results<-readRDS("../../eBird_Pendemic_2021/Objects/TS/models.rda")
#full_results<-full_results[diff_per_95>=0]
ggplot(full_results)+geom_histogram(aes(x=diff_per_95), bins=100)+
  #scale_x_log10()+
  facet_wrap(~col, scale="free")+theme_bw()
full_results[(col=="N_SP")&(country=="United States")]$diff_per_95
full_results[(col=="N_SP")&(country=="United Kingdom")]$diff_per_95
full_results[(col=="N_SP")&(country=="China")]$diff_per_95
full_results[(col=="N_SP")&(country=="Algeria")]$diff_per_95
full_results[diff_per_95==max(full_results[col=="N_SP"]$diff_per_95)]
full_results[diff_per_95==min(full_results[col=="N_SP"]$diff_per_95)]
country_str<-"United States"
full_results$mean_accuracy<-1
full_results$sd_accuracy<-9999

for (country_str in names(predicted_result)){
  item<-  predicted_result[[country_str]]
  item$PREDICTED_N_SP_ACCURACY<-(item$N_SP - item$FITTED_N_SP)/item$N_SP
  full_results[(country==country_str)&(col=="N_SP")]$mean_accuracy<-
    mean(abs(item[which(item$YEAR<=2019),]$PREDICTED_N_SP_ACCURACY), na.rm=T)
  full_results[(country==country_str)&(col=="N_SP")]$sd_accuracy<-
    sd(abs(item[which(item$YEAR<=2019),]$PREDICTED_N_SP_ACCURACY), na.rm=T)
  item$PREDICTED_N_EVENTS_ACCURACY<-(item$N_EVENTS-item$FITTED_N_EVENTS)/item$N_EVENTS
  full_results[(country==country_str)&(col=="N_EVENTS")]$mean_accuracy<-
    mean(abs(item[which(item$YEAR<=2019),]$PREDICTED_N_EVENTS_ACCURACY), na.rm=T)
  full_results[(country==country_str)&(col=="N_EVENTS")]$sd_accuracy<-
    sd(abs(item[which(item$YEAR<=2019),]$PREDICTED_N_EVENTS_ACCURACY), na.rm=T)
  item$PREDICTED_N_OBSERVATION_ACCURACY<-(item$N_OBSERVATION-item$FITTED_N_OBSERVATION)/item$N_OBSERVATION
  full_results[(country==country_str)&(col=="N_OBSERVATION")]$mean_accuracy<-
    mean(abs(item[which(item$YEAR<=2019),]$PREDICTED_N_OBSERVATION_ACCURACY), na.rm=T)
  full_results[(country==country_str)&(col=="N_OBSERVATION")]$sd_accuracy<-
    sd(abs(item[which(item$YEAR<=2019),]$PREDICTED_N_OBSERVATION_ACCURACY), na.rm=T)
  item$PREDICTED_N_OBSERVERS_ACCURACY<-(item$N_OBSERVERS-item$FITTED_N_OBSERVERS)/item$N_OBSERVERS
  full_results[(country==country_str)&(col=="N_OBSERVERS")]$mean_accuracy<-
    mean(abs(item[which(item$YEAR<=2019),]$PREDICTED_N_OBSERVERS_ACCURACY), na.rm=T)
  full_results[(country==country_str)&(col=="N_OBSERVERS")]$sd_accuracy<-
    sd(abs(item[which(item$YEAR<=2019),]$PREDICTED_N_OBSERVERS_ACCURACY), na.rm=T)
  predicted_result[[country_str]]<-item
}
full_results$mean_accuracy_int<-ceiling(full_results$mean_accuracy * 10)
table(full_results[col=="N_SP"]$mean_accuracy_int)
full_results$sd_accuracy_int<-ceiling(full_results$sd_accuracy * 10)
table(full_results[col=="N_SP"]$sd_accuracy_int)
full_results[country=="Cameroon"]
threshold<-0.1
target_countries<-unique(full_results[(mean_accuracy<=threshold)&(sd_accuracy<=threshold)]$country)
target_countries<-target_countries[!(target_countries %in% c("Cyprus"))]
saveRDS(target_countries, "../../eBird_Pendemic_2021/Tables/target_countries.rda")

target_countries<-readRDS("../../eBird_Pendemic_2021/Tables/target_countries.rda")
write.csv(target_countries, "../../eBird_Pendemic_2021/Tables/target_countries.csv", row.names = F, quote=F)
print(paste(target_countries, sep=",", collapse = ","))
#GPS
GDPS<-read.csv("../../eBird_Pendemic_2021/Tables/COVID-19/GDPS.csv", stringsAsFactors = F)
full_results_with_GDP<-merge(full_results, GDPS, by=c("country"="country"), all.x=T)
full_results_with_GDP<-full_results_with_GDP[!is.na(gdpPerCapita)]
unique(covid19_visitors$Type)
covid19_visitors[Entity=="United States"]
#covid19_visitors<-covid19_visitors[, .(N_Vistors)]
full_set<-list()
full_2020<-list()
for (country_str in names(predicted_result)){
  item<-data.table(predicted_result[[country_str]])
  item$DIFFER_N_SP<-"NO"
  item[PREDICTED_N_SP_ACCURACY>threshold]$DIFFER_N_SP<-"10% HIGHER"
  item[PREDICTED_N_SP_ACCURACY<(-1*threshold)]$DIFFER_N_SP<-"10% LOWER"
  item$DIFFER_N_EVENTS<-"NO"
  item[PREDICTED_N_EVENTS_ACCURACY>threshold]$DIFFER_N_EVENTS<-"10% HIGHER"
  item[PREDICTED_N_EVENTS_ACCURACY<(-1*threshold)]$DIFFER_N_EVENTS<-"10% LOWER"
  item$DIFFER_N_OBSERVATION<-"NO"
  item[PREDICTED_N_OBSERVATION_ACCURACY>threshold]$DIFFER_N_OBSERVATION<-"10% HIGHER"
  item[PREDICTED_N_OBSERVATION_ACCURACY<(-1*threshold)]$DIFFER_N_OBSERVATION<-"10% LOWER"
  item$DIFFER_N_OBSERVERS<-"NO"
  item[PREDICTED_N_OBSERVERS_ACCURACY>threshold]$DIFFER_N_OBSERVERS<-"10% HIGHER"
  item[PREDICTED_N_OBSERVERS_ACCURACY<(-1*threshold)]$DIFFER_N_OBSERVERS<-"10% LOWER"
  
  full_set[[country_str]]<-item
  
  item_2020<-item[YEAR>=2020]
  if (nrow(item_2020)==0){
    next()
  }
  cols<-c("YEAR", "MONTH", "COUNTRY", "N_SP", "MONTH_V", "DATE", "FITTED_N_SP",
          "N_LOCKDOWN", "N_DAYS", "LOCKDOWN_RATIO", "LOCKDOWN_TYPE")
  item_2020<-item_2020[, ..cols]
  item_2020$DIFFER_N_SP<-item_2020$N_SP - item_2020$FITTED_N_SP
  cols2<-c("YEAR", "MONTH", "COUNTRY", "N_SP", "FITTED_N_SP", "LOCKDOWN_RATIO", 
           "LOCKDOWN_TYPE", "DIFFER_N_SP")
  item_2020$PEAK_PIT<-"NONE"
  lockdown<-unique(item_2020$LOCKDOWN_TYPE)[2]
  for (lockdown in unique(item_2020$LOCKDOWN_TYPE)){
    if (lockdown=="NO"){
      next()
    }
    item_sub<-item_2020[(LOCKDOWN_TYPE==lockdown)]
    item_2020[(LOCKDOWN_TYPE==lockdown)&(DIFFER_N_SP<0)&(DIFFER_N_SP==min(item_sub$DIFFER_N_SP))]$PEAK_PIT<-"PEAK"
    item_2020[(LOCKDOWN_TYPE==lockdown)&(DIFFER_N_SP>0)&(DIFFER_N_SP==max(item_sub$DIFFER_N_SP))]$PEAK_PIT<-"PIT"
  }
  full_2020[[country_str]]<-item_2020
}
full_set<-rbindlist(full_set)
full_2020<-rbindlist(full_2020)
full_set[PREDICTED_N_SP_ACCURACY<=-65]
full_set<-full_set[COUNTRY %in% target_countries]
full_2020<-full_2020[COUNTRY %in% target_countries]
full_2020<-merge(full_2020, GDPS, by.x="COUNTRY", by.y="country")
write.csv(full_2020, "../../eBird_Pendemic_2021/Tables/lockdown_peak_pit.csv", row.names = F)

#Vistors
covid19_visitors<-readRDS("../../eBird_Pendemic_2021/Tables/COVID-19/covid19_visitors.rda")
covid19_visitors_se<-covid19_visitors[, .(SUM_Vistor=sum(Value),
                                          MEAN_Vistor=mean(Value),
                                          Days=.N),
                                      by=list(Entity, Year, Month, Type)]
covid19_visitors_all<-covid19_visitors[, .(SUM_Vistor=sum(Value),
                                          MEAN_Vistor=mean(Value),
                                          Days=.N),
                                      by=list(Entity, Year, Month)]
covid19_visitors_all$Type<-"all"
covid19_visitors_se<-rbindlist(list(covid19_visitors_se, covid19_visitors_all), use.names=T)
unique(covid19_visitors_se$Type)
covid19_visitors_se$Month_v<-covid19_visitors_se$Year*12+covid19_visitors_se$Month
covid19_visitors_se<-covid19_visitors_se[Entity %in% target_countries]
covid19_visitors_se$Date<-as.Date(sprintf("%d-%d-1", covid19_visitors_se$Year, covid19_visitors_se$Month))
type<-"all"
full_2020_with_vistor<-full_2020
for (type in unique(covid19_visitors_se$Type)){
  if (type=="all"){
    next()
  }
  item<-covid19_visitors_se[Type==type]
  cols<-c("Entity", "Year", "Month", "SUM_Vistor", 
          "MEAN_Vistor", "Days")
  item<-item[, ..cols]
  colnames(item)<-c("COUNTRY", "YEAR", "MONTH",
                    sprintf("%s_SUM_Vistor", type),
                    sprintf("%s_MEAN_Vistor", type),
                    sprintf("%s_Days_Vistor", type))
  
  full_2020_with_vistor<-merge(full_2020_with_vistor, item,
                               by=c("COUNTRY", "YEAR", "MONTH"),
                               all.x=T, all.y=F)
}
write.csv(full_2020_with_vistor, "../../eBird_Pendemic_2021/Tables/lockdown_peak_pit_with_vistors.csv", row.names = F)

ggplot(covid19_visitors_se[Type!="all"])+
  geom_line(aes(x=Date, y=MEAN_Vistor, color=Entity,))+
  theme_bw()+
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  facet_wrap(~Type, scale="free")

mean(abs(sub_set[COUNTRY=="Cameroon"]$PREDICTED_N_SP_ACCURACY))

sub_set<-full_set[YEAR>2019]

sub_set2<-full_set[YEAR<=2019]
range(sub_set2$PREDICTED_N_SP_ACCURACY)

p1<-ggplot(sub_set)+geom_boxplot(aes(x=DIFFER_N_SP, y=N_LOCKDOWN))+
  #ggtitle("Number of species")+
  theme_bw()+
  xlab("Number of species")+ylab("N Lockdown")+
  scale_x_discrete(labels=c("10% increase", "10% decrease", "stable"))
p1
p2<-ggplot(sub_set)+
  geom_boxplot(aes(x=DIFFER_N_EVENTS, y=N_LOCKDOWN))+
  xlab("Number of events")+ylab("N Lockdown")+
  theme_bw()+
  theme(axis.title.y = element_blank())+
  scale_x_discrete(labels=c("10% increase", "10% decrease", "stable"))
p2
p3<-ggplot(sub_set)+
  geom_boxplot(aes(x=DIFFER_N_OBSERVATION, y=N_LOCKDOWN))+
  xlab("Number of observations")+ylab("N Lockdown")+
  theme_bw()+
  theme(axis.title.y = element_blank())+
  scale_x_discrete(labels=c("10% increase", "10% decrease", "stable"))
p3
p4<-ggplot(sub_set)+
  geom_boxplot(aes(x=DIFFER_N_OBSERVERS, y=N_LOCKDOWN))+
  xlab("Number of observers")+ylab("N Lockdown")+
  theme_bw()+
  theme(axis.title.y = element_blank())+
  scale_x_discrete(labels=c("10% increase", "10% decrease", "stable"))
p4
p<-ggarrange(p1, p2, p3, p4, nrow=1)
p
ggsave(p, 
       filename="../../eBird_Pendemic_2021/Figures/Lockdown_influence/Lockdown_influence.png",
       width=12, height=3)

full_results_with_GDP

full_set_with_GDP<-merge(full_set, GDPS, 
                        by.x="COUNTRY", by.y="country", all.x=T)
full_set_with_GDP$PANDEMIC<-ifelse(full_set_with_GDP$YEAR>2019, ">2019", "<=2019")
full_se<-full_set_with_GDP[, .(N_LOCKDOWN=sum(N_LOCKDOWN, na.rm = T),
                             mean_PREDICTED_N_SP_ACCURACY=mean(PREDICTED_N_SP_ACCURACY, na.rm=T),
                             sd_PREDICTED_N_SP_ACCURACY=sd(PREDICTED_N_SP_ACCURACY, na.rm=T),
                             mean_PREDICTED_N_EVENTS_ACCURACY=mean(PREDICTED_N_EVENTS_ACCURACY, na.rm=T),
                             sd_PREDICTED_N_EVENTS_ACCURACY=sd(PREDICTED_N_EVENTS_ACCURACY, na.rm=T),
                             mean_PREDICTED_N_OBSERVATION_ACCURACY=mean(PREDICTED_N_OBSERVATION_ACCURACY, na.rm=T),
                             sd_PREDICTED_N_OBSERVATION_ACCURACY=sd(PREDICTED_N_OBSERVATION_ACCURACY, na.rm=T),
                             mean_PREDICTED_N_OBSERVERS_ACCURACY=mean(PREDICTED_N_OBSERVERS_ACCURACY, na.rm=T),
                             sd_PREDICTED_N_OBSERVERS_ACCURACY=sd(PREDICTED_N_OBSERVERS_ACCURACY, na.rm=T)),
                         by=list(COUNTRY, gdpPerCapita, PANDEMIC)]
errerbar_width<-1000


p1<-ggplot(full_se[!(COUNTRY %in% c("Cyprus"))])+
  geom_point(aes(x=gdpPerCapita, y=mean_PREDICTED_N_SP_ACCURACY, color=N_LOCKDOWN, size=N_LOCKDOWN))+
  geom_text(aes(x=gdpPerCapita, y=mean_PREDICTED_N_SP_ACCURACY, label=COUNTRY), hjust=0.5, vjust=-0.5)+
  geom_smooth(aes(x=gdpPerCapita, y=mean_PREDICTED_N_SP_ACCURACY))+
  scale_colour_gradient2(low = "bisque",
                         mid = "bisque3",
                         high = "black")+
  xlab("GDPc") +ylab("(Predicted - Empirical)/Empirical")+
  scale_x_sqrt()+
  ggtitle("Number of species")+
  theme_bw()+
  theme(legend.position = "none")+
  facet_wrap(~PANDEMIC, scale="free", ncol=1)
p1
ggsave(p1, filename="../../eBird_Pendemic_2021/Figures/Lockdown_influence/N_SP_GDP.png", width=10, height=10)
p2<-ggplot(full_se[!(COUNTRY %in% c("Cyprus"))])+
  geom_point(aes(x=gdpPerCapita, y=mean_PREDICTED_N_OBSERVERS_ACCURACY, color=N_LOCKDOWN, size=N_LOCKDOWN))+
  geom_text(aes(x=gdpPerCapita, y=mean_PREDICTED_N_OBSERVERS_ACCURACY, label=COUNTRY), hjust=0.5, vjust=-0.5)+
  geom_smooth(aes(x=gdpPerCapita, y=mean_PREDICTED_N_OBSERVERS_ACCURACY))+
  scale_colour_gradient2(low = "bisque",
                         mid = "bisque3",
                         high = "black")+
  xlab("GDPc") +ylab("(Predicted - Empirical)/Empirical")+
  scale_x_sqrt()+
  ggtitle("Number of observers")+
  theme_bw()+
  theme(legend.position = "none")+
  facet_wrap(~PANDEMIC, scale="free", ncol=1)
p2
ggsave(p2, filename="../../eBird_Pendemic_2021/Figures/Lockdown_influence/N_OBSERVER_GDP.png", width=10, height=10)

p3<-ggplot(full_se[!(COUNTRY %in% c("Cyprus"))])+
  geom_point(aes(x=gdpPerCapita, y=mean_PREDICTED_N_OBSERVATION_ACCURACY, color=N_LOCKDOWN, size=N_LOCKDOWN))+
  geom_text(aes(x=gdpPerCapita, y=mean_PREDICTED_N_OBSERVATION_ACCURACY, label=COUNTRY), hjust=0.5, vjust=-0.5)+
  geom_smooth(aes(x=gdpPerCapita, y=mean_PREDICTED_N_OBSERVATION_ACCURACY))+
  scale_colour_gradient2(low = "bisque",
                         mid = "bisque3",
                         high = "black")+
  xlab("GDPc") +ylab("(Predicted - Empirical)/Empirical")+
  scale_x_sqrt()+
  ggtitle("Number of observations")+
  theme_bw()+
  theme(legend.position = "none")+
  facet_wrap(~PANDEMIC, scale="free", ncol=1)
p3
ggsave(p3, filename="../../eBird_Pendemic_2021/Figures/Lockdown_influence/N_OBSERVATION_GDP.png", width=10, height=10)

p4<-ggplot(full_se[!(COUNTRY %in% c("Cyprus"))])+
  geom_point(aes(x=gdpPerCapita, y=mean_PREDICTED_N_EVENTS_ACCURACY, color=N_LOCKDOWN, size=N_LOCKDOWN))+
  geom_text(aes(x=gdpPerCapita, y=mean_PREDICTED_N_EVENTS_ACCURACY, label=COUNTRY), hjust=0.5, vjust=-0.5)+
  geom_smooth(aes(x=gdpPerCapita, y=mean_PREDICTED_N_EVENTS_ACCURACY))+
  scale_colour_gradient2(low = "bisque",
                         mid = "bisque3",
                         high = "black")+
  xlab("GDPc") +ylab("(Predicted - Empirical)/Empirical")+
  scale_x_sqrt()+
  ggtitle("Number of events")+
  theme_bw()+
  theme(legend.position = "none")+
  facet_wrap(~PANDEMIC, scale="free", ncol=1)
p4
ggsave(p4, filename="../../eBird_Pendemic_2021/Figures/Lockdown_influence/N_EVENTS_GDP.png", width=10, height=10)

plot(sub_set_with_GDP$gdpPerCapita, sub_set_with_GDP$PREDICTED_N_SP_ACCURACY)
####  TEST CODE  ##################
if (F){
  item[outliers_number$index]$IS_OUTLIERS_NUMBER<-T
  
  autoplot(ts_data) + ggtitle("Number of species per month")
  
  
  autoplot(different) + ggtitle("change month in month")
  
  
  
  autoplot(tsclean(ts_data), series="clean", color='red', lwd=0.9) +
    autolayer(ts_data, series="original", color='gray', lwd=1)
  
  
  # series appears trend-stationary, use to investigate seasonality.
  ggseasonplot(different)+ggtitle("Seasonal change of number of species every year.")
  
  # another seasonal plot
  ggsubseriesplot(different)
  
  #Use a benchmark method to forecast
  #Seasonal native method as our benchmark
  fit<-snaive(different) #Resdisal SD=2.852 
  print(summary(fit))
  checkresiduals(fit)
  
  #Fit ETS method Resdusal SD=2.0504
  fit_ets<-ets(different)N_OBSERVERS
  print(summary(fit_ets))
  checkresiduals(fit_ets)
  
  #Fit an ARIMA model SD=2.240089
  #fit_arima<-auto.arima(ts_data, d=1, D=1, stepwise=F, approximation = F, trace=T)
  #print(summary(fit_arima))
  #checkresiduals(fit_arima)
  
  #forcase
  fcst<-forecast(fit_ets, h=24)
  autoplot(fcst)
  
  library(AnomalyDetection)
  cols<-c("DATE", "MEAN_N_SP_CURRENT")
  AnomalyDetectionVec(df1[between(YEAR, 2015, 2021)]$MEAN_N_SP_CURRENT, period=12, 
                      direction='both', plot=TRUE, alpha=0.01)
}
