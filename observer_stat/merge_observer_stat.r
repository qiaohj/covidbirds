library(RMySQL)
library(data.table)
library(ggplot2)
setwd("/media/huijieqiao/WD12T/eBird/Script/ebird_migration")

if (T){
  countries<-list.dirs("../../Tables/Locality_202105", full.names = F, recursive=F)
  countries<-countries[sample(length(countries), length(countries))]
  country<-"American Samoa"
  df_states_years<-list()
  df_states_year_months<-list()
  df_states_month_time_binss<-list()
  df_country_years<-list()
  df_country_year_months<-list()
  df_country_month_time_binss<-list()
  df_states_year_100s<-list()
  df_states_year_month_100s<-list()
  df_states_month_time_bins_100s<-list()
  df_country_year_100s<-list()
  df_country_year_month_100s<-list()
  df_country_month_time_bins_100s<-list()
  
  for (country in countries){
    print(country)
    
    target<-sprintf("../../Tables/Observers_by_country/%s", country)
    
    df_states_year<-readRDS(sprintf("../../Tables/Observers_by_country/%s/df_states_year.rda", country))
    df_states_years[[country]]<-df_states_year
    df_states_year_month<-readRDS(sprintf("../../Tables/Observers_by_country/%s/df_states_year_month.rda", country))
    df_states_year_months[[country]]<-df_states_year_month
    df_states_month_time_bins<-readRDS(sprintf("../../Tables/Observers_by_country/%s/df_states_month_time_bins.rda", country))
    df_states_month_time_binss[[country]]<-df_states_month_time_bins
    
    df_country_year<-readRDS(sprintf("../../Tables/Observers_by_country/%s/df_country_year.rda", country))
    df_country_years[[country]]<-df_country_year
    df_country_year_month<-readRDS(sprintf("../../Tables/Observers_by_country/%s/df_country_year_month.rda", country))
    df_country_year_months[[country]]<-df_country_year_month
    df_country_month_time_bins<-readRDS(sprintf("../../Tables/Observers_by_country/%s/df_country_month_time_bins.rda", country))
    df_country_month_time_binss[[country]]<-df_country_month_time_bins
    
    df_states_year_100<-readRDS(sprintf("../../Tables/Observers_by_country/%s/df_states_year_threshold_100.rda", country))
    df_states_year_100s[[country]]<-df_states_year_100
    df_states_year_month_100<-readRDS(sprintf("../../Tables/Observers_by_country/%s/df_states_year_month_threshold_100.rda", country))
    df_states_year_month_100s[[country]]<-df_states_year_month_100
    df_states_month_time_bins_100<-readRDS(sprintf("../../Tables/Observers_by_country/%s/df_states_month_time_bins_threshold_100.rda", country))
    df_states_month_time_bins_100s[[country]]<-df_states_month_time_bins_100
    
    df_country_year_100<-readRDS(sprintf("../../Tables/Observers_by_country/%s/df_country_year_threshold_100.rda", country))
    df_country_year_100s[[country]]<-df_country_year_100
    df_country_year_month_100<-readRDS(sprintf("../../Tables/Observers_by_country/%s/df_country_year_month_threshold_100.rda", country))
    df_country_year_month_100s[[country]]<-df_country_year_month_100
    df_country_month_time_bins_100<-readRDS(sprintf("../../Tables/Observers_by_country/%s/df_country_month_time_bins_threshold_100.rda", country))
    df_country_month_time_bins_100s[[country]]<-df_country_month_time_bins_100
  }
  
}

df_states_years<-rbindlist(df_states_years)
df_states_year_months<-rbindlist(df_states_year_months)
df_states_month_time_binss<-rbindlist(df_states_month_time_binss)
df_country_years<-rbindlist(df_country_years)
df_country_year_months<-rbindlist(df_country_year_months)
df_country_month_time_binss<-rbindlist(df_country_month_time_binss)
df_states_year_100s<-rbindlist(df_states_year_100s)
df_states_year_month_100s<-rbindlist(df_states_year_month_100s)
df_states_month_time_bins_100s<-rbindlist(df_states_month_time_bins_100s)
df_country_year_100s<-rbindlist(df_country_year_100s)
df_country_year_month_100s<-rbindlist(df_country_year_month_100s)
df_country_month_time_bins_100s<-rbindlist(df_country_month_time_bins_100s)

saveRDS(df_states_years, "../../Tables/Observers_by_country_merged/rda/df_states_years.rda")
write.csv(df_states_years, "../../Tables/Observers_by_country_merged/csv/df_states_years.csv", row.names = F)

saveRDS(df_states_year_months, "../../Tables/Observers_by_country_merged/rda/df_states_year_months.rda")
write.csv(df_states_year_months, "../../Tables/Observers_by_country_merged/csv/df_states_year_months.csv", row.names = F)

saveRDS(df_states_month_time_binss, "../../Tables/Observers_by_country_merged/rda/df_states_month_time_binss.rda")
write.csv(df_states_month_time_binss, "../../Tables/Observers_by_country_merged/csv/df_states_month_time_binss.csv", row.names = F)

saveRDS(df_country_years, "../../Tables/Observers_by_country_merged/rda/df_country_years.rda")
write.csv(df_country_years, "../../Tables/Observers_by_country_merged/csv/df_country_years.csv", row.names = F)

saveRDS(df_country_year_months, "../../Tables/Observers_by_country_merged/rda/df_country_year_months.rda")
write.csv(df_country_year_months, "../../Tables/Observers_by_country_merged/csv/df_country_year_months.csv", row.names = F)

saveRDS(df_country_month_time_binss, "../../Tables/Observers_by_country_merged/rda/df_country_month_time_binss.rda")
write.csv(df_country_month_time_binss, "../../Tables/Observers_by_country_merged/csv/df_country_month_time_binss.csv", row.names = F)

saveRDS(df_states_year_100s, "../../Tables/Observers_by_country_merged/rda/df_states_year_100s.rda")
write.csv(df_states_year_100s, "../../Tables/Observers_by_country_merged/csv/df_states_year_100s.csv", row.names = F)

saveRDS(df_states_year_month_100s, "../../Tables/Observers_by_country_merged/rda/df_states_year_month_100s.rda")
write.csv(df_states_year_month_100s, "../../Tables/Observers_by_country_merged/csv/df_states_year_month_100s.csv", row.names = F)

saveRDS(df_states_month_time_bins_100s, "../../Tables/Observers_by_country_merged/rda/df_states_month_time_bins_100s.rda")
write.csv(df_states_month_time_bins_100s, "../../Tables/Observers_by_country_merged/csv/df_states_month_time_bins_100s.csv", row.names = F)

saveRDS(df_country_year_100s, "../../Tables/Observers_by_country_merged/rda/df_country_year_100s.rda")
write.csv(df_country_year_100s, "../../Tables/Observers_by_country_merged/csv/df_country_year_100s.csv", row.names = F)

saveRDS(df_country_year_month_100s, "../../Tables/Observers_by_country_merged/rda/df_country_year_month_100s.rda")
write.csv(df_country_year_month_100s, "../../Tables/Observers_by_country_merged/csv/df_country_year_month_100s.csv", row.names = F)

saveRDS(df_country_month_time_bins_100s, "../../Tables/Observers_by_country_merged/rda/df_country_month_time_bins_100s.rda")
write.csv(df_country_month_time_bins_100s, "../../Tables/Observers_by_country_merged/csv/df_country_month_time_bins_100s.csv", row.names = F)
