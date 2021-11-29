library(sf)
library(ggplot2)
library(data.table)
setwd("/media/huijieqiao/WD12T/eBird/Script/ebird_migration")
species_loss_gain<-readRDS("../../eBird_Pendemic_2021/Figures/LOSS_GAIN/species_loss_gain.rda")
species_details<-readRDS("../../eBird_Pendemic_2021/Figures/LOSS_GAIN/species_details.rda")

target_countries<-readRDS("../../eBird_Pendemic_2021/Tables/target_countries.rda")


df<-species_details[TYPE=="Number of events"]
#df<-df[COUNTRY %in% target_countries]
#species_loss_gain<-species_loss_gain[COUNTRY %in% target_countries]
species_loss_gain$P_GAIN_2020<-species_loss_gain$N_GAIN_2020/species_loss_gain$N_ALL_SPECIES*100
species_loss_gain$P_LOSS_2020<-species_loss_gain$N_LOSS_2020/species_loss_gain$N_ALL_SPECIES*100

p<-ggplot(df)+geom_boxplot(aes(x=LOSS_GAIN_2020, Value))+
  scale_y_log10()+
  xlab("Status")+ylab("Number of events")+
  theme_bw()
p
ggsave(p, filename="../../eBird_Pendemic_2021/Figures/LOSS_GAIN/boxplot_number_of_events.png", width=6, height=3)

df_se<-df[, .(N_Species=.N, N_Events=sum(Value)),
          by=list(LOSS_GAIN_2020, COUNTRY)]
p<-ggplot(df_se)+geom_boxplot(aes(x=LOSS_GAIN_2020, N_Species))+
  scale_y_log10()+
  xlab("Status")+ylab("Number of species")+
  theme_bw()
p
ggsave(p, filename="../../eBird_Pendemic_2021/Figures/LOSS_GAIN/boxplot_number_of_species.png",
       width=5, height=3)
cols<-c("COUNTRY", "N_ALL_SPECIES")

df_se_1<-merge(df_se, unique(species_loss_gain[, ..cols]), by="COUNTRY")
df_se_1$P_Species<-df_se_1$N_Species/df_se_1$N_ALL_SPECIES*100
p<-ggplot(df_se_1)+geom_boxplot(aes(x=LOSS_GAIN_2020, P_Species))+
  scale_y_log10()+
  xlab("Status")+ylab("Percentage  of species (%)")+
  theme_bw()
p
ggsave(p, filename="../../eBird_Pendemic_2021/Figures/LOSS_GAIN/boxplot_number_of_species_p.png",
       width=5, height=3)


p<-ggplot(species_loss_gain)+
  geom_point(aes(x=N_LOSS_2020, y=N_GAIN_2020, color=COUNTRY))+
  geom_hline(yintercept = 40, linetype=2)+
  geom_vline(xintercept = 40, linetype=2)+
  scale_x_sqrt()+
  scale_y_sqrt()+
  theme_bw()+
  theme(legend.position = "none")+
  xlab("LOSS")+ylab("GAIN")
p
ggsave(p, filename="../../eBird_Pendemic_2021/Figures/LOSS_GAIN/loss_vs_gain.png")


GDPS<-data.table(
  read.csv("../../eBird_Pendemic_2021/Tables/COVID-19/GDPS.csv", stringsAsFactors = F))
species_loss_gain[COUNTRY=="Virgin Islands (British)"]$COUNTRY<-"British Virgin Islands"
species_loss_gain[COUNTRY=="CuraÃ§ao"]$COUNTRY<-"Curacao"
species_loss_gain[COUNTRY=="Congo"]$COUNTRY<-"Republic of the Congo"
species_loss_gain[COUNTRY=="American Samoa"]$COUNTRY<-"Samoa"

species_loss_gain_with_GDP<-merge(species_loss_gain, GDPS, 
                                  by.x="COUNTRY", by.y="country", all.x=T)
ebird_country<-unique(species_loss_gain_with_GDP[is.na(gdpPerCapita)]$COUNTRY)
species_loss_gain_with_GDP<-species_loss_gain_with_GDP[!is.na(gdpPerCapita)]

threshold_loss<-quantile(species_loss_gain$P_LOSS_2020, c(0.01, 0.99))
threshold_gain<-quantile(species_loss_gain$P_GAIN_2020, c(0.01, 0.99))
countries<-species_loss_gain[(P_LOSS_2020<=threshold_loss[1])|
                               (P_LOSS_2020>=threshold_loss[2])|
                               (P_GAIN_2020<=threshold_gain[1])|
                               (P_GAIN_2020>=threshold_gain[2])]$COUNTRY
countries<-c(countries, c("United Kingdom", "Madagascar", "Indonesia", "Brazil",
                          "United States", "Greenland", "Italy", "China",
                          "Tuvalu", "Heard island", "Papua New Guinea", 
                          "Liberia", "Tanzania", "Kenya"))
label_item<-species_loss_gain[COUNTRY %in% countries]
label_item<-label_item[(P_GAIN_2020>0)|
                         (COUNTRY %in% c("Tuvalu", "Heard island"))]
p1<-ggplot(species_loss_gain)+
  geom_hline(yintercept = 5, linetype=2, color="grey", alpha=0.8)+
  geom_vline(xintercept = 5, linetype=2, color="grey", alpha=0.8)+
  
  geom_point(aes(x=P_LOSS_2020, y=P_GAIN_2020, size=N_ALL_SPECIES), 
             color="grey", alpha=0.5)+
  geom_point(data=label_item, 
             aes(x=P_LOSS_2020, y=P_GAIN_2020, size=N_ALL_SPECIES), 
             color="pink", alpha=0.5)+
  #geom_text(data=species_loss_gain, aes(x=P_LOSS_2020, y=P_GAIN_2020, label=COUNTRY),
  #          position = position_dodge(width = 1),
  #          vjust = -0.7)+
  geom_text(data=label_item, aes(x=P_LOSS_2020, y=P_GAIN_2020, label=COUNTRY),
            position = position_dodge(width = 1),
            vjust = -0.7)+
  scale_x_sqrt(breaks=c(1, 5, 10, seq(0, 100, 20)))+
  scale_y_sqrt(breaks=c(1, 5, 10, seq(0, 100, 20)))+
  theme_bw()+
  theme(legend.position = "none", 
        panel.grid.major = element_blank(), 
        panel.grid.minor=element_blank())+
  xlab("LOSS (%)")+ylab("GAIN (%)")
p1
ggsave(p1, filename="../../eBird_Pendemic_2021/Figures/LOSS_GAIN/loss_vs_gain_p.png")

label_item<-species_loss_gain_with_GDP[COUNTRY %in% countries]
label_item<-label_item[(P_GAIN_2020>0)|
                         (COUNTRY %in% c("Tuvalu", "Heard island"))]

p2<-ggplot(species_loss_gain_with_GDP)+
  geom_hline(yintercept = 5, linetype=2, color="grey", alpha=0.8)+
  geom_vline(xintercept = 5, linetype=2, color="grey", alpha=0.8)+
  
  geom_point(aes(x=P_LOSS_2020, y=P_GAIN_2020, size=gdpPerCapita), 
             color="grey", alpha=0.5)+
  geom_point(data=label_item, 
             aes(x=P_LOSS_2020, y=P_GAIN_2020, size=gdpPerCapita), 
             color="pink", alpha=0.5)+
  #geom_text(data=species_loss_gain, aes(x=P_LOSS_2020, y=P_GAIN_2020, label=COUNTRY),
  #          position = position_dodge(width = 1),
  #          vjust = -0.7)+
  geom_text(data=label_item, aes(x=P_LOSS_2020, y=P_GAIN_2020, label=COUNTRY),
            position = position_dodge(width = 1),
            vjust = -0.7)+
  
  scale_x_sqrt(breaks=c(1, 5, 10, seq(0, 100, 20)))+
  scale_y_sqrt(breaks=c(1, 5, 10, seq(0, 100, 20)))+
  theme_bw()+
  theme(legend.position = "none", 
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor=element_blank())+
  
  xlab("LOSS (%)")+ylab("GAIN (%)")
p2
ggsave(p2, filename="../../eBird_Pendemic_2021/Figures/LOSS_GAIN/loss_vs_gain_p_gdpc.png")
p<-ggpubr::ggarrange(p1, p2)
ggsave(p, 
       filename="../../eBird_Pendemic_2021/Figures/LOSS_GAIN/loss_vs_gain_p_gdpc_and_richness.png",
       width=12, height=6)

p<-ggplot(species_loss_gain_with_GDP)+
  geom_point(aes(x=gdpPerCapita, y=P_GAIN_2020, size=N_ALL_SPECIES), color="red")+
  geom_smooth(aes(x=gdpPerCapita, y=P_GAIN_2020), color="red")+
  geom_point(aes(x=gdpPerCapita, y=P_LOSS_2020, size=N_ALL_SPECIES), color="blue")+
  geom_smooth(aes(x=gdpPerCapita, y=P_LOSS_2020), color="blue")+
  #scale_y_sqrt()+
  #scale_x_log10()+
  theme_bw()+
  theme(legend.position = "none")+
  xlab("GDPC")+ylab("GAIN (red) and LOSS (blue)")
p
ggsave(p, filename="../../eBird_Pendemic_2021/Figures/LOSS_GAIN/loss_vs_gain_p_gdpc_lm.png")

p<-ggplot(species_loss_gain_with_GDP)+
  geom_point(aes(x=gdpPerCapita, y=N_GAIN_2020, size=N_ALL_SPECIES), color="red")+
  geom_smooth(aes(x=gdpPerCapita, y=N_GAIN_2020), color="red")+
  geom_point(aes(x=gdpPerCapita, y=N_LOSS_2020, size=N_ALL_SPECIES), color="blue")+
  geom_smooth(aes(x=gdpPerCapita, y=N_LOSS_2020), color="blue")+
  scale_y_sqrt()+
  scale_x_log10()+
  theme_bw()+
  
  theme(legend.position = "none")+
  xlab("GDPC")+ylab("GAIN (red) and LOSS (blue)")+
  ylim(0, 100)
p
ggsave(p, filename="../../eBird_Pendemic_2021/Figures/LOSS_GAIN/loss_vs_gain_N_gdpc_lm.png")




df_loss<-df[LOSS_GAIN_2020=="LOSS"]
colnames(df_loss)<-c("SCIENTIFIC_NAME", "Value_LOSS", "l1", "l2", "COUNTRY", "l3")
df_gain<-df[LOSS_GAIN_2020=="GAIN"]
colnames(df_gain)<-c("SCIENTIFIC_NAME", "Value_GAIN", "g1", "g2", "COUNTRY", "g3")
df_loss_gain<-merge(df_gain, df_loss, by=c("SCIENTIFIC_NAME", "COUNTRY"), all=T)
df_loss_gain[is.na(Value_GAIN)]$Value_GAIN<-0
df_loss_gain[is.na(Value_LOSS)]$Value_LOSS<-0
df_loss_gain_se<-df_loss_gain[, .(Value_GAIN=mean(Value_GAIN),
                                  Value_GAIN_SD=sd(Value_GAIN),
                                  Value_LOSS=mean(Value_LOSS),
                                  Value_LOSS_SD=sd(Value_LOSS)),
                              by=list(COUNTRY)]

p<-ggplot(df_loss_gain_se)+geom_point(aes(x=Value_LOSS, 
                                       y=Value_GAIN, 
                                       color=COUNTRY))+
  geom_hline(yintercept = 5, linetype=2)+
  geom_vline(xintercept = 5, linetype=2)+
  scale_x_sqrt()+
  scale_y_sqrt()+
  theme_bw()+
  theme(legend.position = "none")
ggsave(p, filename="../../eBird_Pendemic_2021/Figures/LOSS_GAIN/loss_vs_gain_in_value.png")



df_se<-df[, .(Value=mean(Value), SD_Value=sd(Value)),
          by=list(COUNTRY, LOSS_GAIN_2020)]
df_se$COUNTRY<-factor(df_se$COUNTRY, levels=unique(df$COUNTRY), ordered=F)

setorderv(df_se, c("LOSS_GAIN_2020", "Value"), c(-1, 1))
p<-ggplot(df_se)+geom_point(aes(x=COUNTRY, y=Value, color=LOSS_GAIN_2020))+
  scale_y_log10()+
  xlab("Country")+ylab("Number of events")+
  scale_x_discrete(labels =unique(df_se$COUNTRY))+
  theme_bw()
p
