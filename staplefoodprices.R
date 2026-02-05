library(dplyr)
library(ggplot2)


############################################ Scenario 2 (now 3 in draft) ####################################################################

price2<-read.csv("/Users/fdolan/Downloads/ag_commodity_prices_agroforest.csv")

price2 %>% dplyr::select(Units,region,year,sce,sector,value) %>% rename(scenario=sce) %>%
  mutate(scenario=substr(scenario,4,nchar(scenario)))->price2

price2 %>% filter(sector=="OtherGrain"|
               sector=="Wheat"|sector=="Rice"|sector=="Corn"|
               sector=="RootTuber") ->staple2



############################################ Scenario 3 (now 2 in draft) ####################################################################

##### ag prices


scale2T<-read.csv("/Users/fdolan/Downloads/gcam-v7.0-Mac_arm64-Release-Package/output/agprices_Scenario 3 APC2 CT10_5.csv")
scale2T<-scale2T %>% mutate(scenario="S2p2T")

scale2<-read.csv("/Users/fdolan/Downloads/gcam-v7.0-Mac_arm64-Release-Package/output/agprices_Ag Productivity x2.csv")
scale2<-scale2 %>% mutate(scenario="S2p2")


ref<-read.csv("/Users/fdolan/Downloads/gcam-v7.0-Mac_arm64-Release-Package/output/agprices_Reference.csv")
ref=unique(ref)
ref<-ref %>% mutate(scenario="Reference") 

p=rbind(scale2T,scale2,ref)

p %>% filter(sector=="OtherGrain"|
               sector=="Wheat"|sector=="Rice"|sector=="Corn"|
               sector=="RootTuber") ->staple3



############################################ Scenario 1 ####################################################################

sr<-read.csv("/Users/fdolan/Downloads/gcam-v7.0-Mac_arm64-Release-Package/output/agprices_Species Richness 30%.csv")
sr<-sr %>% mutate(scenario="S1sr")

tc<-read.csv("/Users/fdolan/Downloads/gcam-v7.0-Mac_arm64-Release-Package/output/agprices_Total Carbon 30%.csv")
tc<-tc %>% mutate(scenario="S1tc")


p=rbind(sr,tc)

p %>% filter(sector=="Corn"|sector=="OtherGrain"|sector=="Rice"|sector=="Wheat"|sector=="RootTuber") ->staple1



#############################################################################################################

# COMBINE SCENARIOS

staple=rbind(staple1,staple2,staple3)

# Compare to Reference scenario 

 staple %>% filter(scenario=="Reference")->stapleRef
 
 staple %>% filter(scenario != "Reference")->noRef

 noRef %>% left_join(stapleRef,by=c("Units","region","sector","year")) %>%
   mutate(d=((value.x-value.y)/value.y)*100) %>%
   group_by(scenario.x,year,region) %>% summarize(d=mean(d,na.rm=T))->t
 
t %>% filter(scenario.x !="Agroforestry default+ carbon tax" & scenario.x != "Agroforestry default" &
               scenario.x != "Total Carbon 30% with Carbon Tax")->t 

t %>% rename(scenario=scenario.x) %>%
  mutate(scenario=case_when(scenario=="Agroforestry policy+ carbon tax"~"Farm_plus",
                                    scenario=="Agroforestry policy"~"Farm",
                            scenario=="S2p2T"~"Press_plus",
                            scenario=="S2p2"~"Press",
                            scenario=="S1tc"~"NPC_carb",
                            scenario=="S1sr"~"NPC_bio",
                                    .default=scenario)) %>%
  mutate(scenario_family=case_when(scenario=="Reference" ~ "Ref",
                                   scenario=="NPC_carb"~"Nations Prioritize Conservation",
                                   scenario=="NPC_bio"~"Nations Prioritize Conservation",
                                   scenario=="Farm"~"Nature-Friendly Farming",
                                   scenario=="Farm_plus"~"Nature-Friendly Farming",
                                   scenario=="Press"~"People Reduce Pressure",
                                   scenario=="Press_plus"~"People Reduce Pressure",
                                   .default = scenario)) -> t

t %>% filter(year>2020) %>% group_by(year,scenario,scenario_family) %>% summarize(PercentChange=mean(d)) %>% write.csv('global_percentChange_StapleCrops.csv')
 
t %>% mutate(tax=ifelse(substr(scenario,nchar(scenario)-3,nchar(scenario))=="plus","Yes","No")) -> t1

t1 %>% filter(region=="Africa_Eastern" | region=="Africa_Western"|
                region=="Australia_NZ" | region=="Brazil" | region=="Russia" | region=="USA") %>%
  mutate(Income=ifelse(region=="Africa_Eastern"|region=="Africa_Western","Low",
                       ifelse(region=="Brazil"|region=="Russia","Upper-middle","High")))%>%
  mutate(region=factor(region,levels=c("Africa_Eastern","Africa_Western","Brazil","Russia","Australia_NZ","USA"))) ->t2


ggplot(t2)+
  #geom_rect(aes(fill=Income,xmin=-Inf,xmax=Inf,ymin=-Inf,ymax=Inf),alpha=0.05)+scale_fill_brewer(palette="Dark2")+
  geom_line(aes(year,d,col=scenario_family,linetype=tax,group=scenario),size=1)+
   coord_cartesian(xlim=c(2015,2100))+
   scale_y_continuous(transform="pseudo_log",breaks=c(-10,0,10,100,500))+
   xlab("year")+ylab("Percent Change in Average Price of Staple Crops Over Reference")+
   #theme_bw()+
   facet_wrap(~region,scales="free_y",ncol=2)+
  theme(axis.title = element_text(size=14))+
   labs(color="Scenario Family",linetype="Carbon Tax")
 
###########################
aw<-t2 %>% filter(region=="Africa_Western") %>% ggplot()+
  geom_line(aes(year,d,col=scenario_family,linetype=tax,group=scenario),size=1)+
  coord_cartesian(xlim=c(2015,2100))+
  scale_y_continuous(transform="pseudo_log",breaks=c(-10,0,10,100,500))+
  xlab("")+ylab("")+
  theme_bw()+
  facet_wrap(~region,scales="free_y",ncol=2)+
  theme(axis.title = element_text(size=14),strip.background = element_rect(fill = "#1b9e77"),strip.text = element_text(color = "white"),legend.position = "none")

ae<-t2 %>% filter(region=="Africa_Eastern") %>% ggplot()+
  geom_line(aes(year,d,col=scenario_family,linetype=tax,group=scenario),size=1)+
  coord_cartesian(xlim=c(2015,2100))+
  scale_y_continuous(transform="pseudo_log",breaks=c(-10,0,10,100,500))+
  xlab("")+ylab("")+
  theme_bw()+
  facet_wrap(~region,scales="free_y",ncol=2)+
  theme(axis.title = element_text(size=14),strip.background = element_rect(fill = "#1b9e77"),strip.text = element_text(color = "white"),legend.position="none")

br<-t2 %>% filter(region=="Brazil") %>% ggplot()+
  geom_line(aes(year,d,col=scenario_family,linetype=tax,group=scenario),size=1)+
  coord_cartesian(xlim=c(2015,2100))+
  scale_y_continuous(transform="pseudo_log",breaks=c(-10,0,10,100,500))+
  xlab("")+ylab("")+
  theme_bw()+
  facet_wrap(~region,scales="free_y",ncol=2)+
  theme(axis.title = element_text(size=14),strip.background = element_rect(fill = "#d95f02"),strip.text = element_text(color = "white"),legend.position="none")

ru<-t2 %>% filter(region=="Russia") %>% ggplot()+
  geom_line(aes(year,d,col=scenario_family,linetype=tax,group=scenario),size=1)+
  coord_cartesian(xlim=c(2015,2100))+
  scale_y_continuous(transform="pseudo_log",breaks=c(-10,0,10,100,500))+
  xlab("")+ylab("")+
  theme_bw()+
  facet_wrap(~region,scales="free_y",ncol=2)+
  theme(axis.title = element_text(size=14),strip.background = element_rect(fill = "#d95f02"),strip.text = element_text(color = "white"),legend.position="none")

az<-t2 %>% filter(region=="Australia_NZ") %>% ggplot()+
  geom_line(aes(year,d,col=scenario_family,linetype=tax,group=scenario),size=1)+
  coord_cartesian(xlim=c(2015,2100))+
  scale_y_continuous(transform="pseudo_log",breaks=c(-10,0,10,100,500))+
  xlab("")+ylab("")+
  theme_bw()+
  facet_wrap(~region,scales="free_y",ncol=2)+
  theme(axis.title = element_text(size=14),strip.background = element_rect(fill = "#7570b3"),strip.text = element_text(color = "white"),legend.position="none")

us<-t2 %>% filter(region=="USA") %>% ggplot()+
  geom_line(aes(year,d,col=scenario_family,linetype=tax,group=scenario),size=1)+
  coord_cartesian(xlim=c(2015,2100))+
  scale_y_continuous(transform="pseudo_log",breaks=c(-10,0,10,100,500))+
  xlab("")+ylab("")+
  theme_bw()+
  facet_wrap(~region,scales="free_y",ncol=2)+
  theme(axis.title = element_text(size=14),strip.background = element_rect(fill = "#7570b3"),strip.text = element_text(color = "white"),legend.position = "none")#,legend.box="horizontal")+
  #labs(linetype="Carbon Tax",color="Scenario Family")+guides(color = guide_legend(nrow = 1),linetype=guide_legend(nrow=1))

leg<-t2 %>%  ggplot()+
  geom_line(aes(year,d,col=scenario_family,linetype=tax,group=scenario),size=1)+
  geom_col(aes(year,d,fill=Income))+
  coord_cartesian(xlim=c(2015,2100))+
  scale_y_continuous(transform="pseudo_log",breaks=c(-10,0,10,100,500))+
  xlab("")+ylab("")+
  theme_bw()+
  facet_wrap(~region,scales="free_y",ncol=2)+
  theme(axis.title = element_text(size=14),strip.background = element_rect(fill = "#7570b3"),legend.box="horizontal")+
labs(linetype="Carbon Tax",color="Scenario Family",fill="Income Classification")+scale_fill_manual(values=c("#1b9e77","#d95f02","#7570b3"),labels=c("Low","Upper-middle","High"))+
  guides(color = guide_legend(nrow = 3,order=1),linetype=guide_legend(nrow=3,order=2))

legend3<-cowplot::get_legend(leg)
#legend2<-cowplot::get_legend(us)
#legend<-cowplot::get_legend(us)

fig7<-gridExtra::grid.arrange(gridExtra::arrangeGrob(aw,ae,br,ru,az,us,legend3,ncol=2,nrow = 4,layout_matrix=rbind(c(1,2),c(3,4),c(5,6),7),heights=c(1,1,1,.35)),
                        left=grid::textGrob("Percent Increase in Average Price of Staple Crops", rot = 90,gp=grid::gpar(fontsize=14)))

ggsave("staplecropprices_Figure7_newlabels.pdf",fig7,dpi=300,height=9,width=7)
 
 ############################################################ join with area conserved to plot figure 8
 
 # plot by income classification
 # run code in meatdemand.R
 
 meat %>% select(region,year,`Income Classification`) ->m
 
 # use this one for the paper!
 area<-read.csv("land_conserved.csv")
 
 area %>%  mutate(scenario=case_when(scenario=="Species Richness 30%"~"NPC_bio",
                                     scenario=="Total Carbon 30%"~"NPC_carb",
                                     .default = scenario)) ->area
 
 area %>% left_join(t,by=c("scenario","region")) %>% filter(year>2020) %>%
   filter(region=="Brazil"|region=="Africa_Eastern"|region=="Africa_Western"|region=="Africa_Southern"|region=="Russia"|region=="Australia_NZ") %>%
   filter(year==2025 | year==2050 | year==2100) %>%
   mutate(Group=stringr::str_c(region,year)) %>%
   left_join(m,by=c("region","year")) %>%
   mutate(`Income Classification`=factor(`Income Classification`,levels=c("Low","Upper-middle","High")),area=area/1000,year=factor(as.character(year),levels=c("2025","2050","2100"))) %>%
   ggplot()+geom_point(aes(area,d,color=`Income Classification`,shape=region),size=2.5) + 
   geom_line(aes(area,d,color=`Income Classification`,group=Group,linetype=year)) + scale_color_brewer(palette="Dark2") + theme_bw()+#viridis::scale_color_viridis(option="C",discrete=T)+theme_bw()+
   ylab("Change in Price of Staple Crops Over Reference (%)")+xlab(expression(Land~Area~Set~Aside~'in 2025'~(million~km^{"2"})))+labs(linetype="Year",shape="Region")
 
 ggsave("landareaconserved_cropprices_Figure8_newlabels.pdf",dpi=300,height=4.75,width=6)
 
 