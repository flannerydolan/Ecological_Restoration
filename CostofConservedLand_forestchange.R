# Plot area and change of Unmanaged Forests to determine necessary carbon tax. 

la2ct<-read.csv("/Users/fdolan/Downloads/gcam-v7.0-Mac_arm64-Release-Package/output/landallocation_Scenario 1 Species Richness 30.csv")

ref<-la2ct %>% filter(year==2015,landleaf=="forest (unmanaged)") %>%
        select(-year) %>% rename(historic=value)

la2ct %>% filter(landleaf=="forest (unmanaged)") %>% mutate(presentyear=value,lastyear=lag(value),diff=presentyear-lastyear) %>%
  left_join(ref,by=c("landleaf","region","Units")) %>%
  mutate(change=value-historic) %>%
  mutate(color=ifelse(diff>0,"blue","red")) %>%
  ggplot()+geom_col(aes(year,change,fill=color))+facet_wrap(~region,scales="free_y")+
  scale_fill_manual(values=c("blue","red"))+theme(legend.position="none")+
  ylab("Difference in Area of Unmanaged Forests Between Timesteps (thous km2)")+xlim(2015,2100)
  


lpr %>% tidyr::separate(landleaf,into=c("landleaf","region","irr","mgmt"),sep="_")->lpr1

unique(lpr1$landleaf)

# find cost of land put aside (bought by governments) in Scenario 1 Total Carbon

lpr1 %>% filter(year==2025) %>%
  filter(landleaf=="Grassland"| landleaf=="Shrubland"|landleaf=="UnmanagedForest"|
                  landleaf=="UnmanagedPasture") %>% group_by(landleaf,region) %>%
  summarize(value=mean(value)) -> lpr2


lpr2 %>% filter(region=="AmazonR" | region=="TocantinsR"| region=="RioLaPlata") %>%
   ungroup() %>% summarize(value=mean(value))

# brazil=3358296*3250=$10.9B

lpr2 %>% filter(region=="CongoR"|region=="GuineaGulf"|region=="AfrCstW") %>%
  ungroup() %>% summarize(value=mean(value)) 

# africa western= 1065357*3500=$3.7B

lpr2 %>% filter(landleaf=="UnmanagedForest",region=="YeniseiR"| region=="BarentsSea"|region=="ObR"|
                  region=="LenaR"|region=="AmurR"|region=="VolgaR") %>%
  ungroup() %>% summarize(value=mean(value)) 

# russia = 6651580 * 2500 = $16.6B

lpr2 %>% filter(region=="AmazonR") %>% ungroup() %>% summarize(value=mean(value))

# south america southern: 5423742 * 1500 = $8.1B


lpr2 %>% filter(region=="Mackenzie"|region=="HudsonBay"|
                  region=="NWTerr"|region=="PacArctic") %>%
  ungroup() %>% summarize(value=mean(value)) 


# canada= 2514581*900=$2.3B

lpr2 %>% filter(region=="OrinocoR"|region=="SAmerCstNE") %>% 
  ungroup() %>% summarize(value=mean(value)) 

# san= 10531898*400 = $4.1B

lpr2 %>% filter(region=="PacArctic"|region=="UsaPacNW"|region=="MexCstNW") %>% 
  ungroup() %>% summarize(value=mean(value)) 

# USA= 10430257*500=$5.2B

lpr2 %>% filter(region=="Kalimantan"|region=="IrianJaya"|region=="Sumatra") %>% 
  ungroup() %>% summarize(value=mean(value)) 

# Indonesia= 18005394*500= $8.9B

lpr2 %>% filter(region=="ZambeziR") %>% 
  ungroup() %>% summarize(value=mean(value)) 

# Africa Southern=2304966*1000=$2.1B

lpr2 %>% filter(landleaf=="UnmanagedPasture",region=="Yangtze") %>% 
  ungroup() %>% summarize(value=mean(value)) 

# china = 16199000*400=$6.4B

lpr2 %>% filter(landleaf=="UnmanagedPasture",region=="NileR"|region=="ShebJubR") %>% 
  ungroup() %>% summarize(value=mean(value)) 

# africa eastern = 1789126 * 2000 = $3.6B

lpr2 %>% filter(region=="AmazonR") %>% 
  ungroup() %>% summarize(value=mean(value)) 

# columbia =5423742*250= $1.3B

lpr2 %>% filter(region=="AusCstE") %>% 
  ungroup() %>% summarize(value=mean(value)) 
#aus=3754620*500= $1.9B

lpr2 %>% filter(region=="PapuaCst"|region=="BorneoCstN") %>% 
  ungroup() %>% summarize(value=mean(value)) 

# southeast asia = 7295440*300=$2.2B

# total is $77.3B in 1975$ which is $462B in 2025$

########## Species Richness

lpr2 %>% filter(region=="AmazonR" | region=="TocantinsR"| region=="RioLaPlata"|region=="BrzCstN") %>%
  ungroup() %>% summarize(value=mean(value))

lpr2 %>% filter(region=="CongoR"|region=="GuineaGulf"|region=="AfrCstW"|region=="NigerR") %>%
  ungroup() %>% summarize(value=mean(value)) 

lpr2 %>% filter(region=="NileR",landleaf=="UnmanagedPasture")

lpr2 %>% filter(region=="ZambeziR"|region=="AfrCstE") %>% 
  ungroup() %>% summarize(value=mean(value)) 

lpr2 %>% filter(region=="AusCstN"|region=="AusInt") %>% 
  ungroup() %>% summarize(value=mean(value)) 

lpr2 %>% filter(region=="Yangtze",landleaf=="UnmanagedPasture")

lpr2 %>% filter(region=="GangesR")%>% 
  ungroup() %>% summarize(value=mean(value)) 

lpr2 %>% filter(landleaf=="UnmanagedForest",region=="ObR"|region=="YeniseiR"|region=="LenaR") %>%
  ungroup() %>% summarize(value=mean(value)) 

lpr2 %>% filter(region=="OrinocoR"|region=="SAmerCstNE") %>%
  ungroup() %>% summarize(value=mean(value)) 
