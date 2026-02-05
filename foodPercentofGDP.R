library(dplyr)
library(ggplot2)


# Use GINI values for each GCAM region from Kanishka's dataset, median household income from World Bank, 50% vulnerability indicator for food security(?)

# Kanishka's GINI datasets and mapping tools from https://zenodo.org/records/7093997

names2codes<-read.csv("FoodData/Datasets_on_net_income_deciles_ISO_region_global/mapping_files/GCAM_region_names.csv")

countries2regions<-read.csv("FoodData/Datasets_on_net_income_deciles_ISO_region_global/mapping_files/WIDER_mapping_file.csv")

names2codes %>% left_join(countries2regions,by=c("GCAM_region_ID")) ->regcountry

gini<-read.csv("FoodData/Datasets_on_net_income_deciles_ISO_region_global/final_datasets/Final_32_region_deciles_1958-2015.csv")

gini %>% left_join(names2codes,by="GCAM_region_ID")->gini

# World Bank median income data from https://data.worldbank.org/indicator/NY.ADJ.NNTY.PC.KD
# Adjusted net national income per capita (constant 2015 US$)

#"Adjusted net national income is GNI minus consumption of fixed capital and natural resources depletion. The core indicator has been divided by the general population to achieve a per capita estimate. This indicator is expressed in constant prices, meaning the series has been adjusted to account for price changes over time. The reference year for this adjustment is 2015. This indicator is expressed in United States dollars."

income<-read.csv("FoodData/NetIncome/AdjNetIncome.csv",skip=4)

income %>% select(Country.Name,Indicator.Name,X2015) %>% rename(country=Country.Name,Income2015=X2015) -> income
# rename some countries to match Kanishka's file

pop<-read.csv("FoodData/Population/Population.csv",skip=4)

pop %>% select(Country.Name,Indicator.Name,X2015) %>% rename(country=Country.Name,Population2015=X2015) -> pop

pop %>% inner_join(regcountry,by="country") %>% filter(country != "Venezuela" & country != "Malta" & country != "Turkmenistan")-> pop
pop %>% group_by(region) %>% summarize(regionalPop=sum(Population2015))->regpop

# Weight income by population to get weighted average regional income

regpop %>% left_join(pop,by="region") %>% filter(country != "Venezuela" & country != "Malta" & country != "Turkmenistan") %>%
  mutate(popfraction=Population2015/regionalPop) %>%
    left_join(income,by="country") %>% filter(country != "Venezuela" & country != "Malta" & country != "Turkmenistan") %>%
  mutate(weightedIncome=Income2015*popfraction,na.rm=T) %>% 
  group_by(region) %>% summarize(RegionalIncome=sum(weightedIncome)) -> regincome


# apply regional income to deciles in Kanishka's file


regincome %>% left_join(gini,by="region") %>% mutate(IncomeByDecile=region_shares*RegionalIncome) -> GINIincome




gdp<-read.csv("~/Downloads/gcam-v7.0-Mac_arm64-Release-Package/output/gdpmer_Reference.csv")

# convert from millions of 1990$ to 2015$
GINIincome %>% filter(year==2015) %>% select(-year)->GINIincome

gdp %>% mutate(gdp=value*1e6*1.81) %>% left_join(gcam_pop,by=c("region","year")) %>% mutate(gdp_percap=gdp/population) %>%
  select(year,region,gdp_percap) %>% filter(year==2015) %>%
  left_join(GINIincome,by=c("region")) %>% mutate(gdp_income_scalingfactor=IncomeByDecile/gdp_percap) %>%
  select(region,Category,gdp_income_scalingfactor)%>% unique()->incomeScalingFactor
# just in 2015 right now

# apply income scaling factor to per capita GDP through time
gdp %>% mutate(gdp=value*1e6*1.81) %>% left_join(gcam_pop,by=c("region","year")) %>% mutate(gdp_percap=gdp/population) %>%
  select(year,region,gdp_percap) %>% left_join(incomeScalingFactor,by=c("region")) %>% select(year,region,Category,gdp_income_scalingfactor,gdp_percap) %>%
  mutate(Income=gdp_income_scalingfactor*gdp_percap) -> incomeByDecilethroughtime

incomeByDecilethroughtime %>% write.csv("IncomeByDecileThroughTime.csv")


gcam_pop<-read.csv("~/Downloads/gcam-v7.0-Mac_arm64-Release-Package/output/population_Reference.csv")

gcam_pop %>% mutate(population=value*1000)->gcam_pop
# Now need to bring in money spent on food

################### Reference

## food demand

demand<-read.csv('~/Downloads/gcam-v7.0-Mac_arm64-Release-Package/output/fooddemand_Reference.csv')
demand=unique(demand)

# food demand prices
dp<-read.csv('~/Downloads/gcam-v7.0-Mac_arm64-Release-Package/output/fooddemandprice_Reference.csv')
dp=unique(dp)

################### Scenario 1

##### Species Richness

demand<-read.csv('~/Downloads/gcam-v7.0-Mac_arm64-Release-Package/output/fooddemand_Species Richness 30%.csv')
demand=unique(demand)

# food demand prices
dp<-read.csv('~/Downloads/gcam-v7.0-Mac_arm64-Release-Package/output/fooddemandprice_Species Richness 30%.csv')
dp=unique(dp)

###### Total Carbon

demand<-read.csv('~/Downloads/gcam-v7.0-Mac_arm64-Release-Package/output/fooddemand_Total Carbon 30%.csv')
demand=unique(demand)

# food demand prices
dp<-read.csv('~/Downloads/gcam-v7.0-Mac_arm64-Release-Package/output/fooddemandprice_Total Carbon 30%.csv')
dp=unique(dp)

################### Scenario 2

demand<-read.csv("~/Downloads/food_demand_agroforest.csv")
demand=unique(demand)

# food demand prices
dp<-read.csv('~/Downloads/food_demand_prices_agroforest.csv')
dp=unique(dp)

# adjust for Kanishka's scenarios

demand %>% select(-scenario) %>% rename(scenario=sce) -> demand
dp %>% select(-scenario) %>% rename(scenario=sce) -> dp

################### Scenario 3 (now 2)


##### S2p2T

# food demand

demand<-read.csv('~/Downloads/gcam-v7.0-Mac_arm64-Release-Package/output/fooddemand_Scenario 3 APC2 CT10_5.csv')
demand=unique(demand)
demand$scenario="S2p2T"

# food demand prices
dp<-read.csv('~/Downloads/gcam-v7.0-Mac_arm64-Release-Package/output/fooddemandprice_Scenario 3 APC2 CT10_5.csv')
dp=unique(dp)
dp$scenario="S2p2T"

####### S2p2

# food demand

demand<-read.csv('~/Downloads/gcam-v7.0-Mac_arm64-Release-Package/output/fooddemand_Ag Productivity x2.csv')
demand=unique(demand)
demand$scenario="S2p2"
# food demand prices
dp<-read.csv('~/Downloads/gcam-v7.0-Mac_arm64-Release-Package/output/fooddemandprice_Ag Productivity x2.csv')
dp=unique(dp)
dp$scenario="S2p2"
######

#############################################
# in 2005 $ per year
dp %>% mutate(price=value*365*1e6) %>% 
  left_join(demand,by=c("year","region","gcam.consumer","nodeinput","input","scenario")) %>%
  mutate(foodcost=value.y*price) %>% group_by(region,year,scenario) %>% 
  summarize(foodcost=sum(foodcost)) %>% mutate(foodcost=foodcost*1.21)->foodcost # sum nonstaples and staples, convert to 2015 $ from 2005 $


foodcost %>% left_join(gcam_pop,by=c("region","year")) %>% mutate(foodcost_percap=foodcost/population) %>%
  left_join(incomeByDecilethroughtime) %>% mutate(FoodFraction=foodcost_percap/Income) -> foodfraction

# Use this source for what share of income (they use expenditures) spent on food is considered vulnerabile/food insecure? : https://docs.wfp.org/api/documents/WFP-0000161494/download/

# Once we have a reliable threshold, this would be a better metric to use than simply the price increase of staple crops. 
# But, to find what subsidy is needed, we have to decide if we just care about some parts of the income distribution not having increases in price, or if we care more about how much money is spent on food (probably the latter)

# find out how much money it takes so that no one goes over 50% income spent on food (or some other percentage)
# Or change the number and get a range of subsidies for each scenario


### calculate food insecure population
foodfraction %>% mutate(difference=FoodFraction-0.5,NeededSubsidy=difference*Income) %>%
  filter(year>2015) %>% group_by(year,region,Category,scenario) %>%
  summarize(NeededSubsidy=sum(NeededSubsidy,na.rm=T)) %>% left_join(gcam_pop,by=c("region","year")) %>%
  filter(NeededSubsidy>0) %>% mutate(VulnerablePop=population*.1) %>% group_by(region,year,scenario) %>%
  summarize(VulnerablePop=sum(VulnerablePop)) -> vulnpop

###### to make distribution

foodfraction %>% mutate(difference=FoodFraction-0.5,NeededSubsidy=difference*Income) %>%
  filter(year>2015) %>% group_by(year,region,Category,scenario,Income) %>%
  summarize(NeededSubsidy=sum(NeededSubsidy,na.rm=T)) %>% left_join(gcam_pop,by=c("region","year")) %>%
  filter(NeededSubsidy>0) %>% mutate(VulnerablePop=population*.1) %>% group_by(region,year,scenario,Income) %>%
  summarize(VulnerablePop=sum(VulnerablePop)) -> npcbio
  
npcbio$scenario="NPC_bio"

foodfraction %>% mutate(difference=FoodFraction-0.5,NeededSubsidy=difference*Income) %>%
  filter(year>2015) %>% group_by(year,region,Category,scenario,Income) %>%
  summarize(NeededSubsidy=sum(NeededSubsidy,na.rm=T)) %>% left_join(gcam_pop,by=c("region","year")) %>%
  filter(NeededSubsidy>0) %>% mutate(VulnerablePop=population*.1) %>% group_by(region,year,scenario,Income) %>%
  summarize(VulnerablePop=sum(VulnerablePop)) ->npccarb

npccarb$scenario="NPC_carb"

foodfraction %>% mutate(difference=FoodFraction-0.5,NeededSubsidy=difference*Income) %>%
  filter(year>2015) %>% group_by(year,region,Category,scenario,Income) %>%
  summarize(NeededSubsidy=sum(NeededSubsidy,na.rm=T)) %>% left_join(gcam_pop,by=c("region","year")) %>%
  filter(NeededSubsidy>0) %>% mutate(VulnerablePop=population*.1) %>% group_by(region,year,scenario,Income) %>%
  summarize(VulnerablePop=sum(VulnerablePop)) -> farm

farm %>% filter(scenario=="2. Agroforestry policy" | scenario=="4. Agroforestry policy+ carbon tax") -> farm

farm %>% mutate(scenario=ifelse(scenario=="2. Agroforestry policy","Farm","Farm_plus"))->farm

foodfraction %>% mutate(difference=FoodFraction-0.5,NeededSubsidy=difference*Income) %>%
  filter(year>2015) %>% group_by(year,region,Category,scenario,Income) %>%
  summarize(NeededSubsidy=sum(NeededSubsidy,na.rm=T)) %>% left_join(gcam_pop,by=c("region","year")) %>%
  filter(NeededSubsidy>0) %>% mutate(VulnerablePop=population*.1) %>% group_by(region,year,scenario,Income) %>%
  summarize(VulnerablePop=sum(VulnerablePop)) -> pressplus

pressplus$scenario="Press_plus"

foodfraction %>% mutate(difference=FoodFraction-0.5,NeededSubsidy=difference*Income) %>%
  filter(year>2015) %>% group_by(year,region,Category,scenario,Income) %>%
  summarize(NeededSubsidy=sum(NeededSubsidy,na.rm=T)) %>% left_join(gcam_pop,by=c("region","year")) %>%
  filter(NeededSubsidy>0) %>% mutate(VulnerablePop=population*.1) %>% group_by(region,year,scenario,Income) %>%
  summarize(VulnerablePop=sum(VulnerablePop)) ->press

press$scenario="Press"


dist=rbind(npcbio,npccarb,farm,pressplus,press)

dist %>% mutate(region=ifelse(VulnerablePop<20000000,"Other",region)) %>% group_by(Income,region) %>%
  summarize(VulnerablePop=sum(VulnerablePop))->dist

ggplot(dist)+geom_density(aes(Income,fill=region))+facet_wrap(~region)


################ ################ pressplus
vulnpop->vpS2p2
vulnpop->vpS2p2T

vulnpop->vpaf
vpaf %>% select(region,year,VulnerablePop,scenario)->vpaf
vpaf %>% mutate(scenario=substr(scenario,4,nchar(scenario))) ->vpaf
vpaf %>% filter(scenario=="Agroforestry policy" | scenario=="Agroforestry policy+ carbon tax") %>%
  mutate(scenario=ifelse(scenario=="Agroforestry policy","S3p","S3pT")) ->vpaf

vulnpop->vpr
vpr$scenario="Reference"

vulnpop->vpsr
vpsr$scenario="S1sr"

vulnpop->vptc
vptc$scenario="S1tc"


vp=rbind(vpr,vpsr,vptc,vpS2p2,vpS2p2T,vpaf)


############ filter out specific years to plot **USE THIS CODE FOR FIGURE IN MANUSCRIPT


vp %>% filter(year>2020) %>% group_by(scenario,region,year) %>% summarize(vp=sum(VulnerablePop)) ->vp2
vp2 %>% filter(scenario=="Reference") -> vp2_ref
vp2 %>% filter(scenario != "Reference")->vp2
vp2 %>% left_join(vp2_ref,by=c("region","year")) %>% 
  mutate(vp.y=ifelse(is.na(scenario.y),0,vp.y), change=(vp.x-vp.y))->gvp

my_labeller <- as_labeller(c(S1sr="NPC[bio]", S1tc="NPC[carb]", S2p2="Press",S2p2T="Press[plus]",S3p="Farm",S3pT="Farm[plus]"),
                           default = label_parsed)


fig9b<-gvp %>%  rename(scenario=scenario.x) %>%
  mutate(scenario=factor(scenario,levels=c("S1sr","S2p2","S3p","S1tc","S2p2T","S3pT"))) %>%
    mutate(change=change/1e6) %>% mutate(region=ifelse(abs(change)<10,"Other",region)) %>%
  group_by(scenario,year,region) %>% summarize(change=sum(change)) %>%
  ggplot()+theme_bw()+
  geom_rect(xmin=-Inf,xmax=Inf,ymin=-Inf,ymax=0,fill="palegreen1",alpha=0.01)+
  geom_rect(xmin=-Inf,xmax=Inf,ymin=0,ymax=Inf,fill="lightpink1",alpha=0.01)+
  geom_col(aes(year,change,fill=region),color="black",lwd=.25)+
  facet_wrap(~scenario,labeller = my_labeller)+
  ylab("Millions of Food Insecure People vs. Reference Scenario")+
  labs(fill="Region")+
  theme(axis.text.x=element_text(angle=45,vjust=.75),axis.title=element_text(size=12),axis.text=element_text(size=10),legend.position="none")


# plot food vulnerable people in Reference scenario
fig9a<-vp2_ref %>% mutate(vp=vp/1e6) %>% mutate(region=ifelse(abs(vp)<50,"Other",region)) %>%
  group_by(year,region) %>% summarize(vp=sum(vp)) %>%
  ggplot()+theme_bw()+
  geom_rect(xmin=-Inf,xmax=Inf,ymin=0,ymax=Inf,fill="lightpink1",alpha=0.01)+
  geom_col(aes(year,vp,fill=region),color="black",lwd=.25) +
  ylab("Food Insecurity in Reference")+scale_y_continuous(expand=c(0,0),limits=c(0,1700))+
  labs(fill="Region")+theme(axis.title=element_text(size=12),axis.text=element_text(size=10))

cowplot::plot_grid(fig9a,fig9b,labels=c("a","b"),ncol=1,rel_heights = c(1.25,2))

ggsave(height=8.5,width=9,"foodinsecurity_Figure9_newlabels.pdf",dpi=300)


#################################### Subsidy Calculation ###################################################### 
##### S2p2T

# food demand

demand<-read.csv('~/Downloads/gcam-v7.0-Mac_arm64-Release-Package/output/fooddemand_Scenario 3 APC2 CT10_5.csv')
demand=unique(demand)
demand$scenario="S2p2T"

# food demand prices
dp<-read.csv('~/Downloads/gcam-v7.0-Mac_arm64-Release-Package/output/fooddemandprice_Scenario 3 APC2 CT10_5.csv')
dp=unique(dp)
dp$scenario="S2p2T"

####### S2p2

# food demand

demand<-read.csv('~/Downloads/gcam-v7.0-Mac_arm64-Release-Package/output/fooddemand_Ag Productivity x2.csv')
demand=unique(demand)
demand$scenario="S2p2"
# food demand prices
dp<-read.csv('~/Downloads/gcam-v7.0-Mac_arm64-Release-Package/output/fooddemandprice_Ag Productivity x2.csv')
dp=unique(dp)
dp$scenario="S2p2"
######

#############################################
# in 2005 $ per year
dp %>% mutate(price=value*365*1e6) %>% 
  left_join(demand,by=c("year","region","gcam.consumer","nodeinput","input","scenario")) %>%
  mutate(foodcost=value.y*price) %>% group_by(region,year,scenario) %>% 
  summarize(foodcost=sum(foodcost)) %>% mutate(foodcost=foodcost*1.21)->foodcost # sum nonstaples and staples, convert to 2015 $ from 2005 $


foodcost %>% left_join(gcam_pop,by=c("region","year")) %>% mutate(foodcost_percap=foodcost/population) %>%
  left_join(incomeByDecilethroughtime) %>% mutate(FoodFraction=foodcost_percap/Income) -> foodfraction

foodfraction %>% mutate(difference=FoodFraction-0.5,NeededSubsidy=difference*Income) %>%
  filter(year>2015) %>% group_by(year,region,Category,scenario) %>%
  summarize(NeededSubsidy=sum(NeededSubsidy,na.rm=T)) %>% left_join(gcam_pop,by=c("region","year")) %>%
  filter(NeededSubsidy>0) %>% mutate(Subsidy=population*0.1*NeededSubsidy) %>%
  group_by(year,scenario) %>% summarize(Subsidy=sum(Subsidy)) -> subsidy
                                                                                  
### change to another variable based on scenario name so we can bind later
subsidy->subS2p2
subsidy->subsidy_af
subsidy->subS2p2T
subsidy->subsidy_ref
subsidy->subsidy_sr
subsidy->subsidy_tc

subsidy_tc$scenario="S1tc"
subsidy_sr$scenario="S1sr"
subsidy_ref$scenario="Reference"
subsidy_af %>% select(year,Subsidy,scenario)->subsidy_af

subsidy_af %>% filter(scenario=="2. Agroforestry policy"|scenario=="4. Agroforestry policy+ carbon tax") %>%
  mutate(scenario=ifelse(scenario=="2. Agroforestry policy","S3p","S3pT"))->subsidy_af

subsidy=rbind(subS2p2,subS2p2T,subsidy_tc,subsidy_sr,subsidy_ref,subsidy_af)

# convert to billions of 2025$

subsidy %>% mutate(Subsidy2025B=(Subsidy/1e9)*1.36) ->subsidy2025

subsidy2025 %>% filter(scenario=="Reference") -> sub_ref
subsidy2025 %>% filter(scenario != "Reference")->sub
sub %>% left_join(sub_ref,by=c("year")) %>% mutate(change=(Subsidy2025B.x-Subsidy2025B.y)) %>%
  rename(scenario=scenario.x)->sub1

########################## Make Figure 10

sub1 %>% mutate(tax=ifelse(substr(scenario,nchar(scenario),nchar(scenario))=="T","Yes","No")) %>%
  mutate(`Scenario Family`=case_when(scenario=="S1tc"~"Nations Prioritize Conservation",
                                     scenario=="S1sr"~"Nations Prioritize Conservation",
                                     scenario=="S2p2"~"People Reduce Pressure",
                                     scenario=="S2p2T"~"People Reduce Pressure",
                                     scenario=="S3p"~"Nature-Friendly Farming",
                                     scenario=="S3pT"~"Nature-Friendly Farming",
                                     .default=scenario)) ->sub1

sub1 %>% filter(change>0) ->sub2

ggplot(sub2)+geom_line(aes(year,change,col=`Scenario Family`,linetype=tax,group=scenario),size=1)+
  xlim(2025,2100) + ylab(stringr::str_wrap("Subsidy to Prevent Additional Food Insecurity (billion USD)",width=50))+
  theme_bw()+labs(linetype="Carbon Tax")+scale_y_continuous(expand=c(0,0))+
  #theme(axis.title=element_text(size=12),axis.text=element_text(size=10))+
  scale_x_continuous(expand=c(0,0),breaks=c(2025,2050,2075,2100),limits=c(2025,2100))+
  scale_color_manual(values=c("#F8766D","#00BA38","#619CFF"))

ggsave("subsidyfoodinsecurity_Figure10_newlabels.pdf",height=3.5, width=6,dpi=300)

#### try to linearly interpolate subsidy values (not difference from Reference) to use in accounting

                      year=seq(2025,2100,1)
                      
                      annual=as.data.frame(matrix(ncol=2))
                      colnames(annual)=c("year","scenario")
                      block=c()
                      
                      for (i in 2025:2100){
                        block=data.frame(year=c(i,i,i,i,i,i,i,i,i),scenario=c("APC x2 + LMD","APC x1p5 + LMD","LMD","Total Carbon 30%","Species Richness 30%",
                                                                           "1. Agroforestry default","2. Agroforestry policy","3. Agroforestry default+ carbon tax",
                                                                           "4. Agroforestry policy+ carbon tax"))
                        
                        annual=rbind(block,annual)
                      }
                      
                      as.data.frame(annual) %>% left_join(sub,by=c("year","scenario")) %>% group_by(scenario) %>% filter(!is.na(scenario)) %>%
                        mutate(InterpSubsidy=zoo::na.approx(Subsidy2025B,year)) %>% arrange(year) %>%
                        select(year,scenario,InterpSubsidy) %>% tidyr::spread(scenario,InterpSubsidy) %>% write.csv("~/Downloads/requiredsubsidyfoodinsecurity.csv")



