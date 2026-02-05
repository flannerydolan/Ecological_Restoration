library(rgdal)

shp<-readOGR('~/Downloads/gcam_boundaries_moirai_3p1_0p5arcmin_wgs84/main_outputs/region_boundaries_moirai_combined_3p1_0p5arcmin.shp')


readxl::read_excel('CostofLand_TotalCarbon.xlsx') %>% select(Region,`Area Conserved Land`) %>% na.omit() %>%
  rename(region=Region) %>% add_region_ID() %>% rename(reg_id=id) ->area

fortify(shp) %>% mutate(reg_id=as.numeric(id)+1) %>% left_join(area,by="reg_id") %>%
  mutate(`Area Conserved Land`=`Area Conserved Land`/1000) %>% 
  ggplot()+geom_polygon(aes(long,lat,group=group,fill=`Area Conserved Land`),color="black",lwd=.25)+
  theme_bw()+
  scale_fill_distiller(palette="YlGn",direction = 1)+
  labs(fill=stringr::str_wrap("Area Conserved (million km2)",width=20))+
  ylab("Latitude")+xlab("Longitude")


ggsave("conservedarea_map.jpg",dpi=300)
