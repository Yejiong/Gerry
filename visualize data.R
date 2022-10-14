



#######################################





# visualize power distributon by race
ind.data%>%ggplot(aes(power,col=race,fill=race))+
  geom_density(alpha=0.1)+
  scale_fill_startrek()+scale_color_startrek()+
  theme_bw()+
  xlim(c(0,0.00001))+
  facet_grid(state~.,  scales = "fixed")


geom_histogram(aes(y=..density..), binwidth=0.0000001, position="dodge",alpha=0.1)+
  

saveRDS(ind.power,file="ind.power.rds")









st_drop_geometry(blockpower)%>%
  select(race.group,aa.ratio,hisp.ratio,block.power)%>%
  ggplot()+
  geom_density(aes(x=block.power,y=..scaled..,group=race.group,col=race.group))+
  xlim(c(0,0.00001))

temp=st_drop_geometry(blockpower)%>%
  select(eth1_eur,eth1_aa,eth1_hisp,rep.wt,dem.wt, block.power,race.group)%>%
  filter(race.group %in% c("Hispanic","Others"))%>%
  filter(!is.na(block.power))%>%
  mutate(T=ifelse(race.group=="Hispanic",1,0))

View(temp)
#### visualize power
p2=tm_shape(blockpower) +
  tm_polygons("race.group")+
  tm_dots("block.power",size=0.01,alpha=0.1)+
  tm_shape(cong)+
  tm_borders(col='red')


p1=ggqte(jt.rand)
p2=ggqte(jt.rand)
p3=ggqte(jt.rand)

library(patchwork)
p1 + p2 + p3  +
  plot_layout(ncol = 1, nrow = 3, widths = c(1,1), guides = "collect") 







