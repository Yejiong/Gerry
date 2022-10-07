
library(sf)          # classes and functions for vector data
library(spData)        # load geographic data
library(dplyr)
library(tmap)    # for static and interactive maps
library(ggplot2) 
library(ggsci)
library(tools)
library(readr)

library(foreach)
library(doParallel)
registerDoParallel(detectCores())


process<-function(state){
  #state="ca"
  ######### read in congressional district map 2010
  file=paste("congressional2010/",state,"_cong_2012_to_2021/",state,"_cong_2012_to_2021.shp",sep="")
  cong = read_sf(file, quiet = TRUE)
  cong= st_transform(cong, "EPSG:3857")
  
  
  # read in block boundary
  file=paste("block2010/",state,"_2010_b_bound/",state,"_2010_b_bound.shp",sep="")
  block = read_sf(file, quiet = TRUE)
  block= st_transform(block, "EPSG:3857")
  
  
  ######### district affliation
  # centroids by blocks, output a vector
  blockcentroid=st_centroid(block$geometry)
  # district belonging matrix, output a sparse matrix
  belong_mat=matrix(NA,nrow=dim(block)[1],ncol=dim(cong)[1])
  
  belong_mat = foreach(d = 1:dim(cong)[1], .combine = 'cbind',.packages = "sf")%dopar%{
    st_within(blockcentroid, cong[d,], sparse = FALSE)
  }
  
  
  # add a new colum to the block dataset indicating dist
  dist = foreach(i = 1:dim(block)[1], .combine =c)%dopar%{
    ifelse(sum(belong_mat[i,]==TRUE)>0,which(belong_mat[i,]==TRUE), NA )
  }
  block$dist=dist
  
  ####### read in voters at block level
  file=paste("voter2021/",toupper(state),"_L2_Comm_2010BlockAgg/",toupper(state),"_l2_comm_block_agg_20210419.csv",sep="")
  voter=read_csv(file,col_types = cols(geoid = col_character())) 
  #names(voter)
  
  
  # link block with voter using geoid, voter live only in half of the region
  block = block%>%left_join(voter,by=c("GEOID"="geoid"))
  #View(block)
  # remove nonresident blocks
  block = block%>%filter(!is.na(total_reg))
  
  
  ###### calculate voting power: each district has 1 unit of power, divide it among the winners
  # each block's expected voting weight = winner's weight * prop winner + loser's weight * prop loser
  distpower=st_drop_geometry(block)%>%
    select(dist,party_dem,party_rep,total_reg)%>%
    group_by(dist)%>%
    summarise(dem=sum(party_dem), rep=sum(party_rep), tot=sum(total_reg))%>%
    ungroup()%>%mutate(rep.wt=ifelse(rep>dem, 1/rep, 0), 
                       dem.wt=ifelse(rep<dem, 1/dem, 0))
  
  blockpower=block%>%left_join(distpower, by="dist")%>%
    mutate(aa.power=dem.wt*(0.87+(1-0.87-0.07)/2)+rep.wt*(0.07+(1-0.87-0.07)/2),
           hisp.power=dem.wt*(0.63+(1-0.63-0.27)/2)+rep.wt*(0.27+(1-0.63-0.27)/2),
           white.power=dem.wt*(0.39+(1-0.54-0.39)/2)+rep.wt*(0.54+(1-0.54-0.39)/2))
  
  
  aa.power=foreach(i = 1:dim(blockpower)[1], .combine =c)%dopar%{
    if(blockpower$eth1_aa[i]>0){rep(blockpower$aa.power[i],blockpower$eth1_aa[i])}
  }
  hisp.power=foreach(i = 1:dim(blockpower)[1], .combine =c)%dopar%{
    if(blockpower$eth1_hisp[i]>0){rep(blockpower$hisp.power[i],blockpower$eth1_hisp[i])}
  }
  white.power=foreach(i = 1:dim(blockpower)[1], .combine =c)%dopar%{
    if(blockpower$eth1_eur[i]>0){rep(blockpower$white.power[i],blockpower$eth1_eur[i])}
  }
  
  
  ind.power=data.frame(power=c(aa.power,hisp.power,white.power),
                       race=c(rep("Black",length(aa.power)),rep("Hispanic",length(hisp.power)),rep("White",length(white.power))),
                       state=state)
  
  tx.ind.power=ind.power
  ca.ind.power=ind.power
  
  ind.power=rbind(tx.ind.power,ca.ind.power)
  
  ind.power%>%ggplot(aes(power,col=race,fill=race))+
    geom_histogram(aes(y=..density..), binwidth=0.0000001, position="dodge",alpha=0.1)+
    geom_density(alpha=0.1)+
    scale_fill_startrek()+scale_color_startrek()+
    xlim(c(0,0.00001))+
    theme_bw()+
    facet_grid(.~state,  scales = "fixed")
  
  saveRDS(ind.power,file="ind.power.rds")
  
 
  library(qte)
  temp=ind.power%>%mutate(T=ifelse(state=="tx",1,0))%>%filter(race=="White")
  jt.rand <- ci.qtet(power ~ T, data=temp, probs=seq(0.05, 0.95, 0.05), 
                     se=T, iters=10)
  summary(jt.rand)
  
  p1=ggqte(jt.rand)
  p2=ggqte(jt.rand)
  p3=ggqte(jt.rand)
  
  library(patchwork)
  p1 + p2 + p3  +
    plot_layout(ncol = 1, nrow = 3, widths = c(1,1), guides = "collect") 
  
  
  
  
  return(blockpower)
}
tx.data=process("tx")
tx.data[[2]]
tx.data[[3]]

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

library(qte)

jt.rand <- ci.qtet(block.power ~ T, data=temp, probs=seq(0.05,0.95,0.05), 
                   se=T, iters=10)
summary(jt.rand)
ggqte(jt.rand)







